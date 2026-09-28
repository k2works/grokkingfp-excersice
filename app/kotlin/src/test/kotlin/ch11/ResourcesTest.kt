package ch11

import arrow.core.left
import arrow.fx.coroutines.ExitCase
import arrow.fx.coroutines.resource
import arrow.fx.coroutines.resourceScope
import arrow.fx.coroutines.use
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeInstanceOf

class ResourcesTest : FunSpec({

    val towerBridge = Attraction("Tower Bridge", null, Location(LocationId("Q84"), "London", 8_908_081))

    // 接続を受け取って DataAccess を作るだけのスタブ
    fun stubDataAccess(connection: Connection): DataAccess = object : DataAccess {
        override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int) =
            connection.query { listOf(towerBridge) }

        override suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int) =
            connection.query { emptyList<Artist>() }

        override suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int) =
            connection.query { emptyList<Movie>() }
    }

    context("connectionResource") {
        test("use の中では接続が開いていて、終了後に閉じられる") {
            lateinit var captured: Connection

            val result = connectionResource("https://example.org/sparql").use { connection ->
                captured = connection
                connection.isOpen shouldBe true
                connection.address
            }

            result shouldBe "https://example.org/sparql"
            captured.isOpen shouldBe false
        }

        test("処理が失敗しても接続は閉じられる") {
            lateinit var captured: Connection

            shouldThrow<IllegalStateException> {
                connectionResource("https://example.org/sparql").use { connection ->
                    captured = connection
                    error("query failed")
                }
            }

            captured.isOpen shouldBe false
        }

        test("閉じた接続でクエリを実行すると失敗する") {
            val connection = connectionResource("https://example.org/sparql").use { it }

            shouldThrow<IllegalStateException> { connection.query { 1 } }
        }
    }

    context("install と ExitCase") {
        test("解放処理には終了の理由（ExitCase）が渡される") {
            val exitCases = mutableListOf<ExitCase>()
            val tracked = resource {
                install({ "resource" }) { _, exitCase -> exitCases += exitCase }
            }

            tracked.use { it.length }
            runCatching { tracked.use { error("boom") } }

            exitCases[0] shouldBe ExitCase.Completed
            exitCases[1].shouldBeInstanceOf<ExitCase.Failure>()
        }

        test("複数のリソースは取得と逆の順序で解放される") {
            val events = mutableListOf<String>()
            fun tracked(name: String) = resource {
                install({ events += "acquire $name"; name }) { _, _ -> events += "release $name" }
            }

            resourceScope {
                val a = tracked("A").bind()
                val b = tracked("B").bind()
                events += "use $a$b"
            }

            events shouldBe listOf("acquire A", "acquire B", "use AB", "release B", "release A")
        }
    }

    context("アプリケーションの組み立て") {
        test("dataAccessResource は接続を管理しながら DataAccess を提供する") {
            lateinit var captured: DataAccess

            val attractions = dataAccessResource("https://example.org/sparql", ::stubDataAccess).use { dataAccess ->
                captured = dataAccess
                dataAccess.findAttractions("Tower Bridge", AttractionOrdering.ByName, 1)
            }

            attractions shouldBe listOf(towerBridge)
            // キャッシュにないクエリは閉じた接続に届くので失敗する
            shouldThrow<IllegalStateException> {
                captured.findAttractions("Big Ben", AttractionOrdering.ByName, 1)
            }
        }

        test("dataAccessResource が提供する DataAccess はキャッシュ付きである") {
            dataAccessResource("https://example.org/sparql", ::stubDataAccess).use { dataAccess ->
                dataAccess.shouldBeInstanceOf<CachedDataAccess>()
            }
        }

        test("runTravelGuide はリソースを使ってガイドを検索し、良いガイドがなければ SearchReport を返す") {
            val result = runTravelGuide("https://example.org/sparql", ::stubDataAccess, "Tower Bridge")

            result shouldBe SearchReport(listOf(Guide(towerBridge, emptyList())), emptyList()).left()
        }
    }
})
