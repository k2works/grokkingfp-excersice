package ch11

import arrow.core.left
import arrow.core.right
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class TravelGuideTest : FunSpec({

    val wyoming = Location(LocationId("Q1214"), "Wyoming", 586_107)
    val yellowstone = Attraction("Yellowstone National Park", "first national park in the world", wyoming)
    val yellowstoneNoDesc = yellowstone.copy(description = null)
    val hatefulEight = Movie("The Hateful Eight", 155_760_117)
    val heavensGate = Movie("Heaven's Gate", 3_484_331)
    val chrisLeDoux = Artist("Chris LeDoux", 125_000)

    context("ドメインモデル") {
        test("LocationId は value class として値で比較される") {
            LocationId("Q84") shouldBe LocationId("Q84")
        }

        test("data class の copy で一部だけ変えた新しい値を作れる") {
            val renamed = yellowstone.copy(name = "Yellowstone")
            renamed.name shouldBe "Yellowstone"
            renamed.location shouldBe yellowstone.location
            yellowstone.name shouldBe "Yellowstone National Park"
        }

        test("PopCultureSubject は Artist と Movie のどちらかである") {
            val subjects: List<PopCultureSubject> = listOf(chrisLeDoux, hatefulEight)
            val kinds = subjects.map {
                when (it) {
                    is Artist -> "artist"
                    is Movie -> "movie"
                }
            }
            kinds shouldBe listOf("artist", "movie")
        }
    }

    context("guideScore") {
        test("説明あり、アーティスト 0 人、人気映画 2 本のガイドは 65 点") {
            // 30（説明）+ 0（アーティスト）+ 20（映画 2 本）+ 15（興行収入 1.59 億ドル）
            guideScore(Guide(yellowstone, listOf(hatefulEight, heavensGate))) shouldBe 65
        }

        test("説明なし、題材なしのガイドは 0 点") {
            guideScore(Guide(yellowstoneNoDesc, emptyList())) shouldBe 0
        }

        test("説明なし、興行収入 0 の映画 2 本のガイドは 20 点") {
            val guide = Guide(yellowstoneNoDesc, listOf(Movie("A", 0), Movie("B", 0)))
            guideScore(guide) shouldBe 20
        }

        test("アーティストのフォロワー数は 10 万人ごとに 1 点") {
            // 0（説明）+ 10（1 人）+ 1（125,000 フォロワー）
            guideScore(Guide(yellowstoneNoDesc, listOf(chrisLeDoux))) shouldBe 11
        }

        test("題材の数による点数は最大 40 点") {
            val movies = List(10) { Movie("Movie$it", 0) }
            guideScore(Guide(yellowstoneNoDesc, movies)) shouldBe 40
        }

        test("フォロワー数が Int の最大値でもオーバーフローしない") {
            val artists = List(100) { Artist("Artist$it", Int.MAX_VALUE) }
            // 40（題材の数）+ 15（フォロワー数の上限）
            guideScore(Guide(yellowstoneNoDesc, artists)) shouldBe 55
        }
    }

    context("findBestGuide と findGoodGuide") {
        val good = Guide(yellowstone, listOf(hatefulEight, heavensGate))
        val bad = Guide(yellowstoneNoDesc, emptyList())

        test("findBestGuide は最高スコアのガイドを返す") {
            findBestGuide(listOf(bad, good)) shouldBe good
        }

        test("findBestGuide は空のリストに対して null を返す") {
            findBestGuide(emptyList()) shouldBe null
        }

        test("findGoodGuide はスコアが 55 を超えるガイドを Right で返す") {
            findGoodGuide(listOf(bad, good)) shouldBe good.right()
        }

        test("findGoodGuide は良いガイドがなければ SearchReport を Left で返す") {
            findGoodGuide(listOf(bad), listOf("problem")) shouldBe
                SearchReport(listOf(bad), listOf("problem")).left()
        }
    }

    context("travelGuide の各バージョン") {
        val london = Location(LocationId("Q84"), "London", 8_908_081)
        val towerBridge = Attraction("Tower Bridge", null, london)
        val queen = Artist("Queen", 2_050_559)

        val dataAccess = object : DataAccess {
            override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int) =
                listOf(towerBridge, yellowstone).take(limit)

            override suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int) =
                if (locationId == london.id) listOf(queen) else emptyList()

            override suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int) =
                if (locationId == wyoming.id) listOf(hatefulEight, heavensGate) else emptyList()
        }

        test("V1 は最初のアトラクションのガイドを返す") {
            travelGuideV1(dataAccess, "Tower Bridge") shouldBe Guide(towerBridge, listOf(queen))
        }

        test("V1 はアトラクションが見つからなければ null を返す") {
            val empty = object : DataAccess {
                override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int) =
                    emptyList<Attraction>()

                override suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int) =
                    emptyList<Artist>()

                override suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int) =
                    emptyList<Movie>()
            }
            travelGuideV1(empty, "Nowhere") shouldBe null
        }

        test("V2 は複数の候補から最高スコアのガイドを返す") {
            travelGuideV2(dataAccess, "any") shouldBe Guide(yellowstone, listOf(hatefulEight, heavensGate))
        }

        test("guideForAttraction はアーティストと映画を並列に取得してガイドを作る") {
            guideForAttraction(dataAccess, yellowstone) shouldBe
                Guide(yellowstone, listOf(hatefulEight, heavensGate))
        }

        test("V3 は良いガイドがあれば Right を返す") {
            travelGuideV3(dataAccess, "any") shouldBe
                Guide(yellowstone, listOf(hatefulEight, heavensGate)).right()
        }

        test("V3 はアトラクションの取得に失敗すると問題を含む SearchReport を返す") {
            val failing = object : DataAccess by dataAccess {
                override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int): List<Attraction> =
                    throw RuntimeException("fetching failed")
            }
            travelGuideV3(failing, "any") shouldBe SearchReport(emptyList(), listOf("fetching failed")).left()
        }
    }
})
