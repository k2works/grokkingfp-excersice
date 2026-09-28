package ch12

import arrow.core.left
import arrow.core.right
import ch11.Artist
import ch11.Attraction
import ch11.AttractionOrdering
import ch11.Guide
import ch11.Location
import ch11.LocationId
import ch11.Movie
import ch11.SearchReport
import ch11.travelGuideV1
import ch11.travelGuideV3
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class TestDataAccessTest : FunSpec({

    val london = Location(LocationId("Q84"), "London", 8_908_081)
    val towerBridge = Attraction("Tower Bridge", null, london)
    val queen = Artist("Queen", 2_050_559)
    val insideOut = Movie("Inside Out", 857_611_174)

    val yellowstone = Attraction(
        "Yellowstone National Park",
        "first national park in the world",
        Location(LocationId("Q1214"), "Wyoming", 586_107),
    )
    val yosemite = Attraction(
        "Yosemite National Park",
        "national park in California, United States",
        Location(LocationId("Q109661"), "Madera County", 157_327),
    )
    val hatefulEight = Movie("The Hateful Eight", 155_760_117)
    val heavensGate = Movie("Heaven's Gate", 3_484_331)

    context("TestDataAccess スタブ") {
        test("何も指定しなければ空のリストを返す") {
            val dataAccess = TestDataAccess()

            dataAccess.findAttractions("any", AttractionOrdering.ByName, 10) shouldBe emptyList()
            dataAccess.findArtistsFromLocation(london.id, 10) shouldBe emptyList()
            dataAccess.findMoviesAboutLocation(london.id, 10) shouldBe emptyList()
        }

        test("of で指定したデータを返す") {
            val dataAccess = TestDataAccess.of(
                attractions = listOf(towerBridge),
                artists = listOf(queen),
                movies = listOf(insideOut),
            )

            dataAccess.findAttractions("any", AttractionOrdering.ByName, 10) shouldBe listOf(towerBridge)
            dataAccess.findArtistsFromLocation(london.id, 10) shouldBe listOf(queen)
            dataAccess.findMoviesAboutLocation(london.id, 10) shouldBe listOf(insideOut)
        }

        test("limit を超える件数は返さない") {
            val dataAccess = TestDataAccess.of(attractions = listOf(towerBridge, yellowstone, yosemite))

            dataAccess.findAttractions("any", AttractionOrdering.ByName, 2) shouldBe listOf(towerBridge, yellowstone)
        }

        test("失敗をシミュレートできる") {
            val dataAccess = TestDataAccess(attractions = { failWith("Network error") })

            val error = shouldThrow<RuntimeException> {
                dataAccess.findAttractions("any", AttractionOrdering.ByName, 10)
            }
            error.message shouldBe "Network error"
        }

        test("ロケーションごとに異なる結果を返せる") {
            val dataAccess = TestDataAccess(
                attractions = { listOf(towerBridge) },
                artists = { locationId -> if (locationId == london.id) listOf(queen) else emptyList() },
            )

            travelGuideV1(dataAccess, "Tower Bridge") shouldBe Guide(towerBridge, listOf(queen))
        }
    }

    context("SearchReport を返す travelGuide") {
        test("良いガイドが見つからなければ、悪いガイドを含む SearchReport を返す") {
            val dataAccess = TestDataAccess.of(attractions = listOf(yellowstone))

            travelGuideV3(dataAccess, "") shouldBe
                SearchReport(badGuides = listOf(Guide(yellowstone, emptyList())), problems = emptyList()).left()
        }

        test("映画が 2 本あれば良いガイドを返す") {
            val dataAccess = TestDataAccess.of(
                attractions = listOf(yellowstone),
                movies = listOf(hatefulEight, heavensGate),
            )

            travelGuideV3(dataAccess, "") shouldBe Guide(yellowstone, listOf(hatefulEight, heavensGate)).right()
        }

        test("アトラクションの取得に失敗したら、問題を含む SearchReport を返す") {
            val dataAccess = TestDataAccess(attractions = { failWith("fetching failed") })

            travelGuideV3(dataAccess, "") shouldBe
                SearchReport(badGuides = emptyList(), problems = listOf("fetching failed")).left()
        }

        test("一部のアーティスト取得に失敗しても、残りのガイドと問題を SearchReport で返す") {
            val dataAccess = TestDataAccess(
                attractions = { listOf(yosemite, yellowstone) },
                artists = { locationId ->
                    if (locationId == yosemite.location.id) failWith("Yosemite artists fetching failed") else emptyList()
                },
            )

            travelGuideV3(dataAccess, "") shouldBe SearchReport(
                badGuides = listOf(Guide(yellowstone, emptyList())),
                problems = listOf("Yosemite artists fetching failed"),
            ).left()
        }
    }
})
