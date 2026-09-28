package ch12

import arrow.fx.coroutines.resource
import arrow.fx.coroutines.resourceScope
import ch11.Artist
import ch11.Attraction
import ch11.AttractionOrdering
import ch11.CachedDataAccess
import ch11.GOOD_GUIDE_MIN_SCORE
import ch11.Guide
import ch11.Location
import ch11.LocationId
import ch11.Movie
import ch11.PopCultureSubject
import ch11.findBestGuide
import ch11.findGoodGuide
import ch11.guideScore
import ch11.travelGuideV3
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.collections.shouldBeIn
import io.kotest.matchers.ints.shouldBeInRange
import io.kotest.matchers.shouldBe
import io.kotest.property.Arb
import io.kotest.property.arbitrary.Codepoint
import io.kotest.property.arbitrary.alphanumeric
import io.kotest.property.arbitrary.arbitrary
import io.kotest.property.arbitrary.bind
import io.kotest.property.arbitrary.boolean
import io.kotest.property.arbitrary.int
import io.kotest.property.arbitrary.list
import io.kotest.property.arbitrary.orNull
import io.kotest.property.arbitrary.string
import io.kotest.property.checkAll
import io.kotest.property.forAll
import java.util.concurrent.atomic.AtomicInteger

// ============================================
// ジェネレータ（小さな Arb を合成して大きな Arb を作る）
// ============================================

val nonNegativeInt: Arb<Int> = Arb.int(0..Int.MAX_VALUE)

val identifier: Arb<String> = Arb.string(1..10, Codepoint.alphanumeric())

val randomArtist: Arb<Artist> = arbitrary { Artist(identifier.bind(), nonNegativeInt.bind()) }

val randomMovie: Arb<Movie> = Arb.bind(identifier, nonNegativeInt) { name, boxOffice -> Movie(name, boxOffice) }

val randomArtists: Arb<List<Artist>> = Arb.list(randomArtist, 0..100)

val randomMovies: Arb<List<Movie>> = Arb.list(randomMovie, 0..100)

val randomPopCultureSubjects: Arb<List<PopCultureSubject>> = arbitrary { randomMovies.bind() + randomArtists.bind() }

val randomLocation: Arb<Location> = Arb.bind(identifier, identifier, Arb.int(0..10_000_000)) { id, name, population ->
    Location(LocationId("Q$id"), name, population)
}

val randomAttraction: Arb<Attraction> = Arb.bind(identifier, identifier.orNull(), randomLocation) { name, desc, loc ->
    Attraction(name, desc, loc)
}

val randomGuide: Arb<Guide> = Arb.bind(randomAttraction, randomPopCultureSubjects) { attraction, subjects ->
    Guide(attraction, subjects)
}

class PropertyBasedTest : FunSpec({

    val wyoming = Location(LocationId("Q1214"), "Wyoming", 586_107)
    fun yellowstone(description: String?) = Attraction("Yellowstone National Park", description, wyoming)

    context("guideScore のプロパティ") {
        test("スコアはアトラクションの名前と説明の文字列に依存しない") {
            checkAll(Arb.string(), Arb.string()) { name, description ->
                val guide = Guide(
                    Attraction(name, description, wyoming),
                    listOf(Movie("The Hateful Eight", 155_760_117), Movie("Heaven's Gate", 3_484_331)),
                )
                guideScore(guide) shouldBe 65
            }
        }

        test("説明があり興行収入 0 の映画だけなら、スコアは 30 以上 70 以下") {
            checkAll(Arb.int(0..127)) { amountOfMovies ->
                val guide = Guide(yellowstone("description"), List(amountOfMovies) { Movie("Random Movie", 0) })
                guideScore(guide) shouldBeInRange 30..70
            }
        }

        test("説明がなくアーティストと映画が 1 つずつなら、スコアは 20 以上 50 以下") {
            checkAll(nonNegativeInt, nonNegativeInt) { followers, boxOffice ->
                val guide = Guide(yellowstone(null), listOf(Artist("Chris LeDoux", followers), Movie("Movie", boxOffice)))
                guideScore(guide) shouldBeInRange 20..50
            }
        }

        test("アーティストが 1 人だけなら、スコアは 10 以上 25 以下") {
            checkAll(randomArtist) { artist ->
                guideScore(Guide(yellowstone(null), listOf(artist))) shouldBeInRange 10..25
            }
        }

        test("説明も映画もなくアーティストだけなら、スコアは 0 以上 55 以下") {
            checkAll(randomArtists) { artists ->
                guideScore(Guide(yellowstone(null), artists)) shouldBeInRange 0..55
            }
        }

        test("ポップカルチャーの題材だけなら、スコアは 0 以上 70 以下") {
            checkAll(randomPopCultureSubjects) { subjects ->
                guideScore(Guide(yellowstone(null), subjects)) shouldBeInRange 0..70
            }
        }

        test("どんなガイドでもスコアは 0 以上 100 以下") {
            forAll(randomGuide) { guide -> guideScore(guide) in 0..100 }
        }

        test("説明ありのスコアは説明なしより 30 点高い") {
            checkAll(randomAttraction, randomPopCultureSubjects) { attraction, subjects ->
                val withDescription = Guide(attraction.copy(description = "description"), subjects)
                val withoutDescription = Guide(attraction.copy(description = null), subjects)
                guideScore(withDescription) - guideScore(withoutDescription) shouldBe 30
            }
        }

        test("Int のまま合計するとオーバーフローするので Long で合計する") {
            val followers = listOf(Int.MAX_VALUE, Int.MAX_VALUE)
            followers.sum() shouldBe -2
            followers.sumOf { it.toLong() } shouldBe 4_294_967_294L
        }
    }

    context("純粋関数のプロパティ") {
        test("guideScore は同じ入力に対して常に同じ出力を返す（参照透過性）") {
            forAll(randomGuide) { guide -> guideScore(guide) == guideScore(guide) }
        }

        test("findBestGuide の結果は入力に含まれ、他のどのガイドよりスコアが低くない") {
            checkAll(Arb.list(randomGuide, 1..5)) { guides ->
                val best = checkNotNull(findBestGuide(guides))
                best shouldBeIn guides
                guides.all { guideScore(best) >= guideScore(it) } shouldBe true
            }
        }

        test("findGoodGuide は閾値を超えるなら Right、そうでなければすべてのガイドを含む Left を返す") {
            checkAll(Arb.list(randomGuide, 0..5)) { guides ->
                findGoodGuide(guides).fold(
                    ifLeft = { report -> report.badGuides shouldBe guides },
                    ifRight = { guide -> (guideScore(guide) > GOOD_GUIDE_MIN_SCORE) shouldBe true },
                )
            }
        }
    }

    context("スタブを使った travelGuide のプロパティ") {
        test("見つかるガイドは上位 3 件のアトラクションのいずれかである") {
            checkAll(Arb.list(randomAttraction, 0..10), randomMovies) { attractions, movies ->
                val dataAccess = TestDataAccess.of(attractions = attractions, movies = movies)

                travelGuideV3(dataAccess, "any").fold(
                    ifLeft = { report -> report.badGuides.map { it.attraction } shouldBe attractions.take(3) },
                    ifRight = { guide -> guide.attraction shouldBeIn attractions.take(3) },
                )
            }
        }
    }

    context("キャッシュのプロパティ") {
        test("キャッシュ付きでも結果は変わらず、元の呼び出しはクエリの種類の数だけになる") {
            val randomQuery = Arb.bind(identifier, Arb.int(1..3)) { name, limit -> name to limit }
            checkAll(Arb.list(randomQuery, 0..30)) { queries ->
                val calls = AtomicInteger(0)
                val underlying = TestDataAccess(attractions = { name ->
                    calls.incrementAndGet()
                    listOf(Attraction(name, null, wyoming))
                })
                val cached = CachedDataAccess.create(underlying)

                queries.forEach { (name, limit) ->
                    cached.findAttractions(name, AttractionOrdering.ByName, limit) shouldBe
                        listOf(Attraction(name, null, wyoming))
                }

                calls.get() shouldBe queries.distinct().size
            }
        }
    }

    context("Resource のプロパティ") {
        test("任意の数のリソースは、処理の成否にかかわらず取得と逆の順序で解放される") {
            checkAll(Arb.list(identifier, 0..10), Arb.boolean()) { names, shouldFail ->
                val acquired = mutableListOf<String>()
                val released = mutableListOf<String>()
                fun tracked(name: String) = resource {
                    install({ acquired += name; name }) { n, _ -> released += n }
                }

                runCatching {
                    resourceScope {
                        names.forEach { tracked(it).bind() }
                        if (shouldFail) error("failure while using resources")
                    }
                }

                released shouldBe acquired.reversed()
                acquired shouldBe names
            }
        }
    }
})
