package ch11

import arrow.fx.coroutines.parMap
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import java.util.concurrent.atomic.AtomicInteger

class CachedDataAccessTest : FunSpec({

    val london = Location(LocationId("Q84"), "London", 8_908_081)
    val towerBridge = Attraction("Tower Bridge", null, london)
    val queen = Artist("Queen", 2_050_559)
    val insideOut = Movie("Inside Out", 857_611_174)

    // 呼び出し回数を数えるだけの DataAccess
    class CountingDataAccess : DataAccess {
        val attractionCalls = AtomicInteger(0)
        val artistCalls = AtomicInteger(0)
        val movieCalls = AtomicInteger(0)

        override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int): List<Attraction> {
            attractionCalls.incrementAndGet()
            return listOf(towerBridge.copy(name = "$name-$limit"))
        }

        override suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int): List<Artist> {
            artistCalls.incrementAndGet()
            return listOf(queen)
        }

        override suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int): List<Movie> {
            movieCalls.incrementAndGet()
            return listOf(insideOut)
        }
    }

    test("同じクエリは 2 回目以降キャッシュから返される") {
        val underlying = CountingDataAccess()
        val cached = CachedDataAccess.create(underlying)

        cached.findAttractions("Bridge", AttractionOrdering.ByName, 10)
        underlying.attractionCalls.get() shouldBe 1

        cached.findAttractions("Bridge", AttractionOrdering.ByName, 10)
        underlying.attractionCalls.get() shouldBe 1

        cached.findAttractions("Tower", AttractionOrdering.ByName, 10)
        underlying.attractionCalls.get() shouldBe 2
    }

    test("キャッシュされた結果は元の DataAccess の結果と等しい") {
        val underlying = CountingDataAccess()
        val cached = CachedDataAccess.create(underlying)

        val first = cached.findAttractions("Bridge", AttractionOrdering.ByName, 3)
        val second = cached.findAttractions("Bridge", AttractionOrdering.ByName, 3)

        first shouldBe underlying.findAttractions("Bridge", AttractionOrdering.ByName, 3)
        second shouldBe first
    }

    test("パラメータが異なれば別のキャッシュエントリになる") {
        val cached = CachedDataAccess.create(CountingDataAccess())

        cached.findAttractions("Bridge", AttractionOrdering.ByName, 3)
        cached.findAttractions("Bridge", AttractionOrdering.ByLocationPopulation, 3)
        cached.findAttractions("Bridge", AttractionOrdering.ByName, 5)

        cached.cacheSize() shouldBe 3
    }

    test("アーティストと映画もキャッシュされる") {
        val underlying = CountingDataAccess()
        val cached = CachedDataAccess.create(underlying)

        repeat(3) {
            cached.findArtistsFromLocation(london.id, 2) shouldBe listOf(queen)
            cached.findMoviesAboutLocation(london.id, 2) shouldBe listOf(insideOut)
        }

        underlying.artistCalls.get() shouldBe 1
        underlying.movieCalls.get() shouldBe 1
        cached.cacheSize() shouldBe 2
    }

    test("clearCache でキャッシュを空にすると再び元の DataAccess が呼ばれる") {
        val underlying = CountingDataAccess()
        val cached = CachedDataAccess.create(underlying)

        cached.findArtistsFromLocation(london.id, 2)
        cached.clearCache()
        cached.cacheSize() shouldBe 0

        cached.findArtistsFromLocation(london.id, 2)
        underlying.artistCalls.get() shouldBe 2
    }

    test("並行に同じクエリを実行してもキャッシュは壊れない") {
        val cached = CachedDataAccess.create(CountingDataAccess())

        val results = (1..100).toList().parMap { cached.findAttractions("Bridge", AttractionOrdering.ByName, it % 5) }

        results.toSet().size shouldBe 5
        cached.cacheSize() shouldBe 5
    }
})
