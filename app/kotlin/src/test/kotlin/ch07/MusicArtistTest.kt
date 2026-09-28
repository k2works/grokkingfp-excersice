package ch07

import ch07.MusicGenre.HARD_ROCK
import ch07.MusicGenre.HEAVY_METAL
import ch07.MusicGenre.POP
import ch07.SearchCondition.SearchByActiveYears
import ch07.SearchCondition.SearchByGenre
import ch07.SearchCondition.SearchByOrigin
import ch07.YearsActive.ActiveBetween
import ch07.YearsActive.StillActive
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class MusicArtistTest : FunSpec({

    val metallica = Artist("Metallica", HEAVY_METAL, "U.S.", StillActive(1981))
    val ledZeppelin = Artist("Led Zeppelin", HARD_ROCK, "England", ActiveBetween(1968, 1980))
    val blackSabbath = Artist("Black Sabbath", HEAVY_METAL, "England", ActiveBetween(1968, 2017))
    val beatles = Artist("The Beatles", POP, "England", ActiveBetween(1960, 1970))
    val queen = Artist("Queen", HARD_ROCK, "England", StillActive(1970))
    val artists = listOf(metallica, ledZeppelin, blackSabbath, beatles, queen)

    context("直積型と直和型") {
        test("data class は copy で一部だけ変更した新しい値を作れる") {
            val retired = metallica.copy(yearsActive = ActiveBetween(1981, 2030))
            retired.yearsActive shouldBe ActiveBetween(1981, 2030)
            metallica.yearsActive shouldBe StillActive(1981)
        }

        test("data class は構造で等価性を判定する") {
            Artist("Metallica", HEAVY_METAL, "U.S.", StillActive(1981)) shouldBe metallica
        }
    }

    context("when によるパターンマッチング") {
        test("wasArtistActive は活動中のアーティストを判定する") {
            wasArtistActive(metallica, 1990, 2000) shouldBe true
            wasArtistActive(metallica, 1970, 1980) shouldBe false
        }

        test("wasArtistActive は活動期間が重なるかを判定する") {
            wasArtistActive(ledZeppelin, 1975, 1985) shouldBe true
            wasArtistActive(ledZeppelin, 1981, 1990) shouldBe false
            wasArtistActive(ledZeppelin, 1960, 1967) shouldBe false
        }

        test("activeLength は活動年数を返す") {
            activeLength(metallica, 2024) shouldBe 43
            activeLength(ledZeppelin, 2024) shouldBe 12
        }

        test("endYear は活動終了年を nullable 型で返す") {
            endYear(metallica) shouldBe null
            endYear(ledZeppelin) shouldBe 1980
        }

        test("describeActivity は活動状況を説明する") {
            describeActivity(metallica) shouldBe "Metallica has been active since 1981"
            describeActivity(ledZeppelin) shouldBe "Led Zeppelin was active from 1968 to 1980"
        }
    }

    context("検索条件のモデリング") {
        test("ジャンルで検索できる") {
            searchArtists(artists, listOf(SearchByGenre(listOf(HEAVY_METAL)))) shouldBe
                listOf(metallica, blackSabbath)
        }

        test("複数の条件はすべて満たすものを返す") {
            searchArtists(
                artists,
                listOf(SearchByOrigin(listOf("England")), SearchByActiveYears(1970, 1979)),
            ) shouldBe listOf(ledZeppelin, blackSabbath, beatles, queen)
        }

        test("条件が空ならすべてのアーティストを返す") {
            searchArtists(artists, emptyList()) shouldBe artists
        }

        test("searchArtistsAny はいずれかの条件を満たすものを返す") {
            searchArtistsAny(
                artists,
                listOf(SearchByGenre(listOf(POP)), SearchByOrigin(listOf("U.S."))),
            ) shouldBe listOf(metallica, beatles)
        }

        test("searchArtistsAny は条件が空なら空リストを返す") {
            searchArtistsAny(artists, emptyList()) shouldBe emptyList()
        }
    }
})
