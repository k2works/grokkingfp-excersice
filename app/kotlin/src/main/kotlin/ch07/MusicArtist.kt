package ch07

import ch07.SearchCondition.SearchByActiveYears
import ch07.SearchCondition.SearchByGenre
import ch07.SearchCondition.SearchByOrigin
import ch07.YearsActive.ActiveBetween
import ch07.YearsActive.StillActive

/**
 * 第7章: 代数的データ型（ADT）による音楽アーティストのモデリング
 */

enum class MusicGenre { HEAVY_METAL, POP, HARD_ROCK, JAZZ, CLASSICAL }

/** 直和型: 活動中か、活動期間が終わっているかのどちらか */
sealed interface YearsActive {
    data class StillActive(val since: Int) : YearsActive
    data class ActiveBetween(val start: Int, val end: Int) : YearsActive
}

/** 直積型: 名前、ジャンル、出身地、活動期間の組み合わせ */
data class Artist(
    val name: String,
    val genre: MusicGenre,
    val origin: String,
    val yearsActive: YearsActive,
)

// ============================================
// when によるパターンマッチング
// ============================================

fun wasArtistActive(artist: Artist, yearStart: Int, yearEnd: Int): Boolean =
    when (val active = artist.yearsActive) {
        is StillActive -> active.since <= yearEnd
        is ActiveBetween -> active.start <= yearEnd && active.end >= yearStart
    }

fun activeLength(artist: Artist, currentYear: Int): Int =
    when (val active = artist.yearsActive) {
        is StillActive -> currentYear - active.since
        is ActiveBetween -> active.end - active.start
    }

fun endYear(artist: Artist): Int? =
    when (val active = artist.yearsActive) {
        is StillActive -> null
        is ActiveBetween -> active.end
    }

fun describeActivity(artist: Artist): String =
    when (val active = artist.yearsActive) {
        is StillActive -> "${artist.name} has been active since ${active.since}"
        is ActiveBetween -> "${artist.name} was active from ${active.start} to ${active.end}"
    }

// ============================================
// 検索条件のモデリング
// ============================================

sealed interface SearchCondition {
    data class SearchByGenre(val genres: List<MusicGenre>) : SearchCondition
    data class SearchByOrigin(val locations: List<String>) : SearchCondition
    data class SearchByActiveYears(val start: Int, val end: Int) : SearchCondition
}

fun matchesCondition(artist: Artist, condition: SearchCondition): Boolean =
    when (condition) {
        is SearchByGenre -> artist.genre in condition.genres
        is SearchByOrigin -> artist.origin in condition.locations
        is SearchByActiveYears -> wasArtistActive(artist, condition.start, condition.end)
    }

/** すべての条件を満たすアーティストを返す（all は forall 相当） */
fun searchArtists(artists: List<Artist>, requiredConditions: List<SearchCondition>): List<Artist> =
    artists.filter { artist ->
        requiredConditions.all { condition -> matchesCondition(artist, condition) }
    }

/** いずれかの条件を満たすアーティストを返す（any は exists 相当） */
fun searchArtistsAny(artists: List<Artist>, conditions: List<SearchCondition>): List<Artist> =
    artists.filter { artist ->
        conditions.any { condition -> matchesCondition(artist, condition) }
    }
