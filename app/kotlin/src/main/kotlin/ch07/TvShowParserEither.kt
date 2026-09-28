package ch07

import arrow.core.Either
import arrow.core.NonEmptyList
import arrow.core.flatMap
import arrow.core.getOrElse
import arrow.core.left
import arrow.core.mapOrAccumulate
import arrow.core.raise.either
import arrow.core.recover
import arrow.core.right
import ch06.TvShow

/**
 * 第7章: Either による TV 番組のパース（失敗理由を保持する）
 */

// ============================================
// 小さな関数（Either を返す）
// ============================================

fun extractName(rawShow: String): Either<String, String> {
    val bracketOpen = rawShow.indexOf('(')
    return if (bracketOpen > 0) {
        rawShow.substring(0, bracketOpen).trim().right()
    } else {
        "Can't extract name from: $rawShow".left()
    }
}

fun extractYearStart(rawShow: String): Either<String, Int> {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    return if (bracketOpen != -1 && dash > bracketOpen + 1) {
        parseYear(rawShow.substring(bracketOpen + 1, dash), "start year")
    } else {
        "Can't extract start year from: $rawShow".left()
    }
}

fun extractYearEnd(rawShow: String): Either<String, Int> {
    val dash = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    return if (dash != -1 && bracketClose > dash + 1) {
        parseYear(rawShow.substring(dash + 1, bracketClose), "end year")
    } else {
        "Can't extract end year from: $rawShow".left()
    }
}

fun extractSingleYear(rawShow: String): Either<String, Int> {
    val dash = rawShow.indexOf('-')
    val bracketOpen = rawShow.indexOf('(')
    val bracketClose = rawShow.indexOf(')')
    return if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1) {
        parseYear(rawShow.substring(bracketOpen + 1, bracketClose), "single year")
    } else {
        "Can't extract single year from: $rawShow".left()
    }
}

/** nullable 型の結果に失敗理由を付けて Either にする */
fun parseYear(yearStr: String, context: String): Either<String, Int> =
    yearStr.trim().toIntOrNull()?.right() ?: "Can't parse $context: '$yearStr'".left()

// ============================================
// 小さな関数を組み立てる
// ============================================

/** either { } DSL で組み立てる */
fun parseShow(rawShow: String): Either<String, TvShow> = either {
    val name = extractName(rawShow).bind()
    val yearStart = extractYearStart(rawShow).getOrElse { extractSingleYear(rawShow).bind() }
    val yearEnd = extractYearEnd(rawShow).getOrElse { extractSingleYear(rawShow).bind() }
    TvShow(name, yearStart, yearEnd)
}

/** flatMap と recover で組み立てる */
fun parseShowWithFlatMap(rawShow: String): Either<String, TvShow> =
    extractName(rawShow).flatMap { name ->
        extractYearStart(rawShow).recover { extractSingleYear(rawShow).bind() }.flatMap { yearStart ->
            extractYearEnd(rawShow).recover { extractSingleYear(rawShow).bind() }.map { yearEnd ->
                TvShow(name, yearStart, yearEnd)
            }
        }
    }

// ============================================
// エラーハンドリング戦略
// ============================================

/** Best-effort 戦略: パースできたものだけを返す */
fun parseShowsBestEffort(rawShows: List<String>): List<TvShow> =
    rawShows.mapNotNull { parseShow(it).getOrNull() }

/** All-or-nothing 戦略: 最初のエラーで停止する */
fun parseShowsAllOrNothing(rawShows: List<String>): Either<String, List<TvShow>> = either {
    rawShows.map { parseShow(it).bind() }
}

/** エラー収集戦略: すべてのエラーを NonEmptyList に集める */
fun parseShowsCollectErrors(rawShows: List<String>): Either<NonEmptyList<String>, List<TvShow>> =
    rawShows.mapOrAccumulate { parseShow(it).bind() }
