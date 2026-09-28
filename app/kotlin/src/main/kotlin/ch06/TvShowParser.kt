package ch06

/**
 * 第6章: nullable 型による TV 番組のパース
 *
 * 入力例: "Breaking Bad (2008-2013)" -> TvShow("Breaking Bad", 2008, 2013)
 */

// ============================================
// 例外を使う方法（問題あり）
// ============================================

fun parseShowUnsafe(rawShow: String): TvShow {
    val bracketOpen = rawShow.indexOf('(')
    val bracketClose = rawShow.indexOf(')')
    val dash = rawShow.indexOf('-')

    val name = rawShow.substring(0, bracketOpen).trim()
    val yearStart = rawShow.substring(bracketOpen + 1, dash).toInt()
    val yearEnd = rawShow.substring(dash + 1, bracketClose).toInt()

    return TvShow(name, yearStart, yearEnd)
}

// ============================================
// 小さな関数（nullable 型を返す）
// ============================================

fun extractName(rawShow: String): String? {
    val bracketOpen = rawShow.indexOf('(')
    return if (bracketOpen > 0) rawShow.substring(0, bracketOpen).trim() else null
}

fun extractYearStart(rawShow: String): Int? {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    return if (bracketOpen != -1 && dash > bracketOpen + 1) {
        parseYear(rawShow.substring(bracketOpen + 1, dash))
    } else {
        null
    }
}

fun extractYearEnd(rawShow: String): Int? {
    val dash = rawShow.indexOf('-')
    val bracketClose = rawShow.indexOf(')')
    return if (dash != -1 && bracketClose > dash + 1) {
        parseYear(rawShow.substring(dash + 1, bracketClose))
    } else {
        null
    }
}

fun extractSingleYear(rawShow: String): Int? {
    val dash = rawShow.indexOf('-')
    val bracketOpen = rawShow.indexOf('(')
    val bracketClose = rawShow.indexOf(')')
    return if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1) {
        parseYear(rawShow.substring(bracketOpen + 1, bracketClose))
    } else {
        null
    }
}

fun parseYear(yearStr: String): Int? = yearStr.trim().toIntOrNull()

// ============================================
// 小さな関数を組み立てる
// ============================================

/**
 * エルビス演算子 ?: と早期リターンで組み立てる
 */
fun parseShow(rawShow: String): TvShow? {
    val name = extractName(rawShow) ?: return null
    val yearStart = extractYearStart(rawShow) ?: extractSingleYear(rawShow) ?: return null
    val yearEnd = extractYearEnd(rawShow) ?: extractSingleYear(rawShow) ?: return null
    return TvShow(name, yearStart, yearEnd)
}

/**
 * ?.let の連鎖で組み立てる（flatMap のネストに相当）
 */
fun parseShowWithLet(rawShow: String): TvShow? =
    extractName(rawShow)?.let { name ->
        (extractYearStart(rawShow) ?: extractSingleYear(rawShow))?.let { yearStart ->
            (extractYearEnd(rawShow) ?: extractSingleYear(rawShow))?.let { yearEnd ->
                TvShow(name, yearStart, yearEnd)
            }
        }
    }

// ============================================
// エラーハンドリング戦略
// ============================================

/**
 * Best-effort 戦略: パースできたものだけを返す
 */
fun parseShowsBestEffort(rawShows: List<String>): List<TvShow> =
    rawShows.mapNotNull(::parseShow)

/**
 * All-or-nothing 戦略: 1 つでも失敗したら null
 */
fun parseShowsAllOrNothing(rawShows: List<String>): List<TvShow>? {
    val shows = rawShows.mapNotNull(::parseShow)
    return shows.takeIf { it.size == rawShows.size }
}

/**
 * All-or-nothing 戦略を fold で書いたもの（Java 版 / Scala 版の foldLeft に相当）
 */
fun parseShowsAllOrNothingFold(rawShows: List<String>): List<TvShow>? =
    rawShows.fold<String, List<TvShow>?>(emptyList()) { acc, rawShow ->
        acc?.let { shows -> parseShow(rawShow)?.let { show -> shows + show } }
    }
