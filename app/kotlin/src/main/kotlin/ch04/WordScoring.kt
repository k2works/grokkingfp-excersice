package ch04

/**
 * 第4章: ワードスコアリング
 *
 * スコア関数を値として渡し、ランキングのロジックを組み替える例
 */

/** 基本スコア: 'a' を除いた文字数 */
fun score(word: String): Int = word.replace("a", "").length

/** ボーナス: 'c' を含むと 5 */
fun bonus(word: String): Int = if (word.contains("c")) 5 else 0

/** ペナルティ: 's' を含むと 7 */
fun penalty(word: String): Int = if (word.contains("s")) 7 else 0

/** スコア関数を受け取ってランキングを作成 */
fun rankedWords(wordScore: (String) -> Int, words: List<String>): List<String> =
    words.sortedBy(wordScore).reversed()

/** 拡張関数版のランキング - 末尾ラムダでスコア関数を渡せる */
fun List<String>.rankedBy(wordScore: (String) -> Int): List<String> =
    sortedBy(wordScore).reversed()

/** スコア付きのランキングを返す */
fun rankedWordsWithScore(wordScore: (String) -> Int, words: List<String>): List<String> =
    rankedWords(wordScore, words).map { "$it: ${wordScore(it)}" }

/** 閾値以上のスコアを持つ単語を抽出 */
fun highScoringWords(wordScore: (String) -> Int, words: List<String>, threshold: Int): List<String> =
    words.filter { wordScore(it) >= threshold }

/** カリー化版: スコア関数 → 閾値 → 単語リスト の順に受け取り、閾値より大きい単語を返す */
fun highScoringWordsCurried(wordScore: (String) -> Int): (Int) -> (List<String>) -> List<String> =
    { higherThan -> { words -> words.filter { wordScore(it) > higherThan } } }

/** fold で合計スコアを計算 */
fun cumulativeScore(wordScore: (String) -> Int, words: List<String>): Int =
    words.fold(0) { total, word -> total + wordScore(word) }
