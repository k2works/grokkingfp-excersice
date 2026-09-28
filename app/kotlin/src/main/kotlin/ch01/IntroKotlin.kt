package ch01

/**
 * 第1章: 関数型プログラミング入門
 *
 * 命令型プログラミングと関数型プログラミングの違いを学びます。
 * Kotlin ではクラスに属さないトップレベル関数をそのまま定義できます。
 */

// ============================================
// 命令型プログラミング: HOW（どうやるか）を記述
// ============================================

/** 命令型スタイルでワードスコアを計算する（var で状態を変更しながら数える） */
fun calculateScoreImperative(word: String): Int {
    var score = 0
    for (c in word) {
        score++
    }
    return score
}

/** 命令型スタイルで 'a' を除外したスコアを計算する */
fun calculateScoreWithoutAImperative(word: String): Int {
    var score = 0
    for (c in word) {
        if (c != 'a') {
            score++
        }
    }
    return score
}

// ============================================
// 関数型プログラミング: WHAT（何をするか）を記述
// ============================================

/** 関数型スタイルでワードスコアを計算する（式本体関数） */
fun wordScore(word: String): Int = word.length

/** 関数型スタイルで 'a' を除外したスコアを計算する */
fun wordScoreWithoutA(word: String): Int = word.replace("a", "").length

// ============================================
// 基本的な純粋関数の例
// ============================================

fun increment(x: Int): Int = x + 1

fun add(a: Int, b: Int): Int = a + b

fun getFirstCharacter(s: String): Char = s[0]

fun concatenate(a: String, b: String): String = a + b

fun doubleValue(x: Int): Int = x * 2

fun greet(name: String): String = "Hello, $name!"

fun isEven(n: Int): Boolean = n % 2 == 0

// ============================================
// エントリポイント（./gradlew run で実行）
// ============================================

fun main() {
    val word = "functional"
    println("単語: $word")
    println("命令型スコア: ${calculateScoreImperative(word)}")
    println("関数型スコア: ${wordScore(word)}")
    println("'a' を除外したスコア（Scala）: ${wordScoreWithoutA("Scala")}")
    println("increment(6) = ${increment(6)}")
    println("greet(\"FP\") = ${greet("FP")}")
}
