/**
 * 第4章: 関数を値として扱う
 *
 * 関数を他の関数に渡したり、関数から関数を返したりする方法を学びます。
 * TypeScript では関数は第一級オブジェクトなので、変数に代入したり、
 * 引数として渡したりすることができます。
 */

import { pipe, flow } from 'fp-ts/function'
import * as RA from 'fp-ts/ReadonlyArray'
import * as Ord from 'fp-ts/Ord'
import * as N from 'fp-ts/number'
import * as S from 'fp-ts/string'

// =============================================================================
// 4.1 関数を引数として渡す（sortBy、map、filter）
// =============================================================================

/**
 * 単語のスコアを計算する（'a'を除いた文字数）。
 *
 * @example
 * score("java") // => 2
 * score("rust") // => 4
 */
export const score = (word: string): number =>
  word.replace(/a/gi, '').length

/**
 * 単語をスコア順（降順）にソートする。
 *
 * @example
 * rankedWords(["rust", "java"], score) // => ["rust", "java"]
 * rankedWords(["ada", "haskell", "scala"], score) // => ["haskell", "scala", "ada"]
 */
export const rankedWords = (
  words: readonly string[],
  wordScore: (word: string) => number
): readonly string[] =>
  pipe(
    words,
    RA.sortBy([Ord.reverse(Ord.contramap(wordScore)(N.Ord))])
  )

/**
 * 文字列を長さ順（昇順）にソートする。
 *
 * @example
 * sortByLength(["scala", "rust", "ada"]) // => ["ada", "rust", "scala"]
 */
export const sortByLength = (words: readonly string[]): readonly string[] =>
  pipe(
    words,
    RA.sortBy([Ord.contramap((s: string) => s.length)(N.Ord)])
  )

/**
 * 文字列を長さ順（降順）にソートする。
 *
 * @example
 * sortByLengthDesc(["scala", "rust", "ada"]) // => ["scala", "rust", "ada"]
 */
export const sortByLengthDesc = (words: readonly string[]): readonly string[] =>
  pipe(
    words,
    RA.sortBy([Ord.reverse(Ord.contramap((s: string) => s.length)(N.Ord))])
  )

/**
 * 文字列中の's'の数を数える。
 *
 * @example
 * numberOfS("rust") // => 1
 * numberOfS("haskell") // => 1
 * numberOfS("ada") // => 0
 */
export const numberOfS = (s: string): number =>
  s.split('').filter((c) => c === 's').length

/**
 * 文字列を's'の数順（昇順）にソートする。
 *
 * @example
 * sortByNumberOfS(["rust", "ada"]) // => ["ada", "rust"]
 */
export const sortByNumberOfS = (words: readonly string[]): readonly string[] =>
  pipe(
    words,
    RA.sortBy([Ord.contramap(numberOfS)(N.Ord)])
  )

// =============================================================================
// 4.2 map - 各要素に関数を適用
// =============================================================================

/**
 * 各文字列の長さを取得する。
 *
 * @example
 * getLengths(["scala", "rust", "ada"]) // => [5, 4, 3]
 */
export const getLengths = (words: readonly string[]): readonly number[] =>
  pipe(
    words,
    RA.map((word) => word.length)
  )

/**
 * 各単語のスコアを取得する。
 *
 * @example
 * getScores(["rust", "java"], score) // => [4, 2]
 */
export const getScores = (
  words: readonly string[],
  wordScore: (word: string) => number
): readonly number[] =>
  pipe(words, RA.map(wordScore))

/**
 * 全ての数値を2倍にする。
 *
 * @example
 * doubleAll([5, 1, 2, 4, 0]) // => [10, 2, 4, 8, 0]
 */
export const doubleAll = (numbers: readonly number[]): readonly number[] =>
  pipe(
    numbers,
    RA.map((n) => n * 2)
  )

/**
 * 全ての数値を負にする。
 *
 * @example
 * negateAll([5, 1, 2, 4, 0]) // => [-5, -1, -2, -4, 0]
 */
export const negateAll = (numbers: readonly number[]): readonly number[] =>
  pipe(
    numbers,
    RA.map((n) => -n)
  )

// =============================================================================
// 4.3 filter - 条件に合う要素を抽出
// =============================================================================

/**
 * 指定した長さ以下の単語をフィルタする。
 *
 * @example
 * filterShortWords(["scala", "rust", "ada"], 4) // => ["rust", "ada"]
 */
export const filterShortWords = (
  words: readonly string[],
  maxLength: number
): readonly string[] =>
  pipe(
    words,
    RA.filter((word) => word.length <= maxLength)
  )

/**
 * 奇数のみをフィルタする。
 *
 * @example
 * filterOdd([5, 1, 2, 4, 0]) // => [5, 1]
 */
export const filterOdd = (numbers: readonly number[]): readonly number[] =>
  pipe(
    numbers,
    RA.filter((n) => n % 2 === 1)
  )

/**
 * 閾値より大きい数値をフィルタする。
 *
 * @example
 * filterLargerThan([5, 1, 2, 4, 0], 4) // => [5]
 */
export const filterLargerThan = (
  numbers: readonly number[],
  threshold: number
): readonly number[] =>
  pipe(
    numbers,
    RA.filter((n) => n > threshold)
  )

/**
 * 指定したスコアより高い単語をフィルタする。
 *
 * @example
 * highScoringWords(["rust", "java"], score, 3) // => ["rust"]
 */
export const highScoringWords = (
  words: readonly string[],
  wordScore: (word: string) => number,
  higherThan: number
): readonly string[] =>
  pipe(
    words,
    RA.filter((word) => wordScore(word) > higherThan)
  )

// =============================================================================
// 4.4 reduce - 累積演算
// =============================================================================

/**
 * 数値の合計を計算する。
 *
 * @example
 * sumAll([5, 1, 2, 4, 100]) // => 112
 */
export const sumAll = (numbers: readonly number[]): number =>
  pipe(
    numbers,
    RA.reduce(0, (acc, n) => acc + n)
  )

/**
 * 全ての文字列の長さの合計を計算する。
 *
 * @example
 * totalLength(["scala", "rust", "ada"]) // => 12
 */
export const totalLength = (words: readonly string[]): number =>
  pipe(
    words,
    RA.reduce(0, (acc, word) => acc + word.length)
  )

/**
 * 全ての文字列中の's'の数の合計を計算する。
 *
 * @example
 * totalSCount(["scala", "haskell", "rust", "ada"]) // => 3
 */
export const totalSCount = (words: readonly string[]): number =>
  pipe(
    words,
    RA.reduce(0, (acc, word) => acc + numberOfS(word))
  )

/**
 * リスト内の最大値を見つける。
 *
 * @example
 * findMax([5, 1, 2, 4, 15]) // => 15
 */
export const findMax = (numbers: readonly number[]): number | undefined => {
  if (numbers.length === 0) return undefined
  return pipe(
    numbers,
    RA.reduce(numbers[0], (max, n) => (n > max ? n : max))
  )
}

/**
 * 全ての単語のスコアの合計を計算する。
 *
 * @example
 * cumulativeScore(["rust", "java"], score) // => 6
 */
export const cumulativeScore = (
  words: readonly string[],
  wordScore: (word: string) => number
): number =>
  pipe(
    words,
    RA.reduce(0, (acc, word) => acc + wordScore(word))
  )

// =============================================================================
// 4.5 関数を返す関数
// =============================================================================

/**
 * n より大きいかを判定する関数を返す。
 *
 * @example
 * largerThan(4)(5) // => true
 * largerThan(4)(3) // => false
 * [5, 1, 2, 4, 0].filter(largerThan(4)) // => [5]
 */
export const largerThan = (n: number): ((i: number) => boolean) =>
  (i) => i > n

/**
 * n で割り切れるかを判定する関数を返す。
 *
 * @example
 * divisibleBy(5)(15) // => true
 * divisibleBy(5)(7) // => false
 * [5, 1, 2, 4, 15].filter(divisibleBy(5)) // => [5, 15]
 */
export const divisibleBy = (n: number): ((i: number) => boolean) =>
  (i) => i % n === 0

/**
 * 長さが n より短いかを判定する関数を返す。
 *
 * @example
 * shorterThan(4)("ada") // => true
 * shorterThan(4)("scala") // => false
 * ["scala", "ada"].filter(shorterThan(4)) // => ["ada"]
 */
export const shorterThan = (n: number): ((s: string) => boolean) =>
  (s) => s.length < n

/**
 * 's' が count より多く含まれるかを判定する関数を返す。
 *
 * @example
 * containsSMoreThan(0)("rust") // => true
 * containsSMoreThan(0)("ada") // => false
 * ["rust", "ada"].filter(containsSMoreThan(0)) // => ["rust"]
 */
export const containsSMoreThan = (count: number): ((s: string) => boolean) =>
  (s) => numberOfS(s) > count

// =============================================================================
// 4.6 Word Scoring の実践的な例
// =============================================================================

/**
 * 'c' を含む単語にボーナスを与える。
 *
 * @example
 * bonus("scala") // => 5
 * bonus("java") // => 0
 */
export const bonus = (word: string): number =>
  word.includes('c') ? 5 : 0

/**
 * 's' を含む単語にペナルティを与える。
 *
 * @example
 * penalty("rust") // => 7
 * penalty("java") // => 0
 */
export const penalty = (word: string): number =>
  word.includes('s') ? 7 : 0

/**
 * スコア + ボーナス を計算する。
 *
 * @example
 * wordScoreWithBonus("scala") // => 8
 * wordScoreWithBonus("java") // => 2
 */
export const wordScoreWithBonus = (word: string): number =>
  score(word) + bonus(word)

/**
 * スコア + ボーナス - ペナルティ を計算する。
 *
 * @example
 * wordScoreWithBonusAndPenalty("java") // => 2
 * wordScoreWithBonusAndPenalty("scala") // => 1
 * wordScoreWithBonusAndPenalty("rust") // => -3
 */
export const wordScoreWithBonusAndPenalty = (word: string): number =>
  score(word) + bonus(word) - penalty(word)

/**
 * スコア関数を受け取り、閾値と単語リストを受け取る関数を返す。
 * カリー化された関数の例。
 *
 * @example
 * const words = ["ada", "haskell", "scala", "java", "rust"]
 * const scorer = highScoringWordsWithThreshold(wordScoreWithBonusAndPenalty)
 * scorer(1)(words) // => ["java"]
 * scorer(0)(words) // => ["ada", "scala", "java"]
 */
export const highScoringWordsWithThreshold =
  (wordScoreFn: (word: string) => number) =>
  (higherThan: number) =>
  (words: readonly string[]): readonly string[] =>
    pipe(
      words,
      RA.filter((word) => wordScoreFn(word) > higherThan)
    )

// =============================================================================
// 4.7 汎用的な高階関数
// =============================================================================

/**
 * map 関数の実装。
 *
 * @example
 * myMap(x => x * 2, [1, 2, 3]) // => [2, 4, 6]
 * myMap(s => s.length, ["a", "bb", "ccc"]) // => [1, 2, 3]
 */
export const myMap = <T, U>(
  func: (item: T) => U,
  lst: readonly T[]
): readonly U[] =>
  lst.map(func)

/**
 * filter 関数の実装。
 *
 * @example
 * myFilter(x => x > 0, [-1, 0, 1, 2]) // => [1, 2]
 * myFilter(s => s.length > 2, ["a", "bb", "ccc"]) // => ["ccc"]
 */
export const myFilter = <T>(
  predicate: (item: T) => boolean,
  lst: readonly T[]
): readonly T[] =>
  lst.filter(predicate)

/**
 * reduce 関数の実装。
 *
 * @example
 * myReduce((acc, x) => acc + x, [1, 2, 3], 0) // => 6
 * myReduce((acc, s) => acc + s.length, ["a", "bb"], 0) // => 3
 */
export const myReduce = <T, U>(
  func: (acc: U, item: T) => U,
  lst: readonly T[],
  initial: U
): U =>
  lst.reduce(func, initial)

// =============================================================================
// 4.8 プログラミング言語の例
// =============================================================================

/** プログラミング言語 */
export interface ProgrammingLanguage {
  readonly name: string
  readonly year: number
}

/** ProgrammingLanguage を作成する */
export const createLanguage = (
  name: string,
  year: number
): ProgrammingLanguage => ({ name, year })

/**
 * 言語を年でソートする（昇順）。
 *
 * @example
 * const langs = [createLanguage("Scala", 2004), createLanguage("Java", 1995)]
 * sortByYear(langs) // => [{name: "Java", year: 1995}, {name: "Scala", year: 2004}]
 */
export const sortByYear = (
  languages: readonly ProgrammingLanguage[]
): readonly ProgrammingLanguage[] =>
  pipe(
    languages,
    RA.sortBy([Ord.contramap((l: ProgrammingLanguage) => l.year)(N.Ord)])
  )

/**
 * 言語名を取得する。
 *
 * @example
 * const langs = [createLanguage("Scala", 2004), createLanguage("Java", 1995)]
 * getNames(langs) // => ["Scala", "Java"]
 */
export const getNames = (
  languages: readonly ProgrammingLanguage[]
): readonly string[] =>
  pipe(
    languages,
    RA.map((l) => l.name)
  )

/**
 * 指定した年より後に作られた言語をフィルタする。
 *
 * @example
 * const langs = [createLanguage("Scala", 2004), createLanguage("Java", 1995)]
 * filterYoungLanguages(langs, 2000) // => [{name: "Scala", year: 2004}]
 */
export const filterYoungLanguages = (
  languages: readonly ProgrammingLanguage[],
  afterYear: number
): readonly ProgrammingLanguage[] =>
  pipe(
    languages,
    RA.filter((l) => l.year > afterYear)
  )
