/**
 * 第5章: flatMap とネストしたリスト操作
 *
 * chain（flatMap）を使って、ネストしたデータ構造を平坦化しながら変換する方法を学びます。
 * fp-ts では flatMap は chain と呼ばれます。
 */

import { pipe } from 'fp-ts/function'
import * as RA from 'fp-ts/ReadonlyArray'
import * as O from 'fp-ts/Option'

// =============================================================================
// 5.1 基本的な flatten と flatMap
// =============================================================================

/**
 * ネストしたリストを平坦化する。
 *
 * @example
 * flatten([[1, 2], [3, 4], [5]]) // => [1, 2, 3, 4, 5]
 * flatten([["a", "b"], [], ["c"]]) // => ["a", "b", "c"]
 */
export const flatten = <T>(nested: readonly (readonly T[])[]): readonly T[] =>
  pipe(nested, RA.flatten)

/**
 * map してから flatten する。
 * 各要素に関数を適用し、結果のリストを平坦化する。
 *
 * @example
 * flatMap(x => [x, x * 2], [1, 2, 3]) // => [1, 2, 2, 4, 3, 6]
 * flatMap(s => [...s], ["ab", "cd"]) // => ["a", "b", "c", "d"]
 */
export const flatMap = <T, U>(
  func: (item: T) => readonly U[],
  lst: readonly T[]
): readonly U[] =>
  pipe(lst, RA.chain(func))

/**
 * fp-ts の chain を使った flatMap。
 *
 * @example
 * chainExample([1, 2, 3], x => [x, x + 10]) // => [1, 11, 2, 12, 3, 13]
 */
export const chainExample = <T, U>(
  lst: readonly T[],
  func: (item: T) => readonly U[]
): readonly U[] =>
  pipe(lst, RA.chain(func))

// =============================================================================
// 5.2 Book と Movie の例（推薦システム）
// =============================================================================

/** 本を表すイミュータブルなインターフェース */
export interface Book {
  readonly title: string
  readonly authors: readonly string[]
}

/** 映画を表すイミュータブルなインターフェース */
export interface Movie {
  readonly title: string
}

/** Book を作成する */
export const createBook = (title: string, authors: readonly string[]): Book => ({
  title,
  authors,
})

/** Movie を作成する */
export const createMovie = (title: string): Movie => ({ title })

/**
 * 著者の作品の映画化作品を取得する。
 *
 * @example
 * bookAdaptations("Tolkien") // => [{ title: "An Unexpected Journey" }, { title: "The Desolation of Smaug" }]
 * bookAdaptations("Chiusano") // => []
 */
export const bookAdaptations = (author: string): readonly Movie[] => {
  if (author === 'Tolkien') {
    return [
      createMovie('An Unexpected Journey'),
      createMovie('The Desolation of Smaug'),
    ]
  }
  return []
}

/**
 * 全ての本の著者を取得する。
 *
 * @example
 * const books = [
 *   createBook("FP in Scala", ["Chiusano", "Bjarnason"]),
 *   createBook("The Hobbit", ["Tolkien"])
 * ]
 * getAllAuthors(books) // => ["Chiusano", "Bjarnason", "Tolkien"]
 */
export const getAllAuthors = (books: readonly Book[]): readonly string[] =>
  pipe(
    books,
    RA.chain((book) => book.authors)
  )

/**
 * 著者リストから全ての映画化作品を取得する。
 *
 * @example
 * getAllMovies(["Chiusano", "Tolkien"]) // => [{ title: "An Unexpected Journey" }, { title: "The Desolation of Smaug" }]
 */
export const getAllMovies = (authors: readonly string[]): readonly Movie[] =>
  pipe(authors, RA.chain(bookAdaptations))

/**
 * 本のリストから推薦メッセージを生成する。
 *
 * @example
 * const books = [
 *   createBook("FP in Scala", ["Chiusano", "Bjarnason"]),
 *   createBook("The Hobbit", ["Tolkien"])
 * ]
 * recommendationFeed(books)
 * // => ["You may like An Unexpected Journey, because you liked Tolkien's The Hobbit",
 * //     "You may like The Desolation of Smaug, because you liked Tolkien's The Hobbit"]
 */
export const recommendationFeed = (books: readonly Book[]): readonly string[] =>
  pipe(
    books,
    RA.chain((book) =>
      pipe(
        book.authors,
        RA.chain((author) =>
          pipe(
            bookAdaptations(author),
            RA.map(
              (movie) =>
                `You may like ${movie.title}, because you liked ${author}'s ${book.title}`
            )
          )
        )
      )
    )
  )

// =============================================================================
// 5.3 友達の推薦本
// =============================================================================

/**
 * 友達のおすすめ本を取得する。
 *
 * @example
 * recommendedBooks("Alice") // => [{ title: "FP in Scala", ... }, { title: "Get Programming with Scala", ... }]
 * recommendedBooks("Charlie") // => []
 */
export const recommendedBooks = (friend: string): readonly Book[] => {
  const scalaBooks: readonly Book[] = [
    createBook('FP in Scala', ['Chiusano', 'Bjarnason']),
    createBook('Get Programming with Scala', ['Sfregola']),
  ]
  const fictionBooks: readonly Book[] = [
    createBook('Harry Potter', ['Rowling']),
    createBook('The Lord of the Rings', ['Tolkien']),
  ]
  if (friend === 'Alice') return scalaBooks
  if (friend === 'Bob') return fictionBooks
  return []
}

/**
 * 友達のおすすめ本の著者を全て取得する。
 *
 * @example
 * getRecommendedAuthors(["Alice", "Bob", "Charlie"])
 * // => ["Chiusano", "Bjarnason", "Sfregola", "Rowling", "Tolkien"]
 */
export const getRecommendedAuthors = (
  friends: readonly string[]
): readonly string[] =>
  pipe(
    friends,
    RA.chain((friend) =>
      pipe(
        recommendedBooks(friend),
        RA.chain((book) => book.authors)
      )
    )
  )

// =============================================================================
// 5.4 2D/3D ポイントの生成
// =============================================================================

/** 2次元の点 */
export interface Point {
  readonly x: number
  readonly y: number
}

/** 3次元の点 */
export interface Point3d {
  readonly x: number
  readonly y: number
  readonly z: number
}

/** Point を作成する */
export const createPoint = (x: number, y: number): Point => ({ x, y })

/** Point3d を作成する */
export const createPoint3d = (x: number, y: number, z: number): Point3d => ({
  x,
  y,
  z,
})

/**
 * 全ての (x, y) の組み合わせから Point を生成する。
 *
 * @example
 * generatePoints([1], [-2, 7]) // => [{ x: 1, y: -2 }, { x: 1, y: 7 }]
 * generatePoints([1, 2], [-2, 7]) // => [{ x: 1, y: -2 }, { x: 1, y: 7 }, { x: 2, y: -2 }, { x: 2, y: 7 }]
 */
export const generatePoints = (
  xs: readonly number[],
  ys: readonly number[]
): readonly Point[] =>
  pipe(
    xs,
    RA.chain((x) =>
      pipe(
        ys,
        RA.map((y) => createPoint(x, y))
      )
    )
  )

/**
 * 全ての (x, y, z) の組み合わせから Point3d を生成する。
 *
 * @example
 * generatePoints3d([1], [-2, 7], [3, 4])
 * // => [{ x: 1, y: -2, z: 3 }, { x: 1, y: -2, z: 4 }, { x: 1, y: 7, z: 3 }, { x: 1, y: 7, z: 4 }]
 */
export const generatePoints3d = (
  xs: readonly number[],
  ys: readonly number[],
  zs: readonly number[]
): readonly Point3d[] =>
  pipe(
    xs,
    RA.chain((x) =>
      pipe(
        ys,
        RA.chain((y) =>
          pipe(
            zs,
            RA.map((z) => createPoint3d(x, y, z))
          )
        )
      )
    )
  )

// =============================================================================
// 5.5 flatMap でリストサイズを変更
// =============================================================================

/**
 * 各要素を2つに複製する。
 *
 * @example
 * duplicateEach([1, 2, 3]) // => [1, 1, 2, 2, 3, 3]
 */
export const duplicateEach = (numbers: readonly number[]): readonly number[] =>
  pipe(
    numbers,
    RA.chain((n) => [n, n])
  )

/**
 * 各要素とその +10 を含むリストを生成する。
 *
 * @example
 * withIncremented([1, 2, 3]) // => [1, 11, 2, 12, 3, 13]
 */
export const withIncremented = (
  numbers: readonly number[]
): readonly number[] =>
  pipe(
    numbers,
    RA.chain((n) => [n, n + 10])
  )

/**
 * flatMap パターンで偶数のみをフィルタする。
 * 通常は filter を使うが、chain でもフィルタリングできることを示す。
 *
 * @example
 * filterEvenViaChain([1, 2, 3, 4, 5]) // => [2, 4]
 */
export const filterEvenViaChain = (
  numbers: readonly number[]
): readonly number[] =>
  pipe(
    numbers,
    RA.chain((n) => (n % 2 === 0 ? [n] : []))
  )

/**
 * 条件に基づいてフィルタリングする汎用関数（chain パターン）。
 *
 * @example
 * filterViaChain(x => x > 2, [1, 2, 3, 4, 5]) // => [3, 4, 5]
 */
export const filterViaChain = <T>(
  predicate: (item: T) => boolean,
  lst: readonly T[]
): readonly T[] =>
  pipe(
    lst,
    RA.chain((item) => (predicate(item) ? [item] : []))
  )

// =============================================================================
// 5.6 Option との組み合わせ
// =============================================================================

/**
 * Option のリストから Some の値だけを取り出す。
 *
 * @example
 * compactOptions([O.some(1), O.none, O.some(2), O.none, O.some(3)])
 * // => [1, 2, 3]
 */
export const compactOptions = <T>(
  options: readonly O.Option<T>[]
): readonly T[] =>
  pipe(options, RA.compact)

/**
 * リストの各要素に Option を返す関数を適用し、Some の値だけを取り出す。
 *
 * @example
 * filterMapExample(s => s.length > 2 ? O.some(s.toUpperCase()) : O.none, ["a", "abc", "de", "xyz"])
 * // => ["ABC", "XYZ"]
 */
export const filterMapExample = <T, U>(
  func: (item: T) => O.Option<U>,
  lst: readonly T[]
): readonly U[] =>
  pipe(lst, RA.filterMap(func))

// =============================================================================
// 5.7 sequenceT / sequenceArray
// =============================================================================

import { sequenceT } from 'fp-ts/Apply'
import * as A from 'fp-ts/Array'

/**
 * 複数の Option を組み合わせる。
 * 全てが Some の場合のみ Some を返す。
 *
 * @example
 * combineOptions(O.some(1), O.some(2), O.some(3)) // => some([1, 2, 3])
 * combineOptions(O.some(1), O.none, O.some(3)) // => none
 */
export const combineOptions = <T>(
  ...options: readonly O.Option<T>[]
): O.Option<readonly T[]> =>
  pipe(A.sequence(O.Applicative)([...options]))

/**
 * 複数のリストのデカルト積（全ての組み合わせ）を生成する。
 *
 * @example
 * cartesianProduct([1, 2], [3, 4]) // => [[1, 3], [1, 4], [2, 3], [2, 4]]
 */
export const cartesianProduct = <T>(
  xs: readonly T[],
  ys: readonly T[]
): readonly (readonly [T, T])[] =>
  pipe(
    xs,
    RA.chain((x) =>
      pipe(
        ys,
        RA.map((y) => [x, y] as const)
      )
    )
  )

// =============================================================================
// 5.8 traverse
// =============================================================================

/**
 * リストの各要素に Option を返す関数を適用し、
 * 全てが Some なら Some[T[]] を返し、一つでも None なら None を返す。
 *
 * @example
 * const safeDivide = (n: number) => n === 0 ? O.none : O.some(10 / n)
 * traverseOption(safeDivide, [1, 2, 5]) // => some([10, 5, 2])
 * traverseOption(safeDivide, [1, 0, 5]) // => none
 */
export const traverseOption = <T, U>(
  func: (item: T) => O.Option<U>,
  lst: readonly T[]
): O.Option<readonly U[]> =>
  pipe(lst, RA.traverse(O.Applicative)(func))

// =============================================================================
// 5.9 円内の点の判定
// =============================================================================

/**
 * 点が円の内部にあるかを判定する。
 *
 * @example
 * isInside(createPoint(1, 1), 2) // => true
 * isInside(createPoint(5, 2), 2) // => false
 */
export const isInside = (point: Point, radius: number): boolean =>
  radius * radius >= point.x * point.x + point.y * point.y

/**
 * 全てのポイントと半径の組み合わせを生成し、各組み合わせが円内かどうかを判定する。
 *
 * @example
 * const points = [createPoint(5, 2), createPoint(1, 1)]
 * const radiuses = [2, 1]
 * allCombinations(points, radiuses)
 * // => [
 * //   "Point(5,2) is within a radius of 2: false",
 * //   "Point(1,1) is within a radius of 2: true",
 * //   "Point(5,2) is within a radius of 1: false",
 * //   "Point(1,1) is within a radius of 1: false"
 * // ]
 */
export const allCombinations = (
  points: readonly Point[],
  radiuses: readonly number[]
): readonly string[] =>
  pipe(
    radiuses,
    RA.chain((r) =>
      pipe(
        points,
        RA.map(
          (point) =>
            `Point(${point.x},${point.y}) is within a radius of ${r}: ${isInside(point, r)}`
        )
      )
    )
  )

/**
 * 円内にあるポイントのみを抽出する。
 *
 * @example
 * const points = [createPoint(5, 2), createPoint(1, 1)]
 * const radiuses = [2, 1]
 * insidePoints(points, radiuses)
 * // => ["Point(1,1) is within a radius of 2"]
 */
export const insidePoints = (
  points: readonly Point[],
  radiuses: readonly number[]
): readonly string[] =>
  pipe(
    radiuses,
    RA.chain((r) =>
      pipe(
        points,
        RA.filter((point) => isInside(point, r)),
        RA.map((point) => `Point(${point.x},${point.y}) is within a radius of ${r}`)
      )
    )
  )
