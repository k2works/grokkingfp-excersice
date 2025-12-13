/**
 * 第3章: イミュータブルなデータ操作
 *
 * イミュータブル（不変）なデータの操作方法を学びます。
 * データを「変更」する代わりに、新しいデータを「作成」します。
 */

import { pipe } from 'fp-ts/function'
import * as RA from 'fp-ts/ReadonlyArray'

// =============================================================================
// 3.1 イミュータブルなリスト操作
// =============================================================================

/**
 * リストに要素を追加した新しいリストを返す。
 * 元のリストは変更されない。
 *
 * @example
 * const original = ["Apple", "Book"] as const
 * const newList = appended(original, "Mango")
 * // original: ["Apple", "Book"]
 * // newList: ["Apple", "Book", "Mango"]
 */
export const appended = <T>(
  lst: readonly T[],
  element: T
): readonly T[] => [...lst, element]

/**
 * リストに複数の要素を追加した新しいリストを返す。
 *
 * @example
 * appendedAll(["a", "b"], ["c", "d"]) // => ["a", "b", "c", "d"]
 */
export const appendedAll = <T>(
  lst: readonly T[],
  elements: readonly T[]
): readonly T[] => [...lst, ...elements]

/**
 * リストの先頭に要素を追加した新しいリストを返す。
 *
 * @example
 * prepended(["b", "c"], "a") // => ["a", "b", "c"]
 */
export const prepended = <T>(
  lst: readonly T[],
  element: T
): readonly T[] => [element, ...lst]

// =============================================================================
// 3.2 スライス操作
// =============================================================================

/**
 * リストの最初の n 要素を取得する。
 *
 * @example
 * firstN(["a", "b", "c"], 2) // => ["a", "b"]
 * firstN(["a"], 2) // => ["a"]
 */
export const firstN = <T>(lst: readonly T[], n: number): readonly T[] =>
  lst.slice(0, n)

/**
 * リストの最初の2要素を取得する。
 *
 * @example
 * firstTwo(["a", "b", "c"]) // => ["a", "b"]
 */
export const firstTwo = <T>(lst: readonly T[]): readonly T[] =>
  firstN(lst, 2)

/**
 * リストの最後の n 要素を取得する。
 *
 * @example
 * lastN(["a", "b", "c"], 2) // => ["b", "c"]
 * lastN(["a"], 2) // => ["a"]
 */
export const lastN = <T>(lst: readonly T[], n: number): readonly T[] =>
  lst.length < n ? [...lst] : lst.slice(-n)

/**
 * リストの最後の2要素を取得する。
 *
 * @example
 * lastTwo(["a", "b", "c"]) // => ["b", "c"]
 */
export const lastTwo = <T>(lst: readonly T[]): readonly T[] =>
  lastN(lst, 2)

/**
 * 最初の2要素を末尾に移動した新しいリストを返す。
 *
 * @example
 * moveFirstTwoToEnd(["a", "b", "c"]) // => ["c", "a", "b"]
 * moveFirstTwoToEnd(["a", "b", "c", "d"]) // => ["c", "d", "a", "b"]
 */
export const moveFirstTwoToEnd = <T>(lst: readonly T[]): readonly T[] => {
  const first = lst.slice(0, 2)
  const rest = lst.slice(2)
  return [...rest, ...first]
}

/**
 * 最後の要素の前に要素を挿入した新しいリストを返す。
 *
 * @example
 * insertBeforeLast(["a", "b"], "c") // => ["a", "c", "b"]
 * insertBeforeLast(["a"], "b") // => ["b", "a"]
 */
export const insertBeforeLast = <T>(
  lst: readonly T[],
  element: T
): readonly T[] => {
  if (lst.length === 0) return [element]
  return [...lst.slice(0, -1), element, ...lst.slice(-1)]
}

/**
 * リストの中央に要素を挿入した新しいリストを返す。
 *
 * @example
 * insertAtMiddle(["a", "b", "c", "d"], "X") // => ["a", "b", "X", "c", "d"]
 * insertAtMiddle(["a", "b"], "X") // => ["a", "X", "b"]
 */
export const insertAtMiddle = <T>(
  lst: readonly T[],
  element: T
): readonly T[] => {
  const middle = Math.floor(lst.length / 2)
  return [...lst.slice(0, middle), element, ...lst.slice(middle)]
}

// =============================================================================
// 3.3 旅程の再計画
// =============================================================================

/**
 * 指定した都市の前に新しい都市を挿入した旅程を返す。
 *
 * @example
 * const plan = ["Paris", "Berlin", "Kraków"]
 * replan(plan, "Vienna", "Kraków") // => ["Paris", "Berlin", "Vienna", "Kraków"]
 * // plan は変わらない: ["Paris", "Berlin", "Kraków"]
 */
export const replan = (
  plan: readonly string[],
  newCity: string,
  beforeCity: string
): readonly string[] => {
  const index = plan.indexOf(beforeCity)
  if (index === -1) return [...plan, newCity]
  return [...plan.slice(0, index), newCity, ...plan.slice(index)]
}

// =============================================================================
// 3.4 文字列操作（String もイミュータブル）
// =============================================================================

/**
 * 名前を省略形にする。
 *
 * @example
 * abbreviate("Alonzo Church") // => "A. Church"
 * abbreviate("A. Church") // => "A. Church"
 */
export const abbreviate = (name: string): string => {
  const separator = name.indexOf(' ')
  if (separator === -1) return name
  const initial = name.charAt(0)
  const lastName = name.substring(separator + 1)
  return `${initial}. ${lastName}`
}

/**
 * 部分文字列を取得する。
 *
 * @example
 * substring("hello", 1, 4) // => "ell"
 * substring("TypeScript", 0, 4) // => "Type"
 */
export const substring = (s: string, start: number, end: number): string =>
  s.substring(start, end)

// =============================================================================
// 3.5 イミュータブルなオブジェクト
// =============================================================================

/** 2次元の点 */
export interface Point {
  readonly x: number
  readonly y: number
}

/** Point を作成する */
export const createPoint = (x: number, y: number): Point => ({ x, y })

/** Point の x 座標を更新した新しい Point を返す */
export const withX = (point: Point, newX: number): Point => ({
  ...point,
  x: newX,
})

/** Point の y 座標を更新した新しい Point を返す */
export const withY = (point: Point, newY: number): Point => ({
  ...point,
  y: newY,
})

/** 都市 */
export interface City {
  readonly name: string
  readonly population: number
}

/** City を作成する */
export const createCity = (name: string, population: number): City => ({
  name,
  population,
})

/** 人口を更新した新しい City を返す */
export const withPopulation = (city: City, newPopulation: number): City => ({
  ...city,
  population: newPopulation,
})

// =============================================================================
// 3.6 ReadonlySet（イミュータブルな集合）
// =============================================================================

/**
 * Set に要素を追加した新しい Set を返す。
 *
 * @example
 * const s = new Set(["a", "b"])
 * addToSet(s, "c") // => Set(["a", "b", "c"])
 * // s は変わらない
 */
export const addToSet = <T>(s: ReadonlySet<T>, element: T): ReadonlySet<T> =>
  new Set([...s, element])

/**
 * Set から要素を削除した新しい Set を返す。
 *
 * @example
 * const s = new Set(["a", "b", "c"])
 * removeFromSet(s, "b") // => Set(["a", "c"])
 */
export const removeFromSet = <T>(s: ReadonlySet<T>, element: T): ReadonlySet<T> =>
  new Set([...s].filter((e) => e !== element))

// =============================================================================
// 3.7 ReadonlyMap（イミュータブルな辞書）
// =============================================================================

/**
 * Map のキーを更新した新しい Map を返す。
 *
 * @example
 * const m = new Map([["a", 1], ["b", 2]])
 * updateMap(m, "c", 3) // => Map([["a", 1], ["b", 2], ["c", 3]])
 */
export const updateMap = <K, V>(
  m: ReadonlyMap<K, V>,
  key: K,
  value: V
): ReadonlyMap<K, V> => new Map([...m, [key, value]])

/**
 * Map からキーを削除した新しい Map を返す。
 *
 * @example
 * const m = new Map([["a", 1], ["b", 2], ["c", 3]])
 * removeKey(m, "b") // => Map([["a", 1], ["c", 3]])
 */
export const removeKey = <K, V>(
  m: ReadonlyMap<K, V>,
  key: K
): ReadonlyMap<K, V> => new Map([...m].filter(([k]) => k !== key))

// =============================================================================
// 3.8 fp-ts を使ったイミュータブル操作
// =============================================================================

/**
 * fp-ts の ReadonlyArray を使って要素を追加する。
 *
 * @example
 * appendWithFpts(["a", "b"], "c") // => ["a", "b", "c"]
 */
export const appendWithFpts = <T>(
  lst: readonly T[],
  element: T
): readonly T[] =>
  pipe(lst, RA.append(element))

/**
 * fp-ts の ReadonlyArray を使って先頭に要素を追加する。
 *
 * @example
 * prependWithFpts(["b", "c"], "a") // => ["a", "b", "c"]
 */
export const prependWithFpts = <T>(
  lst: readonly T[],
  element: T
): readonly T[] =>
  pipe(lst, RA.prepend(element))

/**
 * fp-ts の ReadonlyArray を使ってリストを連結する。
 *
 * @example
 * concatWithFpts(["a", "b"], ["c", "d"]) // => ["a", "b", "c", "d"]
 */
export const concatWithFpts = <T>(
  lst1: readonly T[],
  lst2: readonly T[]
): readonly T[] =>
  pipe(lst1, RA.concat(lst2))
