/**
 * 第9章: ストリーム処理
 *
 * 遅延評価と無限シーケンスの処理を学びます。
 * TypeScript では Generator 関数とイテレータを使って実装します。
 */

import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import * as RA from 'fp-ts/ReadonlyArray'
import * as T from 'fp-ts/Task'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'

// =============================================================================
// 9.1 LazyList（遅延評価リスト）の基本
// =============================================================================

/**
 * LazyList は遅延評価されるリストを表す。
 * Generator 関数を使って要素を必要な時に生成する。
 */
export type LazyList<A> = Generator<A, void, undefined>

/**
 * 配列から LazyList を作成する。
 *
 * @example
 * const list = fromArray([1, 2, 3])
 * [...list] // => [1, 2, 3]
 */
export function* fromArray<A>(arr: readonly A[]): LazyList<A> {
  for (const item of arr) {
    yield item
  }
}

/**
 * 単一の値から LazyList を作成する。
 *
 * @example
 * const list = singleton(42)
 * [...list] // => [42]
 */
export function* singleton<A>(value: A): LazyList<A> {
  yield value
}

/**
 * 空の LazyList を作成する。
 *
 * @example
 * const list = empty()
 * [...list] // => []
 */
export function* empty<A>(): LazyList<A> {
  // 何も yield しない
}

/**
 * 範囲から LazyList を作成する。
 *
 * @example
 * const list = range(1, 5)
 * [...list] // => [1, 2, 3, 4]
 */
export function* range(start: number, end: number): LazyList<number> {
  for (let i = start; i < end; i++) {
    yield i
  }
}

/**
 * 範囲から LazyList を作成する（終端を含む）。
 *
 * @example
 * const list = rangeInclusive(1, 5)
 * [...list] // => [1, 2, 3, 4, 5]
 */
export function* rangeInclusive(start: number, end: number): LazyList<number> {
  for (let i = start; i <= end; i++) {
    yield i
  }
}

// =============================================================================
// 9.2 無限ストリーム
// =============================================================================

/**
 * 値を無限に繰り返すストリームを作成する。
 *
 * @example
 * const ones = repeatValue(1)
 * take(ones, 5) // => [1, 1, 1, 1, 1]
 */
export function* repeatValue<A>(value: A): LazyList<A> {
  while (true) {
    yield value
  }
}

/**
 * 関数を無限に繰り返し呼び出すストリームを作成する。
 *
 * @example
 * let i = 0
 * const counting = repeatFn(() => i++)
 * take(counting, 3) // => [0, 1, 2]
 */
export function* repeatFn<A>(fn: () => A): LazyList<A> {
  while (true) {
    yield fn()
  }
}

/**
 * 配列を無限に繰り返すストリームを作成する。
 *
 * @example
 * const cycle = cycleArray([1, 2, 3])
 * take(cycle, 8) // => [1, 2, 3, 1, 2, 3, 1, 2]
 */
export function* cycleArray<A>(arr: readonly A[]): LazyList<A> {
  if (arr.length === 0) return
  while (true) {
    for (const item of arr) {
      yield item
    }
  }
}

/**
 * 初期値と生成関数から無限ストリームを作成する（unfold）。
 *
 * @example
 * const powers = iterate(1, n => n * 2)
 * take(powers, 5) // => [1, 2, 4, 8, 16]
 */
export function* iterate<A>(initial: A, fn: (a: A) => A): LazyList<A> {
  let current = initial
  while (true) {
    yield current
    current = fn(current)
  }
}

/**
 * 自然数の無限ストリーム。
 *
 * @example
 * take(naturals(), 5) // => [0, 1, 2, 3, 4]
 */
export function* naturals(): LazyList<number> {
  let n = 0
  while (true) {
    yield n++
  }
}

// =============================================================================
// 9.3 ストリーム操作（変換）
// =============================================================================

/**
 * 最初の n 要素を取得する。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * take(list, 3) // => [1, 2, 3]
 */
export const take = <A>(gen: LazyList<A>, n: number): readonly A[] => {
  const result: A[] = []
  let count = 0
  for (const item of gen) {
    if (count >= n) break
    result.push(item)
    count++
  }
  return result
}

/**
 * 最初の n 要素をスキップする。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * take(drop(list, 2), 3) // => [3, 4, 5]
 */
export function* drop<A>(gen: LazyList<A>, n: number): LazyList<A> {
  let count = 0
  for (const item of gen) {
    if (count >= n) {
      yield item
    }
    count++
  }
}

/**
 * 条件を満たす間、要素を取得する。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * [...takeWhile(list, x => x < 4)] // => [1, 2, 3]
 */
export function* takeWhile<A>(
  gen: LazyList<A>,
  predicate: (a: A) => boolean
): LazyList<A> {
  for (const item of gen) {
    if (!predicate(item)) break
    yield item
  }
}

/**
 * 条件を満たす間、要素をスキップする。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * [...dropWhile(list, x => x < 3)] // => [3, 4, 5]
 */
export function* dropWhile<A>(
  gen: LazyList<A>,
  predicate: (a: A) => boolean
): LazyList<A> {
  let dropping = true
  for (const item of gen) {
    if (dropping && predicate(item)) {
      continue
    }
    dropping = false
    yield item
  }
}

/**
 * 各要素を変換する（map）。
 *
 * @example
 * const list = fromArray([1, 2, 3])
 * [...mapStream(list, x => x * 2)] // => [2, 4, 6]
 */
export function* mapStream<A, B>(
  gen: LazyList<A>,
  fn: (a: A) => B
): LazyList<B> {
  for (const item of gen) {
    yield fn(item)
  }
}

/**
 * 条件を満たす要素のみを取得する（filter）。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * [...filterStream(list, x => x % 2 === 0)] // => [2, 4]
 */
export function* filterStream<A>(
  gen: LazyList<A>,
  predicate: (a: A) => boolean
): LazyList<A> {
  for (const item of gen) {
    if (predicate(item)) {
      yield item
    }
  }
}

/**
 * 各要素を展開して結合する（flatMap）。
 *
 * @example
 * const list = fromArray([1, 2, 3])
 * [...flatMapStream(list, x => fromArray([x, x * 10]))]
 * // => [1, 10, 2, 20, 3, 30]
 */
export function* flatMapStream<A, B>(
  gen: LazyList<A>,
  fn: (a: A) => LazyList<B>
): LazyList<B> {
  for (const item of gen) {
    yield* fn(item)
  }
}

// =============================================================================
// 9.4 ストリームの結合
// =============================================================================

/**
 * 2つのストリームを結合する（append）。
 *
 * @example
 * const list1 = fromArray([1, 2])
 * const list2 = fromArray([3, 4])
 * [...append(list1, list2)] // => [1, 2, 3, 4]
 */
export function* append<A>(gen1: LazyList<A>, gen2: LazyList<A>): LazyList<A> {
  yield* gen1
  yield* gen2
}

/**
 * 2つのストリームをペアにして結合する（zip）。
 *
 * @example
 * const list1 = fromArray([1, 2, 3])
 * const list2 = fromArray(['a', 'b', 'c'])
 * [...zipStream(list1, list2)] // => [[1, 'a'], [2, 'b'], [3, 'c']]
 */
export function* zipStream<A, B>(
  gen1: LazyList<A>,
  gen2: LazyList<B>
): LazyList<readonly [A, B]> {
  const iter1 = gen1[Symbol.iterator]()
  const iter2 = gen2[Symbol.iterator]()
  while (true) {
    const next1 = iter1.next()
    const next2 = iter2.next()
    if (next1.done || next2.done) break
    yield [next1.value, next2.value] as const
  }
}

/**
 * 2つのストリームを関数で結合する（zipWith）。
 *
 * @example
 * const list1 = fromArray([1, 2, 3])
 * const list2 = fromArray([10, 20, 30])
 * [...zipWith(list1, list2, (a, b) => a + b)] // => [11, 22, 33]
 */
export function* zipWith<A, B, C>(
  gen1: LazyList<A>,
  gen2: LazyList<B>,
  fn: (a: A, b: B) => C
): LazyList<C> {
  const iter1 = gen1[Symbol.iterator]()
  const iter2 = gen2[Symbol.iterator]()
  while (true) {
    const next1 = iter1.next()
    const next2 = iter2.next()
    if (next1.done || next2.done) break
    yield fn(next1.value, next2.value)
  }
}

/**
 * インデックス付きでストリームを走査する（zipWithIndex）。
 *
 * @example
 * const list = fromArray(['a', 'b', 'c'])
 * [...zipWithIndex(list)] // => [['a', 0], ['b', 1], ['c', 2]]
 */
export function* zipWithIndex<A>(
  gen: LazyList<A>
): LazyList<readonly [A, number]> {
  let index = 0
  for (const item of gen) {
    yield [item, index] as const
    index++
  }
}

// =============================================================================
// 9.5 スライディングウィンドウ
// =============================================================================

/**
 * スライディングウィンドウでストリームを処理する。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * [...sliding(list, 3)]
 * // => [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
 */
export function* sliding<A>(
  gen: LazyList<A>,
  windowSize: number
): LazyList<readonly A[]> {
  const buffer: A[] = []
  for (const item of gen) {
    buffer.push(item)
    if (buffer.length === windowSize) {
      yield [...buffer]
      buffer.shift()
    }
  }
}

/**
 * 指定サイズのチャンクに分割する。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5, 6, 7])
 * [...chunk(list, 3)] // => [[1, 2, 3], [4, 5, 6], [7]]
 */
export function* chunk<A>(
  gen: LazyList<A>,
  size: number
): LazyList<readonly A[]> {
  let buffer: A[] = []
  for (const item of gen) {
    buffer.push(item)
    if (buffer.length === size) {
      yield buffer
      buffer = []
    }
  }
  if (buffer.length > 0) {
    yield buffer
  }
}

// =============================================================================
// 9.6 畳み込み（Fold/Reduce）
// =============================================================================

/**
 * ストリームを畳み込む（reduce）。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * foldStream(list, 0, (acc, x) => acc + x) // => 15
 */
export const foldStream = <A, B>(
  gen: LazyList<A>,
  initial: B,
  fn: (acc: B, a: A) => B
): B => {
  let result = initial
  for (const item of gen) {
    result = fn(result, item)
  }
  return result
}

/**
 * ストリームを配列に変換する。
 *
 * @example
 * const list = fromArray([1, 2, 3])
 * toArray(list) // => [1, 2, 3]
 */
export const toArray = <A>(gen: LazyList<A>): readonly A[] => {
  return [...gen]
}

/**
 * ストリームの最初の要素を取得する。
 *
 * @example
 * const list = fromArray([1, 2, 3])
 * head(list) // => some(1)
 */
export const head = <A>(gen: LazyList<A>): O.Option<A> => {
  const result = gen.next()
  return result.done ? O.none : O.some(result.value)
}

/**
 * ストリームの最後の要素を取得する。
 * 注意: 無限ストリームには使用しないこと。
 *
 * @example
 * const list = fromArray([1, 2, 3])
 * last(list) // => some(3)
 */
export const last = <A>(gen: LazyList<A>): O.Option<A> => {
  let lastValue: O.Option<A> = O.none
  for (const item of gen) {
    lastValue = O.some(item)
  }
  return lastValue
}

/**
 * 条件を満たす最初の要素を取得する。
 *
 * @example
 * const list = fromArray([1, 2, 3, 4, 5])
 * find(list, x => x > 3) // => some(4)
 */
export const find = <A>(
  gen: LazyList<A>,
  predicate: (a: A) => boolean
): O.Option<A> => {
  for (const item of gen) {
    if (predicate(item)) {
      return O.some(item)
    }
  }
  return O.none
}

// =============================================================================
// 9.7 トレンド検出（為替レートの例）
// =============================================================================

/**
 * 値のリストが上昇トレンドかどうかを判定する。
 *
 * @example
 * trending([0.81, 0.82, 0.83]) // => true
 * trending([0.81, 0.84, 0.83]) // => false
 */
export const trending = (rates: readonly number[]): boolean => {
  if (rates.length <= 1) return false
  const prev = RA.dropRight(1)(rates)
  const curr = RA.dropLeft(1)(rates)
  return pipe(
    RA.zip(curr)(prev),
    RA.every(([p, c]) => c > p)
  )
}

/**
 * 値のリストが下降トレンドかどうかを判定する。
 *
 * @example
 * downtrending([0.83, 0.82, 0.81]) // => true
 * downtrending([0.81, 0.84, 0.83]) // => false
 */
export const downtrending = (rates: readonly number[]): boolean => {
  if (rates.length <= 1) return false
  const prev = RA.dropRight(1)(rates)
  const curr = RA.dropLeft(1)(rates)
  return pipe(
    RA.zip(curr)(prev),
    RA.every(([p, c]) => c < p)
  )
}

/**
 * 値のリストが安定している（全て同じ値）かどうかを判定する。
 *
 * @example
 * isStable([5, 5, 5]) // => true
 * isStable([5, 5, 6]) // => false
 */
export const isStable = (values: readonly number[]): boolean => {
  if (values.length < 2) return false
  const first = values[0]
  return values.every((v) => v === first)
}

/**
 * スライディングウィンドウでトレンドを検出する。
 *
 * @example
 * const rates = fromArray([0.80, 0.81, 0.82, 0.83, 0.82])
 * [...detectTrends(rates, 3)]
 * // => [{ window: [0.80, 0.81, 0.82], trending: true }, ...]
 */
export function* detectTrends(
  gen: LazyList<number>,
  windowSize: number
): LazyList<{ readonly window: readonly number[]; readonly trending: boolean }> {
  for (const window of sliding(gen, windowSize)) {
    yield { window, trending: trending(window) }
  }
}

// =============================================================================
// 9.8 サイコロストリーム
// =============================================================================

/**
 * サイコロを振る無限ストリームを作成する。
 *
 * @example
 * const dice = dieStream()
 * take(dice, 5) // => [3, 1, 6, 2, 4] (ランダム)
 */
export function* dieStream(): LazyList<number> {
  while (true) {
    yield Math.floor(Math.random() * 6) + 1
  }
}

/**
 * 指定した値が出るまでサイコロを振り続ける。
 *
 * @example
 * const until6 = dieUntil(6)
 * [...until6] // => [3, 1, 5, 2, 6] (6が出るまで)
 */
export function* dieUntil(target: number): LazyList<number> {
  while (true) {
    const roll = Math.floor(Math.random() * 6) + 1
    yield roll
    if (roll === target) break
  }
}

/**
 * サイコロを振って合計が指定値以上になるまでのストリーム。
 *
 * @example
 * const rolls = dieUntilSum(20)
 * [...rolls] // => サイコロの値（合計が20以上になるまで）
 */
export function* dieUntilSum(targetSum: number): LazyList<number> {
  let sum = 0
  while (sum < targetSum) {
    const roll = Math.floor(Math.random() * 6) + 1
    yield roll
    sum += roll
  }
}

// =============================================================================
// 9.9 ストリームと IO の組み合わせ
// =============================================================================

/**
 * Task（非同期）を繰り返し実行するストリームを作成する。
 * 各要素は非同期に取得される。
 */
export const repeatTask = <A>(task: T.Task<A>) =>
  async function* (): AsyncGenerator<A, void, undefined> {
    while (true) {
      yield await task()
    }
  }

/**
 * AsyncGenerator から最初の n 要素を取得する。
 */
export const takeAsync = async <A>(
  gen: AsyncGenerator<A, void, undefined>,
  n: number
): Promise<readonly A[]> => {
  const result: A[] = []
  let count = 0
  for await (const item of gen) {
    if (count >= n) break
    result.push(item)
    count++
  }
  return result
}

/**
 * 指定間隔で値を生成する非同期ストリームを作成する。
 *
 * @example
 * const ticks = interval(1000)
 * // 1秒ごとに 0, 1, 2, ... を生成
 */
export const interval = (ms: number) =>
  async function* (): AsyncGenerator<number, void, undefined> {
    let count = 0
    while (true) {
      await new Promise((resolve) => setTimeout(resolve, ms))
      yield count++
    }
  }

/**
 * 2つの非同期ストリームを結合する（左側の値を返す）。
 * 右側のストリームのペースで進む。
 */
export const zipLeftAsync = <A, B>(
  left: () => AsyncGenerator<A, void, undefined>,
  right: () => AsyncGenerator<B, void, undefined>
) =>
  async function* (): AsyncGenerator<A, void, undefined> {
    const leftIter = left()
    const rightIter = right()
    while (true) {
      const [leftResult, rightResult] = await Promise.all([
        leftIter.next(),
        rightIter.next(),
      ])
      if (leftResult.done || rightResult.done) break
      yield leftResult.value
    }
  }

// =============================================================================
// 9.10 ストリームユーティリティ
// =============================================================================

/**
 * ストリームが空かどうかを判定する。
 *
 * @example
 * isEmpty(empty()) // => true
 * isEmpty(fromArray([1])) // => false
 */
export const isEmpty = <A>(gen: LazyList<A>): boolean => {
  return gen.next().done === true
}

/**
 * ストリームの要素数を数える。
 * 注意: 無限ストリームには使用しないこと。
 *
 * @example
 * count(fromArray([1, 2, 3])) // => 3
 */
export const count = <A>(gen: LazyList<A>): number => {
  let n = 0
  for (const _ of gen) {
    n++
  }
  return n
}

/**
 * 全ての要素が条件を満たすかどうかを判定する。
 *
 * @example
 * forAll(fromArray([2, 4, 6]), x => x % 2 === 0) // => true
 */
export const forAll = <A>(
  gen: LazyList<A>,
  predicate: (a: A) => boolean
): boolean => {
  for (const item of gen) {
    if (!predicate(item)) return false
  }
  return true
}

/**
 * いずれかの要素が条件を満たすかどうかを判定する。
 *
 * @example
 * exists(fromArray([1, 3, 5]), x => x % 2 === 0) // => false
 * exists(fromArray([1, 2, 5]), x => x % 2 === 0) // => true
 */
export const exists = <A>(
  gen: LazyList<A>,
  predicate: (a: A) => boolean
): boolean => {
  for (const item of gen) {
    if (predicate(item)) return true
  }
  return false
}

/**
 * 重複を除去する（連続した重複のみ）。
 *
 * @example
 * [...dedup(fromArray([1, 1, 2, 2, 3, 1, 1]))]
 * // => [1, 2, 3, 1]
 */
export function* dedup<A>(gen: LazyList<A>): LazyList<A> {
  let prev: O.Option<A> = O.none
  for (const item of gen) {
    if (O.isNone(prev) || prev.value !== item) {
      yield item
      prev = O.some(item)
    }
  }
}

/**
 * ストリームにインターリーブする（交互に要素を取る）。
 *
 * @example
 * const list1 = fromArray([1, 2, 3])
 * const list2 = fromArray([10, 20, 30])
 * [...interleave(list1, list2)] // => [1, 10, 2, 20, 3, 30]
 */
export function* interleave<A>(
  gen1: LazyList<A>,
  gen2: LazyList<A>
): LazyList<A> {
  const iter1 = gen1[Symbol.iterator]()
  const iter2 = gen2[Symbol.iterator]()
  while (true) {
    const next1 = iter1.next()
    if (next1.done) {
      yield* { [Symbol.iterator]: () => iter2 }
      break
    }
    yield next1.value

    const next2 = iter2.next()
    if (next2.done) {
      yield* { [Symbol.iterator]: () => iter1 }
      break
    }
    yield next2.value
  }
}

/**
 * 累積和を計算する。
 *
 * @example
 * [...scan(fromArray([1, 2, 3, 4]), 0, (a, b) => a + b)]
 * // => [0, 1, 3, 6, 10]
 */
export function* scan<A, B>(
  gen: LazyList<A>,
  initial: B,
  fn: (acc: B, a: A) => B
): LazyList<B> {
  let acc = initial
  yield acc
  for (const item of gen) {
    acc = fn(acc, item)
    yield acc
  }
}

/**
 * フィボナッチ数列の無限ストリーム。
 *
 * @example
 * take(fibonacci(), 10) // => [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
 */
export function* fibonacci(): LazyList<number> {
  let [a, b] = [0, 1]
  while (true) {
    yield a
    ;[a, b] = [b, a + b]
  }
}

/**
 * 素数の無限ストリーム（エラトステネスの篩）。
 *
 * @example
 * take(primes(), 10) // => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
 */
export function* primes(): LazyList<number> {
  const sieve: Set<number> = new Set()
  let n = 2
  while (true) {
    if (!sieve.has(n)) {
      yield n
      // n の倍数をマーク
      for (let i = n * n; i < n * n + n * 100; i += n) {
        sieve.add(i)
      }
    }
    n++
  }
}
