/**
 * 第8章: IO モナドの導入
 *
 * 副作用を純粋関数内で安全に記述する方法を学びます。
 * fp-ts では IO<A> (同期) と Task<A> (非同期) を使います。
 */

import { pipe } from 'fp-ts/function'
import * as IO from 'fp-ts/IO'
import * as T from 'fp-ts/Task'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as RA from 'fp-ts/ReadonlyArray'

// =============================================================================
// 8.1 IO の基本
// =============================================================================

/**
 * IO<A> は「実行すると A 型の値を返す副作用のある計算」を表す。
 * IO 値を作成しただけでは副作用は発生しない。
 * () で実行すると副作用が発生する。
 */

/**
 * 純粋な値を IO にラップする。
 *
 * @example
 * const io = pureIO(42)
 * io() // => 42 (副作用なし)
 */
export const pureIO = <A>(a: A): IO.IO<A> => IO.of(a)

/**
 * 副作用のある計算を IO にラップする。
 *
 * @example
 * const io = delayIO(() => console.log("Hello"))
 * // この時点では何も出力されない
 * io() // => "Hello" が出力される
 */
export const delayIO = <A>(thunk: () => A): IO.IO<A> => thunk

/**
 * 現在時刻を取得する IO。
 *
 * @example
 * const now = getCurrentTime()
 * now() // => 現在のタイムスタンプ
 */
export const getCurrentTime: IO.IO<number> = () => Date.now()

/**
 * 乱数を生成する IO。
 *
 * @example
 * const random = getRandomNumber()
 * random() // => 0-1 の乱数
 */
export const getRandomNumber: IO.IO<number> = () => Math.random()

// =============================================================================
// 8.2 サイコロを振る例
// =============================================================================

/**
 * 不純な関数（副作用あり）- 直接乱数を返す。
 */
export const castTheDieImpure = (): number => Math.floor(Math.random() * 6) + 1

/**
 * IO を使った純粋な記述。
 * 作成時点では副作用は発生しない。
 *
 * @example
 * const die = castTheDie()
 * // まだ実行されていない
 * die() // => 1-6 のランダムな値
 */
export const castTheDie: IO.IO<number> = () => castTheDieImpure()

/**
 * サイコロを2回振って合計を返す。
 * IO の合成例。
 *
 * @example
 * const dice = castTheDieTwice()
 * dice() // => 2-12 のランダムな値
 */
export const castTheDieTwice: IO.IO<number> = pipe(
  castTheDie,
  IO.chain((first) =>
    pipe(
      castTheDie,
      IO.map((second) => first + second)
    )
  )
)

/**
 * サイコロを n 回振って合計を返す。
 *
 * @example
 * const dice = castTheDieNTimes(3)
 * dice() // => 3-18 のランダムな値
 */
export const castTheDieNTimes = (n: number): IO.IO<number> => {
  if (n <= 0) return IO.of(0)
  const casts = RA.replicate(n, castTheDie)
  return pipe(
    casts,
    IO.sequenceArray,
    IO.map(RA.reduce(0, (acc, val) => acc + val))
  )
}

// =============================================================================
// 8.3 IO の合成
// =============================================================================

/**
 * IO の値を変換する（map）。
 *
 * @example
 * const doubled = mapIO(IO.of(5), x => x * 2)
 * doubled() // => 10
 */
export const mapIO = <A, B>(io: IO.IO<A>, f: (a: A) => B): IO.IO<B> =>
  pipe(io, IO.map(f))

/**
 * IO を返す関数を適用する（chain/flatMap）。
 *
 * @example
 * const result = chainIO(IO.of(5), x => IO.of(x * 2))
 * result() // => 10
 */
export const chainIO = <A, B>(
  io: IO.IO<A>,
  f: (a: A) => IO.IO<B>
): IO.IO<B> => pipe(io, IO.chain(f))

/**
 * 複数の IO を順番に実行して結果を配列で返す（sequence）。
 *
 * @example
 * const ios = [IO.of(1), IO.of(2), IO.of(3)]
 * const combined = sequenceIO(ios)
 * combined() // => [1, 2, 3]
 */
export const sequenceIO = <A>(ios: readonly IO.IO<A>[]): IO.IO<readonly A[]> =>
  pipe(ios, IO.sequenceArray)

// =============================================================================
// 8.4 Task（非同期 IO）
// =============================================================================

/**
 * Task<A> は「実行すると Promise<A> を返す非同期計算」を表す。
 * IO と同様に、作成時点では実行されない。
 */

/**
 * 純粋な値を Task にラップする。
 *
 * @example
 * const task = pureTask(42)
 * await task() // => 42
 */
export const pureTask = <A>(a: A): T.Task<A> => T.of(a)

/**
 * 指定ミリ秒後に値を返す Task。
 *
 * @example
 * const delayed = delayedTask(100, "Hello")
 * await delayed() // => 100ms 後に "Hello"
 */
export const delayedTask = <A>(ms: number, value: A): T.Task<A> =>
  () =>
    new Promise((resolve) => setTimeout(() => resolve(value), ms))

/**
 * Task の値を変換する（map）。
 */
export const mapTask = <A, B>(task: T.Task<A>, f: (a: A) => B): T.Task<B> =>
  pipe(task, T.map(f))

/**
 * Task を返す関数を適用する（chain/flatMap）。
 */
export const chainTask = <A, B>(
  task: T.Task<A>,
  f: (a: A) => T.Task<B>
): T.Task<B> => pipe(task, T.chain(f))

/**
 * 複数の Task を順番に実行して結果を配列で返す（sequence）。
 *
 * @example
 * const tasks = [T.of(1), T.of(2), T.of(3)]
 * const combined = sequenceTask(tasks)
 * await combined() // => [1, 2, 3]
 */
export const sequenceTask = <A>(
  tasks: readonly T.Task<A>[]
): T.Task<readonly A[]> => pipe(tasks, T.sequenceArray)

// =============================================================================
// 8.5 TaskEither（非同期 + エラーハンドリング）
// =============================================================================

/**
 * TaskEither<E, A> は「実行すると Promise<Either<E, A>> を返す非同期計算」。
 * 成功時は Right<A>、失敗時は Left<E> を返す。
 */

/**
 * 成功値を TaskEither にラップする。
 *
 * @example
 * const task = rightTask(42)
 * await task() // => right(42)
 */
export const rightTask = <E, A>(a: A): TE.TaskEither<E, A> => TE.right(a)

/**
 * エラー値を TaskEither にラップする。
 *
 * @example
 * const task = leftTask("error")
 * await task() // => left("error")
 */
export const leftTask = <E, A>(e: E): TE.TaskEither<E, A> => TE.left(e)

/**
 * Promise を TaskEither に変換する。
 * reject された場合は Left になる。
 *
 * @example
 * const task = fromPromise(() => Promise.resolve(42), String)
 * await task() // => right(42)
 */
export const fromPromise = <E, A>(
  promise: () => Promise<A>,
  onRejected: (reason: unknown) => E
): TE.TaskEither<E, A> => TE.tryCatch(promise, onRejected)

// =============================================================================
// 8.6 ミーティングスケジューリングの例
// =============================================================================

/** ミーティング時間を表すインターフェース */
export interface MeetingTime {
  readonly startHour: number
  readonly endHour: number
}

/** MeetingTime を作成する */
export const createMeetingTime = (
  startHour: number,
  endHour: number
): MeetingTime => ({ startHour, endHour })

/**
 * 2つのミーティングが重複しているかを判定する（純粋関数）。
 *
 * @example
 * meetingsOverlap({ startHour: 9, endHour: 10 }, { startHour: 10, endHour: 11 })
 * // => false (隣接しているが重複なし)
 *
 * meetingsOverlap({ startHour: 9, endHour: 11 }, { startHour: 10, endHour: 12 })
 * // => true
 */
export const meetingsOverlap = (
  meeting1: MeetingTime,
  meeting2: MeetingTime
): boolean =>
  meeting1.startHour < meeting2.endHour &&
  meeting2.startHour < meeting1.endHour

/**
 * 空き時間を計算する（純粋関数）。
 *
 * @example
 * const existing = [{ startHour: 9, endHour: 10 }, { startHour: 14, endHour: 15 }]
 * possibleMeetings(existing, 8, 17, 1)
 * // => [{ startHour: 8, endHour: 9 }, { startHour: 10, endHour: 11 }, ...]
 */
export const possibleMeetings = (
  existingMeetings: readonly MeetingTime[],
  startHour: number,
  endHour: number,
  lengthHours: number
): readonly MeetingTime[] => {
  const slots = pipe(
    RA.makeBy(endHour - lengthHours - startHour + 1, (i) => startHour + i),
    RA.map((start) => createMeetingTime(start, start + lengthHours))
  )
  return pipe(
    slots,
    RA.filter((slot) =>
      existingMeetings.every((meeting) => !meetingsOverlap(meeting, slot))
    )
  )
}

/**
 * API からカレンダーエントリを取得する（シミュレーション）。
 * 実際には外部 API 呼び出しになる。
 */
export const calendarEntriesApi = (
  name: string
): TE.TaskEither<string, readonly MeetingTime[]> => {
  // シミュレーション: Alice と Bob には予定がある
  const mockData: Record<string, readonly MeetingTime[]> = {
    Alice: [createMeetingTime(9, 10), createMeetingTime(14, 15)],
    Bob: [createMeetingTime(10, 11), createMeetingTime(15, 16)],
  }
  return name in mockData
    ? TE.right(mockData[name])
    : TE.left(`Calendar not found for ${name}`)
}

/**
 * 複数の参加者の予定を取得して結合する。
 *
 * @example
 * const meetings = scheduledMeetings(["Alice", "Bob"])
 * await meetings() // => right([...])
 */
export const scheduledMeetings = (
  attendees: readonly string[]
): TE.TaskEither<string, readonly MeetingTime[]> =>
  pipe(
    attendees,
    RA.traverse(TE.ApplicativeSeq)(calendarEntriesApi),
    TE.map(RA.flatten)
  )

// =============================================================================
// 8.7 orElse によるエラーハンドリング
// =============================================================================

/**
 * TaskEither が失敗した場合に代替を使用する。
 *
 * @example
 * const task = orElseTE(TE.left("error"), () => TE.right(42))
 * await task() // => right(42)
 */
export const orElseTE = <E, A>(
  te: TE.TaskEither<E, A>,
  alternative: (e: E) => TE.TaskEither<E, A>
): TE.TaskEither<E, A> => pipe(te, TE.orElse(alternative))

/**
 * リトライ関数。指定回数まで再試行する。
 *
 * @example
 * const task = retry(unstableApi, 3)
 * // 最大3回まで再試行
 */
export const retry = <E, A>(
  action: TE.TaskEither<E, A>,
  maxRetries: number
): TE.TaskEither<E, A> => {
  if (maxRetries <= 0) return action
  return pipe(
    action,
    TE.orElse(() => retry(action, maxRetries - 1))
  )
}

/**
 * リトライしてもダメならデフォルト値を返す。
 *
 * @example
 * const task = retryWithDefault(unstableApi, 3, [])
 * await task() // => 成功 or []
 */
export const retryWithDefault = <E, A>(
  action: TE.TaskEither<E, A>,
  maxRetries: number,
  defaultValue: A
): T.Task<A> =>
  pipe(
    retry(action, maxRetries),
    TE.getOrElse(() => T.of(defaultValue))
  )

// =============================================================================
// 8.8 IO のユーティリティ
// =============================================================================

/**
 * 値をコンソールに出力する IO。
 *
 * @example
 * const io = consoleLog("Hello")
 * io() // => コンソールに "Hello" が出力される
 */
export const consoleLog = (message: string): IO.IO<void> => () =>
  console.log(message)

/**
 * 複数の IO を順番に実行し、最後の結果を返す。
 *
 * @example
 * const io = chainAll(IO.of(1), IO.of(2), IO.of(3))
 * io() // => 3
 */
export const chainAll = <A>(...ios: readonly IO.IO<A>[]): IO.IO<A> => {
  if (ios.length === 0) throw new Error('At least one IO required')
  return ios.reduce((acc, io) =>
    pipe(
      acc,
      IO.chain(() => io)
    )
  )
}

/**
 * メッセージを出力して同じ値を返す IO。
 * デバッグ用。
 *
 * @example
 * const io = printAndReturn("Hello")
 * io() // => "Hello" を出力して "Hello" を返す
 */
export const printAndReturn = (message: string): IO.IO<string> => () => {
  console.log(message)
  return message
}

// =============================================================================
// 8.9 IO と Either の組み合わせ
// =============================================================================

/**
 * IO<Either<E, A>> を表す型。
 * 同期的だがエラーを返す可能性のある計算。
 */
export type IOEither<E, A> = IO.IO<E.Either<E, A>>

/**
 * 成功値を IOEither にラップする。
 */
export const rightIOE = <E, A>(a: A): IOEither<E, A> => IO.of(E.right(a))

/**
 * エラー値を IOEither にラップする。
 */
export const leftIOE = <E, A>(e: E): IOEither<E, A> => IO.of(E.left(e))

/**
 * 例外をスローする可能性のある関数を IOEither にラップする。
 *
 * @example
 * const io = tryCatchIO(() => JSON.parse("invalid"), String)
 * io() // => left("Unexpected token ...")
 */
export const tryCatchIO = <E, A>(
  thunk: () => A,
  onThrow: (error: unknown) => E
): IOEither<E, A> => () => {
  try {
    return E.right(thunk())
  } catch (e) {
    return E.left(onThrow(e))
  }
}

// =============================================================================
// 8.10 メモ化とキャッシュ
// =============================================================================

/**
 * IO の結果をメモ化する。
 * 最初の実行結果がキャッシュされ、以降は同じ値を返す。
 *
 * @example
 * let count = 0
 * const io = memoizeIO(() => { count++; return count })
 * io() // => 1
 * io() // => 1 (キャッシュされた値)
 */
export const memoizeIO = <A>(io: IO.IO<A>): IO.IO<A> => {
  let cached: O.Option<A> = O.none
  return () =>
    pipe(
      cached,
      O.getOrElse(() => {
        const result = io()
        cached = O.some(result)
        return result
      })
    )
}
