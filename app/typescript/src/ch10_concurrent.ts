/**
 * 第10章: 並行・並列処理
 *
 * 関数型プログラミングにおける並行処理を学びます。
 * TypeScript では Promise と fp-ts の Task を使って実装します。
 */

import { pipe } from 'fp-ts/function'
import * as T from 'fp-ts/Task'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as RA from 'fp-ts/ReadonlyArray'
import * as IO from 'fp-ts/IO'

// =============================================================================
// 10.1 Ref - アトミックな共有状態
// =============================================================================

/**
 * Ref は、複数の非同期処理から安全にアクセスできる参照を表す。
 * TypeScript では単純なクロージャで実装する。
 * 注意: 真のアトミック性は保証されないが、単一スレッド環境では安全。
 */
export interface Ref<A> {
  /** 現在の値を取得 */
  readonly get: T.Task<A>
  /** 値を設定 */
  readonly set: (value: A) => T.Task<void>
  /** アトミックに更新 */
  readonly update: (f: (a: A) => A) => T.Task<void>
  /** 更新して古い値を返す */
  readonly getAndUpdate: (f: (a: A) => A) => T.Task<A>
  /** 更新して新しい値を返す */
  readonly updateAndGet: (f: (a: A) => A) => T.Task<A>
  /** 更新して結果を返す（modify） */
  readonly modify: <B>(f: (a: A) => readonly [A, B]) => T.Task<B>
}

/**
 * 初期値から Ref を作成する。
 *
 * @example
 * const counter = createRef(0)
 * await counter.update(n => n + 1)()
 * await counter.get() // => 1
 */
export const createRef = <A>(initial: A): Ref<A> => {
  let value = initial
  return {
    get: () => Promise.resolve(value),
    set: (newValue: A) => () => {
      value = newValue
      return Promise.resolve()
    },
    update: (f: (a: A) => A) => () => {
      value = f(value)
      return Promise.resolve()
    },
    getAndUpdate: (f: (a: A) => A) => () => {
      const old = value
      value = f(value)
      return Promise.resolve(old)
    },
    updateAndGet: (f: (a: A) => A) => () => {
      value = f(value)
      return Promise.resolve(value)
    },
    modify: <B>(f: (a: A) => readonly [A, B]) => () => {
      const [newValue, result] = f(value)
      value = newValue
      return Promise.resolve(result)
    },
  }
}

/**
 * IO ベースの同期 Ref を作成する。
 */
export interface RefIO<A> {
  readonly get: IO.IO<A>
  readonly set: (value: A) => IO.IO<void>
  readonly update: (f: (a: A) => A) => IO.IO<void>
  readonly getAndUpdate: (f: (a: A) => A) => IO.IO<A>
  readonly updateAndGet: (f: (a: A) => A) => IO.IO<A>
}

/**
 * 同期 Ref を作成する。
 */
export const createRefIO = <A>(initial: A): RefIO<A> => {
  let value = initial
  return {
    get: () => value,
    set: (newValue: A) => () => {
      value = newValue
    },
    update: (f: (a: A) => A) => () => {
      value = f(value)
    },
    getAndUpdate: (f: (a: A) => A) => () => {
      const old = value
      value = f(value)
      return old
    },
    updateAndGet: (f: (a: A) => A) => () => {
      value = f(value)
      return value
    },
  }
}

// =============================================================================
// 10.2 並列実行
// =============================================================================

/**
 * 複数の Task を並列実行して結果を配列で返す（parSequence）。
 *
 * @example
 * const tasks = [T.of(1), T.of(2), T.of(3)]
 * await parSequence(tasks)() // => [1, 2, 3]
 */
export const parSequence = <A>(
  tasks: readonly T.Task<A>[]
): T.Task<readonly A[]> => () => Promise.all(tasks.map((t) => t()))

/**
 * 複数の TaskEither を並列実行して結果を配列で返す。
 * いずれかが失敗した場合は最初のエラーを返す。
 *
 * @example
 * const tasks = [TE.right(1), TE.right(2), TE.right(3)]
 * await parSequenceTE(tasks)() // => right([1, 2, 3])
 */
export const parSequenceTE = <E, A>(
  tasks: readonly TE.TaskEither<E, A>[]
): TE.TaskEither<E, readonly A[]> => async () => {
  const results = await Promise.all(tasks.map((t) => t()))
  const firstError = results.find(E.isLeft)
  if (firstError && E.isLeft(firstError)) {
    return firstError as E.Either<E, readonly A[]>
  }
  return E.right(
    results.map((r) => (E.isRight(r) ? r.right : (undefined as never)))
  )
}

/**
 * 複数の Task を並列実行して、完了したものから結果を返す。
 * Promise.race と同等。
 *
 * @example
 * const tasks = [delayedTask(100, 1), delayedTask(50, 2)]
 * await race(tasks)() // => 2 (先に完了)
 */
export const race = <A>(tasks: readonly T.Task<A>[]): T.Task<A> => () =>
  Promise.race(tasks.map((t) => t()))

/**
 * 複数の Task を並列実行して、最初に成功したものを返す。
 * 全て失敗した場合は最後のエラーを返す。
 */
export const raceSuccess = <A>(
  tasks: readonly T.Task<A>[]
): T.Task<O.Option<A>> => async () => {
  if (tasks.length === 0) return O.none
  try {
    const result = await Promise.any(tasks.map((t) => t()))
    return O.some(result)
  } catch {
    return O.none
  }
}

/**
 * 配列の各要素に対して Task を適用し、並列実行する（parTraverse）。
 *
 * @example
 * const result = await parTraverse([1, 2, 3], n => T.of(n * 2))()
 * // => [2, 4, 6]
 */
export const parTraverse = <A, B>(
  items: readonly A[],
  f: (a: A) => T.Task<B>
): T.Task<readonly B[]> => parSequence(items.map(f))

/**
 * 配列の各要素に対して TaskEither を適用し、並列実行する。
 */
export const parTraverseTE = <A, E, B>(
  items: readonly A[],
  f: (a: A) => TE.TaskEither<E, B>
): TE.TaskEither<E, readonly B[]> => parSequenceTE(items.map(f))

// =============================================================================
// 10.3 遅延とスリープ
// =============================================================================

/**
 * 指定ミリ秒後に値を返す Task。
 *
 * @example
 * const task = delay(100, 42)
 * await task() // 100ms 後に 42
 */
export const delay = <A>(ms: number, value: A): T.Task<A> => () =>
  new Promise((resolve) => setTimeout(() => resolve(value), ms))

/**
 * 指定ミリ秒スリープする Task。
 *
 * @example
 * await sleep(100)() // 100ms スリープ
 */
export const sleep = (ms: number): T.Task<void> => delay(ms, undefined)

/**
 * 指定ミリ秒後に Task を実行する。
 *
 * @example
 * const task = after(100, T.of(42))
 * await task() // 100ms 後に 42
 */
export const after = <A>(ms: number, task: T.Task<A>): T.Task<A> =>
  pipe(
    sleep(ms),
    T.chain(() => task)
  )

// =============================================================================
// 10.4 Fiber - 軽量スレッドのシミュレーション
// =============================================================================

/**
 * Fiber は、バックグラウンドで実行される計算を表す。
 * TypeScript では AbortController を使ってキャンセルを実装する。
 */
export interface Fiber<A> {
  /** 結果を待機して取得 */
  readonly join: T.Task<A>
  /** 実行をキャンセル */
  readonly cancel: T.Task<void>
  /** キャンセルされたかどうか */
  readonly isCancelled: IO.IO<boolean>
}

/**
 * Task を Fiber として起動する。
 * バックグラウンドで実行が開始される。
 *
 * @example
 * const fiber = start(T.of(42))
 * const result = await fiber.join()
 * // => 42
 */
export const start = <A>(task: T.Task<A>): Fiber<A> => {
  let cancelled = false
  let resolveResult: ((value: A) => void) | null = null
  let rejectResult: ((error: unknown) => void) | null = null

  const resultPromise = new Promise<A>((resolve, reject) => {
    resolveResult = resolve
    rejectResult = reject
  })

  // バックグラウンドで実行
  task()
    .then((result) => {
      if (!cancelled && resolveResult) {
        resolveResult(result)
      }
    })
    .catch((error) => {
      if (!cancelled && rejectResult) {
        rejectResult(error)
      }
    })

  return {
    join: () => resultPromise,
    cancel: () => {
      cancelled = true
      return Promise.resolve()
    },
    isCancelled: () => cancelled,
  }
}

/**
 * キャンセル可能な Task を作成する。
 * AbortSignal を受け取り、キャンセル時に中断する。
 */
export interface CancellableTask<A> {
  readonly run: T.Task<O.Option<A>>
  readonly cancel: IO.IO<void>
}

/**
 * キャンセル可能な Task を作成する。
 *
 * @example
 * const { run, cancel } = cancellable(async (signal) => {
 *   await sleep(1000)()
 *   if (signal.aborted) return O.none
 *   return O.some(42)
 * })
 */
export const cancellable = <A>(
  task: (signal: AbortSignal) => Promise<O.Option<A>>
): CancellableTask<A> => {
  const controller = new AbortController()
  return {
    run: () => task(controller.signal),
    cancel: () => {
      controller.abort()
    },
  }
}

// =============================================================================
// 10.5 永続的な実行
// =============================================================================

/**
 * Task を永遠に繰り返し実行する（foreverM）。
 * 注意: キャンセルしないと永遠に実行される。
 *
 * @example
 * // 1秒ごとに "tick" を出力
 * const ticker = forever(
 *   pipe(sleep(1000), T.chain(() => T.of(console.log("tick"))))
 * )
 */
export const forever = <A>(task: T.Task<A>): T.Task<never> =>
  pipe(
    task,
    T.chain(() => forever(task))
  )

/**
 * 指定回数だけ Task を繰り返し実行する。
 *
 * @example
 * let count = 0
 * await repeatN(T.of(count++), 3)()
 * // count は 3
 */
export const repeatN = <A>(task: T.Task<A>, n: number): T.Task<void> => {
  if (n <= 0) return T.of(undefined)
  return pipe(
    task,
    T.chain(() => repeatN(task, n - 1))
  )
}

/**
 * 条件が真の間、Task を繰り返し実行する。
 *
 * @example
 * let count = 0
 * await repeatWhile(T.of(count++), () => count < 5)()
 * // count は 5
 */
export const repeatWhile = <A>(
  task: T.Task<A>,
  condition: () => boolean
): T.Task<void> => {
  if (!condition()) return T.of(undefined)
  return pipe(
    task,
    T.chain(() => repeatWhile(task, condition))
  )
}

/**
 * キャンセル可能な永続実行を提供する。
 * cancel が呼ばれるまで Task を繰り返し実行する。
 */
export interface CancellableForever {
  readonly cancel: IO.IO<void>
  readonly isCancelled: IO.IO<boolean>
}

/**
 * キャンセル可能な永続実行を開始する。
 *
 * @example
 * const { cancel, isCancelled } = startForever(
 *   pipe(sleep(100), T.chain(() => T.of(console.log("tick"))))
 * )
 * await sleep(500)()
 * cancel()
 */
export const startForever = <A>(task: T.Task<A>): CancellableForever => {
  let cancelled = false

  const runLoop = async (): Promise<void> => {
    while (!cancelled) {
      await task()
    }
  }

  // バックグラウンドで開始
  runLoop()

  return {
    cancel: () => {
      cancelled = true
    },
    isCancelled: () => cancelled,
  }
}

// =============================================================================
// 10.6 チェックインのリアルタイム集計
// =============================================================================

/** 都市を表す型 */
export interface City {
  readonly name: string
}

/** 都市の統計情報 */
export interface CityStats {
  readonly city: City
  readonly checkIns: number
}

/**
 * City を作成する。
 */
export const createCity = (name: string): City => ({ name })

/**
 * CityStats を作成する。
 */
export const createCityStats = (city: City, checkIns: number): CityStats => ({
  city,
  checkIns,
})

/**
 * チェックイン数のマップからトップ N 都市を計算する（純粋関数）。
 *
 * @example
 * const map = new Map([['Tokyo', 100], ['Osaka', 50], ['Kyoto', 30]])
 * topCities(map, 2) // => [{ city: 'Tokyo', checkIns: 100 }, { city: 'Osaka', checkIns: 50 }]
 */
export const topCities = (
  cityCheckIns: ReadonlyMap<string, number>,
  n: number
): readonly CityStats[] =>
  pipe(
    Array.from(cityCheckIns.entries()),
    RA.map(([name, checkIns]) => createCityStats(createCity(name), checkIns)),
    RA.sort({
      compare: (a, b) => b.checkIns - a.checkIns,
      equals: (a, b) => a.checkIns === b.checkIns,
    }),
    RA.takeLeft(n)
  )

/**
 * チェックインを保存する（Ref を使用）。
 */
export const storeCheckIn =
  (storedCheckIns: Ref<ReadonlyMap<string, number>>) =>
  (city: City): T.Task<void> =>
    storedCheckIns.update((map) => {
      const current = map.get(city.name) ?? 0
      return new Map([...map, [city.name, current + 1]])
    })

/**
 * ランキングを更新する（Ref を使用）。
 */
export const updateRanking =
  (
    storedCheckIns: Ref<ReadonlyMap<string, number>>,
    storedRanking: Ref<readonly CityStats[]>,
    topN: number
  ): T.Task<void> =>
  async () => {
    const checkIns = await storedCheckIns.get()
    const ranking = topCities(checkIns, topN)
    await storedRanking.set(ranking)()
  }

/**
 * チェックイン処理の結果を表すインターフェース。
 * 呼び出し元に制御を返しつつ、バックグラウンドで処理を続ける。
 */
export interface ProcessingCheckIns {
  /** 現在のランキングを取得 */
  readonly currentRanking: T.Task<readonly CityStats[]>
  /** 現在のチェックイン数を取得 */
  readonly currentCheckIns: T.Task<ReadonlyMap<string, number>>
  /** 処理を停止 */
  readonly stop: IO.IO<void>
  /** 処理が停止しているかどうか */
  readonly isStopped: IO.IO<boolean>
}

/**
 * チェックイン処理を開始する。
 * バックグラウンドでランキングを継続的に更新する。
 *
 * @example
 * const processing = startProcessingCheckIns(3)
 * await processing.currentRanking() // 現在のランキング
 * processing.stop() // 停止
 */
export const startProcessingCheckIns = (topN: number): ProcessingCheckIns => {
  const storedCheckIns = createRef<ReadonlyMap<string, number>>(new Map())
  const storedRanking = createRef<readonly CityStats[]>([])
  let stopped = false

  // バックグラウンドでランキングを更新
  const runRankingLoop = async (): Promise<void> => {
    while (!stopped) {
      await updateRanking(storedCheckIns, storedRanking, topN)()
      await sleep(10)() // 10ms 間隔で更新
    }
  }

  runRankingLoop()

  return {
    currentRanking: storedRanking.get,
    currentCheckIns: storedCheckIns.get,
    stop: () => {
      stopped = true
    },
    isStopped: () => stopped,
  }
}

/**
 * チェックインを処理する（ProcessingCheckIns と連携）。
 */
export const processCheckIn =
  (processing: ProcessingCheckIns, storedCheckIns: Ref<ReadonlyMap<string, number>>) =>
  (city: City): T.Task<void> =>
    storeCheckIn(storedCheckIns)(city)

// =============================================================================
// 10.7 並列サイコロ
// =============================================================================

/**
 * サイコロを振る Task。
 */
export const castTheDieTask: T.Task<number> = () =>
  Promise.resolve(Math.floor(Math.random() * 6) + 1)

/**
 * n 個のサイコロを並列に振って結果を返す。
 *
 * @example
 * const results = await castDiceParallel(3)()
 * // => [4, 2, 6] (ランダム)
 */
export const castDiceParallel = (n: number): T.Task<readonly number[]> =>
  parSequence(RA.replicate(n, castTheDieTask))

/**
 * n 個のサイコロを並列に振って合計を返す。
 *
 * @example
 * const sum = await castDiceParallelSum(3)()
 * // => 12 (3-18 のランダム)
 */
export const castDiceParallelSum = (n: number): T.Task<number> =>
  pipe(
    castDiceParallel(n),
    T.map((results) => results.reduce((a, b) => a + b, 0))
  )

/**
 * n 個のサイコロを並列に振り、結果を Ref に保存する。
 *
 * @example
 * const ref = createRef<readonly number[]>([])
 * await castDiceAndStore(ref, 3)()
 * const results = await ref.get()
 * // => [4, 2, 6] (ランダム)
 */
export const castDiceAndStore = (
  storedCasts: Ref<readonly number[]>,
  n: number
): T.Task<void> =>
  pipe(
    castDiceParallel(n),
    T.chain((results) => storedCasts.set(results))
  )

// =============================================================================
// 10.8 タイムアウトと競争
// =============================================================================

/**
 * Task にタイムアウトを設定する。
 * タイムアウトした場合は none を返す。
 *
 * @example
 * const task = timeout(delay(1000, 42), 500)
 * await task() // => none (タイムアウト)
 */
export const timeout = <A>(
  task: T.Task<A>,
  ms: number
): T.Task<O.Option<A>> => async () => {
  const timeoutPromise = new Promise<O.Option<A>>((resolve) =>
    setTimeout(() => resolve(O.none), ms)
  )
  const taskPromise = task().then(O.some)
  return Promise.race([timeoutPromise, taskPromise])
}

/**
 * Task にタイムアウトを設定し、タイムアウト時はデフォルト値を返す。
 *
 * @example
 * const task = timeoutWithDefault(delay(1000, 42), 500, 0)
 * await task() // => 0 (タイムアウト)
 */
export const timeoutWithDefault = <A>(
  task: T.Task<A>,
  ms: number,
  defaultValue: A
): T.Task<A> =>
  pipe(
    timeout(task, ms),
    T.map(O.getOrElse(() => defaultValue))
  )

/**
 * 2つの Task を競争させ、先に完了した方の結果を返す。
 *
 * @example
 * const task = raceTwo(delay(100, 'first'), delay(200, 'second'))
 * await task() // => 'first'
 */
export const raceTwo = <A>(task1: T.Task<A>, task2: T.Task<A>): T.Task<A> =>
  race([task1, task2])

// =============================================================================
// 10.9 並列 Map/Reduce
// =============================================================================

/**
 * 配列を並列に処理して結果を集約する。
 *
 * @example
 * const result = await parMapReduce(
 *   [1, 2, 3],
 *   n => T.of(n * 2),
 *   0,
 *   (a, b) => a + b
 * )()
 * // => 12
 */
export const parMapReduce = <A, B>(
  items: readonly A[],
  mapper: (a: A) => T.Task<B>,
  initial: B,
  reducer: (acc: B, b: B) => B
): T.Task<B> =>
  pipe(
    parTraverse(items, mapper),
    T.map((results) => results.reduce(reducer, initial))
  )

/**
 * 配列を並列にフィルタリングする。
 * 述語関数が非同期の場合に使用。
 *
 * @example
 * const result = await parFilter(
 *   [1, 2, 3, 4, 5],
 *   n => T.of(n % 2 === 0)
 * )()
 * // => [2, 4]
 */
export const parFilter = <A>(
  items: readonly A[],
  predicate: (a: A) => T.Task<boolean>
): T.Task<readonly A[]> =>
  pipe(
    parTraverse(items, (item) =>
      pipe(
        predicate(item),
        T.map((keep) => (keep ? O.some(item) : O.none))
      )
    ),
    T.map(RA.compact)
  )

// =============================================================================
// 10.10 ユーティリティ
// =============================================================================

/**
 * 指定間隔で Task を繰り返し実行し、結果を収集する。
 *
 * @example
 * const results = await collectWithInterval(
 *   T.of(Date.now()),
 *   100,
 *   5
 * )()
 * // => [timestamp1, timestamp2, ...]
 */
export const collectWithInterval = <A>(
  task: T.Task<A>,
  intervalMs: number,
  count: number
): T.Task<readonly A[]> => async () => {
  const results: A[] = []
  for (let i = 0; i < count; i++) {
    if (i > 0) {
      await sleep(intervalMs)()
    }
    results.push(await task())
  }
  return results
}

/**
 * 指定時間内に Task を繰り返し実行し、結果を収集する。
 *
 * @example
 * const results = await collectForDuration(
 *   T.of(Date.now()),
 *   1000
 * )()
 * // => 1秒間に収集したタイムスタンプの配列
 */
export const collectForDuration = <A>(
  task: T.Task<A>,
  durationMs: number
): T.Task<readonly A[]> => async () => {
  const results: A[] = []
  const endTime = Date.now() + durationMs
  while (Date.now() < endTime) {
    results.push(await task())
  }
  return results
}

/**
 * Semaphore - 同時実行数を制限する。
 */
export interface Semaphore {
  /** 許可を取得して Task を実行 */
  readonly withPermit: <A>(task: T.Task<A>) => T.Task<A>
  /** 現在の許可数を取得 */
  readonly available: IO.IO<number>
}

/**
 * Semaphore を作成する。
 *
 * @example
 * const sem = createSemaphore(2) // 同時に2つまで実行
 * await Promise.all([
 *   sem.withPermit(longTask)(),
 *   sem.withPermit(longTask)(),
 *   sem.withPermit(longTask)() // 3つ目は2つ目が終わるまで待機
 * ])
 */
export const createSemaphore = (permits: number): Semaphore => {
  let available = permits
  const queue: Array<() => void> = []

  const acquire = (): Promise<void> => {
    if (available > 0) {
      available--
      return Promise.resolve()
    }
    return new Promise<void>((resolve) => {
      queue.push(resolve)
    })
  }

  const release = (): void => {
    const next = queue.shift()
    if (next) {
      next()
    } else {
      available++
    }
  }

  return {
    withPermit:
      <A>(task: T.Task<A>): T.Task<A> =>
      async () => {
        await acquire()
        try {
          return await task()
        } finally {
          release()
        }
      },
    available: () => available,
  }
}

/**
 * 指定された同時実行数で Task を並列実行する。
 *
 * @example
 * const results = await parSequenceN(tasks, 3)() // 同時に3つまで実行
 */
export const parSequenceN = <A>(
  tasks: readonly T.Task<A>[],
  concurrency: number
): T.Task<readonly A[]> => {
  const sem = createSemaphore(concurrency)
  return parSequence(tasks.map((t) => sem.withPermit(t)))
}
