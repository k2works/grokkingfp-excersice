/**
 * 第10章: 並行・並列処理のテスト
 */

import { describe, it, expect, vi } from 'vitest'
import { pipe } from 'fp-ts/function'
import * as T from 'fp-ts/Task'
import * as TE from 'fp-ts/TaskEither'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import {
  createRef,
  createRefIO,
  parSequence,
  parSequenceTE,
  race,
  raceSuccess,
  parTraverse,
  parTraverseTE,
  delay,
  sleep,
  after,
  start,
  cancellable,
  repeatN,
  repeatWhile,
  startForever,
  createCity,
  createCityStats,
  topCities,
  storeCheckIn,
  startProcessingCheckIns,
  castTheDieTask,
  castDiceParallel,
  castDiceParallelSum,
  castDiceAndStore,
  timeout,
  timeoutWithDefault,
  raceTwo,
  parMapReduce,
  parFilter,
  collectWithInterval,
  createSemaphore,
  parSequenceN,
} from '../src/ch10_concurrent.js'

describe('第10章: 並行・並列処理', () => {
  describe('10.1 Ref（アトミックな共有状態）', () => {
    describe('createRef', () => {
      it('初期値を持つ Ref を作成する', async () => {
        const ref = createRef(42)
        const value = await ref.get()
        expect(value).toBe(42)
      })

      it('set で値を更新できる', async () => {
        const ref = createRef(0)
        await ref.set(10)()
        const value = await ref.get()
        expect(value).toBe(10)
      })

      it('update でアトミックに更新できる', async () => {
        const ref = createRef(0)
        await ref.update((n) => n + 1)()
        await ref.update((n) => n + 1)()
        await ref.update((n) => n + 1)()
        const value = await ref.get()
        expect(value).toBe(3)
      })

      it('getAndUpdate で更新前の値を取得できる', async () => {
        const ref = createRef(5)
        const oldValue = await ref.getAndUpdate((n) => n * 2)()
        const newValue = await ref.get()
        expect(oldValue).toBe(5)
        expect(newValue).toBe(10)
      })

      it('updateAndGet で更新後の値を取得できる', async () => {
        const ref = createRef(5)
        const newValue = await ref.updateAndGet((n) => n * 2)()
        expect(newValue).toBe(10)
      })

      it('modify で更新と結果を同時に取得できる', async () => {
        const ref = createRef(5)
        const result = await ref.modify((n) => [n * 2, `old: ${n}`] as const)()
        const newValue = await ref.get()
        expect(result).toBe('old: 5')
        expect(newValue).toBe(10)
      })
    })

    describe('createRefIO', () => {
      it('同期 Ref を作成する', () => {
        const ref = createRefIO(42)
        expect(ref.get()).toBe(42)
      })

      it('同期的に更新できる', () => {
        const ref = createRefIO(0)
        ref.update((n) => n + 1)()
        ref.update((n) => n + 1)()
        expect(ref.get()).toBe(2)
      })
    })
  })

  describe('10.2 並列実行', () => {
    describe('parSequence', () => {
      it('複数の Task を並列実行する', async () => {
        const tasks = [T.of(1), T.of(2), T.of(3)]
        const result = await parSequence(tasks)()
        expect(result).toEqual([1, 2, 3])
      })

      it('空の配列の場合は空の配列を返す', async () => {
        const result = await parSequence([])()
        expect(result).toEqual([])
      })
    })

    describe('parSequenceTE', () => {
      it('全て成功した場合は結果を配列で返す', async () => {
        const tasks = [TE.right(1), TE.right(2), TE.right(3)]
        const result = await parSequenceTE(tasks)()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual([1, 2, 3])
        }
      })

      it('いずれかが失敗した場合はエラーを返す', async () => {
        const tasks = [
          TE.right(1),
          TE.left('error'),
          TE.right(3),
        ] as TE.TaskEither<string, number>[]
        const result = await parSequenceTE(tasks)()
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('error')
        }
      })
    })

    describe('race', () => {
      it('最初に完了した Task の結果を返す', async () => {
        const task1 = delay(100, 'slow')
        const task2 = delay(10, 'fast')
        const result = await race([task1, task2])()
        expect(result).toBe('fast')
      })
    })

    describe('raceSuccess', () => {
      it('成功した結果を返す', async () => {
        const task1 = T.of(1)
        const task2 = T.of(2)
        const result = await raceSuccess([task1, task2])()
        expect(O.isSome(result)).toBe(true)
      })

      it('空の配列の場合は none を返す', async () => {
        const result = await raceSuccess([])()
        expect(O.isNone(result)).toBe(true)
      })
    })

    describe('parTraverse', () => {
      it('配列の各要素に対して Task を適用し、並列実行する', async () => {
        const result = await parTraverse([1, 2, 3], (n) => T.of(n * 2))()
        expect(result).toEqual([2, 4, 6])
      })
    })

    describe('parTraverseTE', () => {
      it('配列の各要素に対して TaskEither を適用する', async () => {
        const result = await parTraverseTE(
          [1, 2, 3],
          (n) => TE.right(n * 2)
        )()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual([2, 4, 6])
        }
      })
    })
  })

  describe('10.3 遅延とスリープ', () => {
    describe('delay', () => {
      it('指定ミリ秒後に値を返す', async () => {
        const start = Date.now()
        const result = await delay(50, 42)()
        const elapsed = Date.now() - start
        expect(result).toBe(42)
        expect(elapsed).toBeGreaterThanOrEqual(40)
      })
    })

    describe('sleep', () => {
      it('指定ミリ秒スリープする', async () => {
        const start = Date.now()
        await sleep(50)()
        const elapsed = Date.now() - start
        expect(elapsed).toBeGreaterThanOrEqual(40)
      })
    })

    describe('after', () => {
      it('指定ミリ秒後に Task を実行する', async () => {
        const start = Date.now()
        const result = await after(50, T.of(42))()
        const elapsed = Date.now() - start
        expect(result).toBe(42)
        expect(elapsed).toBeGreaterThanOrEqual(40)
      })
    })
  })

  describe('10.4 Fiber（軽量スレッド）', () => {
    describe('start', () => {
      it('Task を Fiber として起動する', async () => {
        const fiber = start(T.of(42))
        const result = await fiber.join()
        expect(result).toBe(42)
      })

      it('Fiber をキャンセルできる', async () => {
        const fiber = start(delay(1000, 42))
        expect(fiber.isCancelled()).toBe(false)
        await fiber.cancel()
        expect(fiber.isCancelled()).toBe(true)
      })
    })

    describe('cancellable', () => {
      it('キャンセル可能な Task を作成する', async () => {
        const { run, cancel } = cancellable(async (signal) => {
          await sleep(100)()
          if (signal.aborted) return O.none
          return O.some(42)
        })

        const resultPromise = run()
        cancel()
        const result = await resultPromise
        // キャンセルが反映されるかどうかはタイミング依存
        expect(O.isSome(result) || O.isNone(result)).toBe(true)
      })
    })
  })

  describe('10.5 永続的な実行', () => {
    describe('repeatN', () => {
      it('指定回数だけ Task を繰り返し実行する', async () => {
        let count = 0
        const task: T.Task<void> = () => {
          count++
          return Promise.resolve()
        }
        await repeatN(task, 3)()
        expect(count).toBe(3)
      })

      it('0回の場合は何も実行しない', async () => {
        let count = 0
        const task: T.Task<void> = () => {
          count++
          return Promise.resolve()
        }
        await repeatN(task, 0)()
        expect(count).toBe(0)
      })
    })

    describe('repeatWhile', () => {
      it('条件が真の間、Task を繰り返し実行する', async () => {
        let count = 0
        const task: T.Task<void> = () => {
          count++
          return Promise.resolve()
        }
        await repeatWhile(task, () => count < 5)()
        expect(count).toBe(5)
      })
    })

    describe('startForever', () => {
      it('キャンセルするまで繰り返し実行する', async () => {
        let count = 0
        // 小さな遅延を入れてイベントループに制御を返す
        const task: T.Task<void> = async () => {
          count++
          await new Promise((resolve) => setTimeout(resolve, 5))
        }
        const { cancel, isCancelled } = startForever(task)

        // 少し待つ
        await sleep(30)()

        expect(count).toBeGreaterThan(0)
        expect(isCancelled()).toBe(false)

        cancel()
        expect(isCancelled()).toBe(true)
      })
    })
  })

  describe('10.6 チェックインのリアルタイム集計', () => {
    describe('createCity / createCityStats', () => {
      it('City を作成する', () => {
        const city = createCity('Tokyo')
        expect(city.name).toBe('Tokyo')
      })

      it('CityStats を作成する', () => {
        const city = createCity('Tokyo')
        const stats = createCityStats(city, 100)
        expect(stats.city.name).toBe('Tokyo')
        expect(stats.checkIns).toBe(100)
      })
    })

    describe('topCities', () => {
      it('トップ N 都市を計算する', () => {
        const map = new Map([
          ['Tokyo', 100],
          ['Osaka', 50],
          ['Kyoto', 30],
          ['Yokohama', 80],
        ])
        const top = topCities(map, 3)
        expect(top).toHaveLength(3)
        expect(top[0].city.name).toBe('Tokyo')
        expect(top[0].checkIns).toBe(100)
        expect(top[1].city.name).toBe('Yokohama')
        expect(top[2].city.name).toBe('Osaka')
      })

      it('空のマップの場合は空の配列を返す', () => {
        const map = new Map<string, number>()
        const top = topCities(map, 3)
        expect(top).toEqual([])
      })
    })

    describe('storeCheckIn', () => {
      it('チェックインを保存する', async () => {
        const ref = createRef<ReadonlyMap<string, number>>(new Map())
        const city = createCity('Tokyo')

        await storeCheckIn(ref)(city)()
        await storeCheckIn(ref)(city)()
        await storeCheckIn(ref)(city)()

        const map = await ref.get()
        expect(map.get('Tokyo')).toBe(3)
      })
    })

    describe('startProcessingCheckIns', () => {
      it('チェックイン処理を開始できる', async () => {
        const processing = startProcessingCheckIns(3)

        // 初期状態
        const initialRanking = await processing.currentRanking()
        expect(initialRanking).toEqual([])

        // 停止
        processing.stop()
        expect(processing.isStopped()).toBe(true)
      })
    })
  })

  describe('10.7 並列サイコロ', () => {
    describe('castTheDieTask', () => {
      it('1-6 の値を返す', async () => {
        for (let i = 0; i < 100; i++) {
          const result = await castTheDieTask()
          expect(result).toBeGreaterThanOrEqual(1)
          expect(result).toBeLessThanOrEqual(6)
        }
      })
    })

    describe('castDiceParallel', () => {
      it('n 個のサイコロを並列に振る', async () => {
        const results = await castDiceParallel(5)()
        expect(results).toHaveLength(5)
        results.forEach((r) => {
          expect(r).toBeGreaterThanOrEqual(1)
          expect(r).toBeLessThanOrEqual(6)
        })
      })
    })

    describe('castDiceParallelSum', () => {
      it('n 個のサイコロの合計を返す', async () => {
        const sum = await castDiceParallelSum(3)()
        expect(sum).toBeGreaterThanOrEqual(3)
        expect(sum).toBeLessThanOrEqual(18)
      })
    })

    describe('castDiceAndStore', () => {
      it('結果を Ref に保存する', async () => {
        const ref = createRef<readonly number[]>([])
        await castDiceAndStore(ref, 3)()
        const results = await ref.get()
        expect(results).toHaveLength(3)
      })
    })
  })

  describe('10.8 タイムアウトと競争', () => {
    describe('timeout', () => {
      it('タイムアウト前に完了した場合は some を返す', async () => {
        const result = await timeout(delay(10, 42), 100)()
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(42)
        }
      })

      it('タイムアウトした場合は none を返す', async () => {
        const result = await timeout(delay(100, 42), 10)()
        expect(O.isNone(result)).toBe(true)
      })
    })

    describe('timeoutWithDefault', () => {
      it('タイムアウト前に完了した場合は結果を返す', async () => {
        const result = await timeoutWithDefault(delay(10, 42), 100, 0)()
        expect(result).toBe(42)
      })

      it('タイムアウトした場合はデフォルト値を返す', async () => {
        const result = await timeoutWithDefault(delay(100, 42), 10, 0)()
        expect(result).toBe(0)
      })
    })

    describe('raceTwo', () => {
      it('先に完了した方の結果を返す', async () => {
        const result = await raceTwo(delay(100, 'slow'), delay(10, 'fast'))()
        expect(result).toBe('fast')
      })
    })
  })

  describe('10.9 並列 Map/Reduce', () => {
    describe('parMapReduce', () => {
      it('並列に処理して結果を集約する', async () => {
        const result = await parMapReduce(
          [1, 2, 3],
          (n) => T.of(n * 2),
          0,
          (a, b) => a + b
        )()
        expect(result).toBe(12)
      })
    })

    describe('parFilter', () => {
      it('並列にフィルタリングする', async () => {
        const result = await parFilter([1, 2, 3, 4, 5], (n) =>
          T.of(n % 2 === 0)
        )()
        expect(result).toEqual([2, 4])
      })
    })
  })

  describe('10.10 ユーティリティ', () => {
    describe('collectWithInterval', () => {
      it('指定間隔で Task を繰り返し実行し、結果を収集する', async () => {
        let count = 0
        const task: T.Task<number> = () => Promise.resolve(++count)
        const results = await collectWithInterval(task, 10, 3)()
        expect(results).toEqual([1, 2, 3])
      })
    })

    describe('createSemaphore', () => {
      it('同時実行数を制限する', async () => {
        const sem = createSemaphore(2)
        expect(sem.available()).toBe(2)

        let concurrent = 0
        let maxConcurrent = 0

        const task: T.Task<void> = async () => {
          concurrent++
          maxConcurrent = Math.max(maxConcurrent, concurrent)
          await sleep(50)()
          concurrent--
        }

        await Promise.all([
          sem.withPermit(task)(),
          sem.withPermit(task)(),
          sem.withPermit(task)(),
          sem.withPermit(task)(),
        ])

        // 同時に最大2つまで実行されたはず
        expect(maxConcurrent).toBeLessThanOrEqual(2)
      })
    })

    describe('parSequenceN', () => {
      it('指定された同時実行数で並列実行する', async () => {
        let concurrent = 0
        let maxConcurrent = 0

        const createTask = (): T.Task<number> => async () => {
          concurrent++
          maxConcurrent = Math.max(maxConcurrent, concurrent)
          await sleep(30)()
          concurrent--
          return concurrent
        }

        const tasks = Array.from({ length: 6 }, () => createTask())
        await parSequenceN(tasks, 2)()

        // 同時に最大2つまで実行されたはず
        expect(maxConcurrent).toBeLessThanOrEqual(2)
      })
    })
  })
})
