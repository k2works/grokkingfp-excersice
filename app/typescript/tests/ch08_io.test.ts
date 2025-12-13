/**
 * 第8章: IO モナドのテスト
 */

import { describe, it, expect, vi } from 'vitest'
import * as IO from 'fp-ts/IO'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import {
  pureIO,
  delayIO,
  getCurrentTime,
  getRandomNumber,
  castTheDieImpure,
  castTheDie,
  castTheDieTwice,
  castTheDieNTimes,
  mapIO,
  chainIO,
  sequenceIO,
  pureTask,
  delayedTask,
  mapTask,
  chainTask,
  sequenceTask,
  rightTask,
  leftTask,
  fromPromise,
  createMeetingTime,
  meetingsOverlap,
  possibleMeetings,
  calendarEntriesApi,
  scheduledMeetings,
  orElseTE,
  retry,
  retryWithDefault,
  consoleLog,
  chainAll,
  printAndReturn,
  rightIOE,
  leftIOE,
  tryCatchIO,
  memoizeIO,
} from '../src/ch08_io.js'

describe('第8章: IO モナド', () => {
  describe('8.1 IO の基本', () => {
    describe('pureIO', () => {
      it('値を IO にラップする', () => {
        const io = pureIO(42)
        expect(io()).toBe(42)
      })

      it('文字列を IO にラップする', () => {
        const io = pureIO('hello')
        expect(io()).toBe('hello')
      })
    })

    describe('delayIO', () => {
      it('サンクを IO にラップする', () => {
        let counter = 0
        const io = delayIO(() => {
          counter++
          return counter
        })
        // まだ実行されていない
        expect(counter).toBe(0)
        // 実行
        const result = io()
        expect(counter).toBe(1)
        expect(result).toBe(1)
      })
    })

    describe('getCurrentTime', () => {
      it('現在のタイムスタンプを返す', () => {
        const before = Date.now()
        const time = getCurrentTime()
        const after = Date.now()
        expect(time).toBeGreaterThanOrEqual(before)
        expect(time).toBeLessThanOrEqual(after)
      })
    })

    describe('getRandomNumber', () => {
      it('0-1 の間の乱数を返す', () => {
        const random = getRandomNumber()
        expect(random).toBeGreaterThanOrEqual(0)
        expect(random).toBeLessThan(1)
      })
    })
  })

  describe('8.2 サイコロを振る例', () => {
    describe('castTheDieImpure', () => {
      it('1-6 の値を返す', () => {
        for (let i = 0; i < 100; i++) {
          const result = castTheDieImpure()
          expect(result).toBeGreaterThanOrEqual(1)
          expect(result).toBeLessThanOrEqual(6)
        }
      })
    })

    describe('castTheDie', () => {
      it('IO にラップされた 1-6 の値を返す', () => {
        for (let i = 0; i < 100; i++) {
          const result = castTheDie()
          expect(result).toBeGreaterThanOrEqual(1)
          expect(result).toBeLessThanOrEqual(6)
        }
      })
    })

    describe('castTheDieTwice', () => {
      it('2-12 の値を返す', () => {
        for (let i = 0; i < 100; i++) {
          const result = castTheDieTwice()
          expect(result).toBeGreaterThanOrEqual(2)
          expect(result).toBeLessThanOrEqual(12)
        }
      })
    })

    describe('castTheDieNTimes', () => {
      it('0回振ると0を返す', () => {
        const result = castTheDieNTimes(0)()
        expect(result).toBe(0)
      })

      it('1回振ると1-6の値を返す', () => {
        for (let i = 0; i < 100; i++) {
          const result = castTheDieNTimes(1)()
          expect(result).toBeGreaterThanOrEqual(1)
          expect(result).toBeLessThanOrEqual(6)
        }
      })

      it('3回振ると3-18の値を返す', () => {
        for (let i = 0; i < 100; i++) {
          const result = castTheDieNTimes(3)()
          expect(result).toBeGreaterThanOrEqual(3)
          expect(result).toBeLessThanOrEqual(18)
        }
      })
    })
  })

  describe('8.3 IO の合成', () => {
    describe('mapIO', () => {
      it('IO の値を変換する', () => {
        const io = IO.of(5)
        const doubled = mapIO(io, (x) => x * 2)
        expect(doubled()).toBe(10)
      })
    })

    describe('chainIO', () => {
      it('IO を返す関数を適用する', () => {
        const io = IO.of(5)
        const result = chainIO(io, (x) => IO.of(x * 2))
        expect(result()).toBe(10)
      })
    })

    describe('sequenceIO', () => {
      it('複数の IO を順番に実行して配列で返す', () => {
        const ios = [IO.of(1), IO.of(2), IO.of(3)]
        const combined = sequenceIO(ios)
        expect(combined()).toEqual([1, 2, 3])
      })

      it('空の配列の場合は空の配列を返す', () => {
        const combined = sequenceIO([])
        expect(combined()).toEqual([])
      })
    })
  })

  describe('8.4 Task（非同期 IO）', () => {
    describe('pureTask', () => {
      it('値を Task にラップする', async () => {
        const task = pureTask(42)
        const result = await task()
        expect(result).toBe(42)
      })
    })

    describe('delayedTask', () => {
      it('指定ミリ秒後に値を返す', async () => {
        const start = Date.now()
        const task = delayedTask(50, 'hello')
        const result = await task()
        const elapsed = Date.now() - start
        expect(result).toBe('hello')
        expect(elapsed).toBeGreaterThanOrEqual(40)
      })
    })

    describe('mapTask', () => {
      it('Task の値を変換する', async () => {
        const task = pureTask(5)
        const doubled = mapTask(task, (x) => x * 2)
        const result = await doubled()
        expect(result).toBe(10)
      })
    })

    describe('chainTask', () => {
      it('Task を返す関数を適用する', async () => {
        const task = pureTask(5)
        const result = await chainTask(task, (x) => pureTask(x * 2))()
        expect(result).toBe(10)
      })
    })

    describe('sequenceTask', () => {
      it('複数の Task を順番に実行して配列で返す', async () => {
        const tasks = [pureTask(1), pureTask(2), pureTask(3)]
        const combined = sequenceTask(tasks)
        const result = await combined()
        expect(result).toEqual([1, 2, 3])
      })
    })
  })

  describe('8.5 TaskEither', () => {
    describe('rightTask', () => {
      it('成功値を TaskEither にラップする', async () => {
        const task = rightTask<string, number>(42)
        const result = await task()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(42)
        }
      })
    })

    describe('leftTask', () => {
      it('エラー値を TaskEither にラップする', async () => {
        const task = leftTask<string, number>('error')
        const result = await task()
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('error')
        }
      })
    })

    describe('fromPromise', () => {
      it('成功した Promise は right になる', async () => {
        const task = fromPromise(
          () => Promise.resolve(42),
          (e) => String(e)
        )
        const result = await task()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(42)
        }
      })

      it('失敗した Promise は left になる', async () => {
        const task = fromPromise(
          () => Promise.reject(new Error('test error')),
          (e) => (e instanceof Error ? e.message : String(e))
        )
        const result = await task()
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('test error')
        }
      })
    })
  })

  describe('8.6 ミーティングスケジューリング', () => {
    describe('createMeetingTime', () => {
      it('MeetingTime を作成する', () => {
        const meeting = createMeetingTime(9, 10)
        expect(meeting.startHour).toBe(9)
        expect(meeting.endHour).toBe(10)
      })
    })

    describe('meetingsOverlap', () => {
      it('重複するミーティングを検出する', () => {
        const m1 = createMeetingTime(9, 11)
        const m2 = createMeetingTime(10, 12)
        expect(meetingsOverlap(m1, m2)).toBe(true)
      })

      it('隣接するミーティングは重複しない', () => {
        const m1 = createMeetingTime(9, 10)
        const m2 = createMeetingTime(10, 11)
        expect(meetingsOverlap(m1, m2)).toBe(false)
      })

      it('離れたミーティングは重複しない', () => {
        const m1 = createMeetingTime(9, 10)
        const m2 = createMeetingTime(14, 15)
        expect(meetingsOverlap(m1, m2)).toBe(false)
      })

      it('完全に含まれるミーティングは重複する', () => {
        const m1 = createMeetingTime(9, 15)
        const m2 = createMeetingTime(11, 13)
        expect(meetingsOverlap(m1, m2)).toBe(true)
      })
    })

    describe('possibleMeetings', () => {
      it('空き時間を計算する', () => {
        const existing = [createMeetingTime(9, 10), createMeetingTime(14, 15)]
        const possible = possibleMeetings(existing, 8, 17, 1)
        expect(possible).toContainEqual(createMeetingTime(8, 9))
        expect(possible).toContainEqual(createMeetingTime(10, 11))
        expect(possible).not.toContainEqual(createMeetingTime(9, 10))
        expect(possible).not.toContainEqual(createMeetingTime(14, 15))
      })

      it('全て埋まっている場合は空を返す', () => {
        const existing = [createMeetingTime(8, 17)]
        const possible = possibleMeetings(existing, 8, 17, 1)
        expect(possible).toEqual([])
      })
    })

    describe('calendarEntriesApi', () => {
      it('Alice の予定を取得できる', async () => {
        const result = await calendarEntriesApi('Alice')()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right.length).toBe(2)
        }
      })

      it('存在しないユーザーはエラーを返す', async () => {
        const result = await calendarEntriesApi('Unknown')()
        expect(E.isLeft(result)).toBe(true)
      })
    })

    describe('scheduledMeetings', () => {
      it('複数の参加者の予定を結合する', async () => {
        const result = await scheduledMeetings(['Alice', 'Bob'])()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right.length).toBe(4)
        }
      })

      it('存在しないユーザーが含まれるとエラーを返す', async () => {
        const result = await scheduledMeetings(['Alice', 'Unknown'])()
        expect(E.isLeft(result)).toBe(true)
      })
    })
  })

  describe('8.7 orElse によるエラーハンドリング', () => {
    describe('orElseTE', () => {
      it('成功時はそのまま返す', async () => {
        const task = rightTask<string, number>(42)
        const result = await orElseTE(task, () => rightTask(0))()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(42)
        }
      })

      it('失敗時は代替を使用する', async () => {
        const task = leftTask<string, number>('error')
        const result = await orElseTE(task, () => rightTask(0))()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(0)
        }
      })
    })

    describe('retry', () => {
      it('成功時は1回で終了', async () => {
        let callCount = 0
        const action = rightTask<string, number>(42)
        const actionWithCounter = () => {
          callCount++
          return action()
        }
        const wrappedAction = () => actionWithCounter()
        const result = await retry(wrappedAction, 3)()
        expect(E.isRight(result)).toBe(true)
      })
    })

    describe('retryWithDefault', () => {
      it('全て失敗したらデフォルト値を返す', async () => {
        const action = leftTask<string, number>('error')
        const result = await retryWithDefault(action, 3, 0)()
        expect(result).toBe(0)
      })

      it('成功時はその値を返す', async () => {
        const action = rightTask<string, number>(42)
        const result = await retryWithDefault(action, 3, 0)()
        expect(result).toBe(42)
      })
    })
  })

  describe('8.8 IO のユーティリティ', () => {
    describe('consoleLog', () => {
      it('コンソールに出力する IO を作成する', () => {
        const logSpy = vi.spyOn(console, 'log').mockImplementation(() => {})
        const io = consoleLog('Hello')
        io()
        expect(logSpy).toHaveBeenCalledWith('Hello')
        logSpy.mockRestore()
      })
    })

    describe('chainAll', () => {
      it('複数の IO を順番に実行し、最後の結果を返す', () => {
        const io1 = IO.of(1)
        const io2 = IO.of(2)
        const io3 = IO.of(3)
        const result = chainAll(io1, io2, io3)
        expect(result()).toBe(3)
      })

      it('1つの IO の場合はその結果を返す', () => {
        const io = IO.of(42)
        const result = chainAll(io)
        expect(result()).toBe(42)
      })

      it('空の場合はエラーをスローする', () => {
        expect(() => chainAll()).toThrow('At least one IO required')
      })
    })

    describe('printAndReturn', () => {
      it('メッセージを出力して同じ値を返す', () => {
        const logSpy = vi.spyOn(console, 'log').mockImplementation(() => {})
        const io = printAndReturn('Hello')
        const result = io()
        expect(logSpy).toHaveBeenCalledWith('Hello')
        expect(result).toBe('Hello')
        logSpy.mockRestore()
      })
    })
  })

  describe('8.9 IOEither', () => {
    describe('rightIOE', () => {
      it('成功値を IOEither にラップする', () => {
        const ioe = rightIOE<string, number>(42)
        const result = ioe()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(42)
        }
      })
    })

    describe('leftIOE', () => {
      it('エラー値を IOEither にラップする', () => {
        const ioe = leftIOE<string, number>('error')
        const result = ioe()
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('error')
        }
      })
    })

    describe('tryCatchIO', () => {
      it('成功時は right を返す', () => {
        const ioe = tryCatchIO(
          () => JSON.parse('{"a": 1}'),
          (e) => String(e)
        )
        const result = ioe()
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual({ a: 1 })
        }
      })

      it('失敗時は left を返す', () => {
        const ioe = tryCatchIO(
          () => JSON.parse('invalid'),
          (e) => (e instanceof Error ? e.message : String(e))
        )
        const result = ioe()
        expect(E.isLeft(result)).toBe(true)
      })
    })
  })

  describe('8.10 メモ化とキャッシュ', () => {
    describe('memoizeIO', () => {
      it('最初の実行結果をキャッシュする', () => {
        let counter = 0
        const io = memoizeIO(() => {
          counter++
          return counter
        })
        expect(io()).toBe(1)
        expect(io()).toBe(1)
        expect(io()).toBe(1)
        expect(counter).toBe(1) // 1回だけ実行された
      })

      it('異なる memoizeIO インスタンスは独立している', () => {
        let counter = 0
        const originalIO: IO.IO<number> = () => {
          counter++
          return counter
        }
        const memoized1 = memoizeIO(originalIO)
        const memoized2 = memoizeIO(originalIO)
        expect(memoized1()).toBe(1)
        expect(memoized2()).toBe(2)
        expect(memoized1()).toBe(1)
        expect(memoized2()).toBe(2)
      })
    })
  })
})
