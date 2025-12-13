/**
 * 第9章: ストリーム処理のテスト
 */

import { describe, it, expect } from 'vitest'
import * as O from 'fp-ts/Option'
import {
  fromArray,
  singleton,
  empty,
  range,
  rangeInclusive,
  repeatValue,
  repeatFn,
  cycleArray,
  iterate,
  naturals,
  take,
  drop,
  takeWhile,
  dropWhile,
  mapStream,
  filterStream,
  flatMapStream,
  append,
  zipStream,
  zipWith,
  zipWithIndex,
  sliding,
  chunk,
  foldStream,
  toArray,
  head,
  last,
  find,
  trending,
  downtrending,
  isStable,
  detectTrends,
  dieStream,
  dieUntil,
  dieUntilSum,
  isEmpty,
  count,
  forAll,
  exists,
  dedup,
  interleave,
  scan,
  fibonacci,
  primes,
} from '../src/ch09_streams.js'

describe('第9章: ストリーム処理', () => {
  describe('9.1 LazyList の基本', () => {
    describe('fromArray', () => {
      it('配列から LazyList を作成する', () => {
        const list = fromArray([1, 2, 3])
        expect([...list]).toEqual([1, 2, 3])
      })

      it('空の配列から空の LazyList を作成する', () => {
        const list = fromArray([])
        expect([...list]).toEqual([])
      })
    })

    describe('singleton', () => {
      it('単一の値から LazyList を作成する', () => {
        const list = singleton(42)
        expect([...list]).toEqual([42])
      })
    })

    describe('empty', () => {
      it('空の LazyList を作成する', () => {
        const list = empty()
        expect([...list]).toEqual([])
      })
    })

    describe('range', () => {
      it('範囲から LazyList を作成する（終端を含まない）', () => {
        const list = range(1, 5)
        expect([...list]).toEqual([1, 2, 3, 4])
      })

      it('同じ値の場合は空を返す', () => {
        const list = range(5, 5)
        expect([...list]).toEqual([])
      })
    })

    describe('rangeInclusive', () => {
      it('範囲から LazyList を作成する（終端を含む）', () => {
        const list = rangeInclusive(1, 5)
        expect([...list]).toEqual([1, 2, 3, 4, 5])
      })
    })
  })

  describe('9.2 無限ストリーム', () => {
    describe('repeatValue', () => {
      it('値を無限に繰り返す', () => {
        const ones = repeatValue(1)
        expect(take(ones, 5)).toEqual([1, 1, 1, 1, 1])
      })
    })

    describe('repeatFn', () => {
      it('関数を無限に繰り返し呼び出す', () => {
        let i = 0
        const counting = repeatFn(() => i++)
        expect(take(counting, 5)).toEqual([0, 1, 2, 3, 4])
      })
    })

    describe('cycleArray', () => {
      it('配列を無限に繰り返す', () => {
        const cycle = cycleArray([1, 2, 3])
        expect(take(cycle, 8)).toEqual([1, 2, 3, 1, 2, 3, 1, 2])
      })

      it('空の配列の場合は空を返す', () => {
        const cycle = cycleArray([])
        expect(take(cycle, 5)).toEqual([])
      })
    })

    describe('iterate', () => {
      it('初期値と生成関数から無限ストリームを作成する', () => {
        const powers = iterate(1, (n) => n * 2)
        expect(take(powers, 5)).toEqual([1, 2, 4, 8, 16])
      })
    })

    describe('naturals', () => {
      it('自然数の無限ストリームを返す', () => {
        const nats = naturals()
        expect(take(nats, 5)).toEqual([0, 1, 2, 3, 4])
      })
    })
  })

  describe('9.3 ストリーム操作（変換）', () => {
    describe('take', () => {
      it('最初の n 要素を取得する', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect(take(list, 3)).toEqual([1, 2, 3])
      })

      it('n が要素数より大きい場合は全て返す', () => {
        const list = fromArray([1, 2, 3])
        expect(take(list, 10)).toEqual([1, 2, 3])
      })

      it('n が 0 の場合は空を返す', () => {
        const list = fromArray([1, 2, 3])
        expect(take(list, 0)).toEqual([])
      })
    })

    describe('drop', () => {
      it('最初の n 要素をスキップする', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect(take(drop(list, 2), 10)).toEqual([3, 4, 5])
      })

      it('n が 0 の場合は全て返す', () => {
        const list = fromArray([1, 2, 3])
        expect(take(drop(list, 0), 10)).toEqual([1, 2, 3])
      })
    })

    describe('takeWhile', () => {
      it('条件を満たす間、要素を取得する', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect([...takeWhile(list, (x) => x < 4)]).toEqual([1, 2, 3])
      })

      it('最初から条件を満たさない場合は空を返す', () => {
        const list = fromArray([5, 4, 3, 2, 1])
        expect([...takeWhile(list, (x) => x < 4)]).toEqual([])
      })
    })

    describe('dropWhile', () => {
      it('条件を満たす間、要素をスキップする', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect([...dropWhile(list, (x) => x < 3)]).toEqual([3, 4, 5])
      })
    })

    describe('mapStream', () => {
      it('各要素を変換する', () => {
        const list = fromArray([1, 2, 3])
        expect([...mapStream(list, (x) => x * 2)]).toEqual([2, 4, 6])
      })
    })

    describe('filterStream', () => {
      it('条件を満たす要素のみを取得する', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect([...filterStream(list, (x) => x % 2 === 0)]).toEqual([2, 4])
      })
    })

    describe('flatMapStream', () => {
      it('各要素を展開して結合する', () => {
        const list = fromArray([1, 2, 3])
        const result = [...flatMapStream(list, (x) => fromArray([x, x * 10]))]
        expect(result).toEqual([1, 10, 2, 20, 3, 30])
      })
    })
  })

  describe('9.4 ストリームの結合', () => {
    describe('append', () => {
      it('2つのストリームを結合する', () => {
        const list1 = fromArray([1, 2])
        const list2 = fromArray([3, 4])
        expect([...append(list1, list2)]).toEqual([1, 2, 3, 4])
      })
    })

    describe('zipStream', () => {
      it('2つのストリームをペアにして結合する', () => {
        const list1 = fromArray([1, 2, 3])
        const list2 = fromArray(['a', 'b', 'c'])
        expect([...zipStream(list1, list2)]).toEqual([
          [1, 'a'],
          [2, 'b'],
          [3, 'c'],
        ])
      })

      it('長さが異なる場合は短い方に合わせる', () => {
        const list1 = fromArray([1, 2, 3])
        const list2 = fromArray(['a', 'b'])
        expect([...zipStream(list1, list2)]).toEqual([
          [1, 'a'],
          [2, 'b'],
        ])
      })
    })

    describe('zipWith', () => {
      it('2つのストリームを関数で結合する', () => {
        const list1 = fromArray([1, 2, 3])
        const list2 = fromArray([10, 20, 30])
        expect([...zipWith(list1, list2, (a, b) => a + b)]).toEqual([11, 22, 33])
      })
    })

    describe('zipWithIndex', () => {
      it('インデックス付きでストリームを走査する', () => {
        const list = fromArray(['a', 'b', 'c'])
        expect([...zipWithIndex(list)]).toEqual([
          ['a', 0],
          ['b', 1],
          ['c', 2],
        ])
      })
    })
  })

  describe('9.5 スライディングウィンドウ', () => {
    describe('sliding', () => {
      it('スライディングウィンドウでストリームを処理する', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect([...sliding(list, 3)]).toEqual([
          [1, 2, 3],
          [2, 3, 4],
          [3, 4, 5],
        ])
      })

      it('要素数がウィンドウサイズより小さい場合は空を返す', () => {
        const list = fromArray([1, 2])
        expect([...sliding(list, 3)]).toEqual([])
      })
    })

    describe('chunk', () => {
      it('指定サイズのチャンクに分割する', () => {
        const list = fromArray([1, 2, 3, 4, 5, 6, 7])
        expect([...chunk(list, 3)]).toEqual([[1, 2, 3], [4, 5, 6], [7]])
      })

      it('要素数がサイズの倍数の場合', () => {
        const list = fromArray([1, 2, 3, 4, 5, 6])
        expect([...chunk(list, 3)]).toEqual([
          [1, 2, 3],
          [4, 5, 6],
        ])
      })
    })
  })

  describe('9.6 畳み込み', () => {
    describe('foldStream', () => {
      it('ストリームを畳み込む', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        expect(foldStream(list, 0, (acc, x) => acc + x)).toBe(15)
      })

      it('空のストリームの場合は初期値を返す', () => {
        const list = empty<number>()
        expect(foldStream(list, 100, (acc, x) => acc + x)).toBe(100)
      })
    })

    describe('toArray', () => {
      it('ストリームを配列に変換する', () => {
        const list = fromArray([1, 2, 3])
        expect(toArray(list)).toEqual([1, 2, 3])
      })
    })

    describe('head', () => {
      it('最初の要素を取得する', () => {
        const list = fromArray([1, 2, 3])
        const result = head(list)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(1)
        }
      })

      it('空のストリームの場合は none を返す', () => {
        const list = empty<number>()
        expect(O.isNone(head(list))).toBe(true)
      })
    })

    describe('last', () => {
      it('最後の要素を取得する', () => {
        const list = fromArray([1, 2, 3])
        const result = last(list)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(3)
        }
      })
    })

    describe('find', () => {
      it('条件を満たす最初の要素を取得する', () => {
        const list = fromArray([1, 2, 3, 4, 5])
        const result = find(list, (x) => x > 3)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(4)
        }
      })

      it('見つからない場合は none を返す', () => {
        const list = fromArray([1, 2, 3])
        expect(O.isNone(find(list, (x) => x > 10))).toBe(true)
      })
    })
  })

  describe('9.7 トレンド検出', () => {
    describe('trending', () => {
      it('上昇トレンドを検出する', () => {
        expect(trending([0.81, 0.82, 0.83])).toBe(true)
        expect(trending([0.81, 0.84, 0.83])).toBe(false)
      })

      it('要素が1つ以下の場合は false を返す', () => {
        expect(trending([0.81])).toBe(false)
        expect(trending([])).toBe(false)
      })
    })

    describe('downtrending', () => {
      it('下降トレンドを検出する', () => {
        expect(downtrending([0.83, 0.82, 0.81])).toBe(true)
        expect(downtrending([0.83, 0.80, 0.81])).toBe(false)
      })
    })

    describe('isStable', () => {
      it('安定している（全て同じ値）かどうかを判定する', () => {
        expect(isStable([5, 5, 5])).toBe(true)
        expect(isStable([5, 5, 6])).toBe(false)
      })

      it('要素が2つ未満の場合は false を返す', () => {
        expect(isStable([5])).toBe(false)
        expect(isStable([])).toBe(false)
      })
    })

    describe('detectTrends', () => {
      it('スライディングウィンドウでトレンドを検出する', () => {
        const rates = fromArray([0.8, 0.81, 0.82, 0.83, 0.82])
        const trends = [...detectTrends(rates, 3)]
        expect(trends).toHaveLength(3)
        expect(trends[0].trending).toBe(true) // [0.8, 0.81, 0.82]
        expect(trends[1].trending).toBe(true) // [0.81, 0.82, 0.83]
        expect(trends[2].trending).toBe(false) // [0.82, 0.83, 0.82]
      })
    })
  })

  describe('9.8 サイコロストリーム', () => {
    describe('dieStream', () => {
      it('1-6 の値を生成する', () => {
        const dice = dieStream()
        const results = take(dice, 100)
        results.forEach((r) => {
          expect(r).toBeGreaterThanOrEqual(1)
          expect(r).toBeLessThanOrEqual(6)
        })
      })
    })

    describe('dieUntil', () => {
      it('指定した値が出るまでサイコロを振り続ける', () => {
        const until6 = dieUntil(6)
        const results = [...until6]
        expect(results[results.length - 1]).toBe(6)
        results.forEach((r) => {
          expect(r).toBeGreaterThanOrEqual(1)
          expect(r).toBeLessThanOrEqual(6)
        })
      })
    })

    describe('dieUntilSum', () => {
      it('合計が指定値以上になるまでサイコロを振る', () => {
        const rolls = dieUntilSum(20)
        const results = [...rolls]
        const sum = results.reduce((a, b) => a + b, 0)
        expect(sum).toBeGreaterThanOrEqual(20)
        results.forEach((r) => {
          expect(r).toBeGreaterThanOrEqual(1)
          expect(r).toBeLessThanOrEqual(6)
        })
      })
    })
  })

  describe('9.10 ストリームユーティリティ', () => {
    describe('isEmpty', () => {
      it('空のストリームを判定する', () => {
        expect(isEmpty(empty())).toBe(true)
        expect(isEmpty(fromArray([1]))).toBe(false)
      })
    })

    describe('count', () => {
      it('ストリームの要素数を数える', () => {
        expect(count(fromArray([1, 2, 3]))).toBe(3)
        expect(count(empty())).toBe(0)
      })
    })

    describe('forAll', () => {
      it('全ての要素が条件を満たすかどうかを判定する', () => {
        expect(forAll(fromArray([2, 4, 6]), (x) => x % 2 === 0)).toBe(true)
        expect(forAll(fromArray([2, 3, 6]), (x) => x % 2 === 0)).toBe(false)
      })

      it('空のストリームは true を返す', () => {
        expect(forAll(empty<number>(), (x) => x > 0)).toBe(true)
      })
    })

    describe('exists', () => {
      it('いずれかの要素が条件を満たすかどうかを判定する', () => {
        expect(exists(fromArray([1, 2, 3]), (x) => x % 2 === 0)).toBe(true)
        expect(exists(fromArray([1, 3, 5]), (x) => x % 2 === 0)).toBe(false)
      })
    })

    describe('dedup', () => {
      it('連続した重複を除去する', () => {
        expect([...dedup(fromArray([1, 1, 2, 2, 3, 1, 1]))]).toEqual([
          1, 2, 3, 1,
        ])
      })
    })

    describe('interleave', () => {
      it('2つのストリームを交互に結合する', () => {
        const list1 = fromArray([1, 2, 3])
        const list2 = fromArray([10, 20, 30])
        expect([...interleave(list1, list2)]).toEqual([1, 10, 2, 20, 3, 30])
      })

      it('長さが異なる場合は残りを追加する', () => {
        const list1 = fromArray([1, 2])
        const list2 = fromArray([10, 20, 30, 40])
        expect([...interleave(list1, list2)]).toEqual([1, 10, 2, 20, 30, 40])
      })
    })

    describe('scan', () => {
      it('累積値を計算する', () => {
        expect([...scan(fromArray([1, 2, 3, 4]), 0, (a, b) => a + b)]).toEqual([
          0, 1, 3, 6, 10,
        ])
      })
    })

    describe('fibonacci', () => {
      it('フィボナッチ数列を生成する', () => {
        expect(take(fibonacci(), 10)).toEqual([0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
      })
    })

    describe('primes', () => {
      it('素数を生成する', () => {
        expect(take(primes(), 10)).toEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29])
      })
    })
  })
})
