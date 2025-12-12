/**
 * 第2章: 純粋関数とテストのテスト
 */

import { describe, it, expect } from 'vitest'
import * as O from 'fp-ts/Option'
import {
  add,
  stringLength,
  wordScore,
  bonusScore,
  getDiscountPercentage,
  calculateDiscount,
  calculateFinalPrice,
  getTipPercentage,
  calculateTip,
  referentialTransparencyExample,
  appendExclamation,
  countVowels,
  doubleAll,
  filterPositive,
  findLongest,
  allPositive,
  anyNegative,
  sumList,
  average,
  wordScoreNoA,
  applyTwice,
  compose,
  composeWithFlow,
  formatTimestamp,
} from '../src/ch02_pure_functions.js'

describe('第2章: 純粋関数とテスト', () => {
  describe('2.1 純粋関数の基本', () => {
    describe('add', () => {
      it('2つの数値を加算する', () => {
        expect(add(2, 3)).toBe(5)
        expect(add(-1, 1)).toBe(0)
        expect(add(0, 0)).toBe(0)
      })
    })

    describe('stringLength', () => {
      it('文字列の長さを返す', () => {
        expect(stringLength('hello')).toBe(5)
        expect(stringLength('')).toBe(0)
      })
    })

    describe('wordScore', () => {
      it('単語のスコアを返す', () => {
        expect(wordScore('Scala')).toBe(5)
        expect(wordScore('TypeScript')).toBe(10)
      })
    })
  })

  describe('2.2 ボーナススコア計算', () => {
    describe('bonusScore', () => {
      it('cを含む場合はボーナスを付与', () => {
        expect(bonusScore('Scala')).toBe(10)
        expect(bonusScore('cat')).toBe(8)
        expect(bonusScore('TypeScript')).toBe(15)
      })

      it('cを含まない場合はボーナスなし', () => {
        expect(bonusScore('Python')).toBe(6)
        expect(bonusScore('Java')).toBe(4)
      })
    })
  })

  describe('2.3 ショッピングカートの割引計算', () => {
    describe('getDiscountPercentage', () => {
      it('Bookを含む場合は5%割引', () => {
        expect(getDiscountPercentage(['Apple', 'Book'])).toBe(5)
        expect(getDiscountPercentage(['Book'])).toBe(5)
      })

      it('Bookを含まない場合は0%割引', () => {
        expect(getDiscountPercentage(['Apple', 'Orange'])).toBe(0)
        expect(getDiscountPercentage([])).toBe(0)
      })
    })

    describe('calculateDiscount', () => {
      it('割引額を計算する', () => {
        expect(calculateDiscount(100, 10)).toBe(10)
        expect(calculateDiscount(100, 0)).toBe(0)
        expect(calculateDiscount(50, 20)).toBe(10)
      })
    })

    describe('calculateFinalPrice', () => {
      it('最終価格を計算する', () => {
        expect(calculateFinalPrice(100, ['Apple', 'Book'])).toBe(95)
        expect(calculateFinalPrice(100, ['Apple'])).toBe(100)
      })
    })
  })

  describe('2.4 チップ計算', () => {
    describe('getTipPercentage', () => {
      it('6人以上は20%', () => {
        expect(getTipPercentage(['A', 'B', 'C', 'D', 'E', 'F'])).toBe(20)
        expect(getTipPercentage(['A', 'B', 'C', 'D', 'E', 'F', 'G'])).toBe(20)
      })

      it('1-5人は10%', () => {
        expect(getTipPercentage(['Alice', 'Bob'])).toBe(10)
        expect(getTipPercentage(['A', 'B', 'C', 'D', 'E'])).toBe(10)
        expect(getTipPercentage(['A'])).toBe(10)
      })

      it('0人は0%', () => {
        expect(getTipPercentage([])).toBe(0)
      })
    })

    describe('calculateTip', () => {
      it('チップ額を計算する', () => {
        expect(calculateTip(100, ['Alice', 'Bob'])).toBe(10)
        expect(calculateTip(100, ['A', 'B', 'C', 'D', 'E', 'F'])).toBe(20)
        expect(calculateTip(100, [])).toBe(0)
      })
    })
  })

  describe('2.5 参照透過性', () => {
    describe('referentialTransparencyExample', () => {
      it('参照透過性の例が正しい', () => {
        expect(referentialTransparencyExample()).toBe(true)
      })
    })
  })

  describe('2.6 文字列操作の純粋関数', () => {
    describe('appendExclamation', () => {
      it('感嘆符を追加する', () => {
        expect(appendExclamation('Hello')).toBe('Hello!')
        expect(appendExclamation('')).toBe('!')
      })
    })

    describe('countVowels', () => {
      it('母音の数をカウントする', () => {
        expect(countVowels('hello')).toBe(2)
        expect(countVowels('TypeScript')).toBe(2)
        expect(countVowels('xyz')).toBe(0)
        expect(countVowels('AEIOU')).toBe(5)
      })
    })
  })

  describe('2.7 リスト操作の純粋関数', () => {
    describe('doubleAll', () => {
      it('全ての要素を2倍にする', () => {
        expect(doubleAll([1, 2, 3])).toEqual([2, 4, 6])
        expect(doubleAll([])).toEqual([])
        expect(doubleAll([-1, 0, 1])).toEqual([-2, 0, 2])
      })
    })

    describe('filterPositive', () => {
      it('正の数のみをフィルタリングする', () => {
        expect(filterPositive([1, -2, 3, -4, 5])).toEqual([1, 3, 5])
        expect(filterPositive([-1, -2])).toEqual([])
        expect(filterPositive([1, 2, 3])).toEqual([1, 2, 3])
      })
    })

    describe('findLongest', () => {
      it('最も長い文字列を見つける', () => {
        expect(findLongest(['apple', 'banana', 'cherry'])).toBe('banana')
        expect(findLongest(['a', 'bb', 'ccc'])).toBe('ccc')
      })

      it('空のリストは空文字列を返す', () => {
        expect(findLongest([])).toBe('')
      })
    })

    describe('allPositive', () => {
      it('全て正の数の場合trueを返す', () => {
        expect(allPositive([1, 2, 3])).toBe(true)
      })

      it('負の数が含まれる場合falseを返す', () => {
        expect(allPositive([1, -2, 3])).toBe(false)
      })

      it('空リストはtrue（vacuous truth）', () => {
        expect(allPositive([])).toBe(true)
      })
    })

    describe('anyNegative', () => {
      it('負の数が含まれる場合trueを返す', () => {
        expect(anyNegative([1, -2, 3])).toBe(true)
      })

      it('負の数が含まれない場合falseを返す', () => {
        expect(anyNegative([1, 2, 3])).toBe(false)
      })

      it('空リストはfalse', () => {
        expect(anyNegative([])).toBe(false)
      })
    })

    describe('sumList', () => {
      it('リストの合計を計算する', () => {
        expect(sumList([1, 2, 3, 4, 5])).toBe(15)
        expect(sumList([])).toBe(0)
        expect(sumList([-1, 1])).toBe(0)
      })
    })

    describe('average', () => {
      it('リストの平均を計算する', () => {
        const result = average([1, 2, 3, 4, 5])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(3)
        }
      })

      it('空リストはNoneを返す', () => {
        const result = average([])
        expect(O.isNone(result)).toBe(true)
      })
    })
  })

  describe('2.8 文字 a を除外するワードスコア', () => {
    describe('wordScoreNoA', () => {
      it('aを除外してスコアを計算する', () => {
        expect(wordScoreNoA('Scala')).toBe(3)
        expect(wordScoreNoA('function')).toBe(8)
        expect(wordScoreNoA('')).toBe(0)
        expect(wordScoreNoA('aaa')).toBe(0)
        expect(wordScoreNoA('AAA')).toBe(0)
      })
    })
  })

  describe('2.9 高階関数の基本', () => {
    describe('applyTwice', () => {
      it('関数を2回適用する', () => {
        expect(applyTwice((x) => x + 1, 5)).toBe(7)
        expect(applyTwice((x) => x * 2, 3)).toBe(12)
      })
    })

    describe('compose', () => {
      it('2つの関数を合成する', () => {
        const double = (x: number) => x * 2
        const addOne = (x: number) => x + 1
        const doubleThenAdd = compose(addOne, double)
        expect(doubleThenAdd(5)).toBe(11)
      })
    })

    describe('composeWithFlow', () => {
      it('flow を使って関数を合成する', () => {
        const double = (x: number) => x * 2
        const addOne = (x: number) => x + 1
        const doubleThenAdd = composeWithFlow(double, addOne)
        expect(doubleThenAdd(5)).toBe(11)
      })
    })
  })

  describe('2.10 タイムスタンプフォーマット', () => {
    describe('formatTimestamp', () => {
      it('日付をフォーマットする', () => {
        expect(formatTimestamp(2024, 1, 15)).toBe('2024-01-15')
        expect(formatTimestamp(2024, 12, 31)).toBe('2024-12-31')
      })

      it('ゼロパディングが正しく動作する', () => {
        expect(formatTimestamp(2024, 1, 1)).toBe('2024-01-01')
        expect(formatTimestamp(999, 9, 9)).toBe('0999-09-09')
      })
    })
  })
})
