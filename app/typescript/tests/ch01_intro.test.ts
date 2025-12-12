/**
 * 第1章: 関数型プログラミング入門のテスト
 */

import { describe, it, expect } from 'vitest'
import {
  calculateScoreImperative,
  wordScore,
  increment,
  getFirstCharacter,
  add,
  double,
  greet,
  toUppercase,
  reverseString,
  absolute,
  maxValue,
  minValue,
  clamp,
  isEven,
  isPositive,
  isEmpty,
  MAX_SCORE,
  MIN_SCORE,
  DEFAULT_GREETING,
  getBoundedScore,
  transformString,
  transformNumber,
  calculateBoundedScore,
} from '../src/ch01_intro.js'

describe('第1章: 関数型プログラミング入門', () => {
  describe('1.1 命令型 vs 関数型', () => {
    describe('calculateScoreImperative', () => {
      it('文字列の長さをスコアとして返す', () => {
        expect(calculateScoreImperative('hello')).toBe(5)
        expect(calculateScoreImperative('Scala')).toBe(5)
      })

      it('空文字列は0を返す', () => {
        expect(calculateScoreImperative('')).toBe(0)
      })
    })

    describe('wordScore', () => {
      it('文字列の長さをスコアとして返す', () => {
        expect(wordScore('hello')).toBe(5)
        expect(wordScore('TypeScript')).toBe(10)
      })

      it('空文字列は0を返す', () => {
        expect(wordScore('')).toBe(0)
      })
    })
  })

  describe('1.2 基本的な関数定義', () => {
    describe('increment', () => {
      it('値を1増加させる', () => {
        expect(increment(5)).toBe(6)
        expect(increment(0)).toBe(1)
        expect(increment(-1)).toBe(0)
      })
    })

    describe('getFirstCharacter', () => {
      it('最初の文字を返す', () => {
        expect(getFirstCharacter('hello')).toBe('h')
        expect(getFirstCharacter('TypeScript')).toBe('T')
      })

      it('空文字列は空文字を返す', () => {
        expect(getFirstCharacter('')).toBe('')
      })
    })

    describe('add', () => {
      it('2つの数値を加算する', () => {
        expect(add(2, 3)).toBe(5)
        expect(add(-1, 1)).toBe(0)
        expect(add(0, 0)).toBe(0)
      })
    })

    describe('double', () => {
      it('値を2倍にする', () => {
        expect(double(5)).toBe(10)
        expect(double(0)).toBe(0)
        expect(double(-3)).toBe(-6)
      })
    })

    describe('greet', () => {
      it('挨拶メッセージを生成する', () => {
        expect(greet('World')).toBe('Hello, World!')
        expect(greet('TypeScript')).toBe('Hello, TypeScript!')
      })
    })

    describe('toUppercase', () => {
      it('文字列を大文字に変換する', () => {
        expect(toUppercase('hello')).toBe('HELLO')
        expect(toUppercase('TypeScript')).toBe('TYPESCRIPT')
      })
    })

    describe('reverseString', () => {
      it('文字列を反転する', () => {
        expect(reverseString('hello')).toBe('olleh')
        expect(reverseString('TypeScript')).toBe('tpircSepyT')
      })

      it('空文字列は空文字列を返す', () => {
        expect(reverseString('')).toBe('')
      })
    })
  })

  describe('1.3 条件分岐を含む関数', () => {
    describe('absolute', () => {
      it('絶対値を返す', () => {
        expect(absolute(5)).toBe(5)
        expect(absolute(-5)).toBe(5)
        expect(absolute(0)).toBe(0)
      })
    })

    describe('maxValue', () => {
      it('2つの値のうち大きい方を返す', () => {
        expect(maxValue(3, 5)).toBe(5)
        expect(maxValue(5, 3)).toBe(5)
        expect(maxValue(3, 3)).toBe(3)
      })
    })

    describe('minValue', () => {
      it('2つの値のうち小さい方を返す', () => {
        expect(minValue(3, 5)).toBe(3)
        expect(minValue(5, 3)).toBe(3)
        expect(minValue(3, 3)).toBe(3)
      })
    })

    describe('clamp', () => {
      it('値を指定された範囲内に収める', () => {
        expect(clamp(5, 0, 10)).toBe(5)
        expect(clamp(-5, 0, 10)).toBe(0)
        expect(clamp(15, 0, 10)).toBe(10)
      })

      it('境界値を正しく処理する', () => {
        expect(clamp(0, 0, 10)).toBe(0)
        expect(clamp(10, 0, 10)).toBe(10)
      })
    })
  })

  describe('1.4 述語関数', () => {
    describe('isEven', () => {
      it('偶数の場合trueを返す', () => {
        expect(isEven(4)).toBe(true)
        expect(isEven(0)).toBe(true)
        expect(isEven(-2)).toBe(true)
      })

      it('奇数の場合falseを返す', () => {
        expect(isEven(3)).toBe(false)
        expect(isEven(-1)).toBe(false)
      })
    })

    describe('isPositive', () => {
      it('正の数の場合trueを返す', () => {
        expect(isPositive(5)).toBe(true)
        expect(isPositive(1)).toBe(true)
      })

      it('0または負の数の場合falseを返す', () => {
        expect(isPositive(0)).toBe(false)
        expect(isPositive(-3)).toBe(false)
      })
    })

    describe('isEmpty', () => {
      it('空文字列の場合trueを返す', () => {
        expect(isEmpty('')).toBe(true)
      })

      it('空でない文字列の場合falseを返す', () => {
        expect(isEmpty('hello')).toBe(false)
        expect(isEmpty(' ')).toBe(false)
      })
    })
  })

  describe('1.5 定数と型', () => {
    it('定数が正しく定義されている', () => {
      expect(MAX_SCORE).toBe(100)
      expect(MIN_SCORE).toBe(0)
      expect(DEFAULT_GREETING).toBe('Hello')
    })

    describe('getBoundedScore', () => {
      it('範囲内のスコアはそのまま返す', () => {
        expect(getBoundedScore(50)).toBe(50)
        expect(getBoundedScore(0)).toBe(0)
        expect(getBoundedScore(100)).toBe(100)
      })

      it('範囲外のスコアは境界値に収める', () => {
        expect(getBoundedScore(150)).toBe(100)
        expect(getBoundedScore(-10)).toBe(0)
      })
    })
  })

  describe('1.6 fp-ts の pipe を使った関数合成', () => {
    describe('transformString', () => {
      it('大文字に変換して反転する', () => {
        expect(transformString('hello')).toBe('OLLEH')
        expect(transformString('abc')).toBe('CBA')
      })
    })

    describe('transformNumber', () => {
      it('2倍にしてから1増加させる', () => {
        expect(transformNumber(5)).toBe(11)
        expect(transformNumber(0)).toBe(1)
        expect(transformNumber(-2)).toBe(-3)
      })
    })

    describe('calculateBoundedScore', () => {
      it('文字列の長さを2倍にして範囲内に収める', () => {
        expect(calculateBoundedScore('hello')).toBe(10)
        expect(calculateBoundedScore('hi')).toBe(4)
      })

      it('長い文字列は最大値に収める', () => {
        const longString = 'a'.repeat(60)
        expect(calculateBoundedScore(longString)).toBe(100)
      })

      it('空文字列は0を返す', () => {
        expect(calculateBoundedScore('')).toBe(0)
      })
    })
  })
})
