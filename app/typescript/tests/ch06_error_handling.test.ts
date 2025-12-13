/**
 * 第6章: Option 型による安全なエラーハンドリングのテスト
 */

import { describe, it, expect } from 'vitest'
import * as O from 'fp-ts/Option'
import {
  safeDivide,
  parseNumber,
  safeHead,
  safeLast,
  safeGet,
  createTvShow,
  extractName,
  extractYearStart,
  extractYearEnd,
  extractSingleYear,
  parseShow,
  mapOption,
  flatMapOption,
  filterOption,
  orElseOption,
  getOrElseOption,
  parseShowsBestEffort,
  parseShowsAllOrNothing,
  findFirst,
  findLast,
  forallOption,
  existsOption,
  toArray,
  fromNullable,
  fromPredicate,
  addStrings,
  traverseArray,
  safeMax,
  safeMin,
} from '../src/ch06_error_handling.js'

describe('第6章: Option 型による安全なエラーハンドリング', () => {
  describe('6.1 Option の基本', () => {
    describe('safeDivide', () => {
      it('正常な除算は some を返す', () => {
        const result = safeDivide(10, 2)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('0で除算すると none を返す', () => {
        expect(O.isNone(safeDivide(10, 0))).toBe(true)
      })

      it('整数の除算を行う', () => {
        const result = safeDivide(7, 2)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(3)
        }
      })
    })

    describe('parseNumber', () => {
      it('有効な数値文字列をパースする', () => {
        const result = parseNumber('42')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(42)
        }
      })

      it('無効な文字列は none を返す', () => {
        expect(O.isNone(parseNumber('abc'))).toBe(true)
        expect(O.isNone(parseNumber(''))).toBe(true)
      })
    })

    describe('safeHead', () => {
      it('配列の最初の要素を返す', () => {
        const result = safeHead([1, 2, 3])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(1)
        }
      })

      it('空配列は none を返す', () => {
        expect(O.isNone(safeHead([]))).toBe(true)
      })
    })

    describe('safeLast', () => {
      it('配列の最後の要素を返す', () => {
        const result = safeLast([1, 2, 3])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(3)
        }
      })

      it('空配列は none を返す', () => {
        expect(O.isNone(safeLast([]))).toBe(true)
      })
    })

    describe('safeGet', () => {
      it('有効なインデックスの要素を返す', () => {
        const result = safeGet([1, 2, 3], 1)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(2)
        }
      })

      it('無効なインデックスは none を返す', () => {
        expect(O.isNone(safeGet([1, 2, 3], 10))).toBe(true)
        expect(O.isNone(safeGet([1, 2, 3], -1))).toBe(true)
      })
    })
  })

  describe('6.2 TV番組のパース', () => {
    describe('extractName', () => {
      it('番組名を抽出する', () => {
        const result = extractName('Breaking Bad (2008-2013)')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe('Breaking Bad')
        }
      })

      it('括弧がない場合は none を返す', () => {
        expect(O.isNone(extractName('Breaking Bad'))).toBe(true)
      })

      it('括弧から始まる場合は none を返す', () => {
        expect(O.isNone(extractName('(2008-2013)'))).toBe(true)
      })
    })

    describe('extractYearStart', () => {
      it('開始年を抽出する', () => {
        const result = extractYearStart('Breaking Bad (2008-2013)')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(2008)
        }
      })

      it('ダッシュがない場合は none を返す', () => {
        expect(O.isNone(extractYearStart('Chernobyl (2019)'))).toBe(true)
      })
    })

    describe('extractYearEnd', () => {
      it('終了年を抽出する', () => {
        const result = extractYearEnd('Breaking Bad (2008-2013)')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(2013)
        }
      })

      it('ダッシュがない場合は none を返す', () => {
        expect(O.isNone(extractYearEnd('Chernobyl (2019)'))).toBe(true)
      })
    })

    describe('extractSingleYear', () => {
      it('単年を抽出する', () => {
        const result = extractSingleYear('Chernobyl (2019)')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(2019)
        }
      })

      it('ダッシュがある場合は none を返す', () => {
        expect(O.isNone(extractSingleYear('Breaking Bad (2008-2013)'))).toBe(
          true
        )
      })
    })

    describe('parseShow', () => {
      it('範囲形式の番組をパースする', () => {
        const result = parseShow('Breaking Bad (2008-2013)')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toEqual(createTvShow('Breaking Bad', 2008, 2013))
        }
      })

      it('単年形式の番組をパースする', () => {
        const result = parseShow('Chernobyl (2019)')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toEqual(createTvShow('Chernobyl', 2019, 2019))
        }
      })

      it('無効な形式は none を返す', () => {
        expect(O.isNone(parseShow('Invalid'))).toBe(true)
        expect(O.isNone(parseShow('The Wire 2002-2008'))).toBe(true)
        expect(O.isNone(parseShow('(2008-2013)'))).toBe(true)
      })
    })
  })

  describe('6.3 Option の主要操作', () => {
    describe('mapOption', () => {
      it('some の値を変換する', () => {
        const result = mapOption(O.some(5), (x) => x * 2)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(10)
        }
      })

      it('none はそのまま none を返す', () => {
        expect(O.isNone(mapOption(O.none, (x: number) => x * 2))).toBe(true)
      })
    })

    describe('flatMapOption', () => {
      it('some に Option を返す関数を適用する', () => {
        const result = flatMapOption(O.some(10), (x) => safeDivide(x, 2))
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('関数が none を返すと全体が none になる', () => {
        const result = flatMapOption(O.some(10), (x) => safeDivide(x, 0))
        expect(O.isNone(result)).toBe(true)
      })
    })

    describe('filterOption', () => {
      it('条件を満たす場合は some を返す', () => {
        const result = filterOption(O.some(5), (x) => x > 3)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('条件を満たさない場合は none を返す', () => {
        expect(O.isNone(filterOption(O.some(5), (x) => x > 10))).toBe(true)
      })
    })

    describe('orElseOption', () => {
      it('some の場合はそのまま返す', () => {
        const result = orElseOption(O.some(5), () => O.some(10))
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('none の場合は代替を返す', () => {
        const result = orElseOption(O.none, () => O.some(10))
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(10)
        }
      })
    })

    describe('getOrElseOption', () => {
      it('some の場合はその値を返す', () => {
        expect(getOrElseOption(O.some(5), () => 0)).toBe(5)
      })

      it('none の場合はデフォルト値を返す', () => {
        expect(getOrElseOption(O.none, () => 0)).toBe(0)
      })
    })
  })

  describe('6.4 エラーハンドリング戦略', () => {
    describe('parseShowsBestEffort', () => {
      it('パースできたものだけを返す', () => {
        const result = parseShowsBestEffort([
          'Breaking Bad (2008-2013)',
          'Invalid',
          'Mad Men (2007-2015)',
        ])
        expect(result).toHaveLength(2)
        expect(result[0].title).toBe('Breaking Bad')
        expect(result[1].title).toBe('Mad Men')
      })

      it('全て無効な場合は空配列を返す', () => {
        expect(parseShowsBestEffort(['Invalid', 'Also Invalid'])).toHaveLength(0)
      })
    })

    describe('parseShowsAllOrNothing', () => {
      it('全て成功すると some を返す', () => {
        const result = parseShowsAllOrNothing([
          'Breaking Bad (2008-2013)',
          'Mad Men (2007-2015)',
        ])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toHaveLength(2)
        }
      })

      it('一つでも失敗すると none を返す', () => {
        const result = parseShowsAllOrNothing([
          'Breaking Bad (2008-2013)',
          'Invalid',
        ])
        expect(O.isNone(result)).toBe(true)
      })
    })
  })

  describe('6.5 Option を使った検索', () => {
    describe('findFirst', () => {
      it('条件に合う最初の要素を返す', () => {
        const result = findFirst([1, 2, 3, 4, 5], (x) => x > 3)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(4)
        }
      })

      it('見つからない場合は none を返す', () => {
        expect(O.isNone(findFirst([1, 2, 3], (x) => x > 10))).toBe(true)
      })
    })

    describe('findLast', () => {
      it('条件に合う最後の要素を返す', () => {
        const result = findLast([1, 2, 3, 4, 5], (x) => x > 3)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('見つからない場合は none を返す', () => {
        expect(O.isNone(findLast([1, 2, 3], (x) => x > 10))).toBe(true)
      })
    })
  })

  describe('6.6 forall と exists', () => {
    describe('forallOption', () => {
      it('some で条件を満たす場合は true', () => {
        expect(forallOption(O.some(5), (x) => x < 10)).toBe(true)
      })

      it('some で条件を満たさない場合は false', () => {
        expect(forallOption(O.some(15), (x) => x < 10)).toBe(false)
      })

      it('none の場合は true（全称命題）', () => {
        expect(forallOption(O.none, (x: number) => x < 10)).toBe(true)
      })
    })

    describe('existsOption', () => {
      it('some で条件を満たす場合は true', () => {
        expect(existsOption(O.some(5), (x) => x < 10)).toBe(true)
      })

      it('some で条件を満たさない場合は false', () => {
        expect(existsOption(O.some(15), (x) => x < 10)).toBe(false)
      })

      it('none の場合は false（存在命題）', () => {
        expect(existsOption(O.none, (x: number) => x < 10)).toBe(false)
      })
    })
  })

  describe('6.7 Option のユーティリティ', () => {
    describe('toArray', () => {
      it('some を配列に変換する', () => {
        expect(toArray(O.some(5))).toEqual([5])
      })

      it('none を空配列に変換する', () => {
        expect(toArray(O.none)).toEqual([])
      })
    })

    describe('fromNullable', () => {
      it('値を some に変換する', () => {
        const result = fromNullable(5)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('null を none に変換する', () => {
        expect(O.isNone(fromNullable(null))).toBe(true)
      })

      it('undefined を none に変換する', () => {
        expect(O.isNone(fromNullable(undefined))).toBe(true)
      })
    })

    describe('fromPredicate', () => {
      it('条件を満たす場合は some を返す', () => {
        const result = fromPredicate(5, (x) => x > 0)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('条件を満たさない場合は none を返す', () => {
        expect(O.isNone(fromPredicate(-5, (x) => x > 0))).toBe(true)
      })
    })
  })

  describe('6.8 2つの数値文字列の加算', () => {
    describe('addStrings', () => {
      it('両方が有効な場合は合計を返す', () => {
        const result = addStrings('10', '20')
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(30)
        }
      })

      it('一方が無効な場合は none を返す', () => {
        expect(O.isNone(addStrings('10', 'abc'))).toBe(true)
        expect(O.isNone(addStrings('xyz', '20'))).toBe(true)
      })
    })
  })

  describe('6.9 Option と ReadonlyArray の組み合わせ', () => {
    describe('traverseArray', () => {
      it('全て成功すると some を返す', () => {
        const result = traverseArray(['1', '2', '3'], parseNumber)
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toEqual([1, 2, 3])
        }
      })

      it('一つでも失敗すると none を返す', () => {
        const result = traverseArray(['1', 'abc', '3'], parseNumber)
        expect(O.isNone(result)).toBe(true)
      })
    })

    describe('safeMax', () => {
      it('最大値を返す', () => {
        const result = safeMax([1, 5, 3])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('空配列は none を返す', () => {
        expect(O.isNone(safeMax([]))).toBe(true)
      })
    })

    describe('safeMin', () => {
      it('最小値を返す', () => {
        const result = safeMin([1, 5, 3])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(1)
        }
      })

      it('空配列は none を返す', () => {
        expect(O.isNone(safeMin([]))).toBe(true)
      })
    })
  })
})
