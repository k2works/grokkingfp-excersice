/**
 * 第3章: イミュータブルなデータ操作のテスト
 */

import { describe, it, expect } from 'vitest'
import {
  appended,
  appendedAll,
  prepended,
  firstN,
  firstTwo,
  lastN,
  lastTwo,
  moveFirstTwoToEnd,
  insertBeforeLast,
  insertAtMiddle,
  replan,
  abbreviate,
  substring,
  createPoint,
  withX,
  withY,
  createCity,
  withPopulation,
  addToSet,
  removeFromSet,
  updateMap,
  removeKey,
  appendWithFpts,
  prependWithFpts,
  concatWithFpts,
} from '../src/ch03_immutable_data.js'

describe('第3章: イミュータブルなデータ操作', () => {
  describe('3.1 イミュータブルなリスト操作', () => {
    describe('appended', () => {
      it('要素を追加した新しいリストを返す', () => {
        const original = ['Apple', 'Book'] as const
        const result = appended(original, 'Mango')
        expect(result).toEqual(['Apple', 'Book', 'Mango'])
        expect(original).toEqual(['Apple', 'Book']) // 元のリストは変更されない
      })
    })

    describe('appendedAll', () => {
      it('複数の要素を追加した新しいリストを返す', () => {
        expect(appendedAll(['a', 'b'], ['c', 'd'])).toEqual(['a', 'b', 'c', 'd'])
      })
    })

    describe('prepended', () => {
      it('先頭に要素を追加した新しいリストを返す', () => {
        expect(prepended(['b', 'c'], 'a')).toEqual(['a', 'b', 'c'])
      })
    })
  })

  describe('3.2 スライス操作', () => {
    describe('firstN', () => {
      it('最初のn要素を取得する', () => {
        expect(firstN(['a', 'b', 'c'], 2)).toEqual(['a', 'b'])
        expect(firstN(['a'], 2)).toEqual(['a'])
        expect(firstN([], 2)).toEqual([])
      })
    })

    describe('firstTwo', () => {
      it('最初の2要素を取得する', () => {
        expect(firstTwo(['a', 'b', 'c'])).toEqual(['a', 'b'])
        expect(firstTwo(['a'])).toEqual(['a'])
        expect(firstTwo([])).toEqual([])
      })
    })

    describe('lastN', () => {
      it('最後のn要素を取得する', () => {
        expect(lastN(['a', 'b', 'c'], 2)).toEqual(['b', 'c'])
        expect(lastN(['a'], 2)).toEqual(['a'])
        expect(lastN([], 2)).toEqual([])
      })
    })

    describe('lastTwo', () => {
      it('最後の2要素を取得する', () => {
        expect(lastTwo(['a', 'b', 'c'])).toEqual(['b', 'c'])
        expect(lastTwo(['a'])).toEqual(['a'])
      })
    })

    describe('moveFirstTwoToEnd', () => {
      it('最初の2要素を末尾に移動する', () => {
        expect(moveFirstTwoToEnd(['a', 'b', 'c'])).toEqual(['c', 'a', 'b'])
        expect(moveFirstTwoToEnd(['a', 'b', 'c', 'd'])).toEqual(['c', 'd', 'a', 'b'])
      })
    })

    describe('insertBeforeLast', () => {
      it('最後の要素の前に挿入する', () => {
        expect(insertBeforeLast(['a', 'b'], 'c')).toEqual(['a', 'c', 'b'])
        expect(insertBeforeLast(['a'], 'b')).toEqual(['b', 'a'])
        expect(insertBeforeLast([], 'a')).toEqual(['a'])
      })
    })

    describe('insertAtMiddle', () => {
      it('中央に要素を挿入する', () => {
        expect(insertAtMiddle(['a', 'b', 'c', 'd'], 'X')).toEqual(['a', 'b', 'X', 'c', 'd'])
        expect(insertAtMiddle(['a', 'b'], 'X')).toEqual(['a', 'X', 'b'])
      })
    })
  })

  describe('3.3 旅程の再計画', () => {
    describe('replan', () => {
      it('指定した都市の前に新しい都市を挿入する', () => {
        const plan = ['Paris', 'Berlin', 'Kraków']
        const result = replan(plan, 'Vienna', 'Kraków')
        expect(result).toEqual(['Paris', 'Berlin', 'Vienna', 'Kraków'])
        expect(plan).toEqual(['Paris', 'Berlin', 'Kraków']) // 元の計画は変わらない
      })

      it('存在しない都市の場合は末尾に追加する', () => {
        const plan = ['Paris', 'Berlin']
        expect(replan(plan, 'Vienna', 'Tokyo')).toEqual(['Paris', 'Berlin', 'Vienna'])
      })
    })
  })

  describe('3.4 文字列操作', () => {
    describe('abbreviate', () => {
      it('名前を省略形にする', () => {
        expect(abbreviate('Alonzo Church')).toBe('A. Church')
        expect(abbreviate('A. Church')).toBe('A. Church')
      })

      it('スペースがない場合はそのまま返す', () => {
        expect(abbreviate('Alonzo')).toBe('Alonzo')
      })
    })

    describe('substring', () => {
      it('部分文字列を取得する', () => {
        expect(substring('hello', 1, 4)).toBe('ell')
        expect(substring('TypeScript', 0, 4)).toBe('Type')
      })
    })
  })

  describe('3.5 イミュータブルなオブジェクト', () => {
    describe('Point', () => {
      it('Point を作成できる', () => {
        const p = createPoint(1, 2)
        expect(p.x).toBe(1)
        expect(p.y).toBe(2)
      })

      it('x座標を更新した新しいPointを作成できる', () => {
        const p1 = createPoint(1, 2)
        const p2 = withX(p1, 10)
        expect(p1.x).toBe(1) // 元のPointは変わらない
        expect(p2.x).toBe(10)
        expect(p2.y).toBe(2)
      })

      it('y座標を更新した新しいPointを作成できる', () => {
        const p1 = createPoint(1, 2)
        const p2 = withY(p1, 20)
        expect(p1.y).toBe(2) // 元のPointは変わらない
        expect(p2.y).toBe(20)
      })
    })

    describe('City', () => {
      it('人口を更新した新しいCityを作成できる', () => {
        const tokyo = createCity('Tokyo', 13960000)
        const tokyoUpdated = withPopulation(tokyo, 14000000)
        expect(tokyo.population).toBe(13960000) // 元のCityは変わらない
        expect(tokyoUpdated.population).toBe(14000000)
      })
    })
  })

  describe('3.6 ReadonlySet', () => {
    describe('addToSet', () => {
      it('要素を追加した新しいSetを返す', () => {
        const s = new Set(['a', 'b'])
        const s2 = addToSet(s, 'c')
        expect(s.has('c')).toBe(false) // 元のSetは変わらない
        expect(s2.has('c')).toBe(true)
      })
    })

    describe('removeFromSet', () => {
      it('要素を削除した新しいSetを返す', () => {
        const s = new Set(['a', 'b', 'c'])
        const s2 = removeFromSet(s, 'b')
        expect(s.has('b')).toBe(true) // 元のSetは変わらない
        expect(s2.has('b')).toBe(false)
      })
    })
  })

  describe('3.7 ReadonlyMap', () => {
    describe('updateMap', () => {
      it('キーを更新した新しいMapを返す', () => {
        const m = new Map([['a', 1], ['b', 2]])
        const m2 = updateMap(m, 'c', 3)
        expect(m.has('c')).toBe(false) // 元のMapは変わらない
        expect(m2.get('c')).toBe(3)
      })
    })

    describe('removeKey', () => {
      it('キーを削除した新しいMapを返す', () => {
        const m = new Map([['a', 1], ['b', 2], ['c', 3]])
        const m2 = removeKey(m, 'b')
        expect(m.has('b')).toBe(true) // 元のMapは変わらない
        expect(m2.has('b')).toBe(false)
      })
    })
  })

  describe('3.8 fp-ts を使ったイミュータブル操作', () => {
    describe('appendWithFpts', () => {
      it('要素を追加する', () => {
        expect(appendWithFpts(['a', 'b'], 'c')).toEqual(['a', 'b', 'c'])
      })
    })

    describe('prependWithFpts', () => {
      it('先頭に要素を追加する', () => {
        expect(prependWithFpts(['b', 'c'], 'a')).toEqual(['a', 'b', 'c'])
      })
    })

    describe('concatWithFpts', () => {
      it('リストを連結する', () => {
        expect(concatWithFpts(['a', 'b'], ['c', 'd'])).toEqual(['a', 'b', 'c', 'd'])
      })
    })
  })
})
