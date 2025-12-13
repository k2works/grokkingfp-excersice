/**
 * 第5章: flatMap とネストしたリスト操作のテスト
 */

import { describe, it, expect } from 'vitest'
import * as O from 'fp-ts/Option'
import {
  flatten,
  flatMap,
  chainExample,
  createBook,
  createMovie,
  bookAdaptations,
  getAllAuthors,
  getAllMovies,
  recommendationFeed,
  recommendedBooks,
  getRecommendedAuthors,
  createPoint,
  createPoint3d,
  generatePoints,
  generatePoints3d,
  duplicateEach,
  withIncremented,
  filterEvenViaChain,
  filterViaChain,
  compactOptions,
  filterMapExample,
  combineOptions,
  cartesianProduct,
  traverseOption,
  isInside,
  allCombinations,
  insidePoints,
} from '../src/ch05_flatmap.js'

describe('第5章: flatMap とネストしたリスト操作', () => {
  describe('5.1 基本的な flatten と flatMap', () => {
    describe('flatten', () => {
      it('ネストしたリストを平坦化する', () => {
        expect(flatten([[1, 2], [3, 4], [5]])).toEqual([1, 2, 3, 4, 5])
        expect(flatten([['a', 'b'], [], ['c']])).toEqual(['a', 'b', 'c'])
        expect(flatten([])).toEqual([])
      })
    })

    describe('flatMap', () => {
      it('mapしてからflattenする', () => {
        expect(flatMap((x: number) => [x, x * 2], [1, 2, 3])).toEqual([1, 2, 2, 4, 3, 6])
        expect(flatMap((s: string) => [...s], ['ab', 'cd'])).toEqual(['a', 'b', 'c', 'd'])
      })
    })

    describe('chainExample', () => {
      it('fp-tsのchainを使ったflatMap', () => {
        expect(chainExample([1, 2, 3], (x) => [x, x + 10])).toEqual([1, 11, 2, 12, 3, 13])
      })
    })
  })

  describe('5.2 Book と Movie の例', () => {
    describe('bookAdaptations', () => {
      it('Tolkienの映画化作品を返す', () => {
        const movies = bookAdaptations('Tolkien')
        expect(movies).toHaveLength(2)
        expect(movies[0].title).toBe('An Unexpected Journey')
      })

      it('その他の著者は空リストを返す', () => {
        expect(bookAdaptations('Chiusano')).toEqual([])
      })
    })

    describe('getAllAuthors', () => {
      it('全ての本の著者を取得する', () => {
        const books = [
          createBook('FP in Scala', ['Chiusano', 'Bjarnason']),
          createBook('The Hobbit', ['Tolkien']),
        ]
        expect(getAllAuthors(books)).toEqual(['Chiusano', 'Bjarnason', 'Tolkien'])
      })
    })

    describe('getAllMovies', () => {
      it('著者リストから全ての映画化作品を取得する', () => {
        const movies = getAllMovies(['Chiusano', 'Tolkien'])
        expect(movies).toHaveLength(2)
        expect(movies[0].title).toBe('An Unexpected Journey')
      })
    })

    describe('recommendationFeed', () => {
      it('本のリストから推薦メッセージを生成する', () => {
        const books = [
          createBook('FP in Scala', ['Chiusano', 'Bjarnason']),
          createBook('The Hobbit', ['Tolkien']),
        ]
        const feed = recommendationFeed(books)
        expect(feed).toHaveLength(2)
        expect(feed[0]).toContain('An Unexpected Journey')
        expect(feed[0]).toContain("Tolkien's The Hobbit")
      })
    })
  })

  describe('5.3 友達の推薦本', () => {
    describe('recommendedBooks', () => {
      it('Aliceはscala本を勧める', () => {
        const books = recommendedBooks('Alice')
        expect(books).toHaveLength(2)
        expect(books[0].title).toBe('FP in Scala')
      })

      it('Bobはフィクション本を勧める', () => {
        const books = recommendedBooks('Bob')
        expect(books).toHaveLength(2)
        expect(books[0].title).toBe('Harry Potter')
      })

      it('Charlieは空リストを返す', () => {
        expect(recommendedBooks('Charlie')).toEqual([])
      })
    })

    describe('getRecommendedAuthors', () => {
      it('友達のおすすめ本の著者を全て取得する', () => {
        const authors = getRecommendedAuthors(['Alice', 'Bob', 'Charlie'])
        expect(authors).toEqual([
          'Chiusano',
          'Bjarnason',
          'Sfregola',
          'Rowling',
          'Tolkien',
        ])
      })
    })
  })

  describe('5.4 2D/3D ポイントの生成', () => {
    describe('generatePoints', () => {
      it('全ての組み合わせからPointを生成する', () => {
        const points = generatePoints([1], [-2, 7])
        expect(points).toEqual([
          { x: 1, y: -2 },
          { x: 1, y: 7 },
        ])
      })

      it('複数のxとyの組み合わせを生成する', () => {
        const points = generatePoints([1, 2], [-2, 7])
        expect(points).toHaveLength(4)
      })
    })

    describe('generatePoints3d', () => {
      it('全ての組み合わせからPoint3dを生成する', () => {
        const points = generatePoints3d([1], [-2, 7], [3, 4])
        expect(points).toHaveLength(4)
        expect(points[0]).toEqual({ x: 1, y: -2, z: 3 })
      })
    })
  })

  describe('5.5 flatMap でリストサイズを変更', () => {
    describe('duplicateEach', () => {
      it('各要素を2つに複製する', () => {
        expect(duplicateEach([1, 2, 3])).toEqual([1, 1, 2, 2, 3, 3])
      })
    })

    describe('withIncremented', () => {
      it('各要素とその+10を含むリストを生成する', () => {
        expect(withIncremented([1, 2, 3])).toEqual([1, 11, 2, 12, 3, 13])
      })
    })

    describe('filterEvenViaChain', () => {
      it('chainパターンで偶数のみをフィルタする', () => {
        expect(filterEvenViaChain([1, 2, 3, 4, 5])).toEqual([2, 4])
      })
    })

    describe('filterViaChain', () => {
      it('条件に基づいてフィルタリングする', () => {
        expect(filterViaChain((x: number) => x > 2, [1, 2, 3, 4, 5])).toEqual([3, 4, 5])
      })
    })
  })

  describe('5.6 Option との組み合わせ', () => {
    describe('compactOptions', () => {
      it('Someの値だけを取り出す', () => {
        const options = [O.some(1), O.none, O.some(2), O.none, O.some(3)]
        expect(compactOptions(options)).toEqual([1, 2, 3])
      })
    })

    describe('filterMapExample', () => {
      it('Optionを返す関数を適用しSomeの値だけを取り出す', () => {
        const result = filterMapExample(
          (s: string) => (s.length > 2 ? O.some(s.toUpperCase()) : O.none),
          ['a', 'abc', 'de', 'xyz']
        )
        expect(result).toEqual(['ABC', 'XYZ'])
      })
    })
  })

  describe('5.7 sequenceT', () => {
    describe('combineOptions', () => {
      it('全てがSomeの場合のみSomeを返す', () => {
        const result1 = combineOptions(O.some(1), O.some(2), O.some(3))
        expect(O.isSome(result1)).toBe(true)
        if (O.isSome(result1)) {
          expect(result1.value).toEqual([1, 2, 3])
        }

        const result2 = combineOptions(O.some(1), O.none, O.some(3))
        expect(O.isNone(result2)).toBe(true)
      })
    })

    describe('cartesianProduct', () => {
      it('デカルト積を生成する', () => {
        expect(cartesianProduct([1, 2], [3, 4])).toEqual([
          [1, 3],
          [1, 4],
          [2, 3],
          [2, 4],
        ])
      })
    })
  })

  describe('5.8 traverse', () => {
    describe('traverseOption', () => {
      it('全てSomeなら全体をSomeで包む', () => {
        const safeDivide = (n: number) => (n === 0 ? O.none : O.some(10 / n))
        const result = traverseOption(safeDivide, [1, 2, 5])
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toEqual([10, 5, 2])
        }
      })

      it('一つでもNoneなら全体がNone', () => {
        const safeDivide = (n: number) => (n === 0 ? O.none : O.some(10 / n))
        const result = traverseOption(safeDivide, [1, 0, 5])
        expect(O.isNone(result)).toBe(true)
      })
    })
  })

  describe('5.9 円内の点の判定', () => {
    describe('isInside', () => {
      it('円内の点を判定する', () => {
        expect(isInside(createPoint(1, 1), 2)).toBe(true)
        expect(isInside(createPoint(5, 2), 2)).toBe(false)
      })
    })

    describe('allCombinations', () => {
      it('全てのポイントと半径の組み合わせを生成する', () => {
        const points = [createPoint(5, 2), createPoint(1, 1)]
        const radiuses = [2, 1]
        const result = allCombinations(points, radiuses)
        expect(result).toHaveLength(4)
        expect(result[0]).toContain('Point(5,2)')
        expect(result[0]).toContain('radius of 2')
        expect(result[0]).toContain('false')
        expect(result[1]).toContain('Point(1,1)')
        expect(result[1]).toContain('true')
      })
    })

    describe('insidePoints', () => {
      it('円内にあるポイントのみを抽出する', () => {
        const points = [createPoint(5, 2), createPoint(1, 1)]
        const radiuses = [2, 1]
        const result = insidePoints(points, radiuses)
        expect(result).toHaveLength(1)
        expect(result[0]).toContain('Point(1,1)')
        expect(result[0]).toContain('radius of 2')
      })
    })
  })
})
