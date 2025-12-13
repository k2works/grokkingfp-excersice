/**
 * 第7章: Either 型と複合的なエラー処理のテスト
 */

import { describe, it, expect } from 'vitest'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import {
  safeDivideE,
  parseNumberE,
  createTvShow,
  extractNameE,
  extractYearStartE,
  extractYearEndE,
  extractSingleYearE,
  parseShowE,
  mapEither,
  flatMapEither,
  orElseEither,
  getOrElseEither,
  optionToEither,
  eitherToOption,
  stillActive,
  activeBetween,
  createArtist,
  wasArtistActive,
  activeLength,
  searchByGenre,
  searchByOrigin,
  searchByActiveYears,
  searchArtists,
  searchArtistsAny,
  validateAge,
  validateUsername,
  validateEmail,
  validateUser,
  traverseArrayE,
  collectRights,
  creditCard,
  bankTransfer,
  cash,
  describePayment,
  sampleArtists,
} from '../src/ch07_either.js'

describe('第7章: Either 型と複合的なエラー処理', () => {
  describe('7.1 Either の基本', () => {
    describe('safeDivideE', () => {
      it('正常な除算は right を返す', () => {
        const result = safeDivideE(10, 2)
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(5)
        }
      })

      it('0で除算すると left を返す', () => {
        const result = safeDivideE(10, 0)
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('Division by zero')
        }
      })
    })

    describe('parseNumberE', () => {
      it('有効な数値文字列は right を返す', () => {
        const result = parseNumberE('42')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(42)
        }
      })

      it('無効な文字列は left を返す', () => {
        const result = parseNumberE('abc')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('abc')
        }
      })
    })
  })

  describe('7.2 TV番組のパース（Either版）', () => {
    describe('extractNameE', () => {
      it('番組名を抽出する', () => {
        const result = extractNameE('Breaking Bad (2008-2013)')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe('Breaking Bad')
        }
      })

      it('抽出できない場合はエラーメッセージを返す', () => {
        const result = extractNameE('(2008-2013)')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain("Can't extract name")
        }
      })
    })

    describe('extractYearStartE', () => {
      it('開始年を抽出する', () => {
        const result = extractYearStartE('Breaking Bad (2008-2013)')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(2008)
        }
      })

      it('抽出できない場合はエラーメッセージを返す', () => {
        const result = extractYearStartE('Chernobyl (2019)')
        expect(E.isLeft(result)).toBe(true)
      })
    })

    describe('extractYearEndE', () => {
      it('終了年を抽出する', () => {
        const result = extractYearEndE('Breaking Bad (2008-2013)')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(2013)
        }
      })

      it('抽出できない場合はエラーメッセージを返す', () => {
        const result = extractYearEndE('Chernobyl (2019)')
        expect(E.isLeft(result)).toBe(true)
      })
    })

    describe('extractSingleYearE', () => {
      it('単年を抽出する', () => {
        const result = extractSingleYearE('Chernobyl (2019)')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(2019)
        }
      })

      it('ダッシュがある場合はエラーを返す', () => {
        const result = extractSingleYearE('Breaking Bad (2008-2013)')
        expect(E.isLeft(result)).toBe(true)
      })
    })

    describe('parseShowE', () => {
      it('範囲形式の番組をパースする', () => {
        const result = parseShowE('Breaking Bad (2008-2013)')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual(createTvShow('Breaking Bad', 2008, 2013))
        }
      })

      it('単年形式の番組をパースする', () => {
        const result = parseShowE('Chernobyl (2019)')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual(createTvShow('Chernobyl', 2019, 2019))
        }
      })

      it('無効な形式はエラーメッセージを返す', () => {
        const result = parseShowE('Invalid')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain("Can't extract name")
        }
      })
    })
  })

  describe('7.3 Either の主要操作', () => {
    describe('mapEither', () => {
      it('right の値を変換する', () => {
        const result = mapEither(E.right(5), (x) => x * 2)
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(10)
        }
      })

      it('left はそのまま返す', () => {
        const result = mapEither(E.left('error'), (x: number) => x * 2)
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('error')
        }
      })
    })

    describe('flatMapEither', () => {
      it('right に Either を返す関数を適用する', () => {
        const result = flatMapEither(E.right(10), (x) => safeDivideE(x, 2))
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(5)
        }
      })

      it('関数が left を返すと全体が left になる', () => {
        const result = flatMapEither(E.right(10), (x) => safeDivideE(x, 0))
        expect(E.isLeft(result)).toBe(true)
      })
    })

    describe('orElseEither', () => {
      it('right の場合はそのまま返す', () => {
        const result = orElseEither(E.right(5), () => E.right(10))
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(5)
        }
      })

      it('left の場合は代替を返す', () => {
        const result = orElseEither(E.left('error'), () => E.right(10))
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(10)
        }
      })
    })

    describe('getOrElseEither', () => {
      it('right の場合はその値を返す', () => {
        expect(getOrElseEither(E.right(5), () => 0)).toBe(5)
      })

      it('left の場合はデフォルト値を返す', () => {
        expect(getOrElseEither(E.left('error'), () => 0)).toBe(0)
      })
    })
  })

  describe('7.4 Option から Either への変換', () => {
    describe('optionToEither', () => {
      it('some を right に変換する', () => {
        const result = optionToEither(O.some(5), 'no value')
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(5)
        }
      })

      it('none を left に変換する', () => {
        const result = optionToEither(O.none, 'no value')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toBe('no value')
        }
      })
    })

    describe('eitherToOption', () => {
      it('right を some に変換する', () => {
        const result = eitherToOption(E.right(5))
        expect(O.isSome(result)).toBe(true)
        if (O.isSome(result)) {
          expect(result.value).toBe(5)
        }
      })

      it('left を none に変換する', () => {
        expect(O.isNone(eitherToOption(E.left('error')))).toBe(true)
      })
    })
  })

  describe('7.5 代数的データ型（ADT）', () => {
    describe('YearsActive', () => {
      it('StillActive を作成できる', () => {
        const years = stillActive(1981)
        expect(years._tag).toBe('StillActive')
        expect(years.since).toBe(1981)
      })

      it('ActiveBetween を作成できる', () => {
        const years = activeBetween(1968, 1980)
        expect(years._tag).toBe('ActiveBetween')
        expect(years.start).toBe(1968)
        expect(years.end).toBe(1980)
      })
    })

    describe('Artist', () => {
      it('アーティストを作成できる', () => {
        const artist = createArtist('Metallica', 'HeavyMetal', 'US', stillActive(1981))
        expect(artist.name).toBe('Metallica')
        expect(artist.genre).toBe('HeavyMetal')
        expect(artist.origin).toBe('US')
      })
    })
  })

  describe('7.6 パターンマッチング', () => {
    const metallica = createArtist('Metallica', 'HeavyMetal', 'US', stillActive(1981))
    const ledZeppelin = createArtist('Led Zeppelin', 'HardRock', 'England', activeBetween(1968, 1980))

    describe('wasArtistActive', () => {
      it('StillActive のアーティストが指定期間に活動していたかを判定する', () => {
        expect(wasArtistActive(metallica, 1990, 2000)).toBe(true)
        expect(wasArtistActive(metallica, 1970, 1975)).toBe(false)
      })

      it('ActiveBetween のアーティストが指定期間に活動していたかを判定する', () => {
        expect(wasArtistActive(ledZeppelin, 1970, 1975)).toBe(true)
        expect(wasArtistActive(ledZeppelin, 1990, 2000)).toBe(false)
      })
    })

    describe('activeLength', () => {
      it('StillActive のアーティストの活動年数を計算する', () => {
        expect(activeLength(metallica, 2024)).toBe(43)
      })

      it('ActiveBetween のアーティストの活動年数を計算する', () => {
        expect(activeLength(ledZeppelin, 2024)).toBe(12)
      })
    })
  })

  describe('7.7 検索条件のモデリング', () => {
    describe('searchArtists', () => {
      it('ジャンルで検索する', () => {
        const result = searchArtists(sampleArtists, [searchByGenre(['HeavyMetal'])])
        expect(result).toHaveLength(1)
        expect(result[0].name).toBe('Metallica')
      })

      it('出身地で検索する', () => {
        const result = searchArtists(sampleArtists, [searchByOrigin(['England'])])
        expect(result).toHaveLength(1)
        expect(result[0].name).toBe('Led Zeppelin')
      })

      it('活動期間で検索する', () => {
        const result = searchArtists(sampleArtists, [searchByActiveYears(1970, 1975)])
        expect(result).toHaveLength(2)
      })

      it('複数条件で検索する（AND）', () => {
        const result = searchArtists(sampleArtists, [
          searchByGenre(['HeavyMetal', 'HardRock']),
          searchByActiveYears(1970, 1975),
        ])
        expect(result).toHaveLength(1)
        expect(result[0].name).toBe('Led Zeppelin')
      })
    })

    describe('searchArtistsAny', () => {
      it('いずれかの条件を満たすアーティストを検索する（OR）', () => {
        const result = searchArtistsAny(sampleArtists, [
          searchByGenre(['HeavyMetal']),
          searchByOrigin(['Australia']),
        ])
        expect(result).toHaveLength(2)
      })
    })
  })

  describe('7.8 Either を使ったバリデーション', () => {
    describe('validateAge', () => {
      it('有効な年齢は right を返す', () => {
        const result = validateAge(25)
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toBe(25)
        }
      })

      it('負の年齢はエラーを返す', () => {
        const result = validateAge(-5)
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('negative')
        }
      })

      it('150を超える年齢はエラーを返す', () => {
        const result = validateAge(200)
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('150')
        }
      })
    })

    describe('validateUsername', () => {
      it('有効なユーザー名は right を返す', () => {
        const result = validateUsername('alice')
        expect(E.isRight(result)).toBe(true)
      })

      it('空のユーザー名はエラーを返す', () => {
        const result = validateUsername('')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('empty')
        }
      })

      it('短すぎるユーザー名はエラーを返す', () => {
        const result = validateUsername('ab')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('3 characters')
        }
      })
    })

    describe('validateEmail', () => {
      it('有効なメールアドレスは right を返す', () => {
        const result = validateEmail('alice@example.com')
        expect(E.isRight(result)).toBe(true)
      })

      it('無効なメールアドレスはエラーを返す', () => {
        const result = validateEmail('invalid')
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('email format')
        }
      })
    })

    describe('validateUser', () => {
      it('全て有効な場合は right を返す', () => {
        const result = validateUser('alice', 'alice@example.com', 25)
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual({
            username: 'alice',
            email: 'alice@example.com',
            age: 25,
          })
        }
      })

      it('最初のエラーで失敗する', () => {
        const result = validateUser('', 'alice@example.com', 25)
        expect(E.isLeft(result)).toBe(true)
        if (E.isLeft(result)) {
          expect(result.left).toContain('empty')
        }
      })
    })
  })

  describe('7.9 Either と ReadonlyArray の組み合わせ', () => {
    describe('traverseArrayE', () => {
      it('全て成功すると right を返す', () => {
        const result = traverseArrayE(['1', '2', '3'], parseNumberE)
        expect(E.isRight(result)).toBe(true)
        if (E.isRight(result)) {
          expect(result.right).toEqual([1, 2, 3])
        }
      })

      it('一つでも失敗すると left を返す', () => {
        const result = traverseArrayE(['1', 'abc', '3'], parseNumberE)
        expect(E.isLeft(result)).toBe(true)
      })
    })

    describe('collectRights', () => {
      it('right の要素のみを収集する', () => {
        const result = collectRights(['1', 'abc', '3'], parseNumberE)
        expect(result).toEqual([1, 3])
      })
    })
  })

  describe('7.10 支払い方法の例（ADT）', () => {
    describe('describePayment', () => {
      it('CreditCard を説明する', () => {
        expect(describePayment(creditCard('1234', '12/25'))).toBe(
          'Credit card ending in 1234'
        )
      })

      it('BankTransfer を説明する', () => {
        expect(describePayment(bankTransfer('9876'))).toBe(
          'Bank transfer to account 9876'
        )
      })

      it('Cash を説明する', () => {
        expect(describePayment(cash())).toBe('Cash payment')
      })
    })
  })
})
