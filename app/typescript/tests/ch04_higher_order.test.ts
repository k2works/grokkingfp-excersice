/**
 * 第4章: 関数を値として扱うのテスト
 */

import { describe, it, expect } from 'vitest'
import {
  score,
  rankedWords,
  sortByLength,
  sortByLengthDesc,
  numberOfS,
  sortByNumberOfS,
  getLengths,
  getScores,
  doubleAll,
  negateAll,
  filterShortWords,
  filterOdd,
  filterLargerThan,
  highScoringWords,
  sumAll,
  totalLength,
  totalSCount,
  findMax,
  cumulativeScore,
  largerThan,
  divisibleBy,
  shorterThan,
  containsSMoreThan,
  bonus,
  penalty,
  wordScoreWithBonus,
  wordScoreWithBonusAndPenalty,
  highScoringWordsWithThreshold,
  myMap,
  myFilter,
  myReduce,
  createLanguage,
  sortByYear,
  getNames,
  filterYoungLanguages,
} from '../src/ch04_higher_order.js'

describe('第4章: 関数を値として扱う', () => {
  describe('4.1 関数を引数として渡す', () => {
    describe('score', () => {
      it('aを除いた文字数を返す', () => {
        expect(score('java')).toBe(2)
        expect(score('rust')).toBe(4)
        expect(score('scala')).toBe(3)
      })
    })

    describe('rankedWords', () => {
      it('スコア順（降順）にソートする', () => {
        expect(rankedWords(['rust', 'java'], score)).toEqual(['rust', 'java'])
        expect(rankedWords(['ada', 'haskell', 'scala'], score)).toEqual([
          'haskell',
          'scala',
          'ada',
        ])
      })
    })

    describe('sortByLength', () => {
      it('長さ順（昇順）にソートする', () => {
        expect(sortByLength(['scala', 'rust', 'ada'])).toEqual(['ada', 'rust', 'scala'])
      })
    })

    describe('sortByLengthDesc', () => {
      it('長さ順（降順）にソートする', () => {
        expect(sortByLengthDesc(['scala', 'rust', 'ada'])).toEqual(['scala', 'rust', 'ada'])
      })
    })

    describe('numberOfS', () => {
      it('sの数を数える', () => {
        expect(numberOfS('rust')).toBe(1)
        expect(numberOfS('haskell')).toBe(1)
        expect(numberOfS('ada')).toBe(0)
        expect(numberOfS('mississippi')).toBe(4)
      })
    })

    describe('sortByNumberOfS', () => {
      it('sの数順にソートする', () => {
        expect(sortByNumberOfS(['rust', 'ada'])).toEqual(['ada', 'rust'])
      })
    })
  })

  describe('4.2 map', () => {
    describe('getLengths', () => {
      it('各文字列の長さを取得する', () => {
        expect(getLengths(['scala', 'rust', 'ada'])).toEqual([5, 4, 3])
      })
    })

    describe('getScores', () => {
      it('各単語のスコアを取得する', () => {
        expect(getScores(['rust', 'java'], score)).toEqual([4, 2])
      })
    })

    describe('doubleAll', () => {
      it('全ての数値を2倍にする', () => {
        expect(doubleAll([5, 1, 2, 4, 0])).toEqual([10, 2, 4, 8, 0])
      })
    })

    describe('negateAll', () => {
      it('全ての数値を負にする', () => {
        expect(negateAll([5, 1, 2, 4, 0])).toEqual([-5, -1, -2, -4, -0])
      })
    })
  })

  describe('4.3 filter', () => {
    describe('filterShortWords', () => {
      it('指定した長さ以下の単語をフィルタする', () => {
        expect(filterShortWords(['scala', 'rust', 'ada'], 4)).toEqual(['rust', 'ada'])
      })
    })

    describe('filterOdd', () => {
      it('奇数のみをフィルタする', () => {
        expect(filterOdd([5, 1, 2, 4, 0])).toEqual([5, 1])
      })
    })

    describe('filterLargerThan', () => {
      it('閾値より大きい数値をフィルタする', () => {
        expect(filterLargerThan([5, 1, 2, 4, 0], 4)).toEqual([5])
      })
    })

    describe('highScoringWords', () => {
      it('指定したスコアより高い単語をフィルタする', () => {
        expect(highScoringWords(['rust', 'java'], score, 3)).toEqual(['rust'])
      })
    })
  })

  describe('4.4 reduce', () => {
    describe('sumAll', () => {
      it('数値の合計を計算する', () => {
        expect(sumAll([5, 1, 2, 4, 100])).toBe(112)
        expect(sumAll([])).toBe(0)
      })
    })

    describe('totalLength', () => {
      it('全ての文字列の長さの合計を計算する', () => {
        expect(totalLength(['scala', 'rust', 'ada'])).toBe(12)
      })
    })

    describe('totalSCount', () => {
      it('全ての文字列中のsの数の合計を計算する', () => {
        expect(totalSCount(['scala', 'haskell', 'rust', 'ada'])).toBe(3)
      })
    })

    describe('findMax', () => {
      it('リスト内の最大値を見つける', () => {
        expect(findMax([5, 1, 2, 4, 15])).toBe(15)
      })

      it('空リストの場合はundefinedを返す', () => {
        expect(findMax([])).toBeUndefined()
      })
    })

    describe('cumulativeScore', () => {
      it('全ての単語のスコアの合計を計算する', () => {
        expect(cumulativeScore(['rust', 'java'], score)).toBe(6)
      })
    })
  })

  describe('4.5 関数を返す関数', () => {
    describe('largerThan', () => {
      it('nより大きいかを判定する関数を返す', () => {
        expect(largerThan(4)(5)).toBe(true)
        expect(largerThan(4)(3)).toBe(false)
        expect([5, 1, 2, 4, 0].filter(largerThan(4))).toEqual([5])
      })
    })

    describe('divisibleBy', () => {
      it('nで割り切れるかを判定する関数を返す', () => {
        expect(divisibleBy(5)(15)).toBe(true)
        expect(divisibleBy(5)(7)).toBe(false)
        expect([5, 1, 2, 4, 15].filter(divisibleBy(5))).toEqual([5, 15])
      })
    })

    describe('shorterThan', () => {
      it('長さがnより短いかを判定する関数を返す', () => {
        expect(shorterThan(4)('ada')).toBe(true)
        expect(shorterThan(4)('scala')).toBe(false)
        expect(['scala', 'ada'].filter(shorterThan(4))).toEqual(['ada'])
      })
    })

    describe('containsSMoreThan', () => {
      it('sがcountより多く含まれるかを判定する関数を返す', () => {
        expect(containsSMoreThan(0)('rust')).toBe(true)
        expect(containsSMoreThan(0)('ada')).toBe(false)
        expect(['rust', 'ada'].filter(containsSMoreThan(0))).toEqual(['rust'])
      })
    })
  })

  describe('4.6 Word Scoring', () => {
    describe('bonus', () => {
      it('cを含む単語にボーナスを与える', () => {
        expect(bonus('scala')).toBe(5)
        expect(bonus('java')).toBe(0)
      })
    })

    describe('penalty', () => {
      it('sを含む単語にペナルティを与える', () => {
        expect(penalty('rust')).toBe(7)
        expect(penalty('java')).toBe(0)
      })
    })

    describe('wordScoreWithBonus', () => {
      it('スコア + ボーナス を計算する', () => {
        expect(wordScoreWithBonus('scala')).toBe(8) // 3 + 5
        expect(wordScoreWithBonus('java')).toBe(2) // 2 + 0
      })
    })

    describe('wordScoreWithBonusAndPenalty', () => {
      it('スコア + ボーナス - ペナルティ を計算する', () => {
        expect(wordScoreWithBonusAndPenalty('java')).toBe(2) // 2 + 0 - 0
        expect(wordScoreWithBonusAndPenalty('scala')).toBe(1) // 3 + 5 - 7
        expect(wordScoreWithBonusAndPenalty('rust')).toBe(-3) // 4 + 0 - 7
      })
    })

    describe('highScoringWordsWithThreshold', () => {
      it('カリー化された関数で高スコアの単語をフィルタする', () => {
        const words = ['ada', 'haskell', 'scala', 'java', 'rust']
        const scorer = highScoringWordsWithThreshold(wordScoreWithBonusAndPenalty)
        expect(scorer(1)(words)).toEqual(['java'])
        expect(scorer(0)(words)).toEqual(['ada', 'scala', 'java'])
      })
    })
  })

  describe('4.7 汎用的な高階関数', () => {
    describe('myMap', () => {
      it('map関数を実装する', () => {
        expect(myMap((x: number) => x * 2, [1, 2, 3])).toEqual([2, 4, 6])
        expect(myMap((s: string) => s.length, ['a', 'bb', 'ccc'])).toEqual([1, 2, 3])
      })
    })

    describe('myFilter', () => {
      it('filter関数を実装する', () => {
        expect(myFilter((x: number) => x > 0, [-1, 0, 1, 2])).toEqual([1, 2])
        expect(myFilter((s: string) => s.length > 2, ['a', 'bb', 'ccc'])).toEqual(['ccc'])
      })
    })

    describe('myReduce', () => {
      it('reduce関数を実装する', () => {
        expect(myReduce((acc: number, x: number) => acc + x, [1, 2, 3], 0)).toBe(6)
        expect(myReduce((acc: number, s: string) => acc + s.length, ['a', 'bb'], 0)).toBe(3)
      })
    })
  })

  describe('4.8 プログラミング言語', () => {
    const java = createLanguage('Java', 1995)
    const scala = createLanguage('Scala', 2004)
    const languages = [scala, java]

    describe('sortByYear', () => {
      it('年でソートする（昇順）', () => {
        const sorted = sortByYear(languages)
        expect(sorted[0].name).toBe('Java')
        expect(sorted[1].name).toBe('Scala')
      })
    })

    describe('getNames', () => {
      it('言語名を取得する', () => {
        expect(getNames(languages)).toEqual(['Scala', 'Java'])
      })
    })

    describe('filterYoungLanguages', () => {
      it('指定した年より後に作られた言語をフィルタする', () => {
        const young = filterYoungLanguages(languages, 2000)
        expect(young).toHaveLength(1)
        expect(young[0].name).toBe('Scala')
      })
    })
  })
})
