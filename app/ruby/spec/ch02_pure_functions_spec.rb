# frozen_string_literal: true

require 'spec_helper'
require 'ch02_pure_functions'

RSpec.describe Ch02PureFunctions do
  # ===========================================================================
  # 2.1 Basic pure functions
  # ===========================================================================

  describe '.add' do
    it 'adds two positive numbers' do
      expect(described_class.add(2, 3)).to eq(5)
    end

    it 'adds negative numbers' do
      expect(described_class.add(-2, -3)).to eq(-5)
    end

    it 'is commutative' do
      expect(described_class.add(3, 5)).to eq(described_class.add(5, 3))
    end

    it 'has identity element 0' do
      expect(described_class.add(5, 0)).to eq(5)
    end
  end

  describe '.string_length' do
    it 'returns length of a string' do
      expect(described_class.string_length('hello')).to eq(5)
    end

    it 'returns 0 for empty string' do
      expect(described_class.string_length('')).to eq(0)
    end
  end

  describe '.word_score' do
    it 'returns the length of the word' do
      expect(described_class.word_score('Ruby')).to eq(4)
    end

    it 'is deterministic (same input = same output)' do
      score1 = described_class.word_score('Scala')
      score2 = described_class.word_score('Scala')
      expect(score1).to eq(score2)
    end
  end

  # ===========================================================================
  # 2.2 Bonus score
  # ===========================================================================

  describe '.bonus_score' do
    it 'adds 50 for score > 100' do
      expect(described_class.bonus_score(150)).to eq(200)
    end

    it 'adds 20 for score > 50 and <= 100' do
      expect(described_class.bonus_score(75)).to eq(95)
    end

    it 'returns same score for score <= 50' do
      expect(described_class.bonus_score(30)).to eq(30)
    end

    it 'handles boundary at 100' do
      expect(described_class.bonus_score(100)).to eq(120)
    end

    it 'handles boundary at 101' do
      expect(described_class.bonus_score(101)).to eq(151)
    end

    it 'handles boundary at 50' do
      expect(described_class.bonus_score(50)).to eq(50)
    end

    it 'handles boundary at 51' do
      expect(described_class.bonus_score(51)).to eq(71)
    end
  end

  # ===========================================================================
  # 2.3 Shopping cart discount
  # ===========================================================================

  describe '.get_discount_percentage' do
    it 'returns 5% when cart contains Book' do
      expect(described_class.get_discount_percentage(%w[Apple Book Orange])).to eq(5)
    end

    it 'returns 0% when cart does not contain Book' do
      expect(described_class.get_discount_percentage(%w[Apple Orange])).to eq(0)
    end

    it 'returns 0% for empty cart' do
      expect(described_class.get_discount_percentage([])).to eq(0)
    end
  end

  describe '.calculate_discount' do
    it 'calculates discount correctly' do
      expect(described_class.calculate_discount(100, 5)).to eq(5.0)
    end

    it 'returns 0 for 0 percentage' do
      expect(described_class.calculate_discount(100, 0)).to eq(0.0)
    end

    it 'handles decimal totals' do
      expect(described_class.calculate_discount(99.99, 10)).to be_within(0.01).of(9.999)
    end
  end

  describe '.calculate_final_price' do
    it 'applies discount when cart has Book' do
      expect(described_class.calculate_final_price(100, %w[Book])).to eq(95.0)
    end

    it 'returns full price when cart has no Book' do
      expect(described_class.calculate_final_price(100, %w[Apple])).to eq(100.0)
    end
  end

  # ===========================================================================
  # 2.4 Tip calculation
  # ===========================================================================

  describe '.get_tip_percentage' do
    it 'returns 20% for groups larger than 5' do
      expect(described_class.get_tip_percentage(%w[a b c d e f])).to eq(20)
    end

    it 'returns 10% for groups of 1-5' do
      expect(described_class.get_tip_percentage(%w[a b c])).to eq(10)
    end

    it 'returns 0% for empty group' do
      expect(described_class.get_tip_percentage([])).to eq(0)
    end

    it 'returns 10% for group of 1' do
      expect(described_class.get_tip_percentage(%w[a])).to eq(10)
    end

    it 'returns 10% for group of 5' do
      expect(described_class.get_tip_percentage(%w[a b c d e])).to eq(10)
    end

    it 'returns 20% for group of 6' do
      expect(described_class.get_tip_percentage(%w[a b c d e f])).to eq(20)
    end
  end

  describe '.calculate_tip' do
    it 'calculates tip for large group' do
      expect(described_class.calculate_tip(100, %w[a b c d e f])).to eq(20.0)
    end

    it 'calculates tip for small group' do
      expect(described_class.calculate_tip(100, %w[a b])).to eq(10.0)
    end

    it 'returns 0 for empty group' do
      expect(described_class.calculate_tip(100, [])).to eq(0.0)
    end
  end

  # ===========================================================================
  # 2.5 Referential transparency
  # ===========================================================================

  describe '.referential_transparency_example' do
    it 'demonstrates referential transparency' do
      expect(described_class.referential_transparency_example).to be true
    end
  end

  # ===========================================================================
  # 2.6 String operations
  # ===========================================================================

  describe '.append_exclamation' do
    it 'appends exclamation mark' do
      expect(described_class.append_exclamation('Hello')).to eq('Hello!')
    end

    it 'handles empty string' do
      expect(described_class.append_exclamation('')).to eq('!')
    end
  end

  describe '.count_vowels' do
    it 'counts vowels in a string' do
      expect(described_class.count_vowels('hello')).to eq(2)
    end

    it 'handles uppercase' do
      expect(described_class.count_vowels('HELLO')).to eq(2)
    end

    it 'returns 0 for no vowels' do
      expect(described_class.count_vowels('xyz')).to eq(0)
    end

    it 'handles all vowels' do
      expect(described_class.count_vowels('aeiou')).to eq(5)
    end
  end

  # ===========================================================================
  # 2.7 List operations
  # ===========================================================================

  describe '.double_all' do
    it 'doubles all numbers' do
      expect(described_class.double_all([1, 2, 3])).to eq([2, 4, 6])
    end

    it 'handles empty array' do
      expect(described_class.double_all([])).to eq([])
    end

    it 'handles negative numbers' do
      expect(described_class.double_all([-1, 0, 1])).to eq([-2, 0, 2])
    end
  end

  describe '.filter_positive' do
    it 'filters positive numbers' do
      expect(described_class.filter_positive([-2, -1, 0, 1, 2])).to eq([1, 2])
    end

    it 'handles empty array' do
      expect(described_class.filter_positive([])).to eq([])
    end

    it 'handles all positive' do
      expect(described_class.filter_positive([1, 2, 3])).to eq([1, 2, 3])
    end

    it 'handles all negative' do
      expect(described_class.filter_positive([-1, -2, -3])).to eq([])
    end
  end

  describe '.find_longest' do
    it 'finds the longest string' do
      expect(described_class.find_longest(%w[a bb ccc])).to eq('ccc')
    end

    it 'returns nil for empty array' do
      expect(described_class.find_longest([])).to be_nil
    end

    it 'returns first when equal length' do
      result = described_class.find_longest(%w[ab cd])
      expect(%w[ab cd]).to include(result)
    end
  end

  describe '.all_positive?' do
    it 'returns true when all positive' do
      expect(described_class.all_positive?([1, 2, 3])).to be true
    end

    it 'returns false when any non-positive' do
      expect(described_class.all_positive?([1, 0, 3])).to be false
    end

    it 'returns true for empty array' do
      expect(described_class.all_positive?([])).to be true
    end
  end

  describe '.any_negative?' do
    it 'returns true when any negative' do
      expect(described_class.any_negative?([1, -2, 3])).to be true
    end

    it 'returns false when none negative' do
      expect(described_class.any_negative?([1, 2, 3])).to be false
    end

    it 'returns false for empty array' do
      expect(described_class.any_negative?([])).to be false
    end
  end

  describe '.sum_list' do
    it 'sums all numbers' do
      expect(described_class.sum_list([1, 2, 3, 4, 5])).to eq(15)
    end

    it 'returns 0 for empty array' do
      expect(described_class.sum_list([])).to eq(0)
    end

    it 'handles negative numbers' do
      expect(described_class.sum_list([-1, 1, -2, 2])).to eq(0)
    end
  end

  describe '.average' do
    it 'calculates average' do
      expect(described_class.average([1, 2, 3, 4, 5])).to eq(3.0)
    end

    it 'returns 0.0 for empty array' do
      expect(described_class.average([])).to eq(0.0)
    end

    it 'handles single element' do
      expect(described_class.average([5])).to eq(5.0)
    end
  end

  # ===========================================================================
  # 2.8 Word score without 'a'
  # ===========================================================================

  describe '.word_score_no_a' do
    it 'excludes letter a from score' do
      expect(described_class.word_score_no_a('Scala')).to eq(3)
    end

    it 'excludes uppercase A as well' do
      expect(described_class.word_score_no_a('JAVA')).to eq(2)
    end

    it 'returns full length when no a' do
      expect(described_class.word_score_no_a('Ruby')).to eq(4)
    end

    it 'returns 0 for only a characters' do
      expect(described_class.word_score_no_a('aaa')).to eq(0)
    end
  end

  # ===========================================================================
  # 2.9 Higher-order functions
  # ===========================================================================

  describe '.apply_twice' do
    it 'applies a function twice' do
      increment = ->(x) { x + 1 }
      expect(described_class.apply_twice(increment, 5)).to eq(7)
    end
  end

  describe '.compose' do
    it 'composes two functions (f after g)' do
      add_one = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      composed = described_class.compose(add_one, double)
      expect(composed.call(3)).to eq(7) # double(3) = 6, add_one(6) = 7
    end
  end

  describe '.compose_with_flow' do
    it 'composes functions left to right' do
      add_one = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      composed = described_class.compose_with_flow(add_one, double)
      expect(composed.call(3)).to eq(8) # add_one(3) = 4, double(4) = 8
    end
  end

  # ===========================================================================
  # 2.10 Timestamp formatting
  # ===========================================================================

  describe '.format_timestamp' do
    it 'formats timestamp with default format' do
      # 2024-01-01 00:00:00 UTC
      timestamp = 1_704_067_200
      expect(described_class.format_timestamp(timestamp)).to eq('2024-01-01 00:00:00')
    end

    it 'formats timestamp with custom format' do
      timestamp = 1_704_067_200
      expect(described_class.format_timestamp(timestamp, '%Y-%m-%d')).to eq('2024-01-01')
    end

    it 'is deterministic (same input = same output)' do
      timestamp = 1_704_067_200
      result1 = described_class.format_timestamp(timestamp)
      result2 = described_class.format_timestamp(timestamp)
      expect(result1).to eq(result2)
    end
  end

  # ===========================================================================
  # 2.11 Immutability helpers
  # ===========================================================================

  describe '.deep_freeze' do
    it 'freezes a string' do
      str = +'hello'
      described_class.deep_freeze(str)
      expect(str).to be_frozen
    end

    it 'freezes an array and its elements' do
      arr = [+'a', +'b']
      described_class.deep_freeze(arr)
      expect(arr).to be_frozen
      expect(arr[0]).to be_frozen
    end

    it 'freezes a hash and its values' do
      hash = { a: +'value' }
      described_class.deep_freeze(hash)
      expect(hash).to be_frozen
      expect(hash[:a]).to be_frozen
    end
  end

  describe '.immutable_array' do
    it 'creates a frozen array' do
      arr = described_class.immutable_array(1, 2, 3)
      expect(arr).to be_frozen
      expect(arr).to eq([1, 2, 3])
    end
  end

  describe '.immutable_hash' do
    it 'creates a frozen hash' do
      hash = described_class.immutable_hash(a: 1, b: 2)
      expect(hash).to be_frozen
      expect(hash).to eq({ a: 1, b: 2 })
    end
  end

  # ===========================================================================
  # Pure function properties
  # ===========================================================================

  describe 'pure function properties' do
    it 'word_score is referentially transparent' do
      # The expression word_score("hello") can be replaced with 5
      expect(described_class.word_score('hello')).to eq(5)
      expect(described_class.word_score('hello') + described_class.word_score('world'))
        .to eq(5 + 5)
    end

    it 'add is associative' do
      a, b, c = 1, 2, 3
      expect(described_class.add(described_class.add(a, b), c))
        .to eq(described_class.add(a, described_class.add(b, c)))
    end

    it 'double_all preserves length' do
      arr = [1, 2, 3, 4, 5]
      expect(described_class.double_all(arr).length).to eq(arr.length)
    end

    it 'filter_positive returns subset' do
      arr = [-2, -1, 0, 1, 2]
      result = described_class.filter_positive(arr)
      expect(result.all? { |n| arr.include?(n) }).to be true
    end
  end
end
