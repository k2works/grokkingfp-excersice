# frozen_string_literal: true

require 'spec_helper'
require 'ch04_functions_as_values'

RSpec.describe Ch04FunctionsAsValues do
  # ===========================================================================
  # 4.1 Higher-Order Functions: Passing Functions
  # ===========================================================================

  describe '.score' do
    it 'counts characters excluding a' do
      expect(described_class.score('java')).to eq(2)
    end

    it 'counts all characters when no a' do
      expect(described_class.score('rust')).to eq(4)
    end

    it 'excludes uppercase A as well' do
      expect(described_class.score('Ada')).to eq(1)
    end
  end

  describe '.sort_by_score' do
    it 'sorts words by score function' do
      words = %w[rust java]
      sorted = described_class.sort_by_score(words) { |w| described_class.score(w) }
      expect(sorted).to eq(%w[java rust])
    end
  end

  describe '.ranked_words' do
    it 'ranks words by score descending' do
      words = %w[ada haskell scala java rust]
      ranked = described_class.ranked_words(words) { |w| described_class.score(w) }
      expect(ranked).to eq(%w[haskell rust scala java ada])
    end
  end

  # ===========================================================================
  # 4.2 map - Transform Each Element
  # ===========================================================================

  describe '.lengths' do
    it 'returns length of each string' do
      expect(described_class.lengths(%w[scala rust ada])).to eq([5, 4, 3])
    end
  end

  describe '.double_all' do
    it 'doubles each number' do
      expect(described_class.double_all([1, 2, 3])).to eq([2, 4, 6])
    end
  end

  describe '.map_with' do
    it 'applies function to each element' do
      result = described_class.map_with([1, 2, 3]) { |x| x * x }
      expect(result).to eq([1, 4, 9])
    end
  end

  # ===========================================================================
  # 4.3 filter - Select Elements
  # ===========================================================================

  describe '.odds' do
    it 'filters odd numbers' do
      expect(described_class.odds([1, 2, 3, 4, 5])).to eq([1, 3, 5])
    end
  end

  describe '.evens' do
    it 'filters even numbers' do
      expect(described_class.evens([1, 2, 3, 4, 5])).to eq([2, 4])
    end
  end

  describe '.filter_with' do
    it 'filters by predicate' do
      result = described_class.filter_with([1, 2, 3, 4, 5]) { |x| x > 3 }
      expect(result).to eq([4, 5])
    end
  end

  describe '.larger_than' do
    it 'filters numbers larger than n' do
      expect(described_class.larger_than([5, 1, 2, 4, 0], 4)).to eq([5])
    end

    it 'filters with different threshold' do
      expect(described_class.larger_than([5, 1, 2, 4, 0], 1)).to eq([5, 2, 4])
    end
  end

  # ===========================================================================
  # 4.4 reduce/fold - Combine Elements
  # ===========================================================================

  describe '.sum' do
    it 'sums all numbers' do
      expect(described_class.sum([5, 1, 2, 4, 100])).to eq(112)
    end

    it 'returns 0 for empty list' do
      expect(described_class.sum([])).to eq(0)
    end
  end

  describe '.maximum' do
    it 'finds maximum value' do
      expect(described_class.maximum([5, 1, 2, 4, 15])).to eq(15)
    end

    it 'returns nil for empty list' do
      expect(described_class.maximum([])).to be_nil
    end
  end

  describe '.minimum' do
    it 'finds minimum value' do
      expect(described_class.minimum([5, 1, 2, 4, 15])).to eq(1)
    end

    it 'returns nil for empty list' do
      expect(described_class.minimum([])).to be_nil
    end
  end

  describe '.product' do
    it 'multiplies all numbers' do
      expect(described_class.product([1, 2, 3, 4])).to eq(24)
    end

    it 'returns 1 for empty list' do
      expect(described_class.product([])).to eq(1)
    end
  end

  describe '.fold_with' do
    it 'folds with custom function' do
      result = described_class.fold_with([1, 2, 3], 0) { |acc, x| acc + x }
      expect(result).to eq(6)
    end

    it 'can compute max with fold' do
      result = described_class.fold_with([5, 1, 8, 3], 0) { |max, x| x > max ? x : max }
      expect(result).to eq(8)
    end
  end

  # ===========================================================================
  # 4.5 Struct (similar to case class)
  # ===========================================================================

  describe 'ProgrammingLanguage' do
    let(:java) { described_class::ProgrammingLanguage.new(name: 'Java', year: 1995) }
    let(:scala) { described_class::ProgrammingLanguage.new(name: 'Scala', year: 2004) }

    it 'has name and year' do
      expect(java.name).to eq('Java')
      expect(java.year).to eq(1995)
    end

    it 'can check if young' do
      expect(scala.young?(2000)).to be true
      expect(java.young?(2000)).to be false
    end
  end

  describe '.language_names' do
    let(:languages) do
      [
        described_class::ProgrammingLanguage.new(name: 'Java', year: 1995),
        described_class::ProgrammingLanguage.new(name: 'Scala', year: 2004)
      ]
    end

    it 'returns names of languages' do
      expect(described_class.language_names(languages)).to eq(%w[Java Scala])
    end
  end

  describe '.young_languages' do
    let(:languages) do
      [
        described_class::ProgrammingLanguage.new(name: 'Java', year: 1995),
        described_class::ProgrammingLanguage.new(name: 'Scala', year: 2004),
        described_class::ProgrammingLanguage.new(name: 'Kotlin', year: 2011)
      ]
    end

    it 'filters languages after given year' do
      young = described_class.young_languages(languages, 2000)
      expect(young.map(&:name)).to eq(%w[Scala Kotlin])
    end
  end

  describe '.sort_by_year' do
    let(:languages) do
      [
        described_class::ProgrammingLanguage.new(name: 'Scala', year: 2004),
        described_class::ProgrammingLanguage.new(name: 'Java', year: 1995)
      ]
    end

    it 'sorts by year' do
      sorted = described_class.sort_by_year(languages)
      expect(sorted.map(&:name)).to eq(%w[Java Scala])
    end
  end

  # ===========================================================================
  # 4.6 Returning Functions
  # ===========================================================================

  describe '.larger_than_fn' do
    it 'returns a function that checks if larger than n' do
      larger_than_4 = described_class.larger_than_fn(4)
      expect(larger_than_4.call(5)).to be true
      expect(larger_than_4.call(3)).to be false
    end

    it 'can be used with filter' do
      result = [5, 1, 2, 4, 0].select(&described_class.larger_than_fn(4))
      expect(result).to eq([5])
    end
  end

  describe '.divisible_by' do
    it 'returns a function that checks divisibility' do
      div_by_3 = described_class.divisible_by(3)
      expect(div_by_3.call(9)).to be true
      expect(div_by_3.call(10)).to be false
    end
  end

  describe '.contains_fn' do
    it 'returns a function that checks string containment' do
      has_ll = described_class.contains_fn('ll')
      expect(has_ll.call('hello')).to be true
      expect(has_ll.call('world')).to be false
    end
  end

  describe '.multiply_by' do
    it 'returns a function that multiplies by n' do
      triple = described_class.multiply_by(3)
      expect(triple.call(4)).to eq(12)
    end
  end

  # ===========================================================================
  # 4.7 Currying
  # ===========================================================================

  describe '.larger_than_curried' do
    it 'returns curried larger than function' do
      larger_than = described_class.larger_than_curried
      larger_than_5 = larger_than.call(5)
      expect(larger_than_5.call(10)).to be true
      expect(larger_than_5.call(3)).to be false
    end
  end

  describe '.add_curried' do
    it 'returns curried addition function' do
      add = described_class.add_curried
      add_5 = add.call(5)
      expect(add_5.call(3)).to eq(8)
    end
  end

  describe '.contains_curried' do
    it 'returns curried contains function' do
      contains = described_class.contains_curried
      has_a = contains.call('a')
      expect(has_a.call('apple')).to be true
      expect(has_a.call('hello')).to be false
    end
  end

  # ===========================================================================
  # 4.8 Word Scoring Example
  # ===========================================================================

  describe '.bonus' do
    it 'returns 5 if word contains c' do
      expect(described_class.bonus('scala')).to eq(5)
    end

    it 'returns 0 if word does not contain c' do
      expect(described_class.bonus('java')).to eq(0)
    end
  end

  describe '.penalty' do
    it 'returns 7 if word contains s' do
      expect(described_class.penalty('rust')).to eq(7)
    end

    it 'returns 0 if word does not contain s' do
      expect(described_class.penalty('java')).to eq(0)
    end
  end

  describe '.score_with_bonus' do
    it 'adds bonus for words with c' do
      expect(described_class.score_with_bonus('scala')).to eq(8) # 3 (score) + 5 (bonus)
    end
  end

  describe '.score_with_bonus_and_penalty' do
    it 'applies both bonus and penalty' do
      expect(described_class.score_with_bonus_and_penalty('scala')).to eq(1) # 3 + 5 - 7
    end

    it 'applies only penalty when no c' do
      expect(described_class.score_with_bonus_and_penalty('rust')).to eq(-3) # 4 + 0 - 7
    end
  end

  describe '.ranked_words_with_score' do
    it 'ranks words by custom score' do
      words = %w[ada haskell scala java rust]
      ranked = described_class.ranked_words_with_score(words) { |w| described_class.score_with_bonus(w) }
      expect(ranked.first).to eq('scala')
    end
  end

  # ===========================================================================
  # 4.9 Function Composition
  # ===========================================================================

  describe '.compose' do
    it 'composes two functions (f after g)' do
      add_one = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      composed = described_class.compose(add_one, double)
      expect(composed.call(3)).to eq(7) # double(3) = 6, add_one(6) = 7
    end
  end

  describe '.pipe' do
    it 'applies functions left to right' do
      add_one = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      result = described_class.pipe(3, add_one, double)
      expect(result).to eq(8) # add_one(3) = 4, double(4) = 8
    end

    it 'returns value when no functions' do
      expect(described_class.pipe(5)).to eq(5)
    end
  end

  describe '.and_then' do
    it 'composes functions left to right' do
      add_one = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      composed = described_class.and_then(add_one, double)
      expect(composed.call(3)).to eq(8) # add_one(3) = 4, double(4) = 8
    end
  end

  # ===========================================================================
  # 4.10 Practical Examples
  # ===========================================================================

  describe '.count_where' do
    it 'counts elements matching predicate' do
      expect(described_class.count_where([1, 2, 3, 4, 5]) { |x| x > 3 }).to eq(2)
    end
  end

  describe '.find_first' do
    it 'finds first matching element' do
      expect(described_class.find_first([1, 2, 3, 4, 5]) { |x| x > 3 }).to eq(4)
    end

    it 'returns nil if not found' do
      expect(described_class.find_first([1, 2, 3]) { |x| x > 5 }).to be_nil
    end
  end

  describe '.all_match?' do
    it 'returns true if all match' do
      expect(described_class.all_match?([2, 4, 6], &:even?)).to be true
    end

    it 'returns false if any does not match' do
      expect(described_class.all_match?([2, 3, 6], &:even?)).to be false
    end
  end

  describe '.any_match?' do
    it 'returns true if any matches' do
      expect(described_class.any_match?([1, 2, 3], &:even?)).to be true
    end

    it 'returns false if none matches' do
      expect(described_class.any_match?([1, 3, 5], &:even?)).to be false
    end
  end

  describe '.none_match?' do
    it 'returns true if none matches' do
      expect(described_class.none_match?([1, 3, 5], &:even?)).to be true
    end

    it 'returns false if any matches' do
      expect(described_class.none_match?([1, 2, 3], &:even?)).to be false
    end
  end

  describe '.partition_by' do
    it 'partitions by predicate' do
      evens, odds = described_class.partition_by([1, 2, 3, 4, 5], &:even?)
      expect(evens).to eq([2, 4])
      expect(odds).to eq([1, 3, 5])
    end
  end

  describe '.group_by_fn' do
    it 'groups by key function' do
      result = described_class.group_by_fn(%w[one two three four]) { |s| s.length }
      expect(result[3]).to eq(%w[one two])
      expect(result[5]).to eq(%w[three])
      expect(result[4]).to eq(%w[four])
    end
  end

  describe '.take_while_fn' do
    it 'takes while predicate is true' do
      result = described_class.take_while_fn([1, 2, 3, 4, 1, 2]) { |x| x < 4 }
      expect(result).to eq([1, 2, 3])
    end
  end

  describe '.drop_while_fn' do
    it 'drops while predicate is true' do
      result = described_class.drop_while_fn([1, 2, 3, 4, 1, 2]) { |x| x < 4 }
      expect(result).to eq([4, 1, 2])
    end
  end
end
