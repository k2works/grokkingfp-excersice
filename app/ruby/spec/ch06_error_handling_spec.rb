# frozen_string_literal: true

require 'spec_helper'
require 'ch06_error_handling'

RSpec.describe Ch06ErrorHandling do
  # ===========================================================================
  # 6.1 Basic Maybe Operations
  # ===========================================================================

  describe '.maybe' do
    it 'returns Some for truthy value' do
      result = described_class.maybe(5)
      expect(result).to be_some
      expect(result.value!).to eq(5)
    end

    it 'returns None for nil' do
      result = described_class.maybe(nil)
      expect(result).to be_none
    end
  end

  describe '.some' do
    it 'creates a Some value' do
      result = described_class.some(42)
      expect(result).to be_some
      expect(result.value!).to eq(42)
    end
  end

  describe '.none' do
    it 'creates a None value' do
      result = described_class.none
      expect(result).to be_none
    end
  end

  # ===========================================================================
  # 6.2 Safe Operations
  # ===========================================================================

  describe '.safe_divide' do
    it 'returns Some for valid division' do
      result = described_class.safe_divide(10, 2)
      expect(result).to be_some
      expect(result.value!).to eq(5)
    end

    it 'returns None for division by zero' do
      result = described_class.safe_divide(10, 0)
      expect(result).to be_none
    end
  end

  describe '.safe_parse_int' do
    it 'returns Some for valid integer string' do
      result = described_class.safe_parse_int('42')
      expect(result).to be_some
      expect(result.value!).to eq(42)
    end

    it 'returns Some for negative integer' do
      result = described_class.safe_parse_int('-5')
      expect(result).to be_some
      expect(result.value!).to eq(-5)
    end

    it 'returns None for invalid string' do
      result = described_class.safe_parse_int('abc')
      expect(result).to be_none
    end

    it 'returns None for empty string' do
      result = described_class.safe_parse_int('')
      expect(result).to be_none
    end
  end

  describe '.safe_get' do
    let(:array) { %w[a b c] }

    it 'returns Some for valid index' do
      result = described_class.safe_get(array, 1)
      expect(result).to be_some
      expect(result.value!).to eq('b')
    end

    it 'returns None for negative index' do
      result = described_class.safe_get(array, -1)
      expect(result).to be_none
    end

    it 'returns None for index out of bounds' do
      result = described_class.safe_get(array, 10)
      expect(result).to be_none
    end
  end

  describe '.safe_first and .safe_last' do
    it 'returns Some for non-empty array' do
      expect(described_class.safe_first([1, 2, 3])).to be_some
      expect(described_class.safe_first([1, 2, 3]).value!).to eq(1)
      expect(described_class.safe_last([1, 2, 3]).value!).to eq(3)
    end

    it 'returns None for empty array' do
      expect(described_class.safe_first([])).to be_none
      expect(described_class.safe_last([])).to be_none
    end
  end

  # ===========================================================================
  # 6.3 TV Show Parsing Example
  # ===========================================================================

  describe '.extract_name' do
    it 'extracts name from valid show string' do
      result = described_class.extract_name('Breaking Bad (2008-2013)')
      expect(result).to be_some
      expect(result.value!).to eq('Breaking Bad')
    end

    it 'returns None for string without parenthesis' do
      result = described_class.extract_name('Breaking Bad')
      expect(result).to be_none
    end

    it 'returns None for string starting with parenthesis' do
      result = described_class.extract_name('(2008-2013)')
      expect(result).to be_none
    end
  end

  describe '.extract_year_start' do
    it 'extracts start year from valid show string' do
      result = described_class.extract_year_start('Breaking Bad (2008-2013)')
      expect(result).to be_some
      expect(result.value!).to eq(2008)
    end

    it 'returns None for string without dash' do
      result = described_class.extract_year_start('Chernobyl (2019)')
      expect(result).to be_none
    end

    it 'returns None for invalid year' do
      result = described_class.extract_year_start('Show (abc-2013)')
      expect(result).to be_none
    end
  end

  describe '.extract_year_end' do
    it 'extracts end year from valid show string' do
      result = described_class.extract_year_end('Breaking Bad (2008-2013)')
      expect(result).to be_some
      expect(result.value!).to eq(2013)
    end

    it 'returns None for string without dash' do
      result = described_class.extract_year_end('Chernobyl (2019)')
      expect(result).to be_none
    end
  end

  describe '.extract_single_year' do
    it 'extracts single year from valid show string' do
      result = described_class.extract_single_year('Chernobyl (2019)')
      expect(result).to be_some
      expect(result.value!).to eq(2019)
    end

    it 'returns None for string with dash' do
      result = described_class.extract_single_year('Breaking Bad (2008-2013)')
      expect(result).to be_none
    end
  end

  describe '.parse_show' do
    it 'parses multi-year show' do
      result = described_class.parse_show('Breaking Bad (2008-2013)')
      expect(result).to be_some
      show = result.value!
      expect(show.title).to eq('Breaking Bad')
      expect(show.start_year).to eq(2008)
      expect(show.end_year).to eq(2013)
    end

    it 'parses single-year show' do
      result = described_class.parse_show('Chernobyl (2019)')
      expect(result).to be_some
      show = result.value!
      expect(show.title).to eq('Chernobyl')
      expect(show.start_year).to eq(2019)
      expect(show.end_year).to eq(2019)
    end

    it 'returns None for invalid format' do
      expect(described_class.parse_show('Breaking Bad')).to be_none
      expect(described_class.parse_show('(2008-2013)')).to be_none
      expect(described_class.parse_show('Show (abc-def)')).to be_none
    end
  end

  # ===========================================================================
  # 6.4 Maybe Methods
  # ===========================================================================

  describe '.maybe_map' do
    it 'transforms Some value' do
      result = described_class.maybe_map(described_class.some(5)) { |x| x * 2 }
      expect(result).to be_some
      expect(result.value!).to eq(10)
    end

    it 'returns None for None' do
      result = described_class.maybe_map(described_class.none) { |x| x * 2 }
      expect(result).to be_none
    end
  end

  describe '.get_or_else' do
    it 'returns value for Some' do
      result = described_class.get_or_else(described_class.some(5), 0)
      expect(result).to eq(5)
    end

    it 'returns default for None' do
      result = described_class.get_or_else(described_class.none, 0)
      expect(result).to eq(0)
    end
  end

  describe '.maybe_filter' do
    it 'keeps Some if predicate is true' do
      result = described_class.maybe_filter(described_class.some(5)) { |x| x > 3 }
      expect(result).to be_some
      expect(result.value!).to eq(5)
    end

    it 'returns None if predicate is false' do
      result = described_class.maybe_filter(described_class.some(5)) { |x| x > 10 }
      expect(result).to be_none
    end

    it 'returns None for None' do
      result = described_class.maybe_filter(described_class.none) { |x| x > 3 }
      expect(result).to be_none
    end
  end

  describe '.maybe_to_array' do
    it 'returns [value] for Some' do
      result = described_class.maybe_to_array(described_class.some(5))
      expect(result).to eq([5])
    end

    it 'returns [] for None' do
      result = described_class.maybe_to_array(described_class.none)
      expect(result).to eq([])
    end
  end

  # ===========================================================================
  # 6.5 Error Handling Strategies
  # ===========================================================================

  describe '.parse_shows_best_effort' do
    it 'parses valid shows and ignores invalid' do
      raw_shows = [
        'Breaking Bad (2008-2013)',
        'Invalid Show',
        'Mad Men (2007-2015)'
      ]
      result = described_class.parse_shows_best_effort(raw_shows)
      expect(result.size).to eq(2)
      expect(result.map(&:title)).to eq(['Breaking Bad', 'Mad Men'])
    end

    it 'returns empty for all invalid' do
      raw_shows = ['Invalid', 'Also Invalid']
      result = described_class.parse_shows_best_effort(raw_shows)
      expect(result).to eq([])
    end
  end

  describe '.parse_shows_all_or_nothing' do
    it 'returns Some with all shows if all valid' do
      raw_shows = [
        'Breaking Bad (2008-2013)',
        'Mad Men (2007-2015)'
      ]
      result = described_class.parse_shows_all_or_nothing(raw_shows)
      expect(result).to be_some
      expect(result.value!.size).to eq(2)
    end

    it 'returns None if any invalid' do
      raw_shows = [
        'Breaking Bad (2008-2013)',
        'Invalid Show'
      ]
      result = described_class.parse_shows_all_or_nothing(raw_shows)
      expect(result).to be_none
    end

    it 'returns Some empty list for empty input' do
      result = described_class.parse_shows_all_or_nothing([])
      expect(result).to be_some
      expect(result.value!).to eq([])
    end
  end

  # ===========================================================================
  # 6.6 Practical Examples
  # ===========================================================================

  describe '.find_user' do
    let(:users) do
      [
        described_class::User.new(id: 1, name: 'Alice', email: 'alice@example.com'),
        described_class::User.new(id: 2, name: 'Bob', email: nil)
      ]
    end

    it 'returns Some for existing user' do
      result = described_class.find_user(users, 1)
      expect(result).to be_some
      expect(result.value!.name).to eq('Alice')
    end

    it 'returns None for non-existing user' do
      result = described_class.find_user(users, 999)
      expect(result).to be_none
    end
  end

  describe '.get_email_domain' do
    it 'extracts domain from valid email' do
      result = described_class.get_email_domain('alice@example.com')
      expect(result).to be_some
      expect(result.value!).to eq('example.com')
    end

    it 'returns None for invalid email' do
      result = described_class.get_email_domain('invalid-email')
      expect(result).to be_none
    end
  end

  describe '.safe_average' do
    it 'calculates average for non-empty list' do
      result = described_class.safe_average([1, 2, 3, 4, 5])
      expect(result).to be_some
      expect(result.value!).to eq(3.0)
    end

    it 'returns None for empty list' do
      result = described_class.safe_average([])
      expect(result).to be_none
    end
  end

  # ===========================================================================
  # 6.7 Combining Multiple Maybes
  # ===========================================================================

  describe '.add_strings' do
    it 'adds two valid number strings' do
      result = described_class.add_strings('10', '20')
      expect(result).to be_some
      expect(result.value!).to eq(30)
    end

    it 'returns None if first is invalid' do
      expect(described_class.add_strings('abc', '20')).to be_none
    end

    it 'returns None if second is invalid' do
      expect(described_class.add_strings('10', 'xyz')).to be_none
    end
  end

  describe '.calculate_all' do
    it 'calculates sum of three valid strings' do
      result = described_class.calculate_all('1', '2', '3')
      expect(result).to be_some
      expect(result.value!).to eq(6)
    end

    it 'returns None if any is invalid' do
      expect(described_class.calculate_all('1', 'x', '3')).to be_none
    end
  end

  # ===========================================================================
  # 6.8 forall and exists equivalents
  # ===========================================================================

  describe '.maybe_all?' do
    it 'returns true for None' do
      expect(described_class.maybe_all?(described_class.none) { |x| x > 10 }).to be true
    end

    it 'returns predicate result for Some' do
      expect(described_class.maybe_all?(described_class.some(5)) { |x| x < 10 }).to be true
      expect(described_class.maybe_all?(described_class.some(5)) { |x| x > 10 }).to be false
    end
  end

  describe '.maybe_any?' do
    it 'returns false for None' do
      expect(described_class.maybe_any?(described_class.none) { |x| x > 10 }).to be false
    end

    it 'returns predicate result for Some' do
      expect(described_class.maybe_any?(described_class.some(5)) { |x| x < 10 }).to be true
      expect(described_class.maybe_any?(described_class.some(5)) { |x| x > 10 }).to be false
    end
  end

  describe '.maybe_contains?' do
    it 'returns true if Some contains value' do
      expect(described_class.maybe_contains?(described_class.some(5), 5)).to be true
    end

    it 'returns false if Some contains different value' do
      expect(described_class.maybe_contains?(described_class.some(5), 10)).to be false
    end

    it 'returns false for None' do
      expect(described_class.maybe_contains?(described_class.none, 5)).to be false
    end
  end
end
