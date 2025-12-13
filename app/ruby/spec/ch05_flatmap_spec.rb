# frozen_string_literal: true

require 'spec_helper'
require 'ch05_flatmap'

RSpec.describe Ch05Flatmap do
  # ===========================================================================
  # 5.1 flatten - Flatten Nested Lists
  # ===========================================================================

  describe '.flatten' do
    it 'flattens one level of nesting' do
      nested = [[1, 2], [3, 4], [5]]
      expect(described_class.flatten(nested)).to eq([1, 2, 3, 4, 5])
    end

    it 'handles empty inner lists' do
      nested = [[1, 2], [], [3]]
      expect(described_class.flatten(nested)).to eq([1, 2, 3])
    end
  end

  describe '.flatten_deep' do
    it 'flattens all levels of nesting' do
      nested = [[1, [2, 3]], [[4, 5], 6]]
      expect(described_class.flatten_deep(nested)).to eq([1, 2, 3, 4, 5, 6])
    end
  end

  # ===========================================================================
  # 5.2 flat_map = map + flatten
  # ===========================================================================

  describe '.flat_map' do
    it 'maps and flattens' do
      result = described_class.flat_map([1, 2, 3]) { |x| [x, x * 10] }
      expect(result).to eq([1, 10, 2, 20, 3, 30])
    end
  end

  describe '.duplicate_each' do
    it 'duplicates each element' do
      expect(described_class.duplicate_each([1, 2, 3])).to eq([1, 1, 2, 2, 3, 3])
    end
  end

  describe '.expand_to_range' do
    it 'expands numbers to ranges' do
      expect(described_class.expand_to_range([1, 2, 3])).to eq([1, 1, 2, 1, 2, 3])
    end

    it 'handles empty input' do
      expect(described_class.expand_to_range([])).to eq([])
    end
  end

  # ===========================================================================
  # 5.3 Book and Author Example
  # ===========================================================================

  describe 'Book struct' do
    it 'has title and authors' do
      book = described_class::Book.new(title: 'FP in Scala', authors: %w[Chiusano Bjarnason])
      expect(book.title).to eq('FP in Scala')
      expect(book.authors).to eq(%w[Chiusano Bjarnason])
    end
  end

  describe '.all_authors' do
    let(:books) do
      [
        described_class::Book.new(title: 'FP in Scala', authors: %w[Chiusano Bjarnason]),
        described_class::Book.new(title: 'The Hobbit', authors: ['Tolkien'])
      ]
    end

    it 'returns all authors from all books' do
      expect(described_class.all_authors(books)).to eq(%w[Chiusano Bjarnason Tolkien])
    end
  end

  describe '.unique_authors' do
    let(:books) do
      [
        described_class::Book.new(title: 'Book 1', authors: %w[Author1 Author2]),
        described_class::Book.new(title: 'Book 2', authors: %w[Author2 Author3])
      ]
    end

    it 'returns unique authors' do
      expect(described_class.unique_authors(books)).to eq(%w[Author1 Author2 Author3])
    end
  end

  # ===========================================================================
  # 5.4 flatMap for Filtering
  # ===========================================================================

  describe '.filter_with_flatmap' do
    it 'filters using flatmap pattern' do
      result = described_class.filter_with_flatmap([1, 2, 3, 4, 5]) { |x| x > 3 }
      expect(result).to eq([4, 5])
    end
  end

  describe '.evens_with_flatmap' do
    it 'filters even numbers' do
      expect(described_class.evens_with_flatmap([1, 2, 3, 4, 5, 6])).to eq([2, 4, 6])
    end
  end

  # ===========================================================================
  # 5.5 flatMap Size Changes
  # ===========================================================================

  describe '.with_doubled' do
    it 'increases size (1 -> 2)' do
      result = described_class.with_doubled([1, 2, 3])
      expect(result).to eq([1, 11, 2, 12, 3, 13])
      expect(result.length).to eq(6)
    end
  end

  describe '.doubled' do
    it 'keeps same size (1 -> 1)' do
      result = described_class.doubled([1, 2, 3])
      expect(result).to eq([2, 4, 6])
      expect(result.length).to eq(3)
    end
  end

  describe '.positive_only' do
    it 'decreases size (conditional)' do
      result = described_class.positive_only([-1, 0, 1, 2])
      expect(result).to eq([1, 2])
    end
  end

  # ===========================================================================
  # 5.6 Nested flatMap
  # ===========================================================================

  describe '.book_adaptations' do
    it 'returns movies for Tolkien' do
      movies = described_class.book_adaptations('Tolkien')
      expect(movies.length).to eq(2)
      expect(movies.first.title).to eq('An Unexpected Journey')
    end

    it 'returns empty for unknown author' do
      expect(described_class.book_adaptations('Unknown')).to eq([])
    end
  end

  describe '.recommendations' do
    let(:books) do
      [
        described_class::Book.new(title: 'FP in Scala', authors: %w[Chiusano Bjarnason]),
        described_class::Book.new(title: 'The Hobbit', authors: ['Tolkien'])
      ]
    end

    it 'generates recommendations using nested flatmap' do
      recs = described_class.recommendations(books)
      expect(recs.length).to eq(2)
      expect(recs.first).to include('An Unexpected Journey')
      expect(recs.first).to include('Tolkien')
      expect(recs.first).to include('The Hobbit')
    end
  end

  # ===========================================================================
  # 5.7 Cartesian Product
  # ===========================================================================

  describe '.cartesian_product' do
    it 'generates all combinations' do
      result = described_class.cartesian_product([1, 2], %w[a b])
      expect(result).to eq([[1, 'a'], [1, 'b'], [2, 'a'], [2, 'b']])
    end

    it 'handles empty first list' do
      expect(described_class.cartesian_product([], [1, 2])).to eq([])
    end
  end

  describe '.combine' do
    it 'combines with custom function' do
      result = described_class.combine([1, 2], [10, 20]) { |a, b| a + b }
      expect(result).to eq([11, 21, 12, 22])
    end
  end

  # ===========================================================================
  # 5.8 Points and Circles Example
  # ===========================================================================

  describe 'Point struct' do
    it 'calculates distance from origin' do
      point = described_class::Point.new(x: 3, y: 4)
      expect(point.distance_from_origin).to eq(5.0)
    end
  end

  describe '.inside?' do
    it 'returns true if point is inside circle' do
      point = described_class::Point.new(x: 1, y: 1)
      expect(described_class.inside?(point, 2)).to be true
    end

    it 'returns false if point is outside circle' do
      point = described_class::Point.new(x: 5, y: 2)
      expect(described_class.inside?(point, 2)).to be false
    end
  end

  describe '.points_inside_circles' do
    let(:points) do
      [
        described_class::Point.new(x: 5, y: 2),
        described_class::Point.new(x: 1, y: 1)
      ]
    end
    let(:radiuses) { [2, 1] }

    it 'finds points inside circles' do
      result = described_class.points_inside_circles(points, radiuses)
      expect(result.length).to eq(1)
      expect(result.first[:point].x).to eq(1)
      expect(result.first[:radius]).to eq(2)
    end
  end

  describe '.point_radius_report' do
    let(:points) do
      [
        described_class::Point.new(x: 1, y: 1)
      ]
    end
    let(:radiuses) { [2, 1] }

    it 'generates report for all combinations' do
      result = described_class.point_radius_report(points, radiuses)
      expect(result.length).to eq(2)
      expect(result[0]).to include('radius 2: true')
      expect(result[1]).to include('radius 1: false')
    end
  end

  # ===========================================================================
  # 5.9 Triple Nested flatMap
  # ===========================================================================

  describe '.triplets' do
    it 'generates all triplet combinations' do
      result = described_class.triplets([1, 2], [10, 20], [100, 200])
      expect(result.length).to eq(8)
      expect(result).to include([1, 10, 100])
      expect(result).to include([2, 20, 200])
    end
  end

  describe '.triplet_sums' do
    it 'sums all triplet combinations' do
      result = described_class.triplet_sums([1, 2], [10, 20], [100, 200])
      expect(result).to include(111) # 1 + 10 + 100
      expect(result).to include(222) # 2 + 20 + 200
    end
  end

  # ===========================================================================
  # 5.10 Practical Examples
  # ===========================================================================

  describe '.all_words' do
    it 'extracts all words from sentences' do
      sentences = ['hello world', 'foo bar']
      expect(described_class.all_words(sentences)).to eq(%w[hello world foo bar])
    end
  end

  describe '.all_chars' do
    it 'extracts all characters from strings' do
      expect(described_class.all_chars(%w[ab cd])).to eq(%w[a b c d])
    end
  end

  describe '.flatten_hash_values' do
    it 'flattens hash values' do
      hash = { a: [1, 2], b: [3, 4] }
      expect(described_class.flatten_hash_values(hash)).to eq([1, 2, 3, 4])
    end
  end

  describe '.nested_values' do
    it 'extracts all values from nested hash' do
      hash = { a: 1, b: { c: 2, d: 3 } }
      expect(described_class.nested_values(hash)).to contain_exactly(1, 2, 3)
    end

    it 'handles arrays in hash' do
      hash = { a: [1, 2], b: 3 }
      expect(described_class.nested_values(hash)).to contain_exactly(1, 2, 3)
    end
  end

  # ===========================================================================
  # 5.11 Monadic flatMap Pattern Preview
  # ===========================================================================

  describe '.safe_divide' do
    it 'returns result in array for valid division' do
      expect(described_class.safe_divide(10, 2)).to eq([5])
    end

    it 'returns empty array for division by zero' do
      expect(described_class.safe_divide(10, 0)).to eq([])
    end
  end

  describe '.safe_calculation' do
    it 'chains safe divisions' do
      expect(described_class.safe_calculation(100, 2, 5)).to eq([10])
    end

    it 'returns empty for division by zero in chain' do
      expect(described_class.safe_calculation(100, 0, 5)).to eq([])
      expect(described_class.safe_calculation(100, 2, 0)).to eq([])
    end
  end

  describe '.safe_parse_int' do
    it 'parses valid integer' do
      expect(described_class.safe_parse_int('42')).to eq([42])
    end

    it 'returns empty for invalid integer' do
      expect(described_class.safe_parse_int('abc')).to eq([])
    end
  end

  describe '.parse_and_divide' do
    it 'parses and divides successfully' do
      expect(described_class.parse_and_divide('10', '2')).to eq([5])
    end

    it 'returns empty for parse error' do
      expect(described_class.parse_and_divide('abc', '2')).to eq([])
    end

    it 'returns empty for division by zero' do
      expect(described_class.parse_and_divide('10', '0')).to eq([])
    end
  end
end
