# frozen_string_literal: true

require 'spec_helper'
require 'ch03_immutable_values'

RSpec.describe Ch03ImmutableValues do
  # ===========================================================================
  # 3.1 Basic List Operations
  # ===========================================================================

  describe '.appended' do
    it 'appends element to list' do
      expect(described_class.appended(%w[a b], 'c')).to eq(%w[a b c])
    end

    it 'does not modify original list' do
      original = %w[a b]
      described_class.appended(original, 'c')
      expect(original).to eq(%w[a b])
    end

    it 'appends to empty list' do
      expect(described_class.appended([], 'a')).to eq(['a'])
    end
  end

  describe '.appended_all' do
    it 'appends multiple elements' do
      expect(described_class.appended_all(%w[a b], %w[c d])).to eq(%w[a b c d])
    end

    it 'handles empty second list' do
      expect(described_class.appended_all(%w[a b], [])).to eq(%w[a b])
    end
  end

  describe '.prepended' do
    it 'prepends element to list' do
      expect(described_class.prepended(%w[b c], 'a')).to eq(%w[a b c])
    end
  end

  # ===========================================================================
  # 3.2 Slicing Operations
  # ===========================================================================

  describe '.first_n' do
    it 'returns first n elements' do
      expect(described_class.first_n(%w[a b c d], 2)).to eq(%w[a b])
    end

    it 'returns all if n > size' do
      expect(described_class.first_n(%w[a b], 5)).to eq(%w[a b])
    end

    it 'returns empty for n = 0' do
      expect(described_class.first_n(%w[a b c], 0)).to eq([])
    end
  end

  describe '.last_n' do
    it 'returns last n elements' do
      expect(described_class.last_n(%w[a b c d], 2)).to eq(%w[c d])
    end

    it 'returns all if n > size' do
      expect(described_class.last_n(%w[a b], 5)).to eq(%w[a b])
    end
  end

  describe '.slice' do
    it 'returns slice from start to end' do
      expect(described_class.slice(%w[a b c d], 1, 3)).to eq(%w[b c])
    end

    it 'handles start = 0' do
      expect(described_class.slice(%w[a b c], 0, 2)).to eq(%w[a b])
    end

    it 'handles empty result' do
      expect(described_class.slice(%w[a b c], 2, 2)).to eq([])
    end
  end

  describe '.first_two' do
    it 'returns first two elements' do
      expect(described_class.first_two(%w[a b c])).to eq(%w[a b])
    end
  end

  describe '.last_two' do
    it 'returns last two elements' do
      expect(described_class.last_two(%w[a b c])).to eq(%w[b c])
    end
  end

  # ===========================================================================
  # 3.3 List Transformation Examples
  # ===========================================================================

  describe '.moved_first_two_to_end' do
    it 'moves first two to end' do
      expect(described_class.moved_first_two_to_end(%w[a b c])).to eq(%w[c a b])
    end

    it 'handles list with 4 elements' do
      expect(described_class.moved_first_two_to_end(%w[a b c d])).to eq(%w[c d a b])
    end

    it 'returns same list if size <= 2' do
      expect(described_class.moved_first_two_to_end(%w[a b])).to eq(%w[a b])
    end
  end

  describe '.inserted_before_last' do
    it 'inserts before last element' do
      expect(described_class.inserted_before_last(%w[a b], 'c')).to eq(%w[a c b])
    end

    it 'handles single element list' do
      expect(described_class.inserted_before_last(['a'], 'b')).to eq(%w[b a])
    end

    it 'handles empty list' do
      expect(described_class.inserted_before_last([], 'a')).to eq(['a'])
    end
  end

  describe '.insert_at' do
    it 'inserts at given index' do
      expect(described_class.insert_at(%w[a c], 1, 'b')).to eq(%w[a b c])
    end

    it 'inserts at beginning' do
      expect(described_class.insert_at(%w[b c], 0, 'a')).to eq(%w[a b c])
    end

    it 'inserts at end' do
      expect(described_class.insert_at(%w[a b], 2, 'c')).to eq(%w[a b c])
    end
  end

  # ===========================================================================
  # 3.4 Itinerary Replanning
  # ===========================================================================

  describe '.replan' do
    it 'inserts new city before specified city' do
      plan = %w[Paris Berlin Kraków]
      expect(described_class.replan(plan, 'Vienna', 'Kraków')).to eq(%w[Paris Berlin Vienna Kraków])
    end

    it 'does not modify original plan' do
      plan = %w[Paris Berlin Kraków]
      described_class.replan(plan, 'Vienna', 'Kraków')
      expect(plan).to eq(%w[Paris Berlin Kraków])
    end

    it 'returns same plan if city not found' do
      plan = %w[Paris Berlin]
      expect(described_class.replan(plan, 'Vienna', 'Tokyo')).to eq(%w[Paris Berlin])
    end

    it 'inserts at beginning' do
      plan = %w[Paris Berlin]
      expect(described_class.replan(plan, 'London', 'Paris')).to eq(%w[London Paris Berlin])
    end
  end

  # ===========================================================================
  # 3.5 String Operations
  # ===========================================================================

  describe '.concat_strings' do
    it 'concatenates two strings' do
      expect(described_class.concat_strings('ab', 'cd')).to eq('abcd')
    end
  end

  describe '.substring' do
    it 'returns substring from start to end' do
      expect(described_class.substring('hello', 1, 4)).to eq('ell')
    end

    it 'handles start = 0' do
      expect(described_class.substring('hello', 0, 2)).to eq('he')
    end
  end

  # ===========================================================================
  # 3.6 Name Abbreviation
  # ===========================================================================

  describe '.abbreviate' do
    it 'abbreviates full name' do
      expect(described_class.abbreviate('Alonzo Church')).to eq('A. Church')
    end

    it 'handles already abbreviated name' do
      expect(described_class.abbreviate('A. Church')).to eq('A. Church')
    end

    it 'handles name without space' do
      expect(described_class.abbreviate('Alonzo')).to eq('Alonzo')
    end
  end

  # ===========================================================================
  # 3.7 Shopping Cart Operations
  # ===========================================================================

  describe '.add_to_cart' do
    it 'adds item to cart' do
      expect(described_class.add_to_cart(%w[Apple], 'Book')).to eq(%w[Apple Book])
    end
  end

  describe '.remove_from_cart' do
    it 'removes first occurrence of item' do
      expect(described_class.remove_from_cart(%w[Apple Book Apple], 'Apple')).to eq(%w[Book Apple])
    end

    it 'returns same cart if item not found' do
      expect(described_class.remove_from_cart(%w[Apple Book], 'Orange')).to eq(%w[Apple Book])
    end
  end

  describe '.replace_in_cart' do
    it 'replaces all occurrences of item' do
      expect(described_class.replace_in_cart(%w[Apple Book Apple], 'Apple', 'Orange')).to eq(%w[Orange Book Orange])
    end
  end

  # ===========================================================================
  # 3.8 Immutable Data Structure Helpers
  # ===========================================================================

  describe '.freeze_list' do
    it 'freezes list and elements' do
      list = described_class.freeze_list(%w[a b])
      expect(list).to be_frozen
    end
  end

  describe '.updated_at' do
    it 'updates element at index' do
      expect(described_class.updated_at(%w[a b c], 1, 'x')).to eq(%w[a x c])
    end

    it 'returns same list for invalid index' do
      expect(described_class.updated_at(%w[a b], 5, 'x')).to eq(%w[a b])
    end
  end

  describe '.removed_at' do
    it 'removes element at index' do
      expect(described_class.removed_at(%w[a b c], 1)).to eq(%w[a c])
    end

    it 'returns same list for invalid index' do
      expect(described_class.removed_at(%w[a b], 5)).to eq(%w[a b])
    end
  end

  # ===========================================================================
  # 3.9 Playlist Operations
  # ===========================================================================

  describe '.move_song' do
    it 'moves song from one position to another' do
      playlist = %w[A B C D]
      expect(described_class.move_song(playlist, 0, 2)).to eq(%w[B C A D])
    end

    it 'returns same playlist for same position' do
      playlist = %w[A B C]
      expect(described_class.move_song(playlist, 1, 1)).to eq(%w[A B C])
    end
  end

  describe '.reverse_playlist' do
    it 'reverses the playlist' do
      expect(described_class.reverse_playlist(%w[A B C])).to eq(%w[C B A])
    end
  end

  describe '.first_n_songs' do
    it 'returns first n songs' do
      expect(described_class.first_n_songs(%w[A B C D], 2)).to eq(%w[A B])
    end
  end
end
