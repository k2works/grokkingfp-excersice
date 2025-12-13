# frozen_string_literal: true

require 'spec_helper'
require 'ch01_intro'

RSpec.describe Ch01Intro do
  # ===========================================================================
  # 1.1 Imperative vs Functional
  # ===========================================================================

  describe '.calculate_score_imperative' do
    it 'calculates score imperatively' do
      expect(described_class.calculate_score_imperative('hello')).to eq(5)
    end

    it 'returns 0 for empty string' do
      expect(described_class.calculate_score_imperative('')).to eq(0)
    end
  end

  describe '.word_score' do
    it 'returns the length of the word' do
      expect(described_class.word_score('hello')).to eq(5)
    end

    it 'returns 0 for empty string' do
      expect(described_class.word_score('')).to eq(0)
    end

    it 'counts Unicode characters' do
      expect(described_class.word_score('Ruby')).to eq(4)
    end
  end

  # ===========================================================================
  # 1.2 Basic function definitions
  # ===========================================================================

  describe '.increment' do
    it 'increments a positive number' do
      expect(described_class.increment(5)).to eq(6)
    end

    it 'increments zero' do
      expect(described_class.increment(0)).to eq(1)
    end

    it 'increments a negative number' do
      expect(described_class.increment(-5)).to eq(-4)
    end
  end

  describe '.get_first_character' do
    it 'returns the first character' do
      expect(described_class.get_first_character('hello')).to eq('h')
    end

    it 'returns nil for empty string' do
      expect(described_class.get_first_character('')).to be_nil
    end
  end

  describe '.add' do
    it 'adds two positive numbers' do
      expect(described_class.add(2, 3)).to eq(5)
    end

    it 'adds negative numbers' do
      expect(described_class.add(-2, -3)).to eq(-5)
    end

    it 'adds zero' do
      expect(described_class.add(5, 0)).to eq(5)
    end
  end

  describe '.double' do
    it 'doubles a positive number' do
      expect(described_class.double(5)).to eq(10)
    end

    it 'doubles zero' do
      expect(described_class.double(0)).to eq(0)
    end

    it 'doubles a negative number' do
      expect(described_class.double(-3)).to eq(-6)
    end
  end

  describe '.greet' do
    it 'creates a greeting message' do
      expect(described_class.greet('World')).to eq('Hello, World!')
    end

    it 'greets with a name' do
      expect(described_class.greet('Ruby')).to eq('Hello, Ruby!')
    end
  end

  describe '.to_uppercase' do
    it 'converts to uppercase' do
      expect(described_class.to_uppercase('hello')).to eq('HELLO')
    end

    it 'handles mixed case' do
      expect(described_class.to_uppercase('HeLLo')).to eq('HELLO')
    end

    it 'handles empty string' do
      expect(described_class.to_uppercase('')).to eq('')
    end
  end

  describe '.reverse_string' do
    it 'reverses a string' do
      expect(described_class.reverse_string('hello')).to eq('olleh')
    end

    it 'reverses a palindrome' do
      expect(described_class.reverse_string('madam')).to eq('madam')
    end

    it 'handles empty string' do
      expect(described_class.reverse_string('')).to eq('')
    end
  end

  # ===========================================================================
  # 1.3 Conditional functions
  # ===========================================================================

  describe '.absolute' do
    it 'returns the same value for positive numbers' do
      expect(described_class.absolute(5)).to eq(5)
    end

    it 'returns the negation for negative numbers' do
      expect(described_class.absolute(-5)).to eq(5)
    end

    it 'returns 0 for 0' do
      expect(described_class.absolute(0)).to eq(0)
    end
  end

  describe '.max_value' do
    it 'returns the larger value' do
      expect(described_class.max_value(3, 5)).to eq(5)
    end

    it 'returns the first value when equal' do
      expect(described_class.max_value(5, 5)).to eq(5)
    end

    it 'handles negative numbers' do
      expect(described_class.max_value(-3, -5)).to eq(-3)
    end
  end

  describe '.min_value' do
    it 'returns the smaller value' do
      expect(described_class.min_value(3, 5)).to eq(3)
    end

    it 'returns the first value when equal' do
      expect(described_class.min_value(5, 5)).to eq(5)
    end

    it 'handles negative numbers' do
      expect(described_class.min_value(-3, -5)).to eq(-5)
    end
  end

  describe '.clamp' do
    it 'returns the value when within range' do
      expect(described_class.clamp(5, 0, 10)).to eq(5)
    end

    it 'returns min when value is below range' do
      expect(described_class.clamp(-5, 0, 10)).to eq(0)
    end

    it 'returns max when value is above range' do
      expect(described_class.clamp(15, 0, 10)).to eq(10)
    end

    it 'returns min when value equals min' do
      expect(described_class.clamp(0, 0, 10)).to eq(0)
    end

    it 'returns max when value equals max' do
      expect(described_class.clamp(10, 0, 10)).to eq(10)
    end
  end

  # ===========================================================================
  # 1.4 Predicate functions
  # ===========================================================================

  describe '.even?' do
    it 'returns true for even numbers' do
      expect(described_class.even?(4)).to be true
    end

    it 'returns false for odd numbers' do
      expect(described_class.even?(5)).to be false
    end

    it 'returns true for 0' do
      expect(described_class.even?(0)).to be true
    end

    it 'handles negative even numbers' do
      expect(described_class.even?(-4)).to be true
    end

    it 'handles negative odd numbers' do
      expect(described_class.even?(-5)).to be false
    end
  end

  describe '.positive?' do
    it 'returns true for positive numbers' do
      expect(described_class.positive?(5)).to be true
    end

    it 'returns false for negative numbers' do
      expect(described_class.positive?(-5)).to be false
    end

    it 'returns false for 0' do
      expect(described_class.positive?(0)).to be false
    end
  end

  describe '.empty?' do
    it 'returns true for empty string' do
      expect(described_class.empty?('')).to be true
    end

    it 'returns false for non-empty string' do
      expect(described_class.empty?('hello')).to be false
    end
  end

  describe '.negative?' do
    it 'returns true for negative numbers' do
      expect(described_class.negative?(-5)).to be true
    end

    it 'returns false for positive numbers' do
      expect(described_class.negative?(5)).to be false
    end

    it 'returns false for 0' do
      expect(described_class.negative?(0)).to be false
    end
  end

  describe '.zero?' do
    it 'returns true for 0' do
      expect(described_class.zero?(0)).to be true
    end

    it 'returns false for non-zero' do
      expect(described_class.zero?(5)).to be false
    end
  end

  # ===========================================================================
  # 1.5 Constants and bounded score
  # ===========================================================================

  describe 'constants' do
    it 'defines MAX_SCORE' do
      expect(described_class::MAX_SCORE).to eq(100)
    end

    it 'defines MIN_SCORE' do
      expect(described_class::MIN_SCORE).to eq(0)
    end

    it 'defines DEFAULT_GREETING' do
      expect(described_class::DEFAULT_GREETING).to eq('Hello, World!')
    end
  end

  describe '.get_bounded_score' do
    it 'returns the score when within bounds' do
      expect(described_class.get_bounded_score(50)).to eq(50)
    end

    it 'returns MIN_SCORE when below' do
      expect(described_class.get_bounded_score(-10)).to eq(0)
    end

    it 'returns MAX_SCORE when above' do
      expect(described_class.get_bounded_score(150)).to eq(100)
    end
  end

  # ===========================================================================
  # 1.6 Function composition
  # ===========================================================================

  describe '.transform_string' do
    it 'uppercases and reverses' do
      expect(described_class.transform_string('hello')).to eq('OLLEH')
    end

    it 'handles empty string' do
      expect(described_class.transform_string('')).to eq('')
    end
  end

  describe '.transform_number' do
    it 'doubles then increments' do
      expect(described_class.transform_number(5)).to eq(11)
    end

    it 'handles zero' do
      expect(described_class.transform_number(0)).to eq(1)
    end
  end

  describe '.calculate_bounded_score' do
    it 'calculates bounded score for short word' do
      expect(described_class.calculate_bounded_score('hi')).to eq(2)
    end

    it 'bounds score to MAX_SCORE for very long word' do
      long_word = 'a' * 150
      expect(described_class.calculate_bounded_score(long_word)).to eq(100)
    end
  end

  # ===========================================================================
  # 1.7 Higher-order function basics
  # ===========================================================================

  describe '.apply_twice' do
    it 'applies a function twice' do
      increment = ->(x) { x + 1 }
      expect(described_class.apply_twice(increment, 5)).to eq(7)
    end

    it 'applies doubling twice' do
      double = ->(x) { x * 2 }
      expect(described_class.apply_twice(double, 3)).to eq(12)
    end
  end

  describe '.compose' do
    it 'composes two functions' do
      increment = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      composed = described_class.compose(increment, double)
      expect(composed.call(5)).to eq(11) # double(5) = 10, then increment(10) = 11
    end
  end

  describe '.pipe' do
    it 'pipes value through multiple functions' do
      increment = ->(x) { x + 1 }
      double = ->(x) { x * 2 }
      result = described_class.pipe(5, increment, double)
      expect(result).to eq(12) # increment(5) = 6, then double(6) = 12
    end

    it 'returns value when no functions' do
      expect(described_class.pipe(5)).to eq(5)
    end
  end
end
