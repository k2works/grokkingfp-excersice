# frozen_string_literal: true

require 'spec_helper'
require 'ch09_streams'

RSpec.describe Ch09Streams do
  # ===========================================================================
  # 9.1 LazyStream Basic Operations
  # ===========================================================================

  describe Ch09Streams::LazyStream do
    describe '.from_array' do
      it 'creates stream from array' do
        stream = Ch09Streams::LazyStream.from_array([1, 2, 3])
        expect(stream.to_array).to eq([1, 2, 3])
      end
    end

    describe '.from_range' do
      it 'creates stream from range' do
        stream = Ch09Streams::LazyStream.from_range(1..5)
        expect(stream.to_array).to eq([1, 2, 3, 4, 5])
      end
    end

    describe '.of' do
      it 'creates stream from values' do
        stream = Ch09Streams::LazyStream.of(1, 2, 3)
        expect(stream.to_array).to eq([1, 2, 3])
      end
    end

    describe '.repeat' do
      it 'creates infinite repeating stream' do
        stream = Ch09Streams::LazyStream.repeat(1, 2, 3)
        expect(stream.take(8).to_array).to eq([1, 2, 3, 1, 2, 3, 1, 2])
      end
    end

    describe '.continually' do
      it 'creates infinite stream from block' do
        counter = 0
        stream = Ch09Streams::LazyStream.continually { counter += 1 }
        expect(stream.take(5).to_array).to eq([1, 2, 3, 4, 5])
      end
    end

    describe '.iterate' do
      it 'creates infinite stream from iteration' do
        stream = Ch09Streams::LazyStream.iterate(1) { |n| n * 2 }
        expect(stream.take(5).to_array).to eq([1, 2, 4, 8, 16])
      end
    end

    describe '.empty' do
      it 'creates empty stream' do
        stream = Ch09Streams::LazyStream.empty
        expect(stream.to_array).to eq([])
      end
    end

    describe '.from' do
      it 'creates infinite stream of integers' do
        stream = Ch09Streams::LazyStream.from(10)
        expect(stream.take(5).to_array).to eq([10, 11, 12, 13, 14])
      end
    end
  end

  # ===========================================================================
  # 9.2 Stream Operations
  # ===========================================================================

  describe 'stream operations' do
    let(:stream) { Ch09Streams::LazyStream.from_array([1, 2, 3, 4, 5]) }

    describe '#take' do
      it 'takes first n elements' do
        expect(stream.take(3).to_array).to eq([1, 2, 3])
      end

      it 'takes all if n > size' do
        expect(stream.take(10).to_array).to eq([1, 2, 3, 4, 5])
      end
    end

    describe '#drop' do
      it 'drops first n elements' do
        expect(stream.drop(2).to_array).to eq([3, 4, 5])
      end
    end

    describe '#filter' do
      it 'filters elements' do
        expect(stream.filter(&:even?).to_array).to eq([2, 4])
      end
    end

    describe '#fmap' do
      it 'maps over elements' do
        expect(stream.fmap { |x| x * 2 }.to_array).to eq([2, 4, 6, 8, 10])
      end
    end

    describe '#flat_map' do
      it 'flat maps over elements' do
        result = stream.flat_map { |x| [x, x * 10] }
        expect(result.take(6).to_array).to eq([1, 10, 2, 20, 3, 30])
      end
    end

    describe '#take_while' do
      it 'takes while predicate is true' do
        expect(stream.take_while { |x| x < 4 }.to_array).to eq([1, 2, 3])
      end
    end

    describe '#drop_while' do
      it 'drops while predicate is true' do
        expect(stream.drop_while { |x| x < 3 }.to_array).to eq([3, 4, 5])
      end
    end

    describe '#append' do
      it 'appends another stream' do
        other = Ch09Streams::LazyStream.from_array([6, 7, 8])
        expect(stream.append(other).to_array).to eq([1, 2, 3, 4, 5, 6, 7, 8])
      end
    end

    describe '#zip' do
      it 'zips two streams' do
        other = Ch09Streams::LazyStream.from_array(%w[a b c])
        result = stream.zip(other).to_array
        expect(result).to eq([[1, 'a'], [2, 'b'], [3, 'c']])
      end
    end

    describe '#zip_left' do
      it 'zips and keeps left values' do
        other = Ch09Streams::LazyStream.from_array(%w[a b c])
        result = stream.zip_left(other).to_array
        expect(result).to eq([1, 2, 3])
      end
    end

    describe '#sliding' do
      it 'creates sliding windows' do
        result = stream.sliding(3).to_array
        expect(result).to eq([[1, 2, 3], [2, 3, 4], [3, 4, 5]])
      end

      it 'handles window larger than stream' do
        result = stream.sliding(10).to_array
        expect(result).to eq([])
      end
    end
  end

  # ===========================================================================
  # 9.3 Terminal Operations
  # ===========================================================================

  describe 'terminal operations' do
    let(:stream) { Ch09Streams::LazyStream.from_array([1, 2, 3, 4, 5]) }

    describe '#to_array' do
      it 'converts to array' do
        expect(stream.to_array).to eq([1, 2, 3, 4, 5])
      end
    end

    describe '#head' do
      it 'returns first element' do
        expect(stream.head).to eq(1)
      end
    end

    describe '#fold' do
      it 'folds elements' do
        result = stream.fold(0) { |acc, x| acc + x }
        expect(result).to eq(15)
      end
    end

    describe '#any?' do
      it 'returns true if any match' do
        expect(stream.any? { |x| x > 3 }).to be true
      end

      it 'returns false if none match' do
        expect(stream.any? { |x| x > 10 }).to be false
      end
    end

    describe '#all?' do
      it 'returns true if all match' do
        expect(stream.all? { |x| x > 0 }).to be true
      end

      it 'returns false if any fail' do
        expect(stream.all? { |x| x > 3 }).to be false
      end
    end

    describe '#find' do
      it 'finds first matching element' do
        expect(stream.find { |x| x > 3 }).to eq(4)
      end

      it 'returns nil if not found' do
        expect(stream.find { |x| x > 10 }).to be_nil
      end
    end

    describe '#sum' do
      it 'sums elements' do
        expect(stream.sum).to eq(15)
      end
    end
  end

  # ===========================================================================
  # 9.4 IOStream
  # ===========================================================================

  describe Ch09Streams::IOStream do
    describe '.eval' do
      it 'creates stream from single IO' do
        io = Ch08IO::IO.pure(42)
        stream = Ch09Streams::IOStream.eval(io)
        expect(stream.compile_to_list.run!).to eq([42])
      end
    end

    describe '.repeat_eval' do
      it 'creates infinite stream from IO' do
        counter = 0
        io = Ch08IO::IO.delay { counter += 1 }
        stream = Ch09Streams::IOStream.repeat_eval(io)
        expect(stream.take(5).compile_to_list.run!).to eq([1, 2, 3, 4, 5])
      end
    end

    describe '#filter' do
      it 'filters IO stream' do
        stream = Ch09Streams::IOStream.repeat_eval(Ch08IO::IO.pure(5))
        result = stream.filter { |x| x > 3 }.take(3).compile_to_list.run!
        expect(result).to eq([5, 5, 5])
      end
    end

    describe '#fmap' do
      it 'maps over IO stream' do
        counter = 0
        io = Ch08IO::IO.delay { counter += 1 }
        stream = Ch09Streams::IOStream.repeat_eval(io)
        result = stream.fmap { |x| x * 2 }.take(3).compile_to_list.run!
        expect(result).to eq([2, 4, 6])
      end
    end
  end

  # ===========================================================================
  # 9.5 Die Casting with Streams
  # ===========================================================================

  describe '.infinite_die_casts' do
    it 'returns IO stream of die casts' do
      stream = described_class.infinite_die_casts
      expect(stream).to be_a(Ch09Streams::IOStream)
    end
  end

  describe '.first_n_die_casts' do
    it 'returns first n die casts' do
      io = described_class.first_n_die_casts(5)
      result = io.run!
      expect(result.size).to eq(5)
      expect(result.all? { |r| r >= 1 && r <= 6 }).to be true
    end
  end

  describe '.die_casts_until_six' do
    it 'returns list containing 6' do
      io = described_class.die_casts_until_six
      result = io.run!
      expect(result).to eq([6])
    end
  end

  # ===========================================================================
  # 9.6 Currency Exchange
  # ===========================================================================

  describe Ch09Streams::Currency do
    it 'creates currency struct' do
      currency = Ch09Streams::Currency.new(code: 'USD')
      expect(currency.code).to eq('USD')
    end
  end

  describe '.exchange_table' do
    it 'returns IO of exchange rates' do
      from = Ch09Streams::Currency.new(code: 'USD')
      io = described_class.exchange_table(from)
      result = io.run!
      expect(result).to be_a(Hash)
      expect(result.keys).to include('EUR')
    end
  end

  describe '.extract_single_currency_rate' do
    it 'extracts rate for currency' do
      table = { 'EUR' => 0.85, 'GBP' => 0.73 }
      to = Ch09Streams::Currency.new(code: 'EUR')
      rate = described_class.extract_single_currency_rate(to, table)
      expect(rate).to eq(0.85)
    end

    it 'returns nil for unknown currency' do
      table = { 'EUR' => 0.85 }
      to = Ch09Streams::Currency.new(code: 'JPY')
      rate = described_class.extract_single_currency_rate(to, table)
      expect(rate).to be_nil
    end
  end

  # ===========================================================================
  # 9.7 Trend Detection
  # ===========================================================================

  describe '.trending?' do
    it 'returns true for increasing rates' do
      expect(described_class.trending?([0.81, 0.82, 0.83])).to be true
    end

    it 'returns false for non-increasing rates' do
      expect(described_class.trending?([0.81, 0.84, 0.83])).to be false
    end

    it 'returns false for single element' do
      expect(described_class.trending?([0.81])).to be false
    end

    it 'returns false for empty list' do
      expect(described_class.trending?([])).to be false
    end
  end

  describe '.trending_down?' do
    it 'returns true for decreasing rates' do
      expect(described_class.trending_down?([0.83, 0.82, 0.81])).to be true
    end

    it 'returns false for non-decreasing rates' do
      expect(described_class.trending_down?([0.81, 0.82, 0.83])).to be false
    end
  end

  describe '.stable?' do
    it 'returns true for stable rates' do
      expect(described_class.stable?([5, 5, 5])).to be true
    end

    it 'returns false for varying rates' do
      expect(described_class.stable?([5, 5, 6])).to be false
    end

    it 'returns false for less than 3 elements' do
      expect(described_class.stable?([5, 5])).to be false
    end
  end

  describe '.average_rate' do
    it 'calculates average' do
      expect(described_class.average_rate([1, 2, 3, 4, 5])).to eq(3.0)
    end

    it 'returns nil for empty list' do
      expect(described_class.average_rate([])).to be_nil
    end
  end

  # ===========================================================================
  # 9.8 Pure Stream Examples
  # ===========================================================================

  describe '.evens' do
    it 'returns even numbers' do
      result = described_class.evens(10).to_array
      expect(result).to eq([2, 4, 6, 8, 10])
    end
  end

  describe '.fibonacci' do
    it 'generates fibonacci sequence' do
      result = described_class.fibonacci.take(10).to_array
      expect(result).to eq([0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
    end
  end

  describe '.primes' do
    it 'generates prime numbers' do
      result = described_class.primes.take(10).to_array
      expect(result).to eq([2, 3, 5, 7, 11, 13, 17, 19, 23, 29])
    end
  end

  describe '.prime?' do
    it 'detects primes' do
      expect(described_class.prime?(2)).to be true
      expect(described_class.prime?(17)).to be true
      expect(described_class.prime?(1)).to be false
      expect(described_class.prime?(4)).to be false
    end
  end

  describe '.natural_numbers' do
    it 'generates natural numbers' do
      result = described_class.natural_numbers.take(5).to_array
      expect(result).to eq([1, 2, 3, 4, 5])
    end
  end

  describe '.alternating_booleans' do
    it 'generates alternating booleans' do
      result = described_class.alternating_booleans.take(6).to_array
      expect(result).to eq([true, false, true, false, true, false])
    end
  end

  # ===========================================================================
  # 9.9 Sliding Window Examples
  # ===========================================================================

  describe '.detect_trend' do
    it 'detects trends in values' do
      result = described_class.detect_trend([1, 2, 3, 4, 3], 3).to_array
      expect(result[0][:window]).to eq([1, 2, 3])
      expect(result[0][:trending]).to be true
      expect(result[2][:window]).to eq([3, 4, 3])
      expect(result[2][:trending]).to be false
    end
  end

  describe '.moving_average' do
    it 'calculates moving average' do
      result = described_class.moving_average([1, 2, 3, 4, 5], 3).to_array
      expect(result[0]).to eq(2.0) # (1+2+3)/3
      expect(result[1]).to eq(3.0) # (2+3+4)/3
      expect(result[2]).to eq(4.0) # (3+4+5)/3
    end
  end

  # ===========================================================================
  # 9.10 Stream Utilities
  # ===========================================================================

  describe '.scan' do
    it 'produces intermediate results' do
      stream = Ch09Streams::LazyStream.from_array([1, 2, 3])
      result = described_class.scan(stream, 0) { |acc, x| acc + x }.to_array
      expect(result).to eq([0, 1, 3, 6])
    end
  end

  describe '.running_sum' do
    it 'calculates running sum' do
      stream = Ch09Streams::LazyStream.from_array([1, 2, 3, 4, 5])
      result = described_class.running_sum(stream).to_array
      expect(result).to eq([0, 1, 3, 6, 10, 15])
    end
  end

  describe '.running_product' do
    it 'calculates running product' do
      stream = Ch09Streams::LazyStream.from_array([1, 2, 3, 4])
      result = described_class.running_product(stream).to_array
      expect(result).to eq([1, 1, 2, 6, 24])
    end
  end
end
