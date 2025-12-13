# frozen_string_literal: true

require 'spec_helper'
require 'ch07_result'

RSpec.describe Ch07Result do
  # ===========================================================================
  # 7.1 Basic Result Operations
  # ===========================================================================

  describe '.success' do
    it 'creates a Success value' do
      result = described_class.success(42)
      expect(result).to be_success
      expect(result.value!).to eq(42)
    end
  end

  describe '.failure' do
    it 'creates a Failure value' do
      result = described_class.failure('error')
      expect(result).to be_failure
      expect(result.failure).to eq('error')
    end
  end

  # ===========================================================================
  # 7.2 Safe Operations with Error Messages
  # ===========================================================================

  describe '.safe_divide' do
    it 'returns Success for valid division' do
      result = described_class.safe_divide(10, 2)
      expect(result).to be_success
      expect(result.value!).to eq(5)
    end

    it 'returns Failure with message for division by zero' do
      result = described_class.safe_divide(10, 0)
      expect(result).to be_failure
      expect(result.failure).to include('Division by zero')
    end
  end

  describe '.safe_parse_int' do
    it 'returns Success for valid integer string' do
      result = described_class.safe_parse_int('42')
      expect(result).to be_success
      expect(result.value!).to eq(42)
    end

    it 'returns Failure with message for invalid string' do
      result = described_class.safe_parse_int('abc')
      expect(result).to be_failure
      expect(result.failure).to include("Cannot parse 'abc'")
    end
  end

  describe '.validate_non_empty' do
    it 'returns Success for non-empty string' do
      result = described_class.validate_non_empty('hello', 'Field')
      expect(result).to be_success
      expect(result.value!).to eq('hello')
    end

    it 'returns Failure for empty string' do
      result = described_class.validate_non_empty('', 'Field')
      expect(result).to be_failure
      expect(result.failure).to include('Field cannot be empty')
    end

    it 'returns Failure for nil' do
      result = described_class.validate_non_empty(nil, 'Field')
      expect(result).to be_failure
    end
  end

  # ===========================================================================
  # 7.3 TV Show Parsing with Error Messages
  # ===========================================================================

  describe '.extract_name' do
    it 'returns Success with name' do
      result = described_class.extract_name('Breaking Bad (2008-2013)')
      expect(result).to be_success
      expect(result.value!).to eq('Breaking Bad')
    end

    it 'returns Failure with error message' do
      result = described_class.extract_name('(2008-2013)')
      expect(result).to be_failure
      expect(result.failure).to include('Cannot extract name')
    end
  end

  describe '.parse_show' do
    it 'parses multi-year show' do
      result = described_class.parse_show('Breaking Bad (2008-2013)')
      expect(result).to be_success
      show = result.value!
      expect(show.title).to eq('Breaking Bad')
      expect(show.start_year).to eq(2008)
      expect(show.end_year).to eq(2013)
    end

    it 'parses single-year show' do
      result = described_class.parse_show('Chernobyl (2019)')
      expect(result).to be_success
      show = result.value!
      expect(show.start_year).to eq(2019)
      expect(show.end_year).to eq(2019)
    end

    it 'returns Failure with error message for invalid format' do
      result = described_class.parse_show('Invalid Show')
      expect(result).to be_failure
      expect(result.failure).to be_a(String)
    end
  end

  # ===========================================================================
  # 7.4 Maybe to Result Conversion
  # ===========================================================================

  describe '.maybe_to_result' do
    it 'converts Some to Success' do
      maybe = Dry::Monads::Some(5)
      result = described_class.maybe_to_result(maybe, 'error')
      expect(result).to be_success
      expect(result.value!).to eq(5)
    end

    it 'converts None to Failure with message' do
      maybe = Dry::Monads::None()
      result = described_class.maybe_to_result(maybe, 'value not found')
      expect(result).to be_failure
      expect(result.failure).to eq('value not found')
    end
  end

  # ===========================================================================
  # 7.5 Result Methods
  # ===========================================================================

  describe '.result_map' do
    it 'transforms Success value' do
      result = described_class.result_map(described_class.success(5)) { |x| x * 2 }
      expect(result).to be_success
      expect(result.value!).to eq(10)
    end

    it 'returns Failure unchanged' do
      result = described_class.result_map(described_class.failure('error')) { |x| x * 2 }
      expect(result).to be_failure
      expect(result.failure).to eq('error')
    end
  end

  describe '.result_value_or' do
    it 'returns value for Success' do
      result = described_class.result_value_or(described_class.success(5), 0)
      expect(result).to eq(5)
    end

    it 'returns default for Failure' do
      result = described_class.result_value_or(described_class.failure('error'), 0)
      expect(result).to eq(0)
    end
  end

  describe '.result_to_maybe' do
    it 'converts Success to Some' do
      result = described_class.result_to_maybe(described_class.success(5))
      expect(result).to be_some
      expect(result.value!).to eq(5)
    end

    it 'converts Failure to None' do
      result = described_class.result_to_maybe(described_class.failure('error'))
      expect(result).to be_none
    end
  end

  # ===========================================================================
  # 7.6 Validation Example
  # ===========================================================================

  describe '.validate_age' do
    it 'returns Success for valid age' do
      result = described_class.validate_age(25)
      expect(result).to be_success
      expect(result.value!).to eq(25)
    end

    it 'returns Failure for negative age' do
      result = described_class.validate_age(-5)
      expect(result).to be_failure
      expect(result.failure).to include('negative')
    end

    it 'returns Failure for age > 150' do
      result = described_class.validate_age(200)
      expect(result).to be_failure
      expect(result.failure).to include('150')
    end
  end

  describe '.validate_email' do
    it 'returns Success for valid email' do
      result = described_class.validate_email('test@example.com')
      expect(result).to be_success
    end

    it 'returns Failure for empty email' do
      result = described_class.validate_email('')
      expect(result).to be_failure
      expect(result.failure).to include('empty')
    end

    it 'returns Failure for email without @' do
      result = described_class.validate_email('invalid-email')
      expect(result).to be_failure
      expect(result.failure).to include('@')
    end
  end

  describe '.validate_username' do
    it 'returns Success for valid username' do
      result = described_class.validate_username('alice')
      expect(result).to be_success
    end

    it 'returns Failure for too short username' do
      result = described_class.validate_username('ab')
      expect(result).to be_failure
      expect(result.failure).to include('at least 3')
    end

    it 'returns Failure for too long username' do
      result = described_class.validate_username('a' * 25)
      expect(result).to be_failure
      expect(result.failure).to include('at most 20')
    end
  end

  describe '.validate_person' do
    it 'returns Success for valid person' do
      result = described_class.validate_person('Alice', 25, 'alice@example.com')
      expect(result).to be_success
      person = result.value!
      expect(person.name).to eq('Alice')
      expect(person.age).to eq(25)
      expect(person.email).to eq('alice@example.com')
    end

    it 'returns Failure for invalid name' do
      result = described_class.validate_person('', 25, 'alice@example.com')
      expect(result).to be_failure
    end

    it 'returns Failure for invalid age' do
      result = described_class.validate_person('Alice', -5, 'alice@example.com')
      expect(result).to be_failure
    end

    it 'returns Failure for invalid email' do
      result = described_class.validate_person('Alice', 25, 'invalid')
      expect(result).to be_failure
    end
  end

  # ===========================================================================
  # 7.7 ADT (Algebraic Data Types) with Ruby
  # ===========================================================================

  describe 'YearsActive' do
    describe 'StillActive' do
      let(:still_active) { described_class::YearsActive::StillActive.new(1981) }

      it 'stores since year' do
        expect(still_active.since).to eq(1981)
      end

      it 'checks if active during period' do
        expect(still_active.active_during?(1980, 1990)).to be true
        expect(still_active.active_during?(1970, 1980)).to be false
      end

      it 'calculates active length' do
        expect(still_active.active_length(2024)).to eq(43)
      end
    end

    describe 'ActiveBetween' do
      let(:active_between) { described_class::YearsActive::ActiveBetween.new(1968, 1980) }

      it 'stores start and end years' do
        expect(active_between.start_year).to eq(1968)
        expect(active_between.end_year).to eq(1980)
      end

      it 'checks if active during period' do
        expect(active_between.active_during?(1970, 1975)).to be true
        expect(active_between.active_during?(1981, 1990)).to be false
      end

      it 'calculates active length' do
        expect(active_between.active_length).to eq(12)
      end
    end
  end

  describe 'Artist' do
    let(:metallica) do
      described_class::Artist.new(
        name: 'Metallica',
        genre: :heavy_metal,
        origin: 'U.S.',
        years_active: described_class::YearsActive::StillActive.new(1981)
      )
    end

    let(:led_zeppelin) do
      described_class::Artist.new(
        name: 'Led Zeppelin',
        genre: :hard_rock,
        origin: 'England',
        years_active: described_class::YearsActive::ActiveBetween.new(1968, 1980)
      )
    end

    describe '.was_artist_active?' do
      it 'checks if still active artist was active during period' do
        expect(described_class.was_artist_active?(metallica, 1990, 2000)).to be true
        expect(described_class.was_artist_active?(metallica, 1970, 1980)).to be false
      end

      it 'checks if disbanded artist was active during period' do
        expect(described_class.was_artist_active?(led_zeppelin, 1970, 1975)).to be true
        expect(described_class.was_artist_active?(led_zeppelin, 1990, 2000)).to be false
      end
    end

    describe '.active_length' do
      it 'calculates active length for still active' do
        expect(described_class.active_length(metallica, 2024)).to eq(43)
      end

      it 'calculates active length for disbanded' do
        expect(described_class.active_length(led_zeppelin, 2024)).to eq(12)
      end
    end
  end

  # ===========================================================================
  # 7.8 Search Conditions
  # ===========================================================================

  describe 'SearchCondition' do
    let(:metallica) do
      described_class::Artist.new(
        name: 'Metallica',
        genre: :heavy_metal,
        origin: 'U.S.',
        years_active: described_class::YearsActive::StillActive.new(1981)
      )
    end

    let(:beatles) do
      described_class::Artist.new(
        name: 'The Beatles',
        genre: :pop,
        origin: 'England',
        years_active: described_class::YearsActive::ActiveBetween.new(1960, 1970)
      )
    end

    let(:artists) { [metallica, beatles] }

    describe 'ByGenre' do
      it 'matches artist by genre' do
        condition = described_class::SearchCondition::ByGenre.new(%i[heavy_metal hard_rock])
        expect(condition.matches?(metallica)).to be true
        expect(condition.matches?(beatles)).to be false
      end
    end

    describe 'ByOrigin' do
      it 'matches artist by origin' do
        condition = described_class::SearchCondition::ByOrigin.new(['England'])
        expect(condition.matches?(beatles)).to be true
        expect(condition.matches?(metallica)).to be false
      end
    end

    describe 'ByActiveYears' do
      it 'matches artist by active years' do
        condition = described_class::SearchCondition::ByActiveYears.new(1965, 1970)
        expect(condition.matches?(beatles)).to be true
        expect(condition.matches?(metallica)).to be false
      end
    end

    describe '.search_artists' do
      it 'filters artists by multiple conditions' do
        conditions = [
          described_class::SearchCondition::ByGenre.new(%i[heavy_metal]),
          described_class::SearchCondition::ByOrigin.new(['U.S.'])
        ]
        result = described_class.search_artists(artists, conditions)
        expect(result.map(&:name)).to eq(['Metallica'])
      end

      it 'returns empty if no match' do
        conditions = [
          described_class::SearchCondition::ByGenre.new(%i[jazz])
        ]
        result = described_class.search_artists(artists, conditions)
        expect(result).to be_empty
      end
    end
  end

  # ===========================================================================
  # 7.9 Pattern Matching
  # ===========================================================================

  describe '.describe_years_active' do
    it 'describes still active' do
      years = described_class::YearsActive::StillActive.new(1981)
      expect(described_class.describe_years_active(years)).to eq('Active since 1981')
    end

    it 'describes active between' do
      years = described_class::YearsActive::ActiveBetween.new(1968, 1980)
      expect(described_class.describe_years_active(years)).to eq('Active from 1968 to 1980')
    end
  end

  describe '.describe_payment' do
    it 'describes credit card' do
      payment = described_class::PaymentMethod::CreditCard.new('1234', '12/25')
      expect(described_class.describe_payment(payment)).to eq('Credit card ending in 1234')
    end

    it 'describes bank transfer' do
      payment = described_class::PaymentMethod::BankTransfer.new('9876')
      expect(described_class.describe_payment(payment)).to eq('Bank transfer to account 9876')
    end

    it 'describes cash' do
      payment = described_class::PaymentMethod::Cash.new
      expect(described_class.describe_payment(payment)).to eq('Cash payment')
    end
  end

  # ===========================================================================
  # 7.10 forall and exists for Result
  # ===========================================================================

  describe '.result_all?' do
    it 'returns true for Failure' do
      expect(described_class.result_all?(described_class.failure('error')) { |x| x > 10 }).to be true
    end

    it 'returns predicate result for Success' do
      expect(described_class.result_all?(described_class.success(5)) { |x| x < 10 }).to be true
      expect(described_class.result_all?(described_class.success(5)) { |x| x > 10 }).to be false
    end
  end

  describe '.result_any?' do
    it 'returns false for Failure' do
      expect(described_class.result_any?(described_class.failure('error')) { |x| x > 0 }).to be false
    end

    it 'returns predicate result for Success' do
      expect(described_class.result_any?(described_class.success(5)) { |x| x < 10 }).to be true
      expect(described_class.result_any?(described_class.success(5)) { |x| x > 10 }).to be false
    end
  end

  # ===========================================================================
  # 7.11 Collecting Errors
  # ===========================================================================

  describe '.validate_all' do
    it 'returns Success with all values if all valid' do
      validations = [
        described_class.success(1),
        described_class.success(2),
        described_class.success(3)
      ]
      result = described_class.validate_all(validations)
      expect(result).to be_success
      expect(result.value!).to eq([1, 2, 3])
    end

    it 'returns Failure with all errors if any invalid' do
      validations = [
        described_class.success(1),
        described_class.failure('error1'),
        described_class.failure('error2')
      ]
      result = described_class.validate_all(validations)
      expect(result).to be_failure
      expect(result.failure).to eq(%w[error1 error2])
    end
  end

  describe '.validate_person_collect_errors' do
    it 'returns Success for valid person' do
      result = described_class.validate_person_collect_errors('Alice', 25, 'alice@example.com')
      expect(result).to be_success
    end

    it 'collects multiple errors' do
      result = described_class.validate_person_collect_errors('', -5, 'invalid')
      expect(result).to be_failure
      expect(result.failure.size).to eq(3)
    end
  end
end
