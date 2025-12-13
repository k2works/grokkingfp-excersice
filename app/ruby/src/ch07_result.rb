# frozen_string_literal: true

require 'dry/monads'

# Chapter 7: Result (Either) Type and Complex Error Handling
# Result 型と複合的なエラー処理
module Ch07Result
  include Dry::Monads[:result, :maybe]

  # ===========================================================================
  # 7.1 Basic Result Operations
  # ===========================================================================

  # Create Success (Right equivalent)
  def self.success(value)
    Dry::Monads::Success(value)
  end

  # Create Failure (Left equivalent)
  def self.failure(error)
    Dry::Monads::Failure(error)
  end

  # ===========================================================================
  # 7.2 Safe Operations with Error Messages
  # ===========================================================================

  # Safe division with error message
  def self.safe_divide(a, b)
    return failure("Division by zero: #{a} / #{b}") if b.zero?

    success(a / b)
  end

  # Safe integer parse with error message
  def self.safe_parse_int(str)
    success(Integer(str))
  rescue ArgumentError, TypeError
    failure("Cannot parse '#{str}' as integer")
  end

  # Validate non-empty string
  def self.validate_non_empty(str, field_name)
    if str.nil? || str.strip.empty?
      failure("#{field_name} cannot be empty")
    else
      success(str.strip)
    end
  end

  # ===========================================================================
  # 7.3 TV Show Parsing with Error Messages
  # ===========================================================================

  TvShow = Struct.new(:title, :start_year, :end_year, keyword_init: true)

  # Extract show name with error message
  def self.extract_name(raw_show)
    bracket_open = raw_show.index('(')

    if bracket_open.nil? || bracket_open <= 0
      failure("Cannot extract name from '#{raw_show}'")
    else
      success(raw_show[0...bracket_open].strip)
    end
  end

  # Extract start year with error message
  def self.extract_year_start(raw_show)
    bracket_open = raw_show.index('(')
    dash = raw_show.index('-')

    if bracket_open.nil? || dash.nil? || dash <= bracket_open + 1
      failure("Cannot extract start year from '#{raw_show}'")
    else
      year_str = raw_show[(bracket_open + 1)...dash]
      safe_parse_int(year_str)
    end
  end

  # Extract end year with error message
  def self.extract_year_end(raw_show)
    dash = raw_show.index('-')
    bracket_close = raw_show.index(')')

    if dash.nil? || bracket_close.nil? || bracket_close <= dash + 1
      failure("Cannot extract end year from '#{raw_show}'")
    else
      year_str = raw_show[(dash + 1)...bracket_close]
      safe_parse_int(year_str)
    end
  end

  # Extract single year with error message
  def self.extract_single_year(raw_show)
    dash = raw_show.index('-')
    bracket_open = raw_show.index('(')
    bracket_close = raw_show.index(')')

    if !dash.nil?
      failure("Cannot extract single year (contains dash) from '#{raw_show}'")
    elsif bracket_open.nil? || bracket_close.nil? || bracket_close <= bracket_open + 1
      failure("Cannot extract single year from '#{raw_show}'")
    else
      year_str = raw_show[(bracket_open + 1)...bracket_close]
      safe_parse_int(year_str)
    end
  end

  # Parse TV show with error messages
  def self.parse_show(raw_show)
    extract_name(raw_show).bind do |name|
      year_start = extract_year_start(raw_show).or(extract_single_year(raw_show))
      year_start.bind do |ys|
        year_end = extract_year_end(raw_show).or(extract_single_year(raw_show))
        year_end.fmap do |ye|
          TvShow.new(title: name, start_year: ys, end_year: ye)
        end
      end
    end
  end

  # ===========================================================================
  # 7.4 Maybe to Result Conversion
  # ===========================================================================

  # Convert Maybe to Result with error message
  def self.maybe_to_result(maybe_value, error_msg)
    case maybe_value
    when Dry::Monads::Some
      success(maybe_value.value!)
    else
      failure(error_msg)
    end
  end

  # ===========================================================================
  # 7.5 Result Methods
  # ===========================================================================

  # Map: transform success value
  def self.result_map(result, &fn)
    result.fmap(&fn)
  end

  # Bind/flatMap: chain operations that return Result
  def self.result_bind(result, &fn)
    result.bind(&fn)
  end

  # Or: provide fallback if Failure
  def self.result_or(result, fallback)
    result.or(fallback)
  end

  # Get value or default
  def self.result_value_or(result, default)
    result.value_or(default)
  end

  # Convert Result to Maybe
  def self.result_to_maybe(result)
    result.to_maybe
  end

  # Map error message
  def self.map_failure(result, &fn)
    result.or { |error| failure(fn.call(error)) }
  end

  # ===========================================================================
  # 7.6 Validation Example
  # ===========================================================================

  # Validate age
  def self.validate_age(age)
    if age.negative?
      failure('Age cannot be negative')
    elsif age > 150
      failure('Age cannot be greater than 150')
    else
      success(age)
    end
  end

  # Validate email format
  def self.validate_email(email)
    if email.nil? || email.strip.empty?
      failure('Email cannot be empty')
    elsif !email.include?('@')
      failure('Email must contain @')
    else
      success(email.strip)
    end
  end

  # Validate username
  def self.validate_username(username)
    if username.nil? || username.strip.empty?
      failure('Username cannot be empty')
    elsif username.length < 3
      failure('Username must be at least 3 characters')
    elsif username.length > 20
      failure('Username must be at most 20 characters')
    else
      success(username.strip)
    end
  end

  # Validate user registration
  Person = Struct.new(:name, :age, :email, keyword_init: true)

  def self.validate_person(name, age, email)
    validate_non_empty(name, 'Name').bind do |n|
      validate_age(age).bind do |a|
        validate_email(email).fmap do |e|
          Person.new(name: n, age: a, email: e)
        end
      end
    end
  end

  # ===========================================================================
  # 7.7 ADT (Algebraic Data Types) with Ruby
  # ===========================================================================

  # Using modules and classes to simulate ADT

  # MusicGenre as a set of symbols
  MUSIC_GENRES = %i[heavy_metal pop hard_rock jazz classical].freeze

  def self.valid_genre?(genre)
    MUSIC_GENRES.include?(genre)
  end

  # YearsActive as a discriminated union using classes
  module YearsActive
    class StillActive
      attr_reader :since

      def initialize(since)
        @since = since
      end

      def active_during?(year_start, year_end)
        since <= year_end
      end

      def active_length(current_year)
        current_year - since
      end
    end

    class ActiveBetween
      attr_reader :start_year, :end_year

      def initialize(start_year, end_year)
        @start_year = start_year
        @end_year = end_year
      end

      def active_during?(year_start, year_end)
        start_year <= year_end && end_year >= year_start
      end

      def active_length(_current_year = nil)
        end_year - start_year
      end
    end
  end

  # Artist struct
  Artist = Struct.new(:name, :genre, :origin, :years_active, keyword_init: true)

  # Check if artist was active during period
  def self.was_artist_active?(artist, year_start, year_end)
    artist.years_active.active_during?(year_start, year_end)
  end

  # Get active length
  def self.active_length(artist, current_year)
    artist.years_active.active_length(current_year)
  end

  # ===========================================================================
  # 7.8 Search Conditions (ADT example)
  # ===========================================================================

  module SearchCondition
    class ByGenre
      attr_reader :genres

      def initialize(genres)
        @genres = genres
      end

      def matches?(artist)
        genres.include?(artist.genre)
      end
    end

    class ByOrigin
      attr_reader :locations

      def initialize(locations)
        @locations = locations
      end

      def matches?(artist)
        locations.include?(artist.origin)
      end
    end

    class ByActiveYears
      attr_reader :start_year, :end_year

      def initialize(start_year, end_year)
        @start_year = start_year
        @end_year = end_year
      end

      def matches?(artist)
        artist.years_active.active_during?(start_year, end_year)
      end
    end
  end

  # Search artists with multiple conditions
  def self.search_artists(artists, conditions)
    artists.select do |artist|
      conditions.all? { |condition| condition.matches?(artist) }
    end
  end

  # ===========================================================================
  # 7.9 Pattern Matching (Ruby 3.0+)
  # ===========================================================================

  # Describe years active using pattern matching
  def self.describe_years_active(years_active)
    case years_active
    when YearsActive::StillActive
      "Active since #{years_active.since}"
    when YearsActive::ActiveBetween
      "Active from #{years_active.start_year} to #{years_active.end_year}"
    else
      'Unknown'
    end
  end

  # Describe payment method
  module PaymentMethod
    class CreditCard
      attr_reader :number, :expiry

      def initialize(number, expiry)
        @number = number
        @expiry = expiry
      end
    end

    class BankTransfer
      attr_reader :account_number

      def initialize(account_number)
        @account_number = account_number
      end
    end

    class Cash
    end
  end

  def self.describe_payment(method)
    case method
    when PaymentMethod::CreditCard
      "Credit card ending in #{method.number}"
    when PaymentMethod::BankTransfer
      "Bank transfer to account #{method.account_number}"
    when PaymentMethod::Cash
      'Cash payment'
    else
      'Unknown payment method'
    end
  end

  # ===========================================================================
  # 7.10 forall and exists for Result
  # ===========================================================================

  # All: returns true if Failure, or if predicate is true for Success value
  def self.result_all?(result, &predicate)
    case result
    when Dry::Monads::Success
      predicate.call(result.value!)
    else
      true
    end
  end

  # Any: returns false if Failure, or predicate result for Success value
  def self.result_any?(result, &predicate)
    case result
    when Dry::Monads::Success
      predicate.call(result.value!)
    else
      false
    end
  end

  # ===========================================================================
  # 7.11 Collecting Errors
  # ===========================================================================

  # Validate all and collect errors
  def self.validate_all(validations)
    errors = []
    values = []

    validations.each do |validation|
      case validation
      when Dry::Monads::Success
        values << validation.value!
      when Dry::Monads::Failure
        errors << validation.failure
      end
    end

    if errors.empty?
      success(values)
    else
      failure(errors)
    end
  end

  # Validate person and collect all errors
  def self.validate_person_collect_errors(name, age, email)
    validations = [
      validate_non_empty(name, 'Name'),
      validate_age(age),
      validate_email(email)
    ]

    validate_all(validations).fmap do |values|
      Person.new(name: values[0], age: values[1], email: values[2])
    end
  end
end
