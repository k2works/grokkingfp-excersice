# frozen_string_literal: true

require 'dry/monads'

# Chapter 6: Error Handling with Maybe (Option)
# Maybe 型による安全なエラーハンドリング
module Ch06ErrorHandling
  include Dry::Monads[:maybe]

  # ===========================================================================
  # 6.1 Basic Maybe Operations
  # ===========================================================================

  # Wrap a value in Maybe (Some if truthy, None if nil/false)
  def self.maybe(value)
    Dry::Monads::Maybe(value)
  end

  # Create Some directly
  def self.some(value)
    Dry::Monads::Some(value)
  end

  # Create None directly
  def self.none
    Dry::Monads::None()
  end

  # ===========================================================================
  # 6.2 Safe Operations
  # ===========================================================================

  # Safe division (returns None for division by zero)
  def self.safe_divide(a, b)
    return none if b.zero?

    some(a / b)
  end

  # Safe integer parse
  def self.safe_parse_int(str)
    some(Integer(str))
  rescue ArgumentError, TypeError
    none
  end

  # Safe array access
  def self.safe_get(array, index)
    return none if index < 0 || index >= array.size

    some(array[index])
  end

  # Safe hash access
  def self.safe_fetch(hash, key)
    maybe(hash[key])
  end

  # Safe first element
  def self.safe_first(array)
    maybe(array.first)
  end

  # Safe last element
  def self.safe_last(array)
    maybe(array.last)
  end

  # ===========================================================================
  # 6.3 TV Show Parsing Example
  # ===========================================================================

  # TvShow struct
  TvShow = Struct.new(:title, :start_year, :end_year, keyword_init: true)

  # Extract show name from "Breaking Bad (2008-2013)"
  def self.extract_name(raw_show)
    bracket_open = raw_show.index('(')
    return none if bracket_open.nil? || bracket_open <= 0

    some(raw_show[0...bracket_open].strip)
  end

  # Extract start year from "Breaking Bad (2008-2013)"
  def self.extract_year_start(raw_show)
    bracket_open = raw_show.index('(')
    dash = raw_show.index('-')

    return none if bracket_open.nil? || dash.nil?
    return none if dash <= bracket_open + 1

    year_str = raw_show[(bracket_open + 1)...dash]
    safe_parse_int(year_str)
  end

  # Extract end year from "Breaking Bad (2008-2013)"
  def self.extract_year_end(raw_show)
    dash = raw_show.index('-')
    bracket_close = raw_show.index(')')

    return none if dash.nil? || bracket_close.nil?
    return none if bracket_close <= dash + 1

    year_str = raw_show[(dash + 1)...bracket_close]
    safe_parse_int(year_str)
  end

  # Extract single year from "Chernobyl (2019)"
  def self.extract_single_year(raw_show)
    dash = raw_show.index('-')
    bracket_open = raw_show.index('(')
    bracket_close = raw_show.index(')')

    # Only parse if there's no dash
    return none unless dash.nil?
    return none if bracket_open.nil? || bracket_close.nil?
    return none if bracket_close <= bracket_open + 1

    year_str = raw_show[(bracket_open + 1)...bracket_close]
    safe_parse_int(year_str)
  end

  # Parse a TV show string
  def self.parse_show(raw_show)
    name = extract_name(raw_show)
    year_start = extract_year_start(raw_show).or(extract_single_year(raw_show))
    year_end = extract_year_end(raw_show).or(extract_single_year(raw_show))

    name.bind do |n|
      year_start.bind do |ys|
        year_end.fmap do |ye|
          TvShow.new(title: n, start_year: ys, end_year: ye)
        end
      end
    end
  end

  # ===========================================================================
  # 6.4 Maybe Methods
  # ===========================================================================

  # Map: transform value if present
  def self.maybe_map(maybe_value, &fn)
    maybe_value.fmap(&fn)
  end

  # Bind/flatMap: chain operations that return Maybe
  def self.maybe_bind(maybe_value, &fn)
    maybe_value.bind(&fn)
  end

  # Or: provide fallback if None
  def self.maybe_or(maybe_value, fallback)
    maybe_value.or(fallback)
  end

  # Get value or default
  def self.get_or_else(maybe_value, default)
    maybe_value.value_or(default)
  end

  # Filter: return None if predicate fails
  def self.maybe_filter(maybe_value, &predicate)
    maybe_value.bind do |value|
      predicate.call(value) ? some(value) : none
    end
  end

  # To array (empty array for None, [value] for Some)
  def self.maybe_to_array(maybe_value)
    case maybe_value
    when Dry::Monads::Some
      [maybe_value.value!]
    else
      []
    end
  end

  # ===========================================================================
  # 6.5 Error Handling Strategies
  # ===========================================================================

  # Best-effort: parse what we can, ignore failures
  def self.parse_shows_best_effort(raw_shows)
    raw_shows
      .map { |raw| parse_show(raw) }
      .flat_map { |maybe| maybe_to_array(maybe) }
  end

  # All-or-nothing: all must succeed or return None
  def self.parse_shows_all_or_nothing(raw_shows)
    initial = some([])

    raw_shows.reduce(initial) do |acc, raw|
      parsed = parse_show(raw)
      acc.bind do |shows|
        parsed.fmap { |show| shows + [show] }
      end
    end
  end

  # ===========================================================================
  # 6.6 Practical Examples
  # ===========================================================================

  # Find user by ID
  User = Struct.new(:id, :name, :email, keyword_init: true)

  def self.find_user(users, id)
    maybe(users.find { |u| u.id == id })
  end

  # Get user's email domain
  def self.get_email_domain(email)
    at_index = email.index('@')
    return none if at_index.nil?

    some(email[(at_index + 1)..])
  end

  # Chain: find user and get email domain
  def self.get_user_email_domain(users, id)
    find_user(users, id).bind do |user|
      maybe(user.email).bind do |email|
        get_email_domain(email)
      end
    end
  end

  # Calculate average of numbers (safe for empty list)
  def self.safe_average(numbers)
    return none if numbers.empty?

    some(numbers.sum.to_f / numbers.size)
  end

  # Find maximum (safe for empty list)
  def self.safe_maximum(numbers)
    return none if numbers.empty?

    some(numbers.max)
  end

  # Find minimum (safe for empty list)
  def self.safe_minimum(numbers)
    return none if numbers.empty?

    some(numbers.min)
  end

  # ===========================================================================
  # 6.7 Combining Multiple Maybes
  # ===========================================================================

  # Add two strings as numbers
  def self.add_strings(str1, str2)
    safe_parse_int(str1).bind do |a|
      safe_parse_int(str2).fmap do |b|
        a + b
      end
    end
  end

  # Multiply two strings as numbers
  def self.multiply_strings(str1, str2)
    safe_parse_int(str1).bind do |a|
      safe_parse_int(str2).fmap do |b|
        a * b
      end
    end
  end

  # Calculate using all provided values
  def self.calculate_all(str1, str2, str3)
    safe_parse_int(str1).bind do |a|
      safe_parse_int(str2).bind do |b|
        safe_parse_int(str3).fmap do |c|
          a + b + c
        end
      end
    end
  end

  # ===========================================================================
  # 6.8 forall and exists equivalents
  # ===========================================================================

  # All: returns true if None, or if predicate is true for Some value
  def self.maybe_all?(maybe_value, &predicate)
    case maybe_value
    when Dry::Monads::Some
      predicate.call(maybe_value.value!)
    else
      true
    end
  end

  # Any: returns false if None, or predicate result for Some value
  def self.maybe_any?(maybe_value, &predicate)
    case maybe_value
    when Dry::Monads::Some
      predicate.call(maybe_value.value!)
    else
      false
    end
  end

  # Contains: check if Maybe contains specific value
  def self.maybe_contains?(maybe_value, value)
    case maybe_value
    when Dry::Monads::Some
      maybe_value.value! == value
    else
      false
    end
  end
end
