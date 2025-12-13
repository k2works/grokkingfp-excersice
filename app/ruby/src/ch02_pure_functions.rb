# frozen_string_literal: true

# Chapter 2: Pure Functions
#
# This module demonstrates pure functions and their benefits:
# - Deterministic behavior
# - No side effects
# - Referential transparency
# - Easy testing

module Ch02PureFunctions
  # =============================================================================
  # 2.1 Basic pure functions
  # =============================================================================

  # Pure function: add two numbers
  def self.add(a, b)
    a + b
  end

  # Pure function: get string length
  def self.string_length(s)
    s.length
  end

  # Pure function: word score (length of word)
  def self.word_score(word)
    word.length
  end

  # =============================================================================
  # 2.2 Bonus score
  # =============================================================================

  # Calculate bonus based on score
  def self.bonus_score(score)
    if score > 100
      score + 50
    elsif score > 50
      score + 20
    else
      score
    end
  end

  # =============================================================================
  # 2.3 Shopping cart discount (pure function version)
  # =============================================================================

  # Get discount percentage based on items in cart
  # Pure function: takes items as input, returns percentage
  def self.get_discount_percentage(items)
    if items.include?('Book')
      5
    else
      0
    end
  end

  # Calculate discount amount
  def self.calculate_discount(total, percentage)
    total * percentage / 100.0
  end

  # Calculate final price after discount
  def self.calculate_final_price(total, items)
    discount_percentage = get_discount_percentage(items)
    discount = calculate_discount(total, discount_percentage)
    total - discount
  end

  # =============================================================================
  # 2.4 Tip calculation
  # =============================================================================

  # Get tip percentage based on group size
  def self.get_tip_percentage(names)
    if names.size > 5
      20
    elsif names.size > 0
      10
    else
      0
    end
  end

  # Calculate tip amount
  def self.calculate_tip(bill_amount, names)
    percentage = get_tip_percentage(names)
    bill_amount * percentage / 100.0
  end

  # =============================================================================
  # 2.5 Referential transparency examples
  # =============================================================================

  # Demonstrates referential transparency
  # The result of word_score("Scala") can always be replaced with 5
  def self.referential_transparency_example
    score1 = word_score('Scala')
    score2 = word_score('Scala')
    # score1 and score2 are always equal (both are 5)
    score1 == score2
  end

  # =============================================================================
  # 2.6 String operations (pure)
  # =============================================================================

  # Append exclamation mark
  def self.append_exclamation(s)
    "#{s}!"
  end

  # Count vowels in a string
  def self.count_vowels(s)
    s.downcase.count('aeiou')
  end

  # =============================================================================
  # 2.7 List operations (pure)
  # =============================================================================

  # Double all numbers in a list
  def self.double_all(numbers)
    numbers.map { |n| n * 2 }
  end

  # Filter positive numbers
  def self.filter_positive(numbers)
    numbers.select { |n| n > 0 }
  end

  # Find the longest string
  def self.find_longest(strings)
    strings.max_by(&:length)
  end

  # Check if all numbers are positive
  def self.all_positive?(numbers)
    numbers.all? { |n| n > 0 }
  end

  # Check if any number is negative
  def self.any_negative?(numbers)
    numbers.any? { |n| n < 0 }
  end

  # Sum all numbers
  def self.sum_list(numbers)
    numbers.sum
  end

  # Calculate average
  def self.average(numbers)
    return 0.0 if numbers.empty?

    numbers.sum.to_f / numbers.size
  end

  # =============================================================================
  # 2.8 Word score without 'a'
  # =============================================================================

  # Word score excluding letter 'a'
  def self.word_score_no_a(word)
    word.gsub(/[aA]/, '').length
  end

  # =============================================================================
  # 2.9 Higher-order functions (pure)
  # =============================================================================

  # Apply a function twice
  def self.apply_twice(f, x)
    f.call(f.call(x))
  end

  # Compose two functions
  def self.compose(f, g)
    ->(x) { f.call(g.call(x)) }
  end

  # Compose with flow (left to right)
  def self.compose_with_flow(*functions)
    ->(x) { functions.reduce(x) { |acc, f| f.call(acc) } }
  end

  # =============================================================================
  # 2.10 Timestamp formatting (pure)
  # =============================================================================

  # Format a timestamp (seconds since epoch) to a string
  # Pure function: given the same timestamp, returns the same string
  def self.format_timestamp(timestamp, format = '%Y-%m-%d %H:%M:%S')
    Time.at(timestamp).utc.strftime(format)
  end

  # =============================================================================
  # 2.11 Immutability helpers
  # =============================================================================

  # Freeze an object deeply
  def self.deep_freeze(obj)
    case obj
    when Array
      obj.each { |item| deep_freeze(item) }
      obj.freeze
    when Hash
      obj.each_value { |value| deep_freeze(value) }
      obj.freeze
    when String
      obj.freeze
    else
      obj.freeze if obj.respond_to?(:freeze)
    end
    obj
  end

  # Create an immutable copy of an array
  def self.immutable_array(*elements)
    deep_freeze(elements.dup)
  end

  # Create an immutable copy of a hash
  def self.immutable_hash(**kwargs)
    deep_freeze(kwargs.dup)
  end

  # =============================================================================
  # 2.12 Examples of impure functions (for comparison)
  # =============================================================================

  # NOTE: These are examples of IMPURE functions - do not use in production!

  # Impure: uses external state (current time)
  def self.current_time_impure
    Time.now
  end

  # Impure: uses randomness
  def self.random_number_impure
    rand
  end

  # Impure: performs I/O
  def self.print_impure(message)
    puts message
  end
end
