# frozen_string_literal: true

# Chapter 1: Introduction to Functional Programming
#
# This module demonstrates the basic concepts of functional programming in Ruby:
# - Imperative vs Functional style
# - Basic function definitions
# - Conditional expressions
# - Predicate functions
# - Function composition

module Ch01Intro
  # =============================================================================
  # 1.1 Imperative vs Functional
  # =============================================================================

  # Imperative style: focuses on HOW (step by step)
  def self.calculate_score_imperative(word)
    score = 0
    word.each_char do |_c|
      score += 1
    end
    score
  end

  # Functional style: focuses on WHAT (declarative)
  def self.word_score(word)
    word.length
  end

  # =============================================================================
  # 1.2 Basic function definitions
  # =============================================================================

  # Increment a number by 1
  def self.increment(x)
    x + 1
  end

  # Get the first character of a string
  def self.get_first_character(s)
    s[0]
  end

  # Add two numbers
  def self.add(a, b)
    a + b
  end

  # Double a number
  def self.double(x)
    x * 2
  end

  # Create a greeting message
  def self.greet(name)
    "Hello, #{name}!"
  end

  # Convert string to uppercase
  def self.to_uppercase(s)
    s.upcase
  end

  # Reverse a string
  def self.reverse_string(s)
    s.reverse
  end

  # =============================================================================
  # 1.3 Conditional functions
  # =============================================================================

  # Return absolute value
  def self.absolute(x)
    x >= 0 ? x : -x
  end

  # Return the maximum of two values
  def self.max_value(a, b)
    a >= b ? a : b
  end

  # Return the minimum of two values
  def self.min_value(a, b)
    a <= b ? a : b
  end

  # Clamp a value between min and max
  def self.clamp(value, min_val, max_val)
    min_value(max_value(value, min_val), max_val)
  end

  # =============================================================================
  # 1.4 Predicate functions
  # =============================================================================

  # Check if a number is even
  def self.even?(n)
    (n % 2).zero?
  end

  # Check if a number is positive
  def self.positive?(n)
    n > 0
  end

  # Check if a string is empty
  def self.empty?(s)
    s.empty?
  end

  # Check if a number is negative
  def self.negative?(n)
    n < 0
  end

  # Check if a number is zero
  def self.zero?(n)
    n.zero?
  end

  # =============================================================================
  # 1.5 Constants and bounded score
  # =============================================================================

  MAX_SCORE = 100
  MIN_SCORE = 0
  DEFAULT_GREETING = 'Hello, World!'

  # Get a bounded score (between MIN_SCORE and MAX_SCORE)
  def self.get_bounded_score(score)
    clamp(score, MIN_SCORE, MAX_SCORE)
  end

  # =============================================================================
  # 1.6 Function composition
  # =============================================================================

  # Transform a string: uppercase then reverse
  def self.transform_string(s)
    reverse_string(to_uppercase(s))
  end

  # Transform a number: double then increment
  def self.transform_number(n)
    increment(double(n))
  end

  # Calculate a bounded score from a word
  def self.calculate_bounded_score(word)
    get_bounded_score(word_score(word))
  end

  # =============================================================================
  # 1.7 Higher-order function basics
  # =============================================================================

  # Apply a function twice
  def self.apply_twice(f, x)
    f.call(f.call(x))
  end

  # Compose two functions (f after g)
  def self.compose(f, g)
    ->(x) { f.call(g.call(x)) }
  end

  # Pipe a value through multiple functions
  def self.pipe(value, *functions)
    functions.reduce(value) { |acc, f| f.call(acc) }
  end
end
