# frozen_string_literal: true

# Chapter 4: Functions as Values
# 関数を値として扱う
module Ch04FunctionsAsValues
  # ===========================================================================
  # 4.1 Higher-Order Functions: Passing Functions
  # ===========================================================================

  # Score function: count characters excluding 'a'
  def self.score(word)
    word.gsub(/a/i, '').length
  end

  # Sort words by a scoring function
  def self.sort_by_score(words, &score_fn)
    words.sort_by(&score_fn)
  end

  # Rank words by score (descending)
  def self.ranked_words(words, &score_fn)
    words.sort_by(&score_fn).reverse
  end

  # ===========================================================================
  # 4.2 map - Transform Each Element
  # ===========================================================================

  # Get length of each string
  def self.lengths(strings)
    strings.map(&:length)
  end

  # Double each number
  def self.double_all(numbers)
    numbers.map { |n| n * 2 }
  end

  # Apply a function to each element
  def self.map_with(list, &fn)
    list.map(&fn)
  end

  # ===========================================================================
  # 4.3 filter - Select Elements
  # ===========================================================================

  # Filter odd numbers
  def self.odds(numbers)
    numbers.select(&:odd?)
  end

  # Filter even numbers
  def self.evens(numbers)
    numbers.select(&:even?)
  end

  # Filter by predicate
  def self.filter_with(list, &predicate)
    list.select(&predicate)
  end

  # Filter numbers larger than n
  def self.larger_than(numbers, n)
    numbers.select { |i| i > n }
  end

  # ===========================================================================
  # 4.4 reduce/fold - Combine Elements
  # ===========================================================================

  # Sum all numbers
  def self.sum(numbers)
    numbers.reduce(0, :+)
  end

  # Find maximum
  def self.maximum(numbers)
    return nil if numbers.empty?

    numbers.reduce { |max, n| n > max ? n : max }
  end

  # Find minimum
  def self.minimum(numbers)
    return nil if numbers.empty?

    numbers.reduce { |min, n| n < min ? n : min }
  end

  # Product of all numbers
  def self.product(numbers)
    numbers.reduce(1, :*)
  end

  # Fold with custom function
  def self.fold_with(list, initial, &fn)
    list.reduce(initial, &fn)
  end

  # ===========================================================================
  # 4.5 Struct (similar to case class)
  # ===========================================================================

  # Programming Language struct
  ProgrammingLanguage = Struct.new(:name, :year, keyword_init: true) do
    def young?(threshold_year)
      year > threshold_year
    end
  end

  # Get names of programming languages
  def self.language_names(languages)
    languages.map(&:name)
  end

  # Filter young languages (after given year)
  def self.young_languages(languages, after_year)
    languages.select { |lang| lang.year > after_year }
  end

  # Sort languages by year
  def self.sort_by_year(languages)
    languages.sort_by(&:year)
  end

  # ===========================================================================
  # 4.6 Returning Functions
  # ===========================================================================

  # Returns a function that checks if a number is larger than n
  def self.larger_than_fn(n)
    ->(i) { i > n }
  end

  # Returns a function that checks if a number is divisible by n
  def self.divisible_by(n)
    ->(i) { (i % n).zero? }
  end

  # Returns a function that checks if a string contains a substring
  def self.contains_fn(substring)
    ->(s) { s.include?(substring) }
  end

  # Returns a function that multiplies by n
  def self.multiply_by(n)
    ->(x) { x * n }
  end

  # ===========================================================================
  # 4.7 Currying
  # ===========================================================================

  # Curried version of larger_than check
  def self.larger_than_curried
    ->(n) { ->(i) { i > n } }
  end

  # Curried addition
  def self.add_curried
    ->(a) { ->(b) { a + b } }
  end

  # Curried string contains
  def self.contains_curried
    ->(substring) { ->(s) { s.include?(substring) } }
  end

  # ===========================================================================
  # 4.8 Word Scoring Example
  # ===========================================================================

  # Bonus: add 5 if word contains 'c'
  def self.bonus(word)
    word.downcase.include?('c') ? 5 : 0
  end

  # Penalty: subtract 7 if word contains 's'
  def self.penalty(word)
    word.downcase.include?('s') ? 7 : 0
  end

  # Combined score with bonus
  def self.score_with_bonus(word)
    score(word) + bonus(word)
  end

  # Combined score with bonus and penalty
  def self.score_with_bonus_and_penalty(word)
    score(word) + bonus(word) - penalty(word)
  end

  # Rank words with custom scoring function
  def self.ranked_words_with_score(words, &score_fn)
    words.sort_by(&score_fn).reverse
  end

  # ===========================================================================
  # 4.9 Function Composition
  # ===========================================================================

  # Compose two functions (f after g: f(g(x)))
  def self.compose(f, g)
    ->(x) { f.call(g.call(x)) }
  end

  # Pipe (flow): apply functions left to right
  def self.pipe(value, *fns)
    fns.reduce(value) { |acc, fn| fn.call(acc) }
  end

  # andThen (g after f: g(f(x)))
  def self.and_then(f, g)
    ->(x) { g.call(f.call(x)) }
  end

  # ===========================================================================
  # 4.10 Practical Examples
  # ===========================================================================

  # Count elements matching predicate
  def self.count_where(list, &predicate)
    list.count(&predicate)
  end

  # Find first element matching predicate
  def self.find_first(list, &predicate)
    list.find(&predicate)
  end

  # Check if all elements match predicate
  def self.all_match?(list, &predicate)
    list.all?(&predicate)
  end

  # Check if any element matches predicate
  def self.any_match?(list, &predicate)
    list.any?(&predicate)
  end

  # Check if no elements match predicate
  def self.none_match?(list, &predicate)
    list.none?(&predicate)
  end

  # Partition list by predicate
  def self.partition_by(list, &predicate)
    list.partition(&predicate)
  end

  # Group by key function
  def self.group_by_fn(list, &key_fn)
    list.group_by(&key_fn)
  end

  # Take while predicate is true
  def self.take_while_fn(list, &predicate)
    list.take_while(&predicate)
  end

  # Drop while predicate is true
  def self.drop_while_fn(list, &predicate)
    list.drop_while(&predicate)
  end
end
