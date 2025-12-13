# frozen_string_literal: true

# Chapter 5: flatMap and Nested Structures
# flatMap とネスト構造
module Ch05Flatmap
  # ===========================================================================
  # 5.1 flatten - Flatten Nested Lists
  # ===========================================================================

  # Flatten one level of nesting
  def self.flatten(nested_list)
    nested_list.flatten(1)
  end

  # Flatten all levels of nesting
  def self.flatten_deep(nested_list)
    nested_list.flatten
  end

  # ===========================================================================
  # 5.2 flat_map = map + flatten
  # ===========================================================================

  # flat_map with a block
  def self.flat_map(list, &fn)
    list.flat_map(&fn)
  end

  # Duplicate each element
  def self.duplicate_each(list)
    list.flat_map { |x| [x, x] }
  end

  # Expand each number to a range
  def self.expand_to_range(numbers)
    numbers.flat_map { |n| (1..n).to_a }
  end

  # ===========================================================================
  # 5.3 Book and Author Example
  # ===========================================================================

  # Book struct
  Book = Struct.new(:title, :authors, keyword_init: true)

  # Movie struct
  Movie = Struct.new(:title, keyword_init: true)

  # Get all authors from books
  def self.all_authors(books)
    books.flat_map(&:authors)
  end

  # Get unique authors
  def self.unique_authors(books)
    all_authors(books).uniq
  end

  # ===========================================================================
  # 5.4 flatMap for Filtering
  # ===========================================================================

  # Filter using flat_map (return [x] to keep, [] to remove)
  def self.filter_with_flatmap(list, &predicate)
    list.flat_map { |x| predicate.call(x) ? [x] : [] }
  end

  # Filter even numbers using flat_map
  def self.evens_with_flatmap(numbers)
    numbers.flat_map { |n| n.even? ? [n] : [] }
  end

  # ===========================================================================
  # 5.5 flatMap Size Changes
  # ===========================================================================

  # Increase size (1 -> 2)
  def self.with_doubled(list)
    list.flat_map { |x| [x, x + 10] }
  end

  # Keep same size (1 -> 1)
  def self.doubled(list)
    list.flat_map { |x| [x * 2] }
  end

  # Decrease size (conditional)
  def self.positive_only(list)
    list.flat_map { |x| x.positive? ? [x] : [] }
  end

  # ===========================================================================
  # 5.6 Nested flatMap
  # ===========================================================================

  # Get book adaptations for an author
  def self.book_adaptations(author)
    case author
    when 'Tolkien'
      [Movie.new(title: 'An Unexpected Journey'), Movie.new(title: 'The Desolation of Smaug')]
    when 'Rowling'
      [Movie.new(title: "Harry Potter and the Sorcerer's Stone")]
    else
      []
    end
  end

  # Generate recommendations using nested flat_map
  def self.recommendations(books)
    books.flat_map do |book|
      book.authors.flat_map do |author|
        book_adaptations(author).map do |movie|
          "You may like #{movie.title}, because you liked #{author}'s #{book.title}"
        end
      end
    end
  end

  # ===========================================================================
  # 5.7 Cartesian Product (All Combinations)
  # ===========================================================================

  # Generate all combinations of two lists
  def self.cartesian_product(list1, list2)
    list1.flat_map do |a|
      list2.map do |b|
        [a, b]
      end
    end
  end

  # Generate all combinations with a block
  def self.combine(list1, list2, &fn)
    list1.flat_map do |a|
      list2.map do |b|
        fn.call(a, b)
      end
    end
  end

  # ===========================================================================
  # 5.8 Points and Circles Example
  # ===========================================================================

  # Point struct
  Point = Struct.new(:x, :y, keyword_init: true) do
    def distance_from_origin
      Math.sqrt(x * x + y * y)
    end
  end

  # Check if point is inside circle with given radius
  def self.inside?(point, radius)
    point.x * point.x + point.y * point.y <= radius * radius
  end

  # Find all points inside any of the given radiuses
  def self.points_inside_circles(points, radiuses)
    radiuses.flat_map do |r|
      points.flat_map do |point|
        inside?(point, r) ? [{ point: point, radius: r }] : []
      end
    end
  end

  # Generate report of all combinations
  def self.point_radius_report(points, radiuses)
    radiuses.flat_map do |r|
      points.map do |point|
        "Point(#{point.x}, #{point.y}) is within radius #{r}: #{inside?(point, r)}"
      end
    end
  end

  # ===========================================================================
  # 5.9 Triple Nested flatMap
  # ===========================================================================

  # Generate all triplet combinations
  def self.triplets(list1, list2, list3)
    list1.flat_map do |a|
      list2.flat_map do |b|
        list3.map do |c|
          [a, b, c]
        end
      end
    end
  end

  # Sum triplets
  def self.triplet_sums(list1, list2, list3)
    list1.flat_map do |a|
      list2.flat_map do |b|
        list3.map do |c|
          a + b + c
        end
      end
    end
  end

  # ===========================================================================
  # 5.10 Practical Examples
  # ===========================================================================

  # Get all words from sentences
  def self.all_words(sentences)
    sentences.flat_map { |s| s.split(/\s+/) }
  end

  # Get all characters from strings
  def self.all_chars(strings)
    strings.flat_map(&:chars)
  end

  # Flatten hash values
  def self.flatten_hash_values(hash)
    hash.values.flatten
  end

  # Get all values from nested hash
  def self.nested_values(hash)
    hash.flat_map do |_key, value|
      if value.is_a?(Hash)
        nested_values(value)
      elsif value.is_a?(Array)
        value
      else
        [value]
      end
    end
  end

  # ===========================================================================
  # 5.11 Monadic flatMap Pattern Preview
  # ===========================================================================

  # Safe division (returns array with result or empty)
  def self.safe_divide(a, b)
    b.zero? ? [] : [a / b]
  end

  # Chain safe operations
  def self.safe_calculation(a, b, c)
    # a / b / c safely
    safe_divide(a, b).flat_map do |result1|
      safe_divide(result1, c)
    end
  end

  # Parse integer safely (returns array)
  def self.safe_parse_int(str)
    [Integer(str)]
  rescue ArgumentError
    []
  end

  # Chain parsing and calculation
  def self.parse_and_divide(str1, str2)
    safe_parse_int(str1).flat_map do |a|
      safe_parse_int(str2).flat_map do |b|
        safe_divide(a, b)
      end
    end
  end
end
