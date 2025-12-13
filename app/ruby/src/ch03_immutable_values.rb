# frozen_string_literal: true

# Chapter 3: Immutable Data Operations
# イミュータブルなデータ操作
module Ch03ImmutableValues
  # ===========================================================================
  # 3.1 Basic List Operations
  # ===========================================================================

  # Append an element to a list (returns new list)
  def self.appended(list, element)
    list + [element]
  end

  # Append multiple elements to a list (returns new list)
  def self.appended_all(list, elements)
    list + elements
  end

  # Prepend an element to a list (returns new list)
  def self.prepended(list, element)
    [element] + list
  end

  # ===========================================================================
  # 3.2 Slicing Operations
  # ===========================================================================

  # Get first n elements
  def self.first_n(list, n)
    list.take(n)
  end

  # Get last n elements
  def self.last_n(list, n)
    list.last(n)
  end

  # Slice from start to end (end exclusive)
  def self.slice(list, start_idx, end_idx)
    list[start_idx...end_idx] || []
  end

  # Get first two elements
  def self.first_two(list)
    list.take(2)
  end

  # Get last two elements
  def self.last_two(list)
    list.last(2)
  end

  # ===========================================================================
  # 3.3 List Transformation Examples
  # ===========================================================================

  # Move first two elements to the end
  def self.moved_first_two_to_end(list)
    return list if list.size <= 2

    first_two = list.take(2)
    rest = list.drop(2)
    rest + first_two
  end

  # Insert element before the last element
  def self.inserted_before_last(list, element)
    return [element] if list.empty?

    without_last = list[0...-1]
    last_elem = list[-1..]
    without_last + [element] + last_elem
  end

  # Insert element at specific index
  def self.insert_at(list, index, element)
    before = list.take(index)
    after = list.drop(index)
    before + [element] + after
  end

  # ===========================================================================
  # 3.4 Itinerary Replanning
  # ===========================================================================

  # Replan itinerary: insert new city before specified city
  def self.replan(plan, new_city, before_city)
    index = plan.index(before_city)
    return plan if index.nil?

    before = plan.take(index)
    after = plan.drop(index)
    before + [new_city] + after
  end

  # ===========================================================================
  # 3.5 String Operations (similar to List)
  # ===========================================================================

  # Concatenate two strings
  def self.concat_strings(str1, str2)
    str1 + str2
  end

  # Substring (start to end, end exclusive)
  def self.substring(str, start_idx, end_idx)
    str[start_idx...end_idx] || ''
  end

  # ===========================================================================
  # 3.6 Name Abbreviation
  # ===========================================================================

  # Abbreviate a full name (e.g., "Alonzo Church" -> "A. Church")
  def self.abbreviate(name)
    separator_idx = name.index(' ')
    return name if separator_idx.nil?

    initial = name[0]
    last_name = name[(separator_idx + 1)..]
    "#{initial}. #{last_name}"
  end

  # ===========================================================================
  # 3.7 Shopping Cart Operations
  # ===========================================================================

  # Add item to cart (immutable)
  def self.add_to_cart(cart, item)
    cart + [item]
  end

  # Remove item from cart (immutable, removes first occurrence)
  def self.remove_from_cart(cart, item)
    index = cart.index(item)
    return cart if index.nil?

    cart.take(index) + cart.drop(index + 1)
  end

  # Replace item in cart (immutable)
  def self.replace_in_cart(cart, old_item, new_item)
    cart.map { |item| item == old_item ? new_item : item }
  end

  # ===========================================================================
  # 3.8 Immutable Data Structure Helpers
  # ===========================================================================

  # Create an immutable copy of a list
  def self.freeze_list(list)
    list.map(&:freeze).freeze
  end

  # Update element at index (returns new list)
  def self.updated_at(list, index, new_value)
    return list if index < 0 || index >= list.size

    list.take(index) + [new_value] + list.drop(index + 1)
  end

  # Remove element at index (returns new list)
  def self.removed_at(list, index)
    return list if index < 0 || index >= list.size

    list.take(index) + list.drop(index + 1)
  end

  # ===========================================================================
  # 3.9 Playlist Operations (Example Domain)
  # ===========================================================================

  # Move song to different position
  def self.move_song(playlist, from_idx, to_idx)
    return playlist if from_idx == to_idx
    return playlist if from_idx < 0 || from_idx >= playlist.size
    return playlist if to_idx < 0 || to_idx >= playlist.size

    song = playlist[from_idx]
    without_song = removed_at(playlist, from_idx)
    insert_at(without_song, to_idx, song)
  end

  # Shuffle playlist (deterministic for testing)
  def self.reverse_playlist(playlist)
    playlist.reverse
  end

  # Take first n songs
  def self.first_n_songs(playlist, n)
    playlist.take(n)
  end
end
