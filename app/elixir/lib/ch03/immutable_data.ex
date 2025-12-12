defmodule Ch03.ImmutableData do
  @moduledoc """
  第3章: イミュータブルなデータ操作

  Elixir ではすべてのデータがイミュータブル（不変）です。
  データを「変更」する代わりに、新しいデータを「作成」します。
  """

  # =============================================================================
  # リストの基本操作
  # =============================================================================

  @doc """
  リストに要素を追加（末尾）

  ## Examples

      iex> Ch03.ImmutableData.append(["Apple", "Book"], "Mango")
      ["Apple", "Book", "Mango"]

  """
  @spec append([any()], any()) :: [any()]
  def append(list, element), do: list ++ [element]

  @doc """
  リストに要素を追加（先頭）

  ## Examples

      iex> Ch03.ImmutableData.prepend(["Apple", "Book"], "Mango")
      ["Mango", "Apple", "Book"]

  """
  @spec prepend([any()], any()) :: [any()]
  def prepend(list, element), do: [element | list]

  @doc """
  リストの一部を切り出し

  ## Examples

      iex> Ch03.ImmutableData.slice(["a", "b", "c", "d"], 1, 3)
      ["b", "c"]

  """
  @spec slice([any()], non_neg_integer(), non_neg_integer()) :: [any()]
  def slice(list, start, stop), do: Enum.slice(list, start, stop - start)

  @doc """
  最初の N 要素を取得

  ## Examples

      iex> Ch03.ImmutableData.first_n(["a", "b", "c", "d"], 2)
      ["a", "b"]

  """
  @spec first_n([any()], non_neg_integer()) :: [any()]
  def first_n(list, n), do: Enum.take(list, n)

  @doc """
  最後の N 要素を取得

  ## Examples

      iex> Ch03.ImmutableData.last_n(["a", "b", "c", "d"], 2)
      ["c", "d"]

  """
  @spec last_n([any()], non_neg_integer()) :: [any()]
  def last_n(list, n), do: Enum.take(list, -n)

  @doc """
  最初の N 要素を除いた残りを取得

  ## Examples

      iex> Ch03.ImmutableData.drop_first_n(["a", "b", "c", "d"], 2)
      ["c", "d"]

  """
  @spec drop_first_n([any()], non_neg_integer()) :: [any()]
  def drop_first_n(list, n), do: Enum.drop(list, n)

  # =============================================================================
  # リストの変換
  # =============================================================================

  @doc """
  最初の2要素を末尾に移動

  ## Examples

      iex> Ch03.ImmutableData.move_first_two_to_end(["a", "b", "c"])
      ["c", "a", "b"]

      iex> Ch03.ImmutableData.move_first_two_to_end(["a", "b", "c", "d"])
      ["c", "d", "a", "b"]

  """
  @spec move_first_two_to_end([any()]) :: [any()]
  def move_first_two_to_end(list) do
    first_two = Enum.take(list, 2)
    without_first_two = Enum.drop(list, 2)
    without_first_two ++ first_two
  end

  @doc """
  最後の要素の前に挿入

  ## Examples

      iex> Ch03.ImmutableData.insert_before_last(["a", "b"], "c")
      ["a", "c", "b"]

  """
  @spec insert_before_last([any()], any()) :: [any()]
  def insert_before_last(list, element) do
    last = List.last(list)
    without_last = Enum.drop(list, -1)
    without_last ++ [element, last]
  end

  @doc """
  指定位置に要素を挿入

  ## Examples

      iex> Ch03.ImmutableData.insert_at(["a", "b", "d"], 2, "c")
      ["a", "b", "c", "d"]

  """
  @spec insert_at([any()], non_neg_integer(), any()) :: [any()]
  def insert_at(list, index, element) do
    List.insert_at(list, index, element)
  end

  @doc """
  中央に要素を挿入

  ## Examples

      iex> Ch03.ImmutableData.insert_at_middle(["a", "b", "c", "d"], "X")
      ["a", "b", "X", "c", "d"]

      iex> Ch03.ImmutableData.insert_at_middle(["a", "b"], "X")
      ["a", "X", "b"]

  """
  @spec insert_at_middle([any()], any()) :: [any()]
  def insert_at_middle(list, element) do
    middle = div(length(list), 2)
    before = Enum.take(list, middle)
    after_middle = Enum.drop(list, middle)
    before ++ [element] ++ after_middle
  end

  # =============================================================================
  # 旅程の再計画
  # =============================================================================

  @doc """
  旅程に新しい都市を追加

  ## Examples

      iex> Ch03.ImmutableData.replan(["Paris", "Berlin", "Kraków"], "Vienna", "Kraków")
      ["Paris", "Berlin", "Vienna", "Kraków"]

  """
  @spec replan([String.t()], String.t(), String.t()) :: [String.t()]
  def replan(plan, new_city, before_city) do
    before_city_index = Enum.find_index(plan, &(&1 == before_city))
    cities_before = Enum.take(plan, before_city_index)
    cities_after = Enum.drop(plan, before_city_index)
    cities_before ++ [new_city] ++ cities_after
  end

  # =============================================================================
  # 文字列操作（イミュータブル）
  # =============================================================================

  @doc """
  名前を省略形に変換

  ## Examples

      iex> Ch03.ImmutableData.abbreviate("Alonzo Church")
      "A. Church"

      iex> Ch03.ImmutableData.abbreviate("A. Church")
      "A. Church"

  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(name) do
    initial = String.first(name)
    separator_index = :binary.match(name, " ")

    case separator_index do
      {pos, _} ->
        last_name = String.slice(name, (pos + 1)..-1//1)
        "#{initial}. #{last_name}"

      :nomatch ->
        name
    end
  end

  @doc """
  文字列の結合

  ## Examples

      iex> Ch03.ImmutableData.concat_strings("Hello", " World")
      "Hello World"

  """
  @spec concat_strings(String.t(), String.t()) :: String.t()
  def concat_strings(s1, s2), do: s1 <> s2

  @doc """
  文字列の一部を切り出し

  ## Examples

      iex> Ch03.ImmutableData.substring("Hello World", 0, 5)
      "Hello"

  """
  @spec substring(String.t(), non_neg_integer(), non_neg_integer()) :: String.t()
  def substring(s, start, stop), do: String.slice(s, start, stop - start)

  # =============================================================================
  # リスト vs 文字列の比較
  # =============================================================================

  @doc """
  リストを結合

  ## Examples

      iex> Ch03.ImmutableData.concat_lists(["a", "b"], ["c", "d"])
      ["a", "b", "c", "d"]

  """
  @spec concat_lists([any()], [any()]) :: [any()]
  def concat_lists(list1, list2), do: list1 ++ list2

  # =============================================================================
  # イミュータブルの実演
  # =============================================================================

  @doc """
  イミュータブルの実演：元のデータは変わらない

  ## Examples

      iex> original = [1, 2, 3]
      iex> modified = Ch03.ImmutableData.append(original, 4)
      iex> original
      [1, 2, 3]
      iex> modified
      [1, 2, 3, 4]

  """
  def demonstrate_immutability do
    original = [1, 2, 3]
    modified = append(original, 4)
    {original, modified}
  end
end
