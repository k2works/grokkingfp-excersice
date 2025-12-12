defmodule Ch02.PureFunctions do
  @moduledoc """
  第2章: 純粋関数とテスト

  純粋関数の概念を理解し、副作用を排除することの利点を学びます。
  - 純粋関数の定義と特徴
  - 参照透過性
  - テストの容易さ
  """

  # =============================================================================
  # 純粋関数の基本例
  # =============================================================================

  @doc """
  数値を1増加させる（純粋関数）

  ## Examples

      iex> Ch02.PureFunctions.increment(5)
      6

      iex> Ch02.PureFunctions.increment(0)
      1

      iex> Ch02.PureFunctions.increment(-3)
      -2

  """
  @spec increment(integer()) :: integer()
  def increment(x), do: x + 1

  @doc """
  2つの数値を加算（純粋関数）

  ## Examples

      iex> Ch02.PureFunctions.add(2, 3)
      5

  """
  @spec add(number(), number()) :: number()
  def add(a, b), do: a + b

  @doc """
  数値を2倍にする（純粋関数）

  ## Examples

      iex> Ch02.PureFunctions.double(7)
      14

  """
  @spec double(number()) :: number()
  def double(x), do: x * 2

  @doc """
  文字列の最初の文字を取得（純粋関数）

  ## Examples

      iex> Ch02.PureFunctions.get_first_character("Elixir")
      "E"

  """
  @spec get_first_character(String.t()) :: String.t() | nil
  def get_first_character(s), do: String.first(s)

  # =============================================================================
  # ワードスコア
  # =============================================================================

  @doc """
  単語のスコアを計算（文字数）

  ## Examples

      iex> Ch02.PureFunctions.word_score("Elixir")
      6

      iex> Ch02.PureFunctions.word_score("")
      0

  """
  @spec word_score(String.t()) :: non_neg_integer()
  def word_score(word), do: String.length(word)

  @doc """
  'a' を除外した単語のスコアを計算

  ## Examples

      iex> Ch02.PureFunctions.word_score_without_a("Scala")
      3

      iex> Ch02.PureFunctions.word_score_without_a("banana")
      3

      iex> Ch02.PureFunctions.word_score_without_a("xyz")
      3

  """
  @spec word_score_without_a(String.t()) :: non_neg_integer()
  def word_score_without_a(word) do
    word
    |> String.replace("a", "")
    |> String.length()
  end

  @doc """
  特定の文字を除外した単語のスコアを計算

  ## Examples

      iex> Ch02.PureFunctions.word_score_without_char("Elixir", "i")
      4

  """
  @spec word_score_without_char(String.t(), String.t()) :: non_neg_integer()
  def word_score_without_char(word, char) do
    word
    |> String.replace(char, "")
    |> String.length()
  end

  # =============================================================================
  # 参照透過性の例
  # =============================================================================

  @doc """
  参照透過性の例

  同じ入力に対して常に同じ出力を返すため、
  式をその結果で置き換えても意味が変わらない

  ## Examples

      iex> Ch02.PureFunctions.total_score("Elixir", "Scala")
      9

  """
  @spec total_score(String.t(), String.t()) :: non_neg_integer()
  def total_score(word1, word2) do
    word_score_without_a(word1) + word_score_without_a(word2)
  end

  # =============================================================================
  # 偶数・奇数判定
  # =============================================================================

  @doc """
  偶数かどうかを判定

  ## Examples

      iex> Ch02.PureFunctions.even?(0)
      true

      iex> Ch02.PureFunctions.even?(2)
      true

      iex> Ch02.PureFunctions.even?(1)
      false

      iex> Ch02.PureFunctions.even?(-2)
      true

  """
  @spec even?(integer()) :: boolean()
  def even?(n), do: rem(n, 2) == 0

  @doc """
  奇数かどうかを判定

  ## Examples

      iex> Ch02.PureFunctions.odd?(1)
      true

      iex> Ch02.PureFunctions.odd?(2)
      false

  """
  @spec odd?(integer()) :: boolean()
  def odd?(n), do: rem(n, 2) != 0

  # =============================================================================
  # 絶対値
  # =============================================================================

  @doc """
  絶対値を計算（純粋関数）

  ## Examples

      iex> Ch02.PureFunctions.absolute(5)
      5

      iex> Ch02.PureFunctions.absolute(-5)
      5

      iex> Ch02.PureFunctions.absolute(0)
      0

  """
  @spec absolute(number()) :: number()
  def absolute(n) when n >= 0, do: n
  def absolute(n), do: -n

  # =============================================================================
  # 文字列操作
  # =============================================================================

  @doc """
  文字列を逆順にする

  ## Examples

      iex> Ch02.PureFunctions.reverse_string("hello")
      "olleh"

      iex> Ch02.PureFunctions.reverse_string("")
      ""

  """
  @spec reverse_string(String.t()) :: String.t()
  def reverse_string(s), do: String.reverse(s)

  @doc """
  文字列が回文かどうかを判定

  ## Examples

      iex> Ch02.PureFunctions.palindrome?("radar")
      true

      iex> Ch02.PureFunctions.palindrome?("hello")
      false

      iex> Ch02.PureFunctions.palindrome?("")
      true

  """
  @spec palindrome?(String.t()) :: boolean()
  def palindrome?(s), do: s == String.reverse(s)

  # =============================================================================
  # リスト操作（純粋関数）
  # =============================================================================

  @doc """
  リストの長さを計算

  ## Examples

      iex> Ch02.PureFunctions.list_length([1, 2, 3])
      3

      iex> Ch02.PureFunctions.list_length([])
      0

  """
  @spec list_length([any()]) :: non_neg_integer()
  def list_length(list), do: length(list)

  @doc """
  リストの合計を計算

  ## Examples

      iex> Ch02.PureFunctions.sum([1, 2, 3, 4, 5])
      15

      iex> Ch02.PureFunctions.sum([])
      0

  """
  @spec sum([number()]) :: number()
  def sum(numbers), do: Enum.sum(numbers)

  @doc """
  リストの平均を計算

  ## Examples

      iex> Ch02.PureFunctions.average([1, 2, 3, 4, 5])
      3.0

  """
  @spec average([number()]) :: float() | nil
  def average([]), do: nil
  def average(numbers), do: Enum.sum(numbers) / length(numbers)

  @doc """
  リストの最大値を取得

  ## Examples

      iex> Ch02.PureFunctions.max_value([3, 1, 4, 1, 5, 9])
      {:ok, 9}

      iex> Ch02.PureFunctions.max_value([])
      {:error, :empty_list}

  """
  @spec max_value([number()]) :: {:ok, number()} | {:error, :empty_list}
  def max_value([]), do: {:error, :empty_list}
  def max_value(numbers), do: {:ok, Enum.max(numbers)}

  @doc """
  リストの最小値を取得

  ## Examples

      iex> Ch02.PureFunctions.min_value([3, 1, 4, 1, 5, 9])
      {:ok, 1}

      iex> Ch02.PureFunctions.min_value([])
      {:error, :empty_list}

  """
  @spec min_value([number()]) :: {:ok, number()} | {:error, :empty_list}
  def min_value([]), do: {:error, :empty_list}
  def min_value(numbers), do: {:ok, Enum.min(numbers)}
end
