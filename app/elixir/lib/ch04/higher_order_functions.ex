defmodule Ch04.HigherOrderFunctions do
  @moduledoc """
  第4章: 関数を値として扱う（高階関数）

  高階関数（Higher-Order Function）とは:
  1. 関数を引数として受け取る
  2. 関数を戻り値として返す
  """

  # =============================================================================
  # ワードスコアリング
  # =============================================================================

  @doc """
  単語のスコアを計算（'a' を除外）

  ## Examples

      iex> Ch04.HigherOrderFunctions.score("rust")
      4

      iex> Ch04.HigherOrderFunctions.score("java")
      2

  """
  @spec score(String.t()) :: non_neg_integer()
  def score(word) do
    word
    |> String.replace("a", "")
    |> String.length()
  end

  @doc """
  ボーナススコア（'c' を含む場合 +5）

  ## Examples

      iex> Ch04.HigherOrderFunctions.bonus("scala")
      5

      iex> Ch04.HigherOrderFunctions.bonus("rust")
      0

  """
  @spec bonus(String.t()) :: non_neg_integer()
  def bonus(word) do
    if String.contains?(word, "c"), do: 5, else: 0
  end

  @doc """
  ペナルティスコア（'s' を含む場合 -7）

  ## Examples

      iex> Ch04.HigherOrderFunctions.penalty("scala")
      7

      iex> Ch04.HigherOrderFunctions.penalty("java")
      0

  """
  @spec penalty(String.t()) :: non_neg_integer()
  def penalty(word) do
    if String.contains?(word, "s"), do: 7, else: 0
  end

  # =============================================================================
  # map - 各要素を変換
  # =============================================================================

  @doc """
  リストの各要素を変換

  ## Examples

      iex> Ch04.HigherOrderFunctions.map_example(["elixir", "rust", "ada"], &String.length/1)
      [6, 4, 3]

      iex> Ch04.HigherOrderFunctions.map_example([5, 1, 2, 4, 0], &(&1 * 2))
      [10, 2, 4, 8, 0]

  """
  @spec map_example([any()], (any() -> any())) :: [any()]
  def map_example(list, func), do: Enum.map(list, func)

  @doc """
  各要素を2倍にする

  ## Examples

      iex> Ch04.HigherOrderFunctions.double_all([1, 2, 3])
      [2, 4, 6]

  """
  @spec double_all([number()]) :: [number()]
  def double_all(numbers), do: Enum.map(numbers, &(&1 * 2))

  # =============================================================================
  # filter - 条件に合う要素を抽出
  # =============================================================================

  @doc """
  条件に合う要素を抽出

  ## Examples

      iex> Ch04.HigherOrderFunctions.filter_example([5, 1, 2, 4, 0], &(rem(&1, 2) == 1))
      [5, 1]

      iex> Ch04.HigherOrderFunctions.filter_example([5, 1, 2, 4, 0], &(&1 > 4))
      [5]

  """
  @spec filter_example([any()], (any() -> boolean())) :: [any()]
  def filter_example(list, predicate), do: Enum.filter(list, predicate)

  @doc """
  奇数のみを抽出

  ## Examples

      iex> Ch04.HigherOrderFunctions.odds([5, 1, 2, 4, 0])
      [5, 1]

  """
  @spec odds([integer()]) :: [integer()]
  def odds(numbers), do: Enum.filter(numbers, &(rem(&1, 2) == 1))

  @doc """
  偶数のみを抽出

  ## Examples

      iex> Ch04.HigherOrderFunctions.evens([5, 1, 2, 4, 0])
      [2, 4, 0]

  """
  @spec evens([integer()]) :: [integer()]
  def evens(numbers), do: Enum.filter(numbers, &(rem(&1, 2) == 0))

  # =============================================================================
  # reduce (foldLeft) - 畳み込み
  # =============================================================================

  @doc """
  リストを畳み込む

  ## Examples

      iex> Ch04.HigherOrderFunctions.reduce_example([5, 1, 2, 4, 100], 0, &(&1 + &2))
      112

  """
  @spec reduce_example([any()], any(), (any(), any() -> any())) :: any()
  def reduce_example(list, initial, func), do: Enum.reduce(list, initial, func)

  @doc """
  リストの合計を計算

  ## Examples

      iex> Ch04.HigherOrderFunctions.sum([5, 1, 2, 4, 100])
      112

  """
  @spec sum([number()]) :: number()
  def sum(numbers), do: Enum.reduce(numbers, 0, &(&1 + &2))

  @doc """
  リストの最大値を計算

  ## Examples

      iex> Ch04.HigherOrderFunctions.max_value([5, 1, 2, 4, 15])
      15

  """
  @spec max_value([number()]) :: number()
  def max_value([head | tail]) do
    Enum.reduce(tail, head, fn i, max -> if i > max, do: i, else: max end)
  end

  @doc """
  リストの積を計算

  ## Examples

      iex> Ch04.HigherOrderFunctions.product([1, 2, 3, 4])
      24

  """
  @spec product([number()]) :: number()
  def product(numbers), do: Enum.reduce(numbers, 1, &(&1 * &2))

  # =============================================================================
  # sort_by - ソート基準を関数で指定
  # =============================================================================

  @doc """
  スコアでソート

  ## Examples

      iex> Ch04.HigherOrderFunctions.sort_by_score(["rust", "java"])
      ["java", "rust"]

  """
  @spec sort_by_score([String.t()]) :: [String.t()]
  def sort_by_score(words), do: Enum.sort_by(words, &score/1)

  @doc """
  指定した関数でソート

  ## Examples

      iex> Ch04.HigherOrderFunctions.sort_by_example(["elixir", "rust", "go"], &String.length/1)
      ["go", "rust", "elixir"]

  """
  @spec sort_by_example([any()], (any() -> any())) :: [any()]
  def sort_by_example(list, key_func), do: Enum.sort_by(list, key_func)

  # =============================================================================
  # 関数を返す関数
  # =============================================================================

  @doc """
  N より大きいかを判定する関数を返す

  ## Examples

      iex> larger = Ch04.HigherOrderFunctions.larger_than(4)
      iex> larger.(5)
      true
      iex> larger.(3)
      false

  """
  @spec larger_than(number()) :: (number() -> boolean())
  def larger_than(n), do: fn i -> i > n end

  @doc """
  N で割り切れるかを判定する関数を返す

  ## Examples

      iex> divisible = Ch04.HigherOrderFunctions.divisible_by(3)
      iex> divisible.(9)
      true
      iex> divisible.(7)
      false

  """
  @spec divisible_by(number()) :: (number() -> boolean())
  def divisible_by(n), do: fn i -> rem(i, n) == 0 end

  @doc """
  larger_than を使ったフィルタリング

  ## Examples

      iex> Ch04.HigherOrderFunctions.filter_larger_than([5, 1, 2, 4, 0], 4)
      [5]

      iex> Ch04.HigherOrderFunctions.filter_larger_than([5, 1, 2, 4, 0], 1)
      [5, 2, 4]

  """
  @spec filter_larger_than([number()], number()) :: [number()]
  def filter_larger_than(list, n), do: Enum.filter(list, larger_than(n))

  # =============================================================================
  # ワードランキング
  # =============================================================================

  @doc """
  スコア関数でランキングを作成

  ## Examples

      iex> words = ["ada", "haskell", "scala", "java", "rust"]
      iex> Ch04.HigherOrderFunctions.ranked_words(&Ch04.HigherOrderFunctions.score/1, words)
      ["haskell", "rust", "scala", "java", "ada"]

  """
  @spec ranked_words((String.t() -> integer()), [String.t()]) :: [String.t()]
  def ranked_words(word_score, words) do
    words
    |> Enum.sort_by(word_score)
    |> Enum.reverse()
  end

  @doc """
  基本スコアでランキング

  ## Examples

      iex> words = ["ada", "haskell", "scala", "java", "rust"]
      iex> Ch04.HigherOrderFunctions.ranking_basic(words)
      ["haskell", "rust", "scala", "java", "ada"]

  """
  @spec ranking_basic([String.t()]) :: [String.t()]
  def ranking_basic(words), do: ranked_words(&score/1, words)

  @doc """
  ボーナス付きスコアでランキング

  ## Examples

      iex> words = ["ada", "haskell", "scala", "java", "rust"]
      iex> Ch04.HigherOrderFunctions.ranking_with_bonus(words)
      ["scala", "haskell", "rust", "java", "ada"]

  """
  @spec ranking_with_bonus([String.t()]) :: [String.t()]
  def ranking_with_bonus(words) do
    ranked_words(fn w -> score(w) + bonus(w) end, words)
  end

  @doc """
  ボーナスとペナルティ付きスコアでランキング

  ## Examples

      iex> words = ["ada", "haskell", "scala", "java", "rust"]
      iex> Ch04.HigherOrderFunctions.ranking_with_bonus_and_penalty(words)
      ["java", "scala", "ada", "haskell", "rust"]

  """
  @spec ranking_with_bonus_and_penalty([String.t()]) :: [String.t()]
  def ranking_with_bonus_and_penalty(words) do
    ranked_words(fn w -> score(w) + bonus(w) - penalty(w) end, words)
  end

  # =============================================================================
  # プログラミング言語の例
  # =============================================================================

  @doc """
  言語マップから名前を抽出

  ## Examples

      iex> languages = [%{name: "Java", year: 1995}, %{name: "Scala", year: 2004}]
      iex> Ch04.HigherOrderFunctions.language_names(languages)
      ["Java", "Scala"]

  """
  @spec language_names([%{name: String.t()}]) :: [String.t()]
  def language_names(languages), do: Enum.map(languages, & &1.name)

  @doc """
  指定年以降の言語をフィルタ

  ## Examples

      iex> languages = [%{name: "Java", year: 1995}, %{name: "Scala", year: 2004}]
      iex> Ch04.HigherOrderFunctions.languages_after(languages, 2000)
      [%{name: "Scala", year: 2004}]

  """
  @spec languages_after([%{year: integer()}], integer()) :: [%{year: integer()}]
  def languages_after(languages, year) do
    Enum.filter(languages, &(&1.year > year))
  end

  # =============================================================================
  # 条件カウント
  # =============================================================================

  @doc """
  条件を満たす要素数をカウント

  ## Examples

      iex> Ch04.HigherOrderFunctions.count_where([1, 2, 3, 4, 5], &(&1 > 3))
      2

      iex> Ch04.HigherOrderFunctions.count_where(["a", "bb", "ccc"], &(String.length(&1) > 1))
      2

  """
  @spec count_where([any()], (any() -> boolean())) :: non_neg_integer()
  def count_where(list, predicate), do: Enum.count(list, predicate)
end
