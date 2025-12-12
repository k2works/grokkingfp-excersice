defmodule Ch01.Intro do
  @moduledoc """
  第1章: 関数型プログラミング入門

  命令型プログラミングと関数型プログラミングの違いを学び、
  Elixir の基本構文を理解します。
  """

  # =============================================================================
  # 基本的な関数定義
  # =============================================================================

  @doc """
  数値を1増加させる

  ## Examples

      iex> Ch01.Intro.increment(5)
      6

  """
  @spec increment(integer()) :: integer()
  def increment(x), do: x + 1

  @doc """
  文字列の最初の文字を取得

  ## Examples

      iex> Ch01.Intro.get_first_character("Elixir")
      "E"

  """
  @spec get_first_character(String.t()) :: String.t()
  def get_first_character(s), do: String.first(s)

  @doc """
  単語のスコアを計算（文字数）

  ## Examples

      iex> Ch01.Intro.word_score("Elixir")
      6

  """
  @spec word_score(String.t()) :: non_neg_integer()
  def word_score(word), do: String.length(word)

  # =============================================================================
  # 命令型 vs 関数型の比較
  # =============================================================================

  @doc """
  命令型スタイル: ループで合計を計算

  Elixir ではミュータブルな変数がないため、
  再帰を使って命令型のループをシミュレート

  ## Examples

      iex> Ch01.Intro.sum_imperative_style([1, 2, 3, 4, 5])
      15

  """
  @spec sum_imperative_style([integer()]) :: integer()
  def sum_imperative_style(numbers) do
    do_sum(numbers, 0)
  end

  defp do_sum([], acc), do: acc
  defp do_sum([h | t], acc), do: do_sum(t, acc + h)

  @doc """
  関数型スタイル: 宣言的に合計を計算

  ## Examples

      iex> Ch01.Intro.sum_functional_style([1, 2, 3, 4, 5])
      15

  """
  @spec sum_functional_style([integer()]) :: integer()
  def sum_functional_style(numbers) do
    Enum.sum(numbers)
  end

  # =============================================================================
  # パターンマッチングの基本
  # =============================================================================

  @doc """
  タプルからデータを抽出

  ## Examples

      iex> Ch01.Intro.extract_name({:user, "Alice", 30})
      "Alice"

  """
  @spec extract_name({:user, String.t(), integer()}) :: String.t()
  def extract_name({:user, name, _age}), do: name

  @doc """
  リストの先頭要素を取得

  ## Examples

      iex> Ch01.Intro.head([1, 2, 3])
      {:ok, 1}

      iex> Ch01.Intro.head([])
      {:error, :empty_list}

  """
  @spec head([any()]) :: {:ok, any()} | {:error, :empty_list}
  def head([]), do: {:error, :empty_list}
  def head([h | _t]), do: {:ok, h}

  @doc """
  リストの末尾（先頭以外）を取得

  ## Examples

      iex> Ch01.Intro.tail([1, 2, 3])
      {:ok, [2, 3]}

      iex> Ch01.Intro.tail([])
      {:error, :empty_list}

  """
  @spec tail([any()]) :: {:ok, [any()]} | {:error, :empty_list}
  def tail([]), do: {:error, :empty_list}
  def tail([_h | t]), do: {:ok, t}

  # =============================================================================
  # パイプ演算子
  # =============================================================================

  @doc """
  パイプ演算子を使った関数の連鎖

  文字列を大文字に変換し、空白で分割し、単語数をカウント

  ## Examples

      iex> Ch01.Intro.count_words("hello world elixir")
      3

  """
  @spec count_words(String.t()) :: non_neg_integer()
  def count_words(text) do
    text
    |> String.upcase()
    |> String.split()
    |> length()
  end

  @doc """
  パイプ演算子なしの同等のコード

  ## Examples

      iex> Ch01.Intro.count_words_without_pipe("hello world elixir")
      3

  """
  @spec count_words_without_pipe(String.t()) :: non_neg_integer()
  def count_words_without_pipe(text) do
    length(String.split(String.upcase(text)))
  end

  # =============================================================================
  # 無名関数（ラムダ）
  # =============================================================================

  @doc """
  無名関数の使用例

  ## Examples

      iex> Ch01.Intro.apply_operation(5, 3, fn a, b -> a + b end)
      8

      iex> Ch01.Intro.apply_operation(5, 3, fn a, b -> a * b end)
      15

  """
  @spec apply_operation(number(), number(), (number(), number() -> number())) :: number()
  def apply_operation(a, b, operation) do
    operation.(a, b)
  end

  @doc """
  キャプチャ演算子（&）の使用例

  ## Examples

      iex> Ch01.Intro.double_all([1, 2, 3])
      [2, 4, 6]

  """
  @spec double_all([number()]) :: [number()]
  def double_all(numbers) do
    Enum.map(numbers, &(&1 * 2))
  end

  # =============================================================================
  # ガード節
  # =============================================================================

  @doc """
  ガード節による条件分岐

  ## Examples

      iex> Ch01.Intro.describe_number(5)
      "positive"

      iex> Ch01.Intro.describe_number(-3)
      "negative"

      iex> Ch01.Intro.describe_number(0)
      "zero"

  """
  @spec describe_number(number()) :: String.t()
  def describe_number(n) when n > 0, do: "positive"
  def describe_number(n) when n < 0, do: "negative"
  def describe_number(0), do: "zero"

  @doc """
  型によるガード節

  ## Examples

      iex> Ch01.Intro.describe_type("hello")
      "string"

      iex> Ch01.Intro.describe_type(42)
      "integer"

      iex> Ch01.Intro.describe_type(3.14)
      "float"

      iex> Ch01.Intro.describe_type([1, 2, 3])
      "list"

  """
  @spec describe_type(any()) :: String.t()
  def describe_type(value) when is_binary(value), do: "string"
  def describe_type(value) when is_integer(value), do: "integer"
  def describe_type(value) when is_float(value), do: "float"
  def describe_type(value) when is_list(value), do: "list"
  def describe_type(_value), do: "unknown"
end
