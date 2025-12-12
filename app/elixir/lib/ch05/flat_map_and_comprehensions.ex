defmodule Ch05.FlatMapAndComprehensions do
  @moduledoc """
  第5章: flat_map とネスト構造

  - flatten: ネストしたリストを平坦化
  - flat_map: map + flatten を1つで
  - for 内包表記: ネストした変換を読みやすく
  """

  # =============================================================================
  # 書籍と映画のドメインモデル
  # =============================================================================

  defmodule Book do
    @moduledoc "書籍"
    defstruct [:title, :authors]

    @type t :: %__MODULE__{
            title: String.t(),
            authors: [String.t()]
          }
  end

  defmodule Movie do
    @moduledoc "映画"
    defstruct [:title]

    @type t :: %__MODULE__{title: String.t()}
  end

  # =============================================================================
  # flatten - ネストしたリストを平坦化
  # =============================================================================

  @doc """
  ネストしたリストを平坦化

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.flatten([[1, 2], [3], [4, 5]])
      [1, 2, 3, 4, 5]

      iex> Ch05.FlatMapAndComprehensions.flatten([["a", "b"], ["c"]])
      ["a", "b", "c"]

  """
  @spec flatten([[any()]]) :: [any()]
  def flatten(nested_list), do: List.flatten(nested_list)

  @doc """
  書籍リストから全著者を抽出

  ## Examples

      iex> books = [
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "The Hobbit", authors: ["Tolkien"]}
      ...> ]
      iex> Ch05.FlatMapAndComprehensions.get_all_authors(books)
      ["Chiusano", "Bjarnason", "Tolkien"]

  """
  @spec get_all_authors([Book.t()]) :: [String.t()]
  def get_all_authors(books) do
    books
    |> Enum.map(& &1.authors)
    |> List.flatten()
  end

  # =============================================================================
  # flat_map - map + flatten
  # =============================================================================

  @doc """
  flat_map の例

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.flat_map_example([1, 2, 3], fn i -> [i, i + 10] end)
      [1, 11, 2, 12, 3, 13]

  """
  @spec flat_map_example([any()], (any() -> [any()])) :: [any()]
  def flat_map_example(list, func), do: Enum.flat_map(list, func)

  @doc """
  書籍リストから全著者を抽出（flat_map版）

  ## Examples

      iex> books = [
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "The Hobbit", authors: ["Tolkien"]}
      ...> ]
      iex> Ch05.FlatMapAndComprehensions.get_all_authors_flat_map(books)
      ["Chiusano", "Bjarnason", "Tolkien"]

  """
  @spec get_all_authors_flat_map([Book.t()]) :: [String.t()]
  def get_all_authors_flat_map(books), do: Enum.flat_map(books, & &1.authors)

  # =============================================================================
  # flat_map によるリストサイズの変化
  # =============================================================================

  @doc """
  要素数が増える例

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.duplicate_elements([1, 2, 3])
      [1, 1, 2, 2, 3, 3]

  """
  @spec duplicate_elements([any()]) :: [any()]
  def duplicate_elements(list), do: Enum.flat_map(list, fn i -> [i, i] end)

  @doc """
  要素数が減る例（フィルタリング効果）

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.filter_even([1, 2, 3, 4, 5])
      [2, 4]

  """
  @spec filter_even([integer()]) :: [integer()]
  def filter_even(list) do
    Enum.flat_map(list, fn i ->
      if rem(i, 2) == 0, do: [i], else: []
    end)
  end

  @doc """
  空リストを返すことでフィルタリング

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.filter_by_flat_map([1, 2, 3, 4, 5], &(&1 > 3))
      [4, 5]

  """
  @spec filter_by_flat_map([any()], (any() -> boolean())) :: [any()]
  def filter_by_flat_map(list, predicate) do
    Enum.flat_map(list, fn item ->
      if predicate.(item), do: [item], else: []
    end)
  end

  # =============================================================================
  # 書籍の映画化リコメンデーション
  # =============================================================================

  @doc """
  著者の映画化作品を取得

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.book_adaptations("Tolkien")
      [
        %Ch05.FlatMapAndComprehensions.Movie{title: "An Unexpected Journey"},
        %Ch05.FlatMapAndComprehensions.Movie{title: "The Desolation of Smaug"}
      ]

      iex> Ch05.FlatMapAndComprehensions.book_adaptations("Unknown")
      []

  """
  @spec book_adaptations(String.t()) :: [Movie.t()]
  def book_adaptations("Tolkien") do
    [
      %Movie{title: "An Unexpected Journey"},
      %Movie{title: "The Desolation of Smaug"}
    ]
  end

  def book_adaptations(_author), do: []

  @doc """
  書籍に基づく映画リコメンデーション（ネストした flat_map）

  ## Examples

      iex> books = [
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "The Hobbit", authors: ["Tolkien"]}
      ...> ]
      iex> recommendations = Ch05.FlatMapAndComprehensions.get_recommendations(books)
      iex> length(recommendations)
      2
      iex> hd(recommendations)
      "You may like An Unexpected Journey, because you liked Tolkien's The Hobbit"

  """
  @spec get_recommendations([Book.t()]) :: [String.t()]
  def get_recommendations(books) do
    Enum.flat_map(books, fn book ->
      Enum.flat_map(book.authors, fn author ->
        Enum.map(book_adaptations(author), fn movie ->
          "You may like #{movie.title}, because you liked #{author}'s #{book.title}"
        end)
      end)
    end)
  end

  # =============================================================================
  # for 内包表記
  # =============================================================================

  @doc """
  for 内包表記の基本例

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.for_basic_example()
      [2, 4, 6, 8, 10]

  """
  @spec for_basic_example() :: [integer()]
  def for_basic_example do
    for i <- [1, 2, 3, 4, 5], do: i * 2
  end

  @doc """
  for 内包表記でフィルタリング

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.for_filter_example()
      [2, 4]

  """
  @spec for_filter_example() :: [integer()]
  def for_filter_example do
    for i <- [1, 2, 3, 4, 5], rem(i, 2) == 0, do: i
  end

  @doc """
  for 内包表記で複数リストを組み合わせ

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.for_combination_example()
      [{1, "a"}, {1, "b"}, {2, "a"}, {2, "b"}]

  """
  @spec for_combination_example() :: [{integer(), String.t()}]
  def for_combination_example do
    for x <- [1, 2], y <- ["a", "b"], do: {x, y}
  end

  @doc """
  for 内包表記による映画リコメンデーション

  ## Examples

      iex> books = [
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
      ...>   %Ch05.FlatMapAndComprehensions.Book{title: "The Hobbit", authors: ["Tolkien"]}
      ...> ]
      iex> recommendations = Ch05.FlatMapAndComprehensions.get_recommendations_for(books)
      iex> length(recommendations)
      2

  """
  @spec get_recommendations_for([Book.t()]) :: [String.t()]
  def get_recommendations_for(books) do
    for book <- books,
        author <- book.authors,
        movie <- book_adaptations(author) do
      "You may like #{movie.title}, because you liked #{author}'s #{book.title}"
    end
  end

  # =============================================================================
  # 円内の点の判定
  # =============================================================================

  defmodule Point do
    @moduledoc "2D座標点"
    defstruct [:x, :y]

    @type t :: %__MODULE__{x: number(), y: number()}
  end

  @doc """
  点が円の内側かを判定

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.inside?(%Ch05.FlatMapAndComprehensions.Point{x: 1, y: 1}, 2)
      true

      iex> Ch05.FlatMapAndComprehensions.inside?(%Ch05.FlatMapAndComprehensions.Point{x: 5, y: 2}, 2)
      false

  """
  @spec inside?(Point.t(), number()) :: boolean()
  def inside?(%Point{x: x, y: y}, radius) do
    radius * radius >= x * x + y * y
  end

  @doc """
  全組み合わせの判定結果

  ## Examples

      iex> points = [
      ...>   %Ch05.FlatMapAndComprehensions.Point{x: 5, y: 2},
      ...>   %Ch05.FlatMapAndComprehensions.Point{x: 1, y: 1}
      ...> ]
      iex> radiuses = [2, 1]
      iex> results = Ch05.FlatMapAndComprehensions.all_combinations(points, radiuses)
      iex> length(results)
      4

  """
  @spec all_combinations([Point.t()], [number()]) :: [String.t()]
  def all_combinations(points, radiuses) do
    for r <- radiuses,
        point <- points do
      "Point(#{point.x}, #{point.y}) is within a radius of #{r}: #{inside?(point, r)}"
    end
  end

  @doc """
  円内の点のみを抽出（ガード式使用）

  ## Examples

      iex> points = [
      ...>   %Ch05.FlatMapAndComprehensions.Point{x: 5, y: 2},
      ...>   %Ch05.FlatMapAndComprehensions.Point{x: 1, y: 1}
      ...> ]
      iex> radiuses = [2, 1]
      iex> Ch05.FlatMapAndComprehensions.inside_points(points, radiuses)
      ["Point(1, 1) is within a radius of 2"]

  """
  @spec inside_points([Point.t()], [number()]) :: [String.t()]
  def inside_points(points, radiuses) do
    for r <- radiuses,
        point <- points,
        inside?(point, r) do
      "Point(#{point.x}, #{point.y}) is within a radius of #{r}"
    end
  end

  # =============================================================================
  # for 内包表記と結果の型
  # =============================================================================

  @doc """
  for 内包表記で MapSet を生成

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.for_into_set([1, 2, 2, 3])
      MapSet.new([1, 2, 3])

  """
  @spec for_into_set([any()]) :: MapSet.t()
  def for_into_set(list) do
    for item <- list, into: MapSet.new(), do: item
  end

  @doc """
  for 内包表記で Map を生成

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.for_into_map(["a", "bb", "ccc"])
      %{"a" => 1, "bb" => 2, "ccc" => 3}

  """
  @spec for_into_map([String.t()]) :: %{String.t() => integer()}
  def for_into_map(strings) do
    for s <- strings, into: %{}, do: {s, String.length(s)}
  end

  # =============================================================================
  # ネストした数値計算の例
  # =============================================================================

  @doc """
  flat_map で3つのリストを組み合わせ

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.three_list_combinations()
      [111, 211, 121, 221, 112, 212, 122, 222]

  """
  @spec three_list_combinations() :: [integer()]
  def three_list_combinations do
    Enum.flat_map([1, 2], fn x ->
      Enum.flat_map([10, 20], fn y ->
        Enum.map([100, 200], fn z ->
          x + y + z
        end)
      end)
    end)
  end

  @doc """
  for 内包表記で3つのリストを組み合わせ

  ## Examples

      iex> Ch05.FlatMapAndComprehensions.three_list_combinations_for()
      [111, 211, 121, 221, 112, 212, 122, 222]

  """
  @spec three_list_combinations_for() :: [integer()]
  def three_list_combinations_for do
    for x <- [1, 2],
        y <- [10, 20],
        z <- [100, 200] do
      x + y + z
    end
  end
end
