defmodule Ch06.NilHandling do
  @moduledoc """
  第6章: nil の安全な扱い

  - nil を安全に扱うイディオム
  - パターンマッチングによる nil チェック
  - with 式や関数合成によるフォールバック
  """

  # =============================================================================
  # TV番組のドメインモデル
  # =============================================================================

  defmodule TvShow do
    @moduledoc "TV番組"
    defstruct [:title, :start, :end_year]

    @type t :: %__MODULE__{
            title: String.t(),
            start: integer(),
            end_year: integer()
          }
  end

  # =============================================================================
  # TV番組のソートとパース
  # =============================================================================

  @doc """
  TV番組を放送期間で降順ソート

  ## Examples

      iex> shows = [
      ...>   %Ch06.NilHandling.TvShow{title: "Breaking Bad", start: 2008, end_year: 2013},
      ...>   %Ch06.NilHandling.TvShow{title: "The Wire", start: 2002, end_year: 2008},
      ...>   %Ch06.NilHandling.TvShow{title: "Mad Men", start: 2007, end_year: 2015}
      ...> ]
      iex> Ch06.NilHandling.sort_shows(shows) |> Enum.map(& &1.title)
      ["Mad Men", "The Wire", "Breaking Bad"]

  """
  @spec sort_shows([TvShow.t()]) :: [TvShow.t()]
  def sort_shows(shows) do
    shows
    |> Enum.sort_by(fn show -> show.end_year - show.start end, :desc)
  end

  # =============================================================================
  # 例外ベースのパース（問題のあるアプローチ）
  # =============================================================================

  @doc """
  例外をスローする可能性のあるパース（非推奨）

  ## Examples

      iex> Ch06.NilHandling.parse_show_unsafe!("Breaking Bad (2008-2013)")
      %Ch06.NilHandling.TvShow{title: "Breaking Bad", start: 2008, end_year: 2013}

  """
  @spec parse_show_unsafe!(String.t()) :: TvShow.t()
  def parse_show_unsafe!(raw_show) do
    bracket_open = find_index(raw_show, "(")
    bracket_close = find_index(raw_show, ")")
    dash = find_index(raw_show, "-")

    name = raw_show |> String.slice(0, bracket_open) |> String.trim()
    year_start = raw_show |> String.slice((bracket_open + 1)..(dash - 1)) |> String.to_integer()
    year_end = raw_show |> String.slice((dash + 1)..(bracket_close - 1)) |> String.to_integer()

    %TvShow{title: name, start: year_start, end_year: year_end}
  end

  defp find_index(string, char) do
    case :binary.match(string, char) do
      {index, _} -> index
      :nomatch -> -1
    end
  end

  # =============================================================================
  # nil を返す安全なパース
  # =============================================================================

  @doc """
  番組名を抽出（nil を返す可能性あり）

  ## Examples

      iex> Ch06.NilHandling.extract_name("Breaking Bad (2008-2013)")
      "Breaking Bad"

      iex> Ch06.NilHandling.extract_name("(2008-2013)")
      nil

  """
  @spec extract_name(String.t()) :: String.t() | nil
  def extract_name(raw_show) do
    bracket_open = find_index(raw_show, "(")

    if bracket_open > 0 do
      raw_show |> String.slice(0, bracket_open) |> String.trim()
    else
      nil
    end
  end

  @doc """
  開始年を抽出（nil を返す可能性あり）

  ## Examples

      iex> Ch06.NilHandling.extract_year_start("Breaking Bad (2008-2013)")
      2008

      iex> Ch06.NilHandling.extract_year_start("Mad Men (-2015)")
      nil

      iex> Ch06.NilHandling.extract_year_start("(2002- N/A ) The Wire")
      2002

  """
  @spec extract_year_start(String.t()) :: integer() | nil
  def extract_year_start(raw_show) do
    bracket_open = find_index(raw_show, "(")
    dash = find_index(raw_show, "-")

    with true <- bracket_open != -1 && dash > bracket_open + 1,
         year_str <- String.slice(raw_show, (bracket_open + 1)..(dash - 1)),
         {year, ""} <- Integer.parse(year_str) do
      year
    else
      _ -> nil
    end
  end

  @doc """
  終了年を抽出（nil を返す可能性あり）

  ## Examples

      iex> Ch06.NilHandling.extract_year_end("Breaking Bad (2008-2013)")
      2013

      iex> Ch06.NilHandling.extract_year_end("Stranger Things (2016-)")
      nil

  """
  @spec extract_year_end(String.t()) :: integer() | nil
  def extract_year_end(raw_show) do
    dash = find_index(raw_show, "-")
    bracket_close = find_index(raw_show, ")")

    with true <- dash != -1 && bracket_close > dash + 1,
         year_str <- String.slice(raw_show, (dash + 1)..(bracket_close - 1)),
         {year, ""} <- Integer.parse(year_str) do
      year
    else
      _ -> nil
    end
  end

  @doc """
  単一年を抽出（ミニシリーズ用）

  ## Examples

      iex> Ch06.NilHandling.extract_single_year("Chernobyl (2019)")
      2019

      iex> Ch06.NilHandling.extract_single_year("Breaking Bad (2008-2013)")
      nil

  """
  @spec extract_single_year(String.t()) :: integer() | nil
  def extract_single_year(raw_show) do
    dash = find_index(raw_show, "-")
    bracket_open = find_index(raw_show, "(")
    bracket_close = find_index(raw_show, ")")

    with true <- dash == -1 && bracket_open != -1 && bracket_close > bracket_open + 1,
         year_str <- String.slice(raw_show, (bracket_open + 1)..(bracket_close - 1)),
         {year, ""} <- Integer.parse(year_str) do
      year
    else
      _ -> nil
    end
  end

  # =============================================================================
  # orElse パターン（フォールバック）
  # =============================================================================

  @doc """
  値がnilなら代替値を使用（Scalaの orElse 相当）

  ## Examples

      iex> Ch06.NilHandling.or_else(nil, 42)
      42

      iex> Ch06.NilHandling.or_else(10, 42)
      10

      iex> Ch06.NilHandling.or_else(nil, fn -> 100 end)
      100

  """
  @spec or_else(any() | nil, any() | (-> any())) :: any()
  def or_else(nil, fallback) when is_function(fallback, 0), do: fallback.()
  def or_else(nil, fallback), do: fallback
  def or_else(value, _fallback), do: value

  @doc """
  TV番組をパース（nil を返す可能性あり、orElse パターン使用）

  ## Examples

      iex> Ch06.NilHandling.parse_show("Breaking Bad (2008-2013)")
      %Ch06.NilHandling.TvShow{title: "Breaking Bad", start: 2008, end_year: 2013}

      iex> Ch06.NilHandling.parse_show("Chernobyl (2019)")
      %Ch06.NilHandling.TvShow{title: "Chernobyl", start: 2019, end_year: 2019}

      iex> Ch06.NilHandling.parse_show("Invalid Show")
      nil

      iex> Ch06.NilHandling.parse_show("(2019)")
      nil

  """
  @spec parse_show(String.t()) :: TvShow.t() | nil
  def parse_show(raw_show) do
    name = extract_name(raw_show)
    year_start = or_else(extract_year_start(raw_show), fn -> extract_single_year(raw_show) end)
    year_end = or_else(extract_year_end(raw_show), fn -> extract_single_year(raw_show) end)

    if name && year_start && year_end do
      %TvShow{title: name, start: year_start, end_year: year_end}
    else
      nil
    end
  end

  # =============================================================================
  # リストでのnil処理戦略
  # =============================================================================

  @doc """
  複数のTV番組をパース（ベストエフォート：nilはスキップ）

  ## Examples

      iex> raw = ["Breaking Bad (2008-2013)", "Invalid", "Chernobyl (2019)"]
      iex> shows = Ch06.NilHandling.parse_shows_best_effort(raw)
      iex> length(shows)
      2
      iex> hd(shows).title
      "Breaking Bad"

  """
  @spec parse_shows_best_effort([String.t()]) :: [TvShow.t()]
  def parse_shows_best_effort(raw_shows) do
    raw_shows
    |> Enum.map(&parse_show/1)
    |> Enum.reject(&is_nil/1)
  end

  @doc """
  複数のTV番組をパース（オールオアナッシング：1つでも失敗したらnil）

  ## Examples

      iex> raw = ["Breaking Bad (2008-2013)", "Chernobyl (2019)"]
      iex> {:ok, shows} = Ch06.NilHandling.parse_shows_all_or_nothing(raw)
      iex> length(shows)
      2

      iex> raw = ["Breaking Bad (2008-2013)", "Invalid"]
      iex> Ch06.NilHandling.parse_shows_all_or_nothing(raw)
      nil

  """
  @spec parse_shows_all_or_nothing([String.t()]) :: {:ok, [TvShow.t()]} | nil
  def parse_shows_all_or_nothing(raw_shows) do
    parsed = Enum.map(raw_shows, &parse_show/1)

    if Enum.any?(parsed, &is_nil/1) do
      nil
    else
      {:ok, parsed}
    end
  end

  # =============================================================================
  # nil安全な関数チェーン
  # =============================================================================

  @doc """
  nil安全なmap（値がnilならnilを返す）

  ## Examples

      iex> Ch06.NilHandling.nil_map(5, &(&1 * 2))
      10

      iex> Ch06.NilHandling.nil_map(nil, &(&1 * 2))
      nil

  """
  @spec nil_map(any() | nil, (any() -> any())) :: any() | nil
  def nil_map(nil, _func), do: nil
  def nil_map(value, func), do: func.(value)

  @doc """
  nil安全なflatMap（チェーン用）

  ## Examples

      iex> Ch06.NilHandling.nil_flat_map(5, fn x -> if x > 0, do: x * 2, else: nil end)
      10

      iex> Ch06.NilHandling.nil_flat_map(nil, fn x -> x * 2 end)
      nil

  """
  @spec nil_flat_map(any() | nil, (any() -> any() | nil)) :: any() | nil
  def nil_flat_map(nil, _func), do: nil
  def nil_flat_map(value, func), do: func.(value)

  # =============================================================================
  # 練習問題：年の抽出パターン
  # =============================================================================

  @doc """
  単一年または終了年を抽出

  ## Examples

      iex> Ch06.NilHandling.extract_single_year_or_year_end("A (1992-)")
      nil

      iex> Ch06.NilHandling.extract_single_year_or_year_end("B (2002)")
      2002

      iex> Ch06.NilHandling.extract_single_year_or_year_end("C (-2012)")
      2012

  """
  @spec extract_single_year_or_year_end(String.t()) :: integer() | nil
  def extract_single_year_or_year_end(raw_show) do
    or_else(extract_single_year(raw_show), fn -> extract_year_end(raw_show) end)
  end

  @doc """
  任意の年を抽出（開始、終了、単一のいずれか）

  ## Examples

      iex> Ch06.NilHandling.extract_any_year("A (1992-)")
      1992

      iex> Ch06.NilHandling.extract_any_year("B (2002)")
      2002

      iex> Ch06.NilHandling.extract_any_year("C (-2012)")
      2012

      iex> Ch06.NilHandling.extract_any_year("E (-)")
      nil

  """
  @spec extract_any_year(String.t()) :: integer() | nil
  def extract_any_year(raw_show) do
    extract_year_start(raw_show)
    |> or_else(fn -> extract_year_end(raw_show) end)
    |> or_else(fn -> extract_single_year(raw_show) end)
  end

  @doc """
  名前が存在する場合のみ単一年を抽出

  ## Examples

      iex> Ch06.NilHandling.extract_single_year_if_name_exists("B (2002)")
      2002

      iex> Ch06.NilHandling.extract_single_year_if_name_exists("(2022)")
      nil

      iex> Ch06.NilHandling.extract_single_year_if_name_exists("A (1992-)")
      nil

  """
  @spec extract_single_year_if_name_exists(String.t()) :: integer() | nil
  def extract_single_year_if_name_exists(raw_show) do
    nil_flat_map(extract_name(raw_show), fn _name -> extract_single_year(raw_show) end)
  end

  @doc """
  名前が存在する場合のみ任意の年を抽出

  ## Examples

      iex> Ch06.NilHandling.extract_any_year_if_name_exists("A (1992-)")
      1992

      iex> Ch06.NilHandling.extract_any_year_if_name_exists("(2022)")
      nil

  """
  @spec extract_any_year_if_name_exists(String.t()) :: integer() | nil
  def extract_any_year_if_name_exists(raw_show) do
    nil_flat_map(extract_name(raw_show), fn _name -> extract_any_year(raw_show) end)
  end

  # =============================================================================
  # nil と List の変換
  # =============================================================================

  @doc """
  nilを空リストに、値を単一要素リストに変換

  ## Examples

      iex> Ch06.NilHandling.to_list(42)
      [42]

      iex> Ch06.NilHandling.to_list(nil)
      []

  """
  @spec to_list(any() | nil) :: [any()]
  def to_list(nil), do: []
  def to_list(value), do: [value]

  @doc """
  リストの最初の要素を取得（空ならnil）

  ## Examples

      iex> Ch06.NilHandling.list_head([1, 2, 3])
      1

      iex> Ch06.NilHandling.list_head([])
      nil

  """
  @spec list_head([any()]) :: any() | nil
  def list_head([]), do: nil
  def list_head([head | _]), do: head

  # =============================================================================
  # forall / exists パターン
  # =============================================================================

  @doc """
  nil または条件を満たす場合 true（forall相当）

  ## Examples

      iex> Ch06.NilHandling.nil_forall(nil, fn x -> x > 0 end)
      true

      iex> Ch06.NilHandling.nil_forall(5, fn x -> x > 0 end)
      true

      iex> Ch06.NilHandling.nil_forall(-1, fn x -> x > 0 end)
      false

  """
  @spec nil_forall(any() | nil, (any() -> boolean())) :: boolean()
  def nil_forall(nil, _predicate), do: true
  def nil_forall(value, predicate), do: predicate.(value)

  @doc """
  nil でなく条件を満たす場合 true（exists相当）

  ## Examples

      iex> Ch06.NilHandling.nil_exists(nil, fn x -> x > 0 end)
      false

      iex> Ch06.NilHandling.nil_exists(5, fn x -> x > 0 end)
      true

      iex> Ch06.NilHandling.nil_exists(-1, fn x -> x > 0 end)
      false

  """
  @spec nil_exists(any() | nil, (any() -> boolean())) :: boolean()
  def nil_exists(nil, _predicate), do: false
  def nil_exists(value, predicate), do: predicate.(value)
end
