defmodule Ch07.ErrorHandling do
  @moduledoc """
  第7章: {:ok, value} / {:error, reason} パターンと代数的データ型

  - Result型パターン: {:ok, value} | {:error, reason}
  - with 式による安全な関数合成
  - 代数的データ型（ADT）とパターンマッチング
  """

  # =============================================================================
  # TV番組のドメインモデル（Either版）
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

  @type result(t) :: {:ok, t} | {:error, String.t()}

  # =============================================================================
  # 文字列位置の取得
  # =============================================================================

  defp find_index(string, char) do
    case :binary.match(string, char) do
      {index, _} -> index
      :nomatch -> -1
    end
  end

  # =============================================================================
  # Result型を返す抽出関数
  # =============================================================================

  @doc """
  番組名を抽出（Result型）

  ## Examples

      iex> Ch07.ErrorHandling.extract_name("Breaking Bad (2008-2013)")
      {:ok, "Breaking Bad"}

      iex> Ch07.ErrorHandling.extract_name("(2008-2013)")
      {:error, "Can't extract name from (2008-2013)"}

  """
  @spec extract_name(String.t()) :: result(String.t())
  def extract_name(raw_show) do
    bracket_open = find_index(raw_show, "(")

    if bracket_open > 0 do
      {:ok, raw_show |> String.slice(0, bracket_open) |> String.trim()}
    else
      {:error, "Can't extract name from #{raw_show}"}
    end
  end

  @doc """
  開始年を抽出（Result型）

  ## Examples

      iex> Ch07.ErrorHandling.extract_year_start("Breaking Bad (2008-2013)")
      {:ok, 2008}

      iex> Ch07.ErrorHandling.extract_year_start("Mad Men (-2015)")
      {:error, "Can't extract start year from Mad Men (-2015)"}

      iex> Ch07.ErrorHandling.extract_year_start("The Wire (oops-2008)")
      {:error, "Can't parse 'oops' as integer"}

  """
  @spec extract_year_start(String.t()) :: result(integer())
  def extract_year_start(raw_show) do
    bracket_open = find_index(raw_show, "(")
    dash = find_index(raw_show, "-")

    with true <- bracket_open != -1 && dash > bracket_open + 1,
         year_str <- String.slice(raw_show, (bracket_open + 1)..(dash - 1)),
         {:ok, year} <- parse_int(year_str) do
      {:ok, year}
    else
      false -> {:error, "Can't extract start year from #{raw_show}"}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  終了年を抽出（Result型）

  ## Examples

      iex> Ch07.ErrorHandling.extract_year_end("Breaking Bad (2008-2013)")
      {:ok, 2013}

      iex> Ch07.ErrorHandling.extract_year_end("Stranger Things (2016-)")
      {:error, "Can't extract end year from Stranger Things (2016-)"}

  """
  @spec extract_year_end(String.t()) :: result(integer())
  def extract_year_end(raw_show) do
    dash = find_index(raw_show, "-")
    bracket_close = find_index(raw_show, ")")

    with true <- dash != -1 && bracket_close > dash + 1,
         year_str <- String.slice(raw_show, (dash + 1)..(bracket_close - 1)),
         {:ok, year} <- parse_int(year_str) do
      {:ok, year}
    else
      false -> {:error, "Can't extract end year from #{raw_show}"}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  単一年を抽出（Result型）

  ## Examples

      iex> Ch07.ErrorHandling.extract_single_year("Chernobyl (2019)")
      {:ok, 2019}

      iex> Ch07.ErrorHandling.extract_single_year("Breaking Bad (2008-2013)")
      {:error, "Can't extract single year from Breaking Bad (2008-2013)"}

  """
  @spec extract_single_year(String.t()) :: result(integer())
  def extract_single_year(raw_show) do
    dash = find_index(raw_show, "-")
    bracket_open = find_index(raw_show, "(")
    bracket_close = find_index(raw_show, ")")

    with true <- dash == -1 && bracket_open != -1 && bracket_close > bracket_open + 1,
         year_str <- String.slice(raw_show, (bracket_open + 1)..(bracket_close - 1)),
         {:ok, year} <- parse_int(year_str) do
      {:ok, year}
    else
      false -> {:error, "Can't extract single year from #{raw_show}"}
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_int(str) do
    case Integer.parse(str) do
      {value, ""} -> {:ok, value}
      _ -> {:error, "Can't parse '#{str}' as integer"}
    end
  end

  # =============================================================================
  # Result型のユーティリティ関数
  # =============================================================================

  @doc """
  失敗した場合に代替Result型を使用（orElse相当）

  ## Examples

      iex> Ch07.ErrorHandling.or_else({:ok, 1}, fn -> {:ok, 2} end)
      {:ok, 1}

      iex> Ch07.ErrorHandling.or_else({:error, "failed"}, fn -> {:ok, 2} end)
      {:ok, 2}

      iex> Ch07.ErrorHandling.or_else({:error, "first"}, fn -> {:error, "second"} end)
      {:error, "second"}

  """
  @spec or_else(result(any()), (-> result(any()))) :: result(any())
  def or_else({:ok, _} = result, _fallback), do: result
  def or_else({:error, _}, fallback), do: fallback.()

  @doc """
  Result型の値を変換（map相当）

  ## Examples

      iex> Ch07.ErrorHandling.result_map({:ok, 5}, &(&1 * 2))
      {:ok, 10}

      iex> Ch07.ErrorHandling.result_map({:error, "oops"}, &(&1 * 2))
      {:error, "oops"}

  """
  @spec result_map(result(any()), (any() -> any())) :: result(any())
  def result_map({:ok, value}, func), do: {:ok, func.(value)}
  def result_map({:error, _} = error, _func), do: error

  @doc """
  Result型をチェーン（flatMap相当）

  ## Examples

      iex> Ch07.ErrorHandling.result_flat_map({:ok, 5}, fn x -> {:ok, x * 2} end)
      {:ok, 10}

      iex> Ch07.ErrorHandling.result_flat_map({:ok, 5}, fn _x -> {:error, "failed"} end)
      {:error, "failed"}

      iex> Ch07.ErrorHandling.result_flat_map({:error, "oops"}, fn x -> {:ok, x * 2} end)
      {:error, "oops"}

  """
  @spec result_flat_map(result(any()), (any() -> result(any()))) :: result(any())
  def result_flat_map({:ok, value}, func), do: func.(value)
  def result_flat_map({:error, _} = error, _func), do: error

  # =============================================================================
  # TV番組パース（with式版）
  # =============================================================================

  @doc """
  TV番組をパース（Result型、詳細なエラーメッセージ付き）

  ## Examples

      iex> Ch07.ErrorHandling.parse_show("Breaking Bad (2008-2013)")
      {:ok, %Ch07.ErrorHandling.TvShow{title: "Breaking Bad", start: 2008, end_year: 2013}}

      iex> Ch07.ErrorHandling.parse_show("Chernobyl (2019)")
      {:ok, %Ch07.ErrorHandling.TvShow{title: "Chernobyl", start: 2019, end_year: 2019}}

      iex> Ch07.ErrorHandling.parse_show("(2019)")
      {:error, "Can't extract name from (2019)"}

      iex> Ch07.ErrorHandling.parse_show("The Wire (-)")
      {:error, "Can't extract single year from The Wire (-)"}

  """
  @spec parse_show(String.t()) :: result(TvShow.t())
  def parse_show(raw_show) do
    with {:ok, name} <- extract_name(raw_show),
         {:ok, year_start} <- or_else(extract_year_start(raw_show), fn -> extract_single_year(raw_show) end),
         {:ok, year_end} <- or_else(extract_year_end(raw_show), fn -> extract_single_year(raw_show) end) do
      {:ok, %TvShow{title: name, start: year_start, end_year: year_end}}
    end
  end

  # =============================================================================
  # リストでのResult処理戦略
  # =============================================================================

  @doc """
  複数のTV番組をパース（オールオアナッシング）

  ## Examples

      iex> raw = ["Breaking Bad (2008-2013)", "Chernobyl (2019)"]
      iex> {:ok, shows} = Ch07.ErrorHandling.parse_shows(raw)
      iex> length(shows)
      2

      iex> raw = ["Breaking Bad (2008-2013)", "Invalid"]
      iex> {:error, msg} = Ch07.ErrorHandling.parse_shows(raw)
      iex> String.contains?(msg, "Can't extract")
      true

  """
  @spec parse_shows([String.t()]) :: result([TvShow.t()])
  def parse_shows(raw_shows) do
    raw_shows
    |> Enum.map(&parse_show/1)
    |> Enum.reduce({:ok, []}, fn
      {:ok, show}, {:ok, acc} -> {:ok, acc ++ [show]}
      {:error, _} = error, {:ok, _} -> error
      _, {:error, _} = error -> error
    end)
  end

  @doc """
  複数のTV番組をパース（ベストエフォート）

  ## Examples

      iex> raw = ["Breaking Bad (2008-2013)", "Invalid", "Chernobyl (2019)"]
      iex> shows = Ch07.ErrorHandling.parse_shows_best_effort(raw)
      iex> length(shows)
      2

  """
  @spec parse_shows_best_effort([String.t()]) :: [TvShow.t()]
  def parse_shows_best_effort(raw_shows) do
    raw_shows
    |> Enum.map(&parse_show/1)
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.map(fn {:ok, show} -> show end)
  end

  # =============================================================================
  # 代数的データ型（ADT）の例：音楽アーティスト検索
  # =============================================================================

  defmodule MusicGenre do
    @moduledoc "音楽ジャンル（直和型）"
    @type t :: :heavy_metal | :hard_rock | :pop | :house | :funk | :hip_hop
  end

  defmodule YearsActive do
    @moduledoc """
    活動期間（直和型）

    - still_active: 現在も活動中（開始年のみ）
    - active_between: 過去に活動（開始年と終了年）
    """
    @type t :: {:still_active, integer()} | {:active_between, integer(), integer()}
  end

  defmodule Artist do
    @moduledoc "音楽アーティスト"
    defstruct [:name, :genre, :origin, :years_active]

    @type t :: %__MODULE__{
            name: String.t(),
            genre: Ch07.ErrorHandling.MusicGenre.t(),
            origin: String.t(),
            years_active: Ch07.ErrorHandling.YearsActive.t()
          }
  end

  @doc """
  アーティストが指定期間に活動していたか判定

  ## Examples

      iex> metallica = %Ch07.ErrorHandling.Artist{
      ...>   name: "Metallica",
      ...>   genre: :heavy_metal,
      ...>   origin: "U.S.",
      ...>   years_active: {:still_active, 1981}
      ...> }
      iex> Ch07.ErrorHandling.was_artist_active?(metallica, 2019, 2022)
      true

      iex> led_zeppelin = %Ch07.ErrorHandling.Artist{
      ...>   name: "Led Zeppelin",
      ...>   genre: :hard_rock,
      ...>   origin: "England",
      ...>   years_active: {:active_between, 1968, 1980}
      ...> }
      iex> Ch07.ErrorHandling.was_artist_active?(led_zeppelin, 1990, 2000)
      false

  """
  @spec was_artist_active?(Artist.t(), integer(), integer()) :: boolean()
  def was_artist_active?(%Artist{years_active: years_active}, year_start, year_end) do
    case years_active do
      {:still_active, since} -> since <= year_end
      {:active_between, start, active_end} -> start <= year_end && active_end >= year_start
    end
  end

  @doc """
  アーティストの活動年数を計算

  ## Examples

      iex> metallica = %Ch07.ErrorHandling.Artist{
      ...>   name: "Metallica",
      ...>   genre: :heavy_metal,
      ...>   origin: "U.S.",
      ...>   years_active: {:still_active, 1981}
      ...> }
      iex> Ch07.ErrorHandling.active_length(metallica, 2022)
      41

      iex> led_zeppelin = %Ch07.ErrorHandling.Artist{
      ...>   name: "Led Zeppelin",
      ...>   genre: :hard_rock,
      ...>   origin: "England",
      ...>   years_active: {:active_between, 1968, 1980}
      ...> }
      iex> Ch07.ErrorHandling.active_length(led_zeppelin, 2022)
      12

  """
  @spec active_length(Artist.t(), integer()) :: integer()
  def active_length(%Artist{years_active: years_active}, current_year) do
    case years_active do
      {:still_active, since} -> current_year - since
      {:active_between, start, active_end} -> active_end - start
    end
  end

  # =============================================================================
  # 検索条件（直和型）
  # =============================================================================

  defmodule SearchCondition do
    @moduledoc """
    検索条件（直和型）

    - by_genre: ジャンルで検索
    - by_origin: 出身地で検索
    - by_active_years: 活動期間で検索
    """
    @type t ::
            {:by_genre, [Ch07.ErrorHandling.MusicGenre.t()]}
            | {:by_origin, [String.t()]}
            | {:by_active_years, integer(), integer()}
  end

  @doc """
  アーティストを検索（複数条件をすべて満たすもの）

  ## Examples

      iex> artists = [
      ...>   %Ch07.ErrorHandling.Artist{name: "Metallica", genre: :heavy_metal, origin: "U.S.", years_active: {:still_active, 1981}},
      ...>   %Ch07.ErrorHandling.Artist{name: "Led Zeppelin", genre: :hard_rock, origin: "England", years_active: {:active_between, 1968, 1980}},
      ...>   %Ch07.ErrorHandling.Artist{name: "Bee Gees", genre: :pop, origin: "England", years_active: {:active_between, 1958, 2003}}
      ...> ]
      iex> conditions = [{:by_genre, [:pop]}, {:by_origin, ["England"]}]
      iex> result = Ch07.ErrorHandling.search_artists(artists, conditions)
      iex> length(result)
      1
      iex> hd(result).name
      "Bee Gees"

  """
  @spec search_artists([Artist.t()], [SearchCondition.t()]) :: [Artist.t()]
  def search_artists(artists, conditions) do
    Enum.filter(artists, fn artist ->
      Enum.all?(conditions, fn condition ->
        matches_condition?(artist, condition)
      end)
    end)
  end

  defp matches_condition?(artist, condition) do
    case condition do
      {:by_genre, genres} -> artist.genre in genres
      {:by_origin, locations} -> artist.origin in locations
      {:by_active_years, start_year, end_year} -> was_artist_active?(artist, start_year, end_year)
    end
  end

  # =============================================================================
  # Result型とnil/Optionの変換
  # =============================================================================

  @doc """
  nilをResult型に変換

  ## Examples

      iex> Ch07.ErrorHandling.nil_to_result(42, "no value")
      {:ok, 42}

      iex> Ch07.ErrorHandling.nil_to_result(nil, "no value")
      {:error, "no value"}

  """
  @spec nil_to_result(any() | nil, String.t()) :: result(any())
  def nil_to_result(nil, error_msg), do: {:error, error_msg}
  def nil_to_result(value, _error_msg), do: {:ok, value}

  @doc """
  Result型をnilに変換

  ## Examples

      iex> Ch07.ErrorHandling.result_to_nil({:ok, 42})
      42

      iex> Ch07.ErrorHandling.result_to_nil({:error, "failed"})
      nil

  """
  @spec result_to_nil(result(any())) :: any() | nil
  def result_to_nil({:ok, value}), do: value
  def result_to_nil({:error, _}), do: nil

  # =============================================================================
  # プレイリストの例（パターンマッチング）
  # =============================================================================

  defmodule Song do
    @moduledoc "楽曲"
    defstruct [:artist, :title]

    @type t :: %__MODULE__{
            artist: String.t(),
            title: String.t()
          }
  end

  defmodule PlaylistKind do
    @moduledoc """
    プレイリストの種類（直和型）

    - curated_by_user: ユーザーがキュレーション
    - based_on_artist: アーティストベース
    - based_on_genres: ジャンルベース
    """
    @type t ::
            {:curated_by_user, String.t()}
            | {:based_on_artist, String.t()}
            | {:based_on_genres, MapSet.t(MusicGenre.t())}
  end

  defmodule Playlist do
    @moduledoc "プレイリスト"
    defstruct [:name, :kind, :songs]

    @type t :: %__MODULE__{
            name: String.t(),
            kind: Ch07.ErrorHandling.PlaylistKind.t(),
            songs: [Ch07.ErrorHandling.Song.t()]
          }
  end

  @doc """
  プレイリストから条件に合う曲を収集

  ## Examples

      iex> foo_fighters = "Foo Fighters"
      iex> playlist1 = %Ch07.ErrorHandling.Playlist{
      ...>   name: "This is Foo Fighters",
      ...>   kind: {:based_on_artist, foo_fighters},
      ...>   songs: [
      ...>     %Ch07.ErrorHandling.Song{artist: foo_fighters, title: "Breakout"},
      ...>     %Ch07.ErrorHandling.Song{artist: foo_fighters, title: "Learn To Fly"}
      ...>   ]
      ...> }
      iex> songs = Ch07.ErrorHandling.gather_songs([playlist1], foo_fighters, :funk)
      iex> length(songs)
      2

  """
  @spec gather_songs([Playlist.t()], String.t(), MusicGenre.t()) :: [Song.t()]
  def gather_songs(playlists, artist, genre) do
    Enum.flat_map(playlists, fn playlist ->
      case playlist.kind do
        {:curated_by_user, _user} ->
          Enum.filter(playlist.songs, fn song -> song.artist == artist end)

        {:based_on_artist, playlist_artist} ->
          if playlist_artist == artist, do: playlist.songs, else: []

        {:based_on_genres, genres} ->
          if MapSet.member?(genres, genre), do: playlist.songs, else: []
      end
    end)
  end

  # =============================================================================
  # 練習問題：年の抽出パターン（Result型版）
  # =============================================================================

  @doc """
  単一年または終了年を抽出（Result型）

  ## Examples

      iex> Ch07.ErrorHandling.extract_single_year_or_year_end("B (2002)")
      {:ok, 2002}

      iex> Ch07.ErrorHandling.extract_single_year_or_year_end("C (-2012)")
      {:ok, 2012}

      iex> {:error, _} = Ch07.ErrorHandling.extract_single_year_or_year_end("A (1992-)")
      {:error, "Can't extract end year from A (1992-)"}

  """
  @spec extract_single_year_or_year_end(String.t()) :: result(integer())
  def extract_single_year_or_year_end(raw_show) do
    or_else(extract_single_year(raw_show), fn -> extract_year_end(raw_show) end)
  end

  @doc """
  任意の年を抽出（Result型）

  ## Examples

      iex> Ch07.ErrorHandling.extract_any_year("A (1992-)")
      {:ok, 1992}

      iex> Ch07.ErrorHandling.extract_any_year("B (2002)")
      {:ok, 2002}

      iex> Ch07.ErrorHandling.extract_any_year("C (-2012)")
      {:ok, 2012}

  """
  @spec extract_any_year(String.t()) :: result(integer())
  def extract_any_year(raw_show) do
    extract_year_start(raw_show)
    |> or_else(fn -> extract_year_end(raw_show) end)
    |> or_else(fn -> extract_single_year(raw_show) end)
  end
end
