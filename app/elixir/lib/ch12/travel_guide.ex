defmodule Ch12.TravelGuide do
  @moduledoc """
  第12章: 実践的なアプリケーション構築

  旅行ガイドアプリケーションの実装:
  - ドメインモデルの定義
  - データアクセス層の抽象化（振る舞いとしてのインターフェース）
  - キャッシュの実装
  - エラーハンドリング
  - テスト戦略
  """

  # =============================================================================
  # ドメインモデル
  # =============================================================================

  defmodule LocationId do
    @moduledoc "位置情報 ID（Value Object）"
    defstruct [:value]

    @type t :: %__MODULE__{value: String.t()}

    @doc """
    LocationId を作成

    ## Examples

        iex> Ch12.TravelGuide.LocationId.new("Q123")
        %Ch12.TravelGuide.LocationId{value: "Q123"}

    """
    @spec new(String.t()) :: t()
    def new(value), do: %__MODULE__{value: value}
  end

  defmodule Location do
    @moduledoc "ロケーション"
    defstruct [:id, :name, :population]

    @type t :: %__MODULE__{
            id: LocationId.t(),
            name: String.t(),
            population: non_neg_integer()
          }

    @doc """
    Location を作成

    ## Examples

        iex> id = Ch12.TravelGuide.LocationId.new("Q123")
        iex> loc = Ch12.TravelGuide.Location.new(id, "Tokyo", 13_960_000)
        iex> loc.name
        "Tokyo"

    """
    @spec new(LocationId.t(), String.t(), non_neg_integer()) :: t()
    def new(id, name, population) do
      %__MODULE__{id: id, name: name, population: population}
    end
  end

  defmodule Attraction do
    @moduledoc "アトラクション（観光地）"
    defstruct [:name, :description, :location]

    @type t :: %__MODULE__{
            name: String.t(),
            description: String.t() | nil,
            location: Location.t()
          }

    @doc """
    Attraction を作成

    ## Examples

        iex> id = Ch12.TravelGuide.LocationId.new("Q123")
        iex> loc = Ch12.TravelGuide.Location.new(id, "Tokyo", 13_960_000)
        iex> attraction = Ch12.TravelGuide.Attraction.new("Tokyo Tower", "Famous landmark", loc)
        iex> attraction.name
        "Tokyo Tower"

    """
    @spec new(String.t(), String.t() | nil, Location.t()) :: t()
    def new(name, description, location) do
      %__MODULE__{name: name, description: description, location: location}
    end
  end

  defmodule MusicArtist do
    @moduledoc "ミュージックアーティスト"
    defstruct [:name, :genre]

    @type t :: %__MODULE__{
            name: String.t(),
            genre: String.t() | nil
          }

    @spec new(String.t(), String.t() | nil) :: t()
    def new(name, genre \\ nil), do: %__MODULE__{name: name, genre: genre}
  end

  defmodule Movie do
    @moduledoc "映画"
    defstruct [:name, :year]

    @type t :: %__MODULE__{
            name: String.t(),
            year: non_neg_integer() | nil
          }

    @spec new(String.t(), non_neg_integer() | nil) :: t()
    def new(name, year \\ nil), do: %__MODULE__{name: name, year: year}
  end

  defmodule Hotel do
    @moduledoc "ホテル"
    defstruct [:name, :rating, :location]

    @type t :: %__MODULE__{
            name: String.t(),
            rating: float(),
            location: Location.t()
          }

    @spec new(String.t(), float(), Location.t()) :: t()
    def new(name, rating, location) do
      %__MODULE__{name: name, rating: rating, location: location}
    end
  end

  defmodule SearchReport do
    @moduledoc "検索レポート"
    defstruct [:attractions_searched, :errors]

    @type t :: %__MODULE__{
            attractions_searched: non_neg_integer(),
            errors: [String.t()]
          }

    @doc """
    SearchReport を作成

    ## Examples

        iex> report = Ch12.TravelGuide.SearchReport.new(3, ["Network error"])
        iex> report.attractions_searched
        3

    """
    @spec new(non_neg_integer(), [String.t()]) :: t()
    def new(attractions_searched, errors \\ []) do
      %__MODULE__{attractions_searched: attractions_searched, errors: errors}
    end

    @spec empty() :: t()
    def empty, do: new(0, [])

    @spec add_error(t(), String.t()) :: t()
    def add_error(%__MODULE__{} = report, error) do
      %{report | errors: report.errors ++ [error]}
    end
  end

  defmodule TravelGuide do
    @moduledoc "旅行ガイド"
    defstruct [:attraction, :subjects, :search_report]

    @type t :: %__MODULE__{
            attraction: Attraction.t(),
            subjects: [String.t()],
            search_report: SearchReport.t()
          }

    @doc """
    TravelGuide を作成

    ## Examples

        iex> id = Ch12.TravelGuide.LocationId.new("Q123")
        iex> loc = Ch12.TravelGuide.Location.new(id, "Tokyo", 13_960_000)
        iex> attraction = Ch12.TravelGuide.Attraction.new("Tokyo Tower", nil, loc)
        iex> report = Ch12.TravelGuide.SearchReport.new(1, [])
        iex> guide = Ch12.TravelGuide.TravelGuide.new(attraction, ["Artist 1"], report)
        iex> guide.subjects
        ["Artist 1"]

    """
    @spec new(Attraction.t(), [String.t()], SearchReport.t()) :: t()
    def new(attraction, subjects, search_report) do
      %__MODULE__{
        attraction: attraction,
        subjects: subjects,
        search_report: search_report
      }
    end
  end

  # =============================================================================
  # データアクセス層（振る舞いによる抽象化）
  # =============================================================================

  defmodule DataAccess do
    @moduledoc """
    データアクセス層のインターフェース（振る舞い）

    実装モジュールはこの振る舞いを実装する必要がある。
    """

    @type ordering :: :by_name | :by_location_population

    @callback find_attractions(String.t(), ordering(), pos_integer()) ::
                {:ok, [Attraction.t()]} | {:error, String.t()}

    @callback find_artists_from_location(LocationId.t(), pos_integer()) ::
                {:ok, [MusicArtist.t()]} | {:error, String.t()}

    @callback find_movies_about_location(LocationId.t(), pos_integer()) ::
                {:ok, [Movie.t()]} | {:error, String.t()}

    @callback find_hotels_near_location(LocationId.t(), pos_integer()) ::
                {:ok, [Hotel.t()]} | {:error, String.t()}
  end

  # =============================================================================
  # テスト用のスタブ実装
  # =============================================================================

  defmodule TestDataAccess do
    @moduledoc "テスト用のスタブ実装"
    @behaviour DataAccess

    alias Ch12.TravelGuide.{LocationId, Location, Attraction, MusicArtist, Movie, Hotel}

    @test_location Location.new(
                     LocationId.new("Q123"),
                     "Test City",
                     100_000
                   )

    @impl true
    def find_attractions(name, _ordering, limit) do
      attractions =
        case name do
          "Empty" ->
            []

          "Error" ->
            # 特別なケース: エラーを返す
            nil

          _ ->
            [
              Attraction.new("#{name} Tower", "A famous tower", @test_location),
              Attraction.new("#{name} Park", "A beautiful park", @test_location),
              Attraction.new("#{name} Museum", "A great museum", @test_location)
            ]
        end

      if attractions == nil do
        {:error, "Failed to fetch attractions"}
      else
        {:ok, Enum.take(attractions, limit)}
      end
    end

    @impl true
    def find_artists_from_location(_location_id, limit) do
      artists = [
        MusicArtist.new("Test Artist 1", "Pop"),
        MusicArtist.new("Test Artist 2", "Rock"),
        MusicArtist.new("Test Artist 3", "Jazz")
      ]

      {:ok, Enum.take(artists, limit)}
    end

    @impl true
    def find_movies_about_location(_location_id, limit) do
      movies = [
        Movie.new("Test Movie 1", 2020),
        Movie.new("Test Movie 2", 2021)
      ]

      {:ok, Enum.take(movies, limit)}
    end

    @impl true
    def find_hotels_near_location(_location_id, limit) do
      hotels = [
        Hotel.new("Test Hotel 1", 4.5, @test_location),
        Hotel.new("Test Hotel 2", 4.0, @test_location)
      ]

      {:ok, Enum.take(hotels, limit)}
    end
  end

  # =============================================================================
  # エラーを返すスタブ実装
  # =============================================================================

  defmodule FailingDataAccess do
    @moduledoc "エラーを返すスタブ実装"
    @behaviour DataAccess

    alias Ch12.TravelGuide.{LocationId, Location, Attraction}

    @test_location Location.new(
                     LocationId.new("Q123"),
                     "Test City",
                     100_000
                   )

    @impl true
    def find_attractions(_name, _ordering, _limit) do
      {:ok, [Attraction.new("Test", nil, @test_location)]}
    end

    @impl true
    def find_artists_from_location(_location_id, _limit) do
      {:error, "Network error"}
    end

    @impl true
    def find_movies_about_location(_location_id, _limit) do
      {:error, "Timeout"}
    end

    @impl true
    def find_hotels_near_location(_location_id, _limit) do
      {:error, "Service unavailable"}
    end
  end

  # =============================================================================
  # キャッシュ付きデータアクセス
  # =============================================================================

  defmodule CachedDataAccess do
    @moduledoc """
    Agent を使ったキャッシュ付きデータアクセス

    ## Examples

        iex> {:ok, cache} = Ch12.TravelGuide.CachedDataAccess.start_link(Ch12.TravelGuide.TestDataAccess)
        iex> {:ok, attractions} = Ch12.TravelGuide.CachedDataAccess.find_attractions(cache, "Tokyo", :by_name, 2)
        iex> length(attractions)
        2
        iex> Agent.stop(cache)

    """

    @spec start_link(module()) :: {:ok, pid()}
    def start_link(data_access_module) do
      Agent.start_link(fn ->
        %{
          module: data_access_module,
          attractions_cache: %{},
          artists_cache: %{},
          movies_cache: %{},
          hotels_cache: %{}
        }
      end)
    end

    @spec find_attractions(pid(), String.t(), DataAccess.ordering(), pos_integer()) ::
            {:ok, [Attraction.t()]} | {:error, String.t()}
    def find_attractions(cache, name, ordering, limit) do
      key = "#{name}-#{ordering}-#{limit}"

      Agent.get_and_update(cache, fn state ->
        case Map.get(state.attractions_cache, key) do
          nil ->
            result = state.module.find_attractions(name, ordering, limit)

            case result do
              {:ok, attractions} ->
                new_cache = Map.put(state.attractions_cache, key, attractions)
                {result, %{state | attractions_cache: new_cache}}

              {:error, _} = error ->
                {error, state}
            end

          cached ->
            {{:ok, cached}, state}
        end
      end)
    end

    @spec find_artists_from_location(pid(), LocationId.t(), pos_integer()) ::
            {:ok, [MusicArtist.t()]} | {:error, String.t()}
    def find_artists_from_location(cache, location_id, limit) do
      key = "#{location_id.value}-#{limit}"

      Agent.get_and_update(cache, fn state ->
        case Map.get(state.artists_cache, key) do
          nil ->
            result = state.module.find_artists_from_location(location_id, limit)

            case result do
              {:ok, artists} ->
                new_cache = Map.put(state.artists_cache, key, artists)
                {result, %{state | artists_cache: new_cache}}

              {:error, _} = error ->
                {error, state}
            end

          cached ->
            {{:ok, cached}, state}
        end
      end)
    end

    @spec find_movies_about_location(pid(), LocationId.t(), pos_integer()) ::
            {:ok, [Movie.t()]} | {:error, String.t()}
    def find_movies_about_location(cache, location_id, limit) do
      key = "#{location_id.value}-#{limit}"

      Agent.get_and_update(cache, fn state ->
        case Map.get(state.movies_cache, key) do
          nil ->
            result = state.module.find_movies_about_location(location_id, limit)

            case result do
              {:ok, movies} ->
                new_cache = Map.put(state.movies_cache, key, movies)
                {result, %{state | movies_cache: new_cache}}

              {:error, _} = error ->
                {error, state}
            end

          cached ->
            {{:ok, cached}, state}
        end
      end)
    end

    @spec find_hotels_near_location(pid(), LocationId.t(), pos_integer()) ::
            {:ok, [Hotel.t()]} | {:error, String.t()}
    def find_hotels_near_location(cache, location_id, limit) do
      key = "#{location_id.value}-#{limit}"

      Agent.get_and_update(cache, fn state ->
        case Map.get(state.hotels_cache, key) do
          nil ->
            result = state.module.find_hotels_near_location(location_id, limit)

            case result do
              {:ok, hotels} ->
                new_cache = Map.put(state.hotels_cache, key, hotels)
                {result, %{state | hotels_cache: new_cache}}

              {:error, _} = error ->
                {error, state}
            end

          cached ->
            {{:ok, cached}, state}
        end
      end)
    end

    @spec cache_stats(pid()) :: map()
    def cache_stats(cache) do
      Agent.get(cache, fn state ->
        %{
          attractions: map_size(state.attractions_cache),
          artists: map_size(state.artists_cache),
          movies: map_size(state.movies_cache),
          hotels: map_size(state.hotels_cache)
        }
      end)
    end

    @spec clear_cache(pid()) :: :ok
    def clear_cache(cache) do
      Agent.update(cache, fn state ->
        %{
          state
          | attractions_cache: %{},
            artists_cache: %{},
            movies_cache: %{},
            hotels_cache: %{}
        }
      end)
    end
  end

  # =============================================================================
  # 旅行ガイド生成
  # =============================================================================

  @doc """
  旅行ガイドを生成（モジュールベースのデータアクセス）

  ## Examples

      iex> result = Ch12.TravelGuide.travel_guide(Ch12.TravelGuide.TestDataAccess, "Tokyo")
      iex> case result do
      ...>   {:ok, guide} -> guide.attraction.name
      ...>   _ -> "error"
      ...> end
      "Tokyo Tower"

  """
  @spec travel_guide(module(), String.t()) :: {:ok, TravelGuide.t()} | {:error, String.t()}
  def travel_guide(data_access_module, attraction_name) do
    case data_access_module.find_attractions(attraction_name, :by_location_population, 1) do
      {:ok, []} ->
        {:error, "No attractions found"}

      {:ok, [attraction | _]} ->
        build_guide(data_access_module, attraction, 1)

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  旅行ガイドを生成（キャッシュ付きデータアクセス）

  ## Examples

      iex> {:ok, cache} = Ch12.TravelGuide.CachedDataAccess.start_link(Ch12.TravelGuide.TestDataAccess)
      iex> {:ok, guide} = Ch12.TravelGuide.travel_guide_cached(cache, "Tokyo")
      iex> guide.attraction.name
      "Tokyo Tower"
      iex> Agent.stop(cache)

  """
  @spec travel_guide_cached(pid(), String.t()) :: {:ok, TravelGuide.t()} | {:error, String.t()}
  def travel_guide_cached(cache, attraction_name) do
    case CachedDataAccess.find_attractions(cache, attraction_name, :by_location_population, 1) do
      {:ok, []} ->
        {:error, "No attractions found"}

      {:ok, [attraction | _]} ->
        build_guide_cached(cache, attraction, 1)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_guide(data_access_module, attraction, attractions_searched) do
    location_id = attraction.location.id

    artists_result = data_access_module.find_artists_from_location(location_id, 2)
    movies_result = data_access_module.find_movies_about_location(location_id, 2)

    errors =
      [artists_result, movies_result]
      |> Enum.filter(&match?({:error, _}, &1))
      |> Enum.map(fn {:error, reason} -> reason end)

    artists =
      case artists_result do
        {:ok, list} -> list
        {:error, _} -> []
      end

    movies =
      case movies_result do
        {:ok, list} -> list
        {:error, _} -> []
      end

    subjects = Enum.map(artists, & &1.name) ++ Enum.map(movies, & &1.name)
    report = SearchReport.new(attractions_searched, errors)

    {:ok, TravelGuide.new(attraction, subjects, report)}
  end

  defp build_guide_cached(cache, attraction, attractions_searched) do
    location_id = attraction.location.id

    artists_result = CachedDataAccess.find_artists_from_location(cache, location_id, 2)
    movies_result = CachedDataAccess.find_movies_about_location(cache, location_id, 2)

    errors =
      [artists_result, movies_result]
      |> Enum.filter(&match?({:error, _}, &1))
      |> Enum.map(fn {:error, reason} -> reason end)

    artists =
      case artists_result do
        {:ok, list} -> list
        {:error, _} -> []
      end

    movies =
      case movies_result do
        {:ok, list} -> list
        {:error, _} -> []
      end

    subjects = Enum.map(artists, & &1.name) ++ Enum.map(movies, & &1.name)
    report = SearchReport.new(attractions_searched, errors)

    {:ok, TravelGuide.new(attraction, subjects, report)}
  end

  # =============================================================================
  # 純粋関数（テスト容易）
  # =============================================================================

  @doc """
  人気のあるロケーションをフィルタリング（純粋関数）

  ## Examples

      iex> id1 = Ch12.TravelGuide.LocationId.new("Q1")
      iex> id2 = Ch12.TravelGuide.LocationId.new("Q2")
      iex> locations = [
      ...>   Ch12.TravelGuide.Location.new(id1, "Big City", 1_000_000),
      ...>   Ch12.TravelGuide.Location.new(id2, "Small Town", 10_000)
      ...> ]
      iex> filtered = Ch12.TravelGuide.filter_popular_locations(locations, 100_000)
      iex> length(filtered)
      1
      iex> hd(filtered).name
      "Big City"

  """
  @spec filter_popular_locations([Location.t()], non_neg_integer()) :: [Location.t()]
  def filter_popular_locations(locations, min_population) do
    Enum.filter(locations, fn loc -> loc.population >= min_population end)
  end

  @doc """
  アトラクションを人口でソート（純粋関数）

  ## Examples

      iex> id1 = Ch12.TravelGuide.LocationId.new("Q1")
      iex> id2 = Ch12.TravelGuide.LocationId.new("Q2")
      iex> loc1 = Ch12.TravelGuide.Location.new(id1, "Big City", 1_000_000)
      iex> loc2 = Ch12.TravelGuide.Location.new(id2, "Small Town", 10_000)
      iex> attractions = [
      ...>   Ch12.TravelGuide.Attraction.new("A", nil, loc2),
      ...>   Ch12.TravelGuide.Attraction.new("B", nil, loc1)
      ...> ]
      iex> sorted = Ch12.TravelGuide.sort_by_population(attractions)
      iex> hd(sorted).name
      "B"

  """
  @spec sort_by_population([Attraction.t()]) :: [Attraction.t()]
  def sort_by_population(attractions) do
    Enum.sort_by(attractions, fn a -> a.location.population end, :desc)
  end

  @doc """
  サブジェクトをマージ（純粋関数）

  ## Examples

      iex> artists = [Ch12.TravelGuide.MusicArtist.new("Artist 1", nil)]
      iex> movies = [Ch12.TravelGuide.Movie.new("Movie 1", nil)]
      iex> Ch12.TravelGuide.merge_subjects(artists, movies)
      ["Artist 1", "Movie 1"]

  """
  @spec merge_subjects([MusicArtist.t()], [Movie.t()]) :: [String.t()]
  def merge_subjects(artists, movies) do
    Enum.map(artists, & &1.name) ++ Enum.map(movies, & &1.name)
  end

  @doc """
  検索結果を集計（純粋関数）

  ## Examples

      iex> results = [
      ...>   {:ok, [:a]},
      ...>   {:error, "Error 1"},
      ...>   {:ok, [:b, :c]},
      ...>   {:error, "Error 2"}
      ...> ]
      iex> {successes, errors} = Ch12.TravelGuide.aggregate_results(results)
      iex> successes
      [[:a], [:b, :c]]
      iex> errors
      ["Error 1", "Error 2"]

  """
  @spec aggregate_results([{:ok, any()} | {:error, String.t()}]) ::
          {[any()], [String.t()]}
  def aggregate_results(results) do
    Enum.reduce(results, {[], []}, fn result, {successes, errors} ->
      case result do
        {:ok, value} -> {successes ++ [value], errors}
        {:error, reason} -> {successes, errors ++ [reason]}
      end
    end)
  end

  @doc """
  ガイドにホテル情報を追加（純粋関数）

  ## Examples

      iex> id = Ch12.TravelGuide.LocationId.new("Q123")
      iex> loc = Ch12.TravelGuide.Location.new(id, "Test", 100)
      iex> attraction = Ch12.TravelGuide.Attraction.new("Test", nil, loc)
      iex> report = Ch12.TravelGuide.SearchReport.new(1, [])
      iex> guide = Ch12.TravelGuide.TravelGuide.new(attraction, ["Subject 1"], report)
      iex> hotels = [Ch12.TravelGuide.Hotel.new("Hotel 1", 4.5, loc)]
      iex> enriched = Ch12.TravelGuide.enrich_with_hotels(guide, hotels)
      iex> length(enriched.subjects)
      2

  """
  @spec enrich_with_hotels(TravelGuide.t(), [Hotel.t()]) :: TravelGuide.t()
  def enrich_with_hotels(%TravelGuide{} = guide, hotels) do
    hotel_names = Enum.map(hotels, & &1.name)
    %{guide | subjects: guide.subjects ++ hotel_names}
  end
end
