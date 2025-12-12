defmodule Ch12.TravelGuideTest do
  use ExUnit.Case, async: true
  doctest Ch12.TravelGuide.LocationId
  doctest Ch12.TravelGuide.Location
  doctest Ch12.TravelGuide.Attraction
  doctest Ch12.TravelGuide.SearchReport
  doctest Ch12.TravelGuide.TravelGuide
  doctest Ch12.TravelGuide.CachedDataAccess
  doctest Ch12.TravelGuide

  alias Ch12.TravelGuide.{
    LocationId,
    Location,
    Attraction,
    MusicArtist,
    Movie,
    Hotel,
    SearchReport,
    TravelGuide,
    TestDataAccess,
    FailingDataAccess,
    CachedDataAccess
  }

  # =============================================================================
  # ドメインモデルのテスト
  # =============================================================================

  describe "LocationId" do
    test "値を保持" do
      id = LocationId.new("Q12345")
      assert id.value == "Q12345"
    end
  end

  describe "Location" do
    test "属性を保持" do
      id = LocationId.new("Q123")
      loc = Location.new(id, "Tokyo", 13_960_000)

      assert loc.id == id
      assert loc.name == "Tokyo"
      assert loc.population == 13_960_000
    end
  end

  describe "Attraction" do
    test "属性を保持" do
      id = LocationId.new("Q123")
      loc = Location.new(id, "Tokyo", 13_960_000)
      attraction = Attraction.new("Tokyo Tower", "Famous landmark", loc)

      assert attraction.name == "Tokyo Tower"
      assert attraction.description == "Famous landmark"
      assert attraction.location == loc
    end

    test "description は nil 可" do
      id = LocationId.new("Q123")
      loc = Location.new(id, "Tokyo", 100)
      attraction = Attraction.new("Tower", nil, loc)

      assert attraction.description == nil
    end
  end

  describe "MusicArtist" do
    test "属性を保持" do
      artist = MusicArtist.new("John Doe", "Rock")
      assert artist.name == "John Doe"
      assert artist.genre == "Rock"
    end

    test "genre はデフォルトで nil" do
      artist = MusicArtist.new("Jane Doe")
      assert artist.genre == nil
    end
  end

  describe "Movie" do
    test "属性を保持" do
      movie = Movie.new("Great Movie", 2023)
      assert movie.name == "Great Movie"
      assert movie.year == 2023
    end

    test "year はデフォルトで nil" do
      movie = Movie.new("Another Movie")
      assert movie.year == nil
    end
  end

  describe "Hotel" do
    test "属性を保持" do
      id = LocationId.new("Q123")
      loc = Location.new(id, "Tokyo", 100)
      hotel = Hotel.new("Grand Hotel", 4.5, loc)

      assert hotel.name == "Grand Hotel"
      assert hotel.rating == 4.5
      assert hotel.location == loc
    end
  end

  describe "SearchReport" do
    test "作成と属性" do
      report = SearchReport.new(5, ["Error 1", "Error 2"])
      assert report.attractions_searched == 5
      assert report.errors == ["Error 1", "Error 2"]
    end

    test "空のレポート" do
      report = SearchReport.empty()
      assert report.attractions_searched == 0
      assert report.errors == []
    end

    test "エラーを追加" do
      report = SearchReport.new(1, ["Error 1"])
      updated = SearchReport.add_error(report, "Error 2")
      assert updated.errors == ["Error 1", "Error 2"]
    end
  end

  # =============================================================================
  # TestDataAccess のテスト
  # =============================================================================

  describe "TestDataAccess" do
    test "アトラクションを検索" do
      {:ok, attractions} = TestDataAccess.find_attractions("Tokyo", :by_name, 2)
      assert length(attractions) == 2
      assert hd(attractions).name == "Tokyo Tower"
    end

    test "空の結果" do
      {:ok, attractions} = TestDataAccess.find_attractions("Empty", :by_name, 10)
      assert attractions == []
    end

    test "アーティストを検索" do
      id = LocationId.new("Q123")
      {:ok, artists} = TestDataAccess.find_artists_from_location(id, 2)
      assert length(artists) == 2
    end

    test "映画を検索" do
      id = LocationId.new("Q123")
      {:ok, movies} = TestDataAccess.find_movies_about_location(id, 2)
      assert length(movies) == 2
    end

    test "ホテルを検索" do
      id = LocationId.new("Q123")
      {:ok, hotels} = TestDataAccess.find_hotels_near_location(id, 1)
      assert length(hotels) == 1
    end
  end

  # =============================================================================
  # FailingDataAccess のテスト
  # =============================================================================

  describe "FailingDataAccess" do
    test "アトラクションは成功を返す" do
      {:ok, attractions} = FailingDataAccess.find_attractions("Test", :by_name, 1)
      assert length(attractions) == 1
    end

    test "アーティストはエラーを返す" do
      id = LocationId.new("Q123")
      result = FailingDataAccess.find_artists_from_location(id, 1)
      assert result == {:error, "Network error"}
    end

    test "映画はエラーを返す" do
      id = LocationId.new("Q123")
      result = FailingDataAccess.find_movies_about_location(id, 1)
      assert result == {:error, "Timeout"}
    end

    test "ホテルはエラーを返す" do
      id = LocationId.new("Q123")
      result = FailingDataAccess.find_hotels_near_location(id, 1)
      assert result == {:error, "Service unavailable"}
    end
  end

  # =============================================================================
  # CachedDataAccess のテスト
  # =============================================================================

  describe "CachedDataAccess" do
    test "キャッシュが機能する" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)

      # 初回アクセス
      {:ok, attractions1} = CachedDataAccess.find_attractions(cache, "Tokyo", :by_name, 2)
      stats1 = CachedDataAccess.cache_stats(cache)
      assert stats1.attractions == 1

      # 2回目アクセス（キャッシュから）
      {:ok, attractions2} = CachedDataAccess.find_attractions(cache, "Tokyo", :by_name, 2)
      stats2 = CachedDataAccess.cache_stats(cache)
      assert stats2.attractions == 1

      assert attractions1 == attractions2

      Agent.stop(cache)
    end

    test "異なるパラメータは別のキャッシュエントリ" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)

      {:ok, _} = CachedDataAccess.find_attractions(cache, "Tokyo", :by_name, 2)
      {:ok, _} = CachedDataAccess.find_attractions(cache, "Osaka", :by_name, 2)

      stats = CachedDataAccess.cache_stats(cache)
      assert stats.attractions == 2

      Agent.stop(cache)
    end

    test "キャッシュをクリア" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)

      {:ok, _} = CachedDataAccess.find_attractions(cache, "Tokyo", :by_name, 2)
      stats_before = CachedDataAccess.cache_stats(cache)
      assert stats_before.attractions == 1

      CachedDataAccess.clear_cache(cache)

      stats_after = CachedDataAccess.cache_stats(cache)
      assert stats_after.attractions == 0

      Agent.stop(cache)
    end

    test "アーティストのキャッシュ" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)
      id = LocationId.new("Q123")

      {:ok, _} = CachedDataAccess.find_artists_from_location(cache, id, 2)
      stats = CachedDataAccess.cache_stats(cache)
      assert stats.artists == 1

      Agent.stop(cache)
    end

    test "映画のキャッシュ" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)
      id = LocationId.new("Q123")

      {:ok, _} = CachedDataAccess.find_movies_about_location(cache, id, 2)
      stats = CachedDataAccess.cache_stats(cache)
      assert stats.movies == 1

      Agent.stop(cache)
    end

    test "ホテルのキャッシュ" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)
      id = LocationId.new("Q123")

      {:ok, _} = CachedDataAccess.find_hotels_near_location(cache, id, 1)
      stats = CachedDataAccess.cache_stats(cache)
      assert stats.hotels == 1

      Agent.stop(cache)
    end
  end

  # =============================================================================
  # travel_guide のテスト
  # =============================================================================

  describe "travel_guide/2" do
    test "正常な場合" do
      {:ok, guide} = Ch12.TravelGuide.travel_guide(TestDataAccess, "Tokyo")

      assert guide.attraction.name == "Tokyo Tower"
      assert length(guide.subjects) > 0
      assert guide.search_report.errors == []
    end

    test "アトラクションが見つからない場合" do
      result = Ch12.TravelGuide.travel_guide(TestDataAccess, "Empty")
      assert result == {:error, "No attractions found"}
    end

    test "一部のデータアクセスがエラーを返す場合" do
      {:ok, guide} = Ch12.TravelGuide.travel_guide(FailingDataAccess, "Test")

      # アトラクションは取得できるが、アーティストと映画はエラー
      assert guide.attraction.name == "Test"
      assert guide.subjects == []
      assert length(guide.search_report.errors) == 2
      assert "Network error" in guide.search_report.errors
      assert "Timeout" in guide.search_report.errors
    end
  end

  describe "travel_guide_cached/2" do
    test "正常な場合" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)

      {:ok, guide} = Ch12.TravelGuide.travel_guide_cached(cache, "Tokyo")
      assert guide.attraction.name == "Tokyo Tower"

      Agent.stop(cache)
    end

    test "キャッシュが使われる" do
      {:ok, cache} = CachedDataAccess.start_link(TestDataAccess)

      {:ok, _guide1} = Ch12.TravelGuide.travel_guide_cached(cache, "Tokyo")
      {:ok, guide2} = Ch12.TravelGuide.travel_guide_cached(cache, "Tokyo")

      stats = CachedDataAccess.cache_stats(cache)
      assert stats.attractions >= 1

      assert guide2.attraction.name == "Tokyo Tower"

      Agent.stop(cache)
    end
  end

  # =============================================================================
  # 純粋関数のテスト
  # =============================================================================

  describe "filter_popular_locations/2" do
    test "人口でフィルタリング" do
      id1 = LocationId.new("Q1")
      id2 = LocationId.new("Q2")
      id3 = LocationId.new("Q3")

      locations = [
        Location.new(id1, "Big City", 1_000_000),
        Location.new(id2, "Small Town", 10_000),
        Location.new(id3, "Medium City", 500_000)
      ]

      filtered = Ch12.TravelGuide.filter_popular_locations(locations, 100_000)

      assert length(filtered) == 2
      assert Enum.all?(filtered, fn loc -> loc.population >= 100_000 end)
    end

    test "空のリスト" do
      filtered = Ch12.TravelGuide.filter_popular_locations([], 100)
      assert filtered == []
    end
  end

  describe "sort_by_population/1" do
    test "人口の降順でソート" do
      id1 = LocationId.new("Q1")
      id2 = LocationId.new("Q2")
      id3 = LocationId.new("Q3")

      loc1 = Location.new(id1, "Small", 100)
      loc2 = Location.new(id2, "Big", 1000)
      loc3 = Location.new(id3, "Medium", 500)

      attractions = [
        Attraction.new("A", nil, loc1),
        Attraction.new("B", nil, loc2),
        Attraction.new("C", nil, loc3)
      ]

      sorted = Ch12.TravelGuide.sort_by_population(attractions)

      populations = Enum.map(sorted, & &1.location.population)
      assert populations == [1000, 500, 100]
    end
  end

  describe "merge_subjects/2" do
    test "アーティストと映画をマージ" do
      artists = [MusicArtist.new("Artist 1", nil), MusicArtist.new("Artist 2", nil)]
      movies = [Movie.new("Movie 1", nil)]

      subjects = Ch12.TravelGuide.merge_subjects(artists, movies)

      assert subjects == ["Artist 1", "Artist 2", "Movie 1"]
    end

    test "空のリスト" do
      subjects = Ch12.TravelGuide.merge_subjects([], [])
      assert subjects == []
    end
  end

  describe "aggregate_results/1" do
    test "成功とエラーを分離" do
      results = [
        {:ok, :a},
        {:error, "Error 1"},
        {:ok, :b},
        {:error, "Error 2"},
        {:ok, :c}
      ]

      {successes, errors} = Ch12.TravelGuide.aggregate_results(results)

      assert successes == [:a, :b, :c]
      assert errors == ["Error 1", "Error 2"]
    end

    test "すべて成功" do
      results = [{:ok, 1}, {:ok, 2}, {:ok, 3}]
      {successes, errors} = Ch12.TravelGuide.aggregate_results(results)

      assert successes == [1, 2, 3]
      assert errors == []
    end

    test "すべてエラー" do
      results = [{:error, "E1"}, {:error, "E2"}]
      {successes, errors} = Ch12.TravelGuide.aggregate_results(results)

      assert successes == []
      assert errors == ["E1", "E2"]
    end
  end

  describe "enrich_with_hotels/2" do
    test "ホテル名を追加" do
      id = LocationId.new("Q123")
      loc = Location.new(id, "Test", 100)
      attraction = Attraction.new("Test Attraction", nil, loc)
      report = SearchReport.new(1, [])
      guide = TravelGuide.new(attraction, ["Subject 1"], report)

      hotels = [
        Hotel.new("Hotel A", 4.5, loc),
        Hotel.new("Hotel B", 4.0, loc)
      ]

      enriched = Ch12.TravelGuide.enrich_with_hotels(guide, hotels)

      assert enriched.subjects == ["Subject 1", "Hotel A", "Hotel B"]
    end
  end
end
