defmodule Ch12.Testing do
  @moduledoc """
  第12章: テスト戦略

  関数型プログラミングにおけるテスト:
  - 純粋関数のテスト
  - プロパティベーステスト
  - スタブ/モックの活用
  - 統合テスト
  """

  # =============================================================================
  # テスト用ヘルパー関数
  # =============================================================================

  @doc """
  ランダムな文字列を生成

  ## Examples

      iex> s = Ch12.Testing.random_string(10)
      iex> String.length(s)
      10

  """
  @spec random_string(pos_integer()) :: String.t()
  def random_string(length) do
    :crypto.strong_rand_bytes(length)
    |> Base.encode64()
    |> binary_part(0, length)
  end

  @doc """
  ランダムな正の整数を生成

  ## Examples

      iex> n = Ch12.Testing.random_positive_int(100)
      iex> n >= 1 and n <= 100
      true

  """
  @spec random_positive_int(pos_integer()) :: pos_integer()
  def random_positive_int(max) do
    :rand.uniform(max)
  end

  @doc """
  ランダムな Location を生成

  ## Examples

      iex> loc = Ch12.Testing.random_location()
      iex> is_binary(loc.name)
      true

  """
  @spec random_location() :: Ch12.TravelGuide.Location.t()
  def random_location do
    alias Ch12.TravelGuide.{LocationId, Location}

    id = LocationId.new("Q#{random_positive_int(1_000_000)}")
    name = random_string(8)
    population = random_positive_int(10_000_000)
    Location.new(id, name, population)
  end

  @doc """
  ランダムな Attraction を生成

  ## Examples

      iex> attr = Ch12.Testing.random_attraction()
      iex> is_binary(attr.name)
      true

  """
  @spec random_attraction() :: Ch12.TravelGuide.Attraction.t()
  def random_attraction do
    alias Ch12.TravelGuide.Attraction

    name = random_string(10)
    description = if :rand.uniform(2) == 1, do: random_string(20), else: nil
    location = random_location()
    Attraction.new(name, description, location)
  end

  @doc """
  ランダムな Location のリストを生成

  ## Examples

      iex> locs = Ch12.Testing.random_locations(5)
      iex> length(locs)
      5

  """
  @spec random_locations(non_neg_integer()) :: [Ch12.TravelGuide.Location.t()]
  def random_locations(count) do
    Enum.map(1..count, fn _ -> random_location() end)
  end

  @doc """
  ランダムな Attraction のリストを生成

  ## Examples

      iex> attrs = Ch12.Testing.random_attractions(5)
      iex> length(attrs)
      5

  """
  @spec random_attractions(non_neg_integer()) :: [Ch12.TravelGuide.Attraction.t()]
  def random_attractions(count) do
    Enum.map(1..count, fn _ -> random_attraction() end)
  end

  # =============================================================================
  # プロパティベーステスト用ヘルパー
  # =============================================================================

  @doc """
  プロパティをテスト（指定回数実行）

  ## Examples

      iex> Ch12.Testing.check_property(100, fn ->
      ...>   n = :rand.uniform(100)
      ...>   n >= 1 and n <= 100
      ...> end)
      :ok

  """
  @spec check_property(pos_integer(), (-> boolean())) :: :ok | {:error, String.t()}
  def check_property(iterations, property_fn) do
    results =
      Enum.map(1..iterations, fn _ ->
        try do
          property_fn.()
        rescue
          _ -> false
        end
      end)

    if Enum.all?(results) do
      :ok
    else
      failed_count = Enum.count(results, &(!&1))
      {:error, "Property failed #{failed_count}/#{iterations} times"}
    end
  end

  @doc """
  filter_popular_locations の不変条件をテスト

  ## Examples

      iex> Ch12.Testing.verify_filter_properties(100)
      :ok

  """
  @spec verify_filter_properties(pos_integer()) :: :ok | {:error, String.t()}
  def verify_filter_properties(iterations) do
    alias Ch12.TravelGuide

    check_property(iterations, fn ->
      locations = random_locations(:rand.uniform(20))
      min_population = random_positive_int(5_000_000)

      result = TravelGuide.filter_popular_locations(locations, min_population)

      # プロパティ1: 結果のサイズは入力以下
      size_property = length(result) <= length(locations)

      # プロパティ2: 結果のすべての要素は条件を満たす
      all_meet_condition = Enum.all?(result, fn loc -> loc.population >= min_population end)

      # プロパティ3: 条件を満たす要素はすべて結果に含まれる
      qualifying = Enum.filter(locations, fn loc -> loc.population >= min_population end)
      all_included = Enum.all?(qualifying, fn loc -> loc in result end)

      size_property and all_meet_condition and all_included
    end)
  end

  @doc """
  sort_by_population の不変条件をテスト

  ## Examples

      iex> Ch12.Testing.verify_sort_properties(100)
      :ok

  """
  @spec verify_sort_properties(pos_integer()) :: :ok | {:error, String.t()}
  def verify_sort_properties(iterations) do
    alias Ch12.TravelGuide

    check_property(iterations, fn ->
      attractions = random_attractions(:rand.uniform(20))

      result = TravelGuide.sort_by_population(attractions)

      # プロパティ1: 結果のサイズは入力と同じ
      same_size = length(result) == length(attractions)

      # プロパティ2: 降順にソートされている
      populations = Enum.map(result, fn a -> a.location.population end)

      is_sorted_desc =
        populations
        |> Enum.zip(Enum.drop(populations, 1))
        |> Enum.all?(fn {a, b} -> a >= b end)

      # プロパティ3: すべての要素が保持されている
      all_preserved = Enum.all?(attractions, fn a -> a in result end)

      same_size and is_sorted_desc and all_preserved
    end)
  end

  @doc """
  aggregate_results の不変条件をテスト

  ## Examples

      iex> Ch12.Testing.verify_aggregate_properties(100)
      :ok

  """
  @spec verify_aggregate_properties(pos_integer()) :: :ok | {:error, String.t()}
  def verify_aggregate_properties(iterations) do
    alias Ch12.TravelGuide

    check_property(iterations, fn ->
      # ランダムな結果リストを生成
      count = :rand.uniform(20)

      results =
        Enum.map(1..count, fn _ ->
          if :rand.uniform(2) == 1 do
            {:ok, random_string(5)}
          else
            {:error, "Error: #{random_string(5)}"}
          end
        end)

      {successes, errors} = TravelGuide.aggregate_results(results)

      # プロパティ1: 成功と失敗の合計は入力と同じ
      total_count = length(successes) + length(errors) == count

      # プロパティ2: 成功の数は :ok の数と同じ
      ok_count = Enum.count(results, &match?({:ok, _}, &1))
      success_count_matches = length(successes) == ok_count

      # プロパティ3: エラーの数は :error の数と同じ
      error_count = Enum.count(results, &match?({:error, _}, &1))
      error_count_matches = length(errors) == error_count

      total_count and success_count_matches and error_count_matches
    end)
  end

  # =============================================================================
  # 統合テスト用ヘルパー
  # =============================================================================

  @doc """
  TravelGuide の統合テストを実行

  ## Examples

      iex> {:ok, stats} = Ch12.Testing.run_integration_tests()
      iex> is_integer(stats.passed) and is_integer(stats.failed)
      true

  """
  @spec run_integration_tests() :: {:ok, map()}
  def run_integration_tests do
    alias Ch12.TravelGuide

    tests = [
      {:normal_case, fn ->
        case TravelGuide.travel_guide(TravelGuide.TestDataAccess, "Tokyo") do
          {:ok, guide} ->
            guide.attraction.name == "Tokyo Tower" and
              length(guide.subjects) > 0 and
              guide.search_report.errors == []

          _ ->
            false
        end
      end},
      {:empty_result, fn ->
        case TravelGuide.travel_guide(TravelGuide.TestDataAccess, "Empty") do
          {:error, "No attractions found"} -> true
          _ -> false
        end
      end},
      {:error_handling, fn ->
        case TravelGuide.travel_guide(TravelGuide.FailingDataAccess, "Test") do
          {:ok, guide} ->
            length(guide.search_report.errors) == 2

          _ ->
            false
        end
      end},
      {:cached_access, fn ->
        {:ok, cache} = TravelGuide.CachedDataAccess.start_link(TravelGuide.TestDataAccess)

        result1 = TravelGuide.travel_guide_cached(cache, "Tokyo")
        result2 = TravelGuide.travel_guide_cached(cache, "Tokyo")

        stats = TravelGuide.CachedDataAccess.cache_stats(cache)
        Agent.stop(cache)

        case {result1, result2} do
          {{:ok, g1}, {:ok, g2}} ->
            g1.attraction.name == g2.attraction.name and
              stats.attractions > 0

          _ ->
            false
        end
      end}
    ]

    results =
      Enum.map(tests, fn {name, test_fn} ->
        result =
          try do
            test_fn.()
          rescue
            _ -> false
          end

        {name, result}
      end)

    passed = Enum.count(results, fn {_, r} -> r end)
    failed = Enum.count(results, fn {_, r} -> !r end)

    {:ok, %{passed: passed, failed: failed, results: results}}
  end

  # =============================================================================
  # パフォーマンステスト用ヘルパー
  # =============================================================================

  @doc """
  関数の実行時間を計測

  ## Examples

      iex> {time, result} = Ch12.Testing.measure_time(fn -> Enum.sum(1..1000) end)
      iex> is_integer(time) and result == 500500
      true

  """
  @spec measure_time((-> any())) :: {non_neg_integer(), any()}
  def measure_time(fun) do
    {time, result} = :timer.tc(fun)
    {time, result}
  end

  @doc """
  キャッシュの効果を測定

  ## Examples

      iex> {:ok, stats} = Ch12.Testing.measure_cache_effect(10)
      iex> is_integer(stats.uncached_time) and is_integer(stats.cached_time)
      true

  """
  @spec measure_cache_effect(pos_integer()) :: {:ok, map()}
  def measure_cache_effect(iterations) do
    alias Ch12.TravelGuide

    # キャッシュなしの時間
    {uncached_time, _} =
      measure_time(fn ->
        Enum.each(1..iterations, fn _ ->
          TravelGuide.travel_guide(TravelGuide.TestDataAccess, "Tokyo")
        end)
      end)

    # キャッシュありの時間
    {:ok, cache} = TravelGuide.CachedDataAccess.start_link(TravelGuide.TestDataAccess)

    {cached_time, _} =
      measure_time(fn ->
        Enum.each(1..iterations, fn _ ->
          TravelGuide.travel_guide_cached(cache, "Tokyo")
        end)
      end)

    Agent.stop(cache)

    {:ok,
     %{
       uncached_time: uncached_time,
       cached_time: cached_time,
       speedup: uncached_time / max(cached_time, 1)
     }}
  end
end
