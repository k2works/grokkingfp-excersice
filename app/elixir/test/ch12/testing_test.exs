defmodule Ch12.TestingTest do
  use ExUnit.Case, async: true
  doctest Ch12.Testing

  alias Ch12.Testing

  # =============================================================================
  # ランダムデータ生成のテスト
  # =============================================================================

  describe "random_string/1" do
    test "指定した長さの文字列を生成" do
      s = Testing.random_string(10)
      assert String.length(s) == 10
    end

    test "異なる呼び出しで異なる値" do
      s1 = Testing.random_string(20)
      s2 = Testing.random_string(20)
      # 確率的にはほぼ異なるはず
      refute s1 == s2
    end
  end

  describe "random_positive_int/1" do
    test "指定範囲内の整数を生成" do
      for _ <- 1..100 do
        n = Testing.random_positive_int(50)
        assert n >= 1 and n <= 50
      end
    end
  end

  describe "random_location/0" do
    test "有効な Location を生成" do
      loc = Testing.random_location()

      assert is_binary(loc.name)
      assert is_integer(loc.population)
      assert loc.population > 0
      assert loc.id.value =~ ~r/^Q\d+$/
    end
  end

  describe "random_attraction/0" do
    test "有効な Attraction を生成" do
      attr = Testing.random_attraction()

      assert is_binary(attr.name)
      assert is_nil(attr.description) or is_binary(attr.description)
      assert attr.location != nil
    end
  end

  describe "random_locations/1" do
    test "指定した数のロケーションを生成" do
      locs = Testing.random_locations(5)
      assert length(locs) == 5
    end
  end

  describe "random_attractions/1" do
    test "指定した数のアトラクションを生成" do
      attrs = Testing.random_attractions(3)
      assert length(attrs) == 3
    end
  end

  # =============================================================================
  # プロパティベーステストのテスト
  # =============================================================================

  describe "check_property/2" do
    test "常に true を返すプロパティ" do
      result = Testing.check_property(100, fn -> true end)
      assert result == :ok
    end

    test "常に false を返すプロパティ" do
      result = Testing.check_property(10, fn -> false end)
      assert {:error, message} = result
      assert message =~ "Property failed 10/10 times"
    end

    test "例外を投げるプロパティは失敗として扱う" do
      result = Testing.check_property(10, fn -> raise "Error" end)
      assert {:error, _} = result
    end
  end

  describe "verify_filter_properties/1" do
    test "filter_popular_locations の不変条件をテスト" do
      result = Testing.verify_filter_properties(50)
      assert result == :ok
    end
  end

  describe "verify_sort_properties/1" do
    test "sort_by_population の不変条件をテスト" do
      result = Testing.verify_sort_properties(50)
      assert result == :ok
    end
  end

  describe "verify_aggregate_properties/1" do
    test "aggregate_results の不変条件をテスト" do
      result = Testing.verify_aggregate_properties(50)
      assert result == :ok
    end
  end

  # =============================================================================
  # 統合テストのテスト
  # =============================================================================

  describe "run_integration_tests/0" do
    test "統合テストを実行" do
      {:ok, stats} = Testing.run_integration_tests()

      assert is_integer(stats.passed)
      assert is_integer(stats.failed)
      assert length(stats.results) > 0

      # すべてのテストが通ることを期待
      assert stats.failed == 0
    end
  end

  # =============================================================================
  # パフォーマンステストのテスト
  # =============================================================================

  describe "measure_time/1" do
    test "実行時間と結果を返す" do
      {time, result} = Testing.measure_time(fn -> 1 + 1 end)

      assert is_integer(time)
      assert time >= 0
      assert result == 2
    end
  end

  describe "measure_cache_effect/1" do
    test "キャッシュの効果を測定" do
      {:ok, stats} = Testing.measure_cache_effect(10)

      assert is_integer(stats.uncached_time)
      assert is_integer(stats.cached_time)
      assert is_number(stats.speedup)

      # 統計情報が正しく返されることを確認
      # (キャッシュ効果はタイミング依存で保証できないため速度比較は省略)
      assert stats.speedup >= 0
    end
  end
end
