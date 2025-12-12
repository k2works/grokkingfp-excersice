defmodule Ch10.ConcurrencyTest do
  use ExUnit.Case, async: true
  doctest Ch10.Concurrency

  alias Ch10.Concurrency
  alias Ch10.Concurrency.{City, CityStats, Update}

  describe "top_cities/2" do
    test "トップ N 都市を取得" do
      city_check_ins = %{
        %City{name: "Tokyo"} => 100,
        %City{name: "Sydney"} => 80,
        %City{name: "Dublin"} => 90,
        %City{name: "Lima"} => 70
      }

      top = Concurrency.top_cities(city_check_ins, 2)
      assert length(top) == 2
      assert hd(top).city.name == "Tokyo"
      assert hd(top).check_ins == 100
    end

    test "空のマップ" do
      assert Concurrency.top_cities(%{}, 3) == []
    end
  end

  describe "add_check_in/2" do
    test "新しい都市を追加" do
      city = %City{name: "Tokyo"}
      map = Concurrency.add_check_in(%{}, city)
      assert map[city] == 1
    end

    test "既存の都市をインクリメント" do
      city = %City{name: "Tokyo"}
      map = %{city => 5}
      map = Concurrency.add_check_in(map, city)
      assert map[city] == 6
    end
  end

  describe "Agent による共有状態" do
    test "チェックインストアの基本操作" do
      {:ok, agent} = Concurrency.create_check_in_store()
      city = %City{name: "Tokyo"}

      assert Concurrency.get_check_ins(agent) == %{}

      Concurrency.store_check_in(agent, city)
      assert Concurrency.get_check_ins(agent)[city] == 1

      Concurrency.store_check_in(agent, city)
      assert Concurrency.get_check_ins(agent)[city] == 2

      Agent.stop(agent)
    end

    test "ランキングストアの基本操作" do
      {:ok, agent} = Concurrency.create_ranking_store()

      assert Concurrency.get_ranking(agent) == []

      stats = [%CityStats{city: %City{name: "Tokyo"}, check_ins: 100}]
      Concurrency.update_ranking(agent, stats)

      assert Concurrency.get_ranking(agent) == stats

      Agent.stop(agent)
    end
  end

  describe "parallel_run/1" do
    test "複数のタスクを並列実行" do
      tasks = [fn -> 1 end, fn -> 2 end, fn -> 3 end]
      results = Concurrency.parallel_run(tasks)
      assert results == [1, 2, 3]
    end

    test "空リスト" do
      assert Concurrency.parallel_run([]) == []
    end
  end

  describe "parallel_map/2" do
    test "並列でマッピング" do
      results = Concurrency.parallel_map([1, 2, 3], &(&1 * 2))
      assert results == [2, 4, 6]
    end
  end

  describe "sum_parallel/1" do
    test "並列で合計" do
      ios = [fn -> 1 end, fn -> 2 end, fn -> 3 end]
      assert Concurrency.sum_parallel(ios) == 6
    end
  end

  describe "サイコロ" do
    test "cast_the_die は 1-6 を返す" do
      for _ <- 1..100 do
        result = Concurrency.cast_the_die()
        assert result >= 1 and result <= 6
      end
    end

    test "cast_dice_parallel で複数のサイコロを振る" do
      result = Concurrency.cast_dice_parallel(3)
      assert result >= 3 and result <= 18
    end

    test "cast_dice_and_store で結果を保存" do
      {:ok, agent} = Agent.start_link(fn -> [] end)
      Concurrency.cast_dice_and_store(5, agent)
      results = Agent.get(agent, & &1)

      assert length(results) == 5
      assert Enum.all?(results, fn x -> x >= 1 and x <= 6 end)

      Agent.stop(agent)
    end
  end

  describe "バックグラウンドプロセス" do
    test "start_background でプロセスを起動" do
      {:ok, pid} = Concurrency.start_background(fn -> :ok end)
      assert is_pid(pid)
    end

    test "cancel でプロセスを停止" do
      {:ok, pid} = Concurrency.start_background(fn -> Process.sleep(:infinity) end)
      assert Process.alive?(pid)

      Concurrency.cancel(pid)
      Process.sleep(10)
      refute Process.alive?(pid)
    end

    test "start_repeating で繰り返し実行" do
      {:ok, agent} = Agent.start_link(fn -> 0 end)

      {:ok, pid} =
        Concurrency.start_repeating(
          fn -> Agent.update(agent, &(&1 + 1)) end,
          10
        )

      Process.sleep(50)
      count = Agent.get(agent, & &1)
      assert count > 0

      Concurrency.cancel(pid)
      Agent.stop(agent)
    end
  end

  describe "ProcessingCheckIns" do
    test "チェックイン処理を開始・停止" do
      cities = [
        %City{name: "Tokyo"},
        %City{name: "Sydney"},
        %City{name: "Dublin"}
      ]

      {:ok, processing} = Concurrency.start_processing(cities)
      Process.sleep(50)

      ranking = Concurrency.current_ranking(processing)
      assert is_list(ranking)

      Concurrency.stop_processing(processing)
    end

    test "空のリストでも動作" do
      {:ok, processing} = Concurrency.start_processing([])
      assert Concurrency.current_ranking(processing) == []
      Concurrency.stop_processing(processing)
    end
  end

  describe "count_evens/1" do
    test "偶数をカウント" do
      ios = Enum.map(1..10, fn n -> fn -> n end end)
      count = Concurrency.count_evens(ios)
      assert count == 5
    end

    test "空リスト" do
      assert Concurrency.count_evens([]) == 0
    end
  end

  describe "collect_for/3" do
    test "指定時間内にデータを収集" do
      results = Concurrency.collect_for(100, 10, fn -> :rand.uniform(100) end)
      assert is_list(results)
      assert length(results) > 0
    end
  end

  describe "apply_updates/1" do
    test "更新を並行して適用" do
      updates = [
        %Update{key: "a", value: 1},
        %Update{key: "b", value: 2},
        %Update{key: "c", value: 3}
      ]

      result = Concurrency.apply_updates(updates)
      assert Map.get(result, "a") == 1
      assert Map.get(result, "b") == 2
      assert Map.get(result, "c") == 3
    end
  end

  describe "demo_parallel_sleep/2" do
    test "並列スリープは合計時間より短い" do
      {time, _} = :timer.tc(fn -> Concurrency.demo_parallel_sleep(3, 50) end)
      # 50ms * 3 = 150ms 未満で完了するはず
      assert time < 150_000
    end
  end
end
