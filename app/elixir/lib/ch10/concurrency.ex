defmodule Ch10.Concurrency do
  @moduledoc """
  第10章: 並行処理

  Elixir での並行処理パターン:
  - Agent による共有状態管理（Scala の Ref に相当）
  - Task による並列実行（Scala の parSequence に相当）
  - プロセスによる軽量スレッド（Scala の Fiber に相当）
  """

  # =============================================================================
  # データ型
  # =============================================================================

  defmodule City do
    @moduledoc "都市"
    defstruct [:name]

    @type t :: %__MODULE__{name: String.t()}
  end

  defmodule CityStats do
    @moduledoc "都市の統計"
    defstruct [:city, :check_ins]

    @type t :: %__MODULE__{
            city: City.t(),
            check_ins: non_neg_integer()
          }
  end

  # =============================================================================
  # 純粋関数
  # =============================================================================

  @doc """
  トップ N 都市を取得（純粋関数）

  ## Examples

      iex> city_check_ins = %{
      ...>   %Ch10.Concurrency.City{name: "Tokyo"} => 100,
      ...>   %Ch10.Concurrency.City{name: "Sydney"} => 80,
      ...>   %Ch10.Concurrency.City{name: "Dublin"} => 90
      ...> }
      iex> top = Ch10.Concurrency.top_cities(city_check_ins, 2)
      iex> length(top)
      2
      iex> hd(top).check_ins
      100

  """
  @spec top_cities(map(), pos_integer()) :: [CityStats.t()]
  def top_cities(city_check_ins, n \\ 3) do
    city_check_ins
    |> Enum.map(fn {city, check_ins} ->
      %CityStats{city: city, check_ins: check_ins}
    end)
    |> Enum.sort_by(& &1.check_ins, :desc)
    |> Enum.take(n)
  end

  @doc """
  チェックインを追加（純粋関数）

  ## Examples

      iex> city = %Ch10.Concurrency.City{name: "Tokyo"}
      iex> map = %{}
      iex> map = Ch10.Concurrency.add_check_in(map, city)
      iex> map[city]
      1
      iex> map = Ch10.Concurrency.add_check_in(map, city)
      iex> map[city]
      2

  """
  @spec add_check_in(map(), City.t()) :: map()
  def add_check_in(city_check_ins, city) do
    Map.update(city_check_ins, city, 1, &(&1 + 1))
  end

  # =============================================================================
  # Agent による共有状態（Ref 相当）
  # =============================================================================

  @doc """
  チェックイン状態を保持する Agent を作成

  ## Examples

      iex> {:ok, agent} = Ch10.Concurrency.create_check_in_store()
      iex> is_pid(agent)
      true
      iex> Agent.stop(agent)

  """
  @spec create_check_in_store() :: {:ok, pid()}
  def create_check_in_store do
    Agent.start_link(fn -> %{} end)
  end

  @doc """
  Agent にチェックインを保存

  ## Examples

      iex> {:ok, agent} = Ch10.Concurrency.create_check_in_store()
      iex> city = %Ch10.Concurrency.City{name: "Tokyo"}
      iex> Ch10.Concurrency.store_check_in(agent, city)
      iex> Ch10.Concurrency.store_check_in(agent, city)
      iex> Ch10.Concurrency.get_check_ins(agent)[city]
      2
      iex> Agent.stop(agent)

  """
  @spec store_check_in(pid(), City.t()) :: :ok
  def store_check_in(agent, city) do
    Agent.update(agent, &add_check_in(&1, city))
  end

  @doc """
  Agent からチェックイン状態を取得

  ## Examples

      iex> {:ok, agent} = Ch10.Concurrency.create_check_in_store()
      iex> Ch10.Concurrency.get_check_ins(agent)
      %{}
      iex> Agent.stop(agent)

  """
  @spec get_check_ins(pid()) :: map()
  def get_check_ins(agent) do
    Agent.get(agent, & &1)
  end

  @doc """
  ランキング状態を保持する Agent を作成

  ## Examples

      iex> {:ok, agent} = Ch10.Concurrency.create_ranking_store()
      iex> Ch10.Concurrency.get_ranking(agent)
      []
      iex> Agent.stop(agent)

  """
  @spec create_ranking_store() :: {:ok, pid()}
  def create_ranking_store do
    Agent.start_link(fn -> [] end)
  end

  @doc """
  ランキングを更新

  ## Examples

      iex> {:ok, agent} = Ch10.Concurrency.create_ranking_store()
      iex> stats = [%Ch10.Concurrency.CityStats{city: %Ch10.Concurrency.City{name: "Tokyo"}, check_ins: 100}]
      iex> Ch10.Concurrency.update_ranking(agent, stats)
      iex> Ch10.Concurrency.get_ranking(agent)
      [%Ch10.Concurrency.CityStats{city: %Ch10.Concurrency.City{name: "Tokyo"}, check_ins: 100}]
      iex> Agent.stop(agent)

  """
  @spec update_ranking(pid(), [CityStats.t()]) :: :ok
  def update_ranking(agent, ranking) do
    Agent.update(agent, fn _ -> ranking end)
  end

  @doc """
  ランキングを取得

  ## Examples

      iex> {:ok, agent} = Ch10.Concurrency.create_ranking_store()
      iex> Ch10.Concurrency.get_ranking(agent)
      []
      iex> Agent.stop(agent)

  """
  @spec get_ranking(pid()) :: [CityStats.t()]
  def get_ranking(agent) do
    Agent.get(agent, & &1)
  end

  # =============================================================================
  # Task による並列実行（parSequence 相当）
  # =============================================================================

  @doc """
  複数のタスクを並列実行

  ## Examples

      iex> tasks = [fn -> 1 end, fn -> 2 end, fn -> 3 end]
      iex> Ch10.Concurrency.parallel_run(tasks)
      [1, 2, 3]

  """
  @spec parallel_run([(-> any())]) :: [any()]
  def parallel_run(tasks) do
    tasks
    |> Enum.map(&Task.async/1)
    |> Task.await_many()
  end

  @doc """
  複数の値を並列に変換

  ## Examples

      iex> Ch10.Concurrency.parallel_map([1, 2, 3], &(&1 * 2))
      [2, 4, 6]

  """
  @spec parallel_map([any()], (any() -> any())) :: [any()]
  def parallel_map(items, f) do
    items
    |> Enum.map(fn item -> Task.async(fn -> f.(item) end) end)
    |> Task.await_many()
  end

  @doc """
  複数の非同期操作を並列実行して結果を合計

  ## Examples

      iex> ios = [fn -> 1 end, fn -> 2 end, fn -> 3 end]
      iex> Ch10.Concurrency.sum_parallel(ios)
      6

  """
  @spec sum_parallel([(-> number())]) :: number()
  def sum_parallel(ios) do
    ios
    |> parallel_run()
    |> Enum.sum()
  end

  # =============================================================================
  # サイコロの例
  # =============================================================================

  @doc """
  サイコロを振る

  ## Examples

      iex> result = Ch10.Concurrency.cast_the_die()
      iex> result >= 1 and result <= 6
      true

  """
  @spec cast_the_die() :: integer()
  def cast_the_die do
    :rand.uniform(6)
  end

  @doc """
  サイコロを2つ並行して振り、合計を返す

  ## Examples

      iex> result = Ch10.Concurrency.cast_dice_parallel(2)
      iex> result >= 2 and result <= 12
      true

  """
  @spec cast_dice_parallel(pos_integer()) :: integer()
  def cast_dice_parallel(n) do
    1..n
    |> Enum.map(fn _ -> Task.async(&cast_the_die/0) end)
    |> Task.await_many()
    |> Enum.sum()
  end

  @doc """
  サイコロを並行して振り、結果を Agent に保存

  ## Examples

      iex> {:ok, agent} = Agent.start_link(fn -> [] end)
      iex> Ch10.Concurrency.cast_dice_and_store(3, agent)
      iex> results = Agent.get(agent, & &1)
      iex> length(results)
      3
      iex> Enum.all?(results, fn x -> x >= 1 and x <= 6 end)
      true
      iex> Agent.stop(agent)

  """
  @spec cast_dice_and_store(pos_integer(), pid()) :: :ok
  def cast_dice_and_store(n, agent) do
    tasks =
      1..n
      |> Enum.map(fn _ ->
        Task.async(fn ->
          result = cast_the_die()
          Agent.update(agent, &[result | &1])
          result
        end)
      end)

    Task.await_many(tasks)
    :ok
  end

  # =============================================================================
  # プロセスによる軽量スレッド（Fiber 相当）
  # =============================================================================

  @doc """
  バックグラウンドプロセスを起動

  ## Examples

      iex> {:ok, pid} = Ch10.Concurrency.start_background(fn -> :ok end)
      iex> is_pid(pid)
      true

  """
  @spec start_background((-> any())) :: {:ok, pid()}
  def start_background(f) do
    pid = spawn(f)
    {:ok, pid}
  end

  @doc """
  プロセスをキャンセル（停止）

  ## Examples

      iex> {:ok, pid} = Ch10.Concurrency.start_background(fn -> Process.sleep(:infinity) end)
      iex> Ch10.Concurrency.cancel(pid)
      :ok
      iex> Process.alive?(pid)
      false

  """
  @spec cancel(pid()) :: :ok
  def cancel(pid) do
    Process.exit(pid, :kill)
    :ok
  end

  @doc """
  繰り返し実行するプロセスを起動

  ## Examples

      iex> {:ok, agent} = Agent.start_link(fn -> 0 end)
      iex> {:ok, pid} = Ch10.Concurrency.start_repeating(fn -> Agent.update(agent, &(&1 + 1)) end, 10)
      iex> Process.sleep(50)
      iex> count = Agent.get(agent, & &1)
      iex> count > 0
      true
      iex> Ch10.Concurrency.cancel(pid)
      iex> Agent.stop(agent)

  """
  @spec start_repeating((-> any()), pos_integer()) :: {:ok, pid()}
  def start_repeating(f, interval_ms) do
    pid =
      spawn(fn ->
        repeat_forever(f, interval_ms)
      end)

    {:ok, pid}
  end

  defp repeat_forever(f, interval_ms) do
    f.()
    Process.sleep(interval_ms)
    repeat_forever(f, interval_ms)
  end

  # =============================================================================
  # チェックイン処理の並行版
  # =============================================================================

  defmodule ProcessingCheckIns do
    @moduledoc "チェックイン処理の状態"
    defstruct [:check_in_store, :ranking_store, :ranking_updater_pid, :check_in_processor_pid]

    @type t :: %__MODULE__{
            check_in_store: pid(),
            ranking_store: pid(),
            ranking_updater_pid: pid() | nil,
            check_in_processor_pid: pid() | nil
          }
  end

  @doc """
  チェックイン処理を開始（バックグラウンドで実行）

  ## Examples

      iex> cities = [
      ...>   %Ch10.Concurrency.City{name: "Tokyo"},
      ...>   %Ch10.Concurrency.City{name: "Sydney"},
      ...>   %Ch10.Concurrency.City{name: "Dublin"}
      ...> ]
      iex> {:ok, processing} = Ch10.Concurrency.start_processing(cities)
      iex> Process.sleep(50)
      iex> ranking = Ch10.Concurrency.current_ranking(processing)
      iex> is_list(ranking)
      true
      iex> Ch10.Concurrency.stop_processing(processing)

  """
  @spec start_processing([City.t()]) :: {:ok, ProcessingCheckIns.t()}
  def start_processing(cities) do
    {:ok, check_in_store} = create_check_in_store()
    {:ok, ranking_store} = create_ranking_store()

    # ランキング更新プロセスを起動
    {:ok, ranking_updater_pid} =
      start_repeating(
        fn ->
          check_ins = get_check_ins(check_in_store)
          ranking = top_cities(check_ins)
          update_ranking(ranking_store, ranking)
        end,
        10
      )

    # チェックイン処理プロセスを起動
    check_in_processor_pid =
      spawn(fn ->
        Enum.each(cities, fn city ->
          store_check_in(check_in_store, city)
        end)
      end)

    {:ok,
     %ProcessingCheckIns{
       check_in_store: check_in_store,
       ranking_store: ranking_store,
       ranking_updater_pid: ranking_updater_pid,
       check_in_processor_pid: check_in_processor_pid
     }}
  end

  @doc """
  現在のランキングを取得

  ## Examples

      iex> {:ok, processing} = Ch10.Concurrency.start_processing([])
      iex> Ch10.Concurrency.current_ranking(processing)
      []
      iex> Ch10.Concurrency.stop_processing(processing)

  """
  @spec current_ranking(ProcessingCheckIns.t()) :: [CityStats.t()]
  def current_ranking(%ProcessingCheckIns{ranking_store: ranking_store}) do
    get_ranking(ranking_store)
  end

  @doc """
  処理を停止

  ## Examples

      iex> {:ok, processing} = Ch10.Concurrency.start_processing([])
      iex> Ch10.Concurrency.stop_processing(processing)
      :ok

  """
  @spec stop_processing(ProcessingCheckIns.t()) :: :ok
  def stop_processing(%ProcessingCheckIns{} = processing) do
    if processing.ranking_updater_pid && Process.alive?(processing.ranking_updater_pid) do
      cancel(processing.ranking_updater_pid)
    end

    if processing.check_in_processor_pid && Process.alive?(processing.check_in_processor_pid) do
      cancel(processing.check_in_processor_pid)
    end

    Agent.stop(processing.check_in_store)
    Agent.stop(processing.ranking_store)
    :ok
  end

  # =============================================================================
  # 並行カウント
  # =============================================================================

  @doc """
  並行して偶数をカウント

  ## Examples

      iex> ios = Enum.map(1..10, fn n -> fn -> n end end)
      iex> Ch10.Concurrency.count_evens(ios)
      5

  """
  @spec count_evens([(-> integer())]) :: integer()
  def count_evens(ios) do
    {:ok, counter} = Agent.start_link(fn -> 0 end)

    ios
    |> Enum.map(fn io ->
      Task.async(fn ->
        n = io.()

        if rem(n, 2) == 0 do
          Agent.update(counter, &(&1 + 1))
        end
      end)
    end)
    |> Task.await_many()

    result = Agent.get(counter, & &1)
    Agent.stop(counter)
    result
  end

  # =============================================================================
  # タイムアウト付き収集
  # =============================================================================

  @doc """
  指定時間内にデータを収集

  ## Examples

      iex> results = Ch10.Concurrency.collect_for(100, 10, fn -> :rand.uniform(100) end)
      iex> is_list(results)
      true
      iex> length(results) > 0
      true

  """
  @spec collect_for(pos_integer(), pos_integer(), (-> any())) :: [any()]
  def collect_for(duration_ms, interval_ms, generator) do
    {:ok, collected} = Agent.start_link(fn -> [] end)

    {:ok, producer_pid} =
      start_repeating(
        fn ->
          value = generator.()
          Agent.update(collected, &[value | &1])
        end,
        interval_ms
      )

    Process.sleep(duration_ms)
    cancel(producer_pid)

    result = Agent.get(collected, &Enum.reverse/1)
    Agent.stop(collected)
    result
  end

  # =============================================================================
  # 並行マップ更新
  # =============================================================================

  defmodule Update do
    @moduledoc "更新操作"
    defstruct [:key, :value]

    @type t :: %__MODULE__{
            key: String.t(),
            value: any()
          }
  end

  @doc """
  更新を並行して適用

  ## Examples

      iex> updates = [
      ...>   %Ch10.Concurrency.Update{key: "a", value: 1},
      ...>   %Ch10.Concurrency.Update{key: "b", value: 2},
      ...>   %Ch10.Concurrency.Update{key: "a", value: 3}
      ...> ]
      iex> result = Ch10.Concurrency.apply_updates(updates)
      iex> Map.get(result, "b")
      2

  """
  @spec apply_updates([Update.t()]) :: map()
  def apply_updates(updates) do
    {:ok, map_ref} = Agent.start_link(fn -> %{} end)

    updates
    |> Enum.map(fn update ->
      Task.async(fn ->
        Agent.update(map_ref, &Map.put(&1, update.key, update.value))
      end)
    end)
    |> Task.await_many()

    result = Agent.get(map_ref, & &1)
    Agent.stop(map_ref)
    result
  end

  # =============================================================================
  # Process.sleep vs :timer.sleep
  # =============================================================================

  @doc """
  Elixir の Process.sleep はプロセスをブロックするが、他のプロセスには影響しない

  ## Examples

      iex> {time, _} = :timer.tc(fn ->
      ...>   tasks = Enum.map(1..3, fn _ ->
      ...>     Task.async(fn -> Process.sleep(100) end)
      ...>   end)
      ...>   Task.await_many(tasks)
      ...> end)
      iex> time < 200_000  # 200ms 未満（並列実行のため）
      true

  """
  @spec demo_parallel_sleep(pos_integer(), pos_integer()) :: :ok
  def demo_parallel_sleep(n, sleep_ms) do
    1..n
    |> Enum.map(fn _ -> Task.async(fn -> Process.sleep(sleep_ms) end) end)
    |> Task.await_many()

    :ok
  end
end
