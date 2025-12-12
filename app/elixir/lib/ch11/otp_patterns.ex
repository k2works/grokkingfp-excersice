defmodule Ch11.OtpPatterns do
  @moduledoc """
  第11章: OTP パターン

  Elixir の OTP を使った並行処理:
  - GenServer による状態管理
  - Supervisor による耐障害性
  - Registry によるプロセス登録
  """

  # =============================================================================
  # GenServer によるカウンター
  # =============================================================================

  defmodule Counter do
    @moduledoc """
    GenServer を使ったカウンター

    ## Examples

        iex> {:ok, counter} = Ch11.OtpPatterns.Counter.start_link(0)
        iex> Ch11.OtpPatterns.Counter.get(counter)
        0
        iex> Ch11.OtpPatterns.Counter.increment(counter)
        iex> Ch11.OtpPatterns.Counter.get(counter)
        1
        iex> GenServer.stop(counter)

    """
    use GenServer

    # クライアント API

    @doc "カウンターを開始"
    @spec start_link(integer()) :: GenServer.on_start()
    def start_link(initial_value \\ 0) do
      GenServer.start_link(__MODULE__, initial_value)
    end

    @doc "現在の値を取得"
    @spec get(GenServer.server()) :: integer()
    def get(counter) do
      GenServer.call(counter, :get)
    end

    @doc "インクリメント"
    @spec increment(GenServer.server()) :: :ok
    def increment(counter) do
      GenServer.cast(counter, :increment)
    end

    @doc "指定値を加算"
    @spec add(GenServer.server(), integer()) :: :ok
    def add(counter, value) do
      GenServer.cast(counter, {:add, value})
    end

    @doc "リセット"
    @spec reset(GenServer.server()) :: :ok
    def reset(counter) do
      GenServer.cast(counter, :reset)
    end

    # サーバーコールバック

    @impl true
    def init(initial_value) do
      {:ok, initial_value}
    end

    @impl true
    def handle_call(:get, _from, state) do
      {:reply, state, state}
    end

    @impl true
    def handle_cast(:increment, state) do
      {:noreply, state + 1}
    end

    @impl true
    def handle_cast({:add, value}, state) do
      {:noreply, state + value}
    end

    @impl true
    def handle_cast(:reset, _state) do
      {:noreply, 0}
    end
  end

  # =============================================================================
  # GenServer によるチェックインストア
  # =============================================================================

  defmodule CheckInStore do
    @moduledoc """
    GenServer を使ったチェックインストア

    ## Examples

        iex> {:ok, store} = Ch11.OtpPatterns.CheckInStore.start_link()
        iex> Ch11.OtpPatterns.CheckInStore.check_in(store, "Tokyo")
        iex> Ch11.OtpPatterns.CheckInStore.check_in(store, "Tokyo")
        iex> Ch11.OtpPatterns.CheckInStore.get_count(store, "Tokyo")
        2
        iex> GenServer.stop(store)

    """
    use GenServer

    # クライアント API

    @spec start_link(keyword()) :: GenServer.on_start()
    def start_link(opts \\ []) do
      GenServer.start_link(__MODULE__, %{}, opts)
    end

    @spec check_in(GenServer.server(), String.t()) :: :ok
    def check_in(store, city) do
      GenServer.cast(store, {:check_in, city})
    end

    @spec get_count(GenServer.server(), String.t()) :: non_neg_integer()
    def get_count(store, city) do
      GenServer.call(store, {:get_count, city})
    end

    @spec get_all(GenServer.server()) :: map()
    def get_all(store) do
      GenServer.call(store, :get_all)
    end

    @spec top_cities(GenServer.server(), pos_integer()) :: [{String.t(), non_neg_integer()}]
    def top_cities(store, n \\ 3) do
      GenServer.call(store, {:top_cities, n})
    end

    @spec reset(GenServer.server()) :: :ok
    def reset(store) do
      GenServer.cast(store, :reset)
    end

    # サーバーコールバック

    @impl true
    def init(state) do
      {:ok, state}
    end

    @impl true
    def handle_cast({:check_in, city}, state) do
      new_state = Map.update(state, city, 1, &(&1 + 1))
      {:noreply, new_state}
    end

    @impl true
    def handle_cast(:reset, _state) do
      {:noreply, %{}}
    end

    @impl true
    def handle_call({:get_count, city}, _from, state) do
      {:reply, Map.get(state, city, 0), state}
    end

    @impl true
    def handle_call(:get_all, _from, state) do
      {:reply, state, state}
    end

    @impl true
    def handle_call({:top_cities, n}, _from, state) do
      top =
        state
        |> Enum.sort_by(fn {_city, count} -> count end, :desc)
        |> Enum.take(n)

      {:reply, top, state}
    end
  end

  # =============================================================================
  # GenServer による定期実行タスク
  # =============================================================================

  defmodule PeriodicTask do
    @moduledoc """
    GenServer を使った定期実行タスク

    ## Examples

        iex> {:ok, agent} = Agent.start_link(fn -> 0 end)
        iex> {:ok, task} = Ch11.OtpPatterns.PeriodicTask.start_link(
        ...>   fn -> Agent.update(agent, &(&1 + 1)) end,
        ...>   10
        ...> )
        iex> Process.sleep(50)
        iex> count = Agent.get(agent, & &1)
        iex> count > 0
        true
        iex> GenServer.stop(task)
        iex> Agent.stop(agent)

    """
    use GenServer

    # クライアント API

    @spec start_link((-> any()), pos_integer()) :: GenServer.on_start()
    def start_link(task_fn, interval_ms) do
      GenServer.start_link(__MODULE__, {task_fn, interval_ms})
    end

    @spec stop(GenServer.server()) :: :ok
    def stop(server) do
      GenServer.stop(server)
    end

    # サーバーコールバック

    @impl true
    def init({task_fn, interval_ms}) do
      schedule_work(interval_ms)
      {:ok, %{task_fn: task_fn, interval_ms: interval_ms}}
    end

    @impl true
    def handle_info(:work, %{task_fn: task_fn, interval_ms: interval_ms} = state) do
      task_fn.()
      schedule_work(interval_ms)
      {:noreply, state}
    end

    defp schedule_work(interval_ms) do
      Process.send_after(self(), :work, interval_ms)
    end
  end

  # =============================================================================
  # GenServer による状態マシン
  # =============================================================================

  defmodule TrafficLight do
    @moduledoc """
    GenServer を使った信号機の状態マシン

    ## Examples

        iex> {:ok, light} = Ch11.OtpPatterns.TrafficLight.start_link()
        iex> Ch11.OtpPatterns.TrafficLight.current(light)
        :red
        iex> Ch11.OtpPatterns.TrafficLight.next(light)
        iex> Ch11.OtpPatterns.TrafficLight.current(light)
        :green
        iex> GenServer.stop(light)

    """
    use GenServer

    @type state :: :red | :green | :yellow

    # クライアント API

    @spec start_link() :: GenServer.on_start()
    def start_link do
      GenServer.start_link(__MODULE__, :red)
    end

    @spec current(GenServer.server()) :: state()
    def current(light) do
      GenServer.call(light, :current)
    end

    @spec next(GenServer.server()) :: :ok
    def next(light) do
      GenServer.cast(light, :next)
    end

    # サーバーコールバック

    @impl true
    def init(initial_state) do
      {:ok, initial_state}
    end

    @impl true
    def handle_call(:current, _from, state) do
      {:reply, state, state}
    end

    @impl true
    def handle_cast(:next, state) do
      next_state =
        case state do
          :red -> :green
          :green -> :yellow
          :yellow -> :red
        end

      {:noreply, next_state}
    end
  end

  # =============================================================================
  # GenServer によるキューイング
  # =============================================================================

  defmodule JobQueue do
    @moduledoc """
    GenServer を使ったジョブキュー

    ## Examples

        iex> {:ok, queue} = Ch11.OtpPatterns.JobQueue.start_link()
        iex> Ch11.OtpPatterns.JobQueue.enqueue(queue, :job1)
        iex> Ch11.OtpPatterns.JobQueue.enqueue(queue, :job2)
        iex> Ch11.OtpPatterns.JobQueue.size(queue)
        2
        iex> Ch11.OtpPatterns.JobQueue.dequeue(queue)
        {:ok, :job1}
        iex> Ch11.OtpPatterns.JobQueue.size(queue)
        1
        iex> GenServer.stop(queue)

    """
    use GenServer

    # クライアント API

    @spec start_link() :: GenServer.on_start()
    def start_link do
      GenServer.start_link(__MODULE__, :queue.new())
    end

    @spec enqueue(GenServer.server(), any()) :: :ok
    def enqueue(queue, job) do
      GenServer.cast(queue, {:enqueue, job})
    end

    @spec dequeue(GenServer.server()) :: {:ok, any()} | :empty
    def dequeue(queue) do
      GenServer.call(queue, :dequeue)
    end

    @spec size(GenServer.server()) :: non_neg_integer()
    def size(queue) do
      GenServer.call(queue, :size)
    end

    @spec is_empty?(GenServer.server()) :: boolean()
    def is_empty?(queue) do
      GenServer.call(queue, :is_empty)
    end

    # サーバーコールバック

    @impl true
    def init(queue) do
      {:ok, queue}
    end

    @impl true
    def handle_cast({:enqueue, job}, queue) do
      {:noreply, :queue.in(job, queue)}
    end

    @impl true
    def handle_call(:dequeue, _from, queue) do
      case :queue.out(queue) do
        {{:value, job}, new_queue} -> {:reply, {:ok, job}, new_queue}
        {:empty, queue} -> {:reply, :empty, queue}
      end
    end

    @impl true
    def handle_call(:size, _from, queue) do
      {:reply, :queue.len(queue), queue}
    end

    @impl true
    def handle_call(:is_empty, _from, queue) do
      {:reply, :queue.is_empty(queue), queue}
    end
  end

  # =============================================================================
  # Supervisor による耐障害性
  # =============================================================================

  defmodule CounterSupervisor do
    @moduledoc """
    カウンターを監視する Supervisor

    ## Examples

        iex> {:ok, sup} = Ch11.OtpPatterns.CounterSupervisor.start_link()
        iex> counter = Ch11.OtpPatterns.CounterSupervisor.get_counter(sup)
        iex> is_pid(counter)
        true
        iex> Supervisor.stop(sup)

    """
    use Supervisor

    @spec start_link() :: Supervisor.on_start()
    def start_link do
      Supervisor.start_link(__MODULE__, :ok)
    end

    @spec get_counter(Supervisor.supervisor()) :: pid()
    def get_counter(supervisor) do
      [{_, counter, _, _}] = Supervisor.which_children(supervisor)
      counter
    end

    @impl true
    def init(:ok) do
      children = [
        {Counter, 0}
      ]

      Supervisor.init(children, strategy: :one_for_one)
    end
  end

  # =============================================================================
  # DynamicSupervisor による動的プロセス管理
  # =============================================================================

  defmodule WorkerSupervisor do
    @moduledoc """
    DynamicSupervisor によるワーカー管理

    ## Examples

        iex> {:ok, sup} = Ch11.OtpPatterns.WorkerSupervisor.start_link()
        iex> {:ok, worker} = Ch11.OtpPatterns.WorkerSupervisor.start_worker(sup, 100)
        iex> is_pid(worker)
        true
        iex> Ch11.OtpPatterns.WorkerSupervisor.stop_worker(sup, worker)
        iex> DynamicSupervisor.stop(sup)

    """
    use DynamicSupervisor

    @spec start_link() :: Supervisor.on_start()
    def start_link do
      DynamicSupervisor.start_link(__MODULE__, :ok)
    end

    @spec start_worker(Supervisor.supervisor(), integer()) :: DynamicSupervisor.on_start_child()
    def start_worker(supervisor, initial_value) do
      spec = {Counter, initial_value}
      DynamicSupervisor.start_child(supervisor, spec)
    end

    @spec stop_worker(Supervisor.supervisor(), pid()) :: :ok | {:error, :not_found}
    def stop_worker(supervisor, worker) do
      DynamicSupervisor.terminate_child(supervisor, worker)
    end

    @spec count_workers(Supervisor.supervisor()) :: non_neg_integer()
    def count_workers(supervisor) do
      DynamicSupervisor.count_children(supervisor).active
    end

    @impl true
    def init(:ok) do
      DynamicSupervisor.init(strategy: :one_for_one)
    end
  end

  # =============================================================================
  # Task.Supervisor による並行タスク管理
  # =============================================================================

  defmodule TaskRunner do
    @moduledoc """
    Task.Supervisor を使った並行タスク実行

    ## Examples

        iex> {:ok, runner} = Ch11.OtpPatterns.TaskRunner.start_link()
        iex> results = Ch11.OtpPatterns.TaskRunner.run_all(runner, [fn -> 1 end, fn -> 2 end])
        iex> Enum.sort(results)
        [1, 2]
        iex> Supervisor.stop(runner)

    """

    @spec start_link() :: Supervisor.on_start()
    def start_link do
      Task.Supervisor.start_link()
    end

    @spec run_async(Supervisor.supervisor(), (-> any())) :: Task.t()
    def run_async(supervisor, task_fn) do
      Task.Supervisor.async(supervisor, task_fn)
    end

    @spec run_all(Supervisor.supervisor(), [(-> any())]) :: [any()]
    def run_all(supervisor, task_fns) do
      task_fns
      |> Enum.map(&Task.Supervisor.async(supervisor, &1))
      |> Task.await_many()
    end

    @spec run_with_timeout(Supervisor.supervisor(), (-> any()), pos_integer()) ::
            {:ok, any()} | {:error, :timeout}
    def run_with_timeout(supervisor, task_fn, timeout_ms) do
      task = Task.Supervisor.async_nolink(supervisor, task_fn)

      case Task.yield(task, timeout_ms) do
        {:ok, result} ->
          {:ok, result}

        nil ->
          Task.shutdown(task, :brutal_kill)
          {:error, :timeout}
      end
    end
  end

  # =============================================================================
  # Registry を使った名前付きプロセス
  # =============================================================================

  defmodule NamedCounter do
    @moduledoc """
    Registry を使った名前付きカウンター

    ## Examples

        iex> {:ok, _} = Registry.start_link(keys: :unique, name: Ch11.OtpPatterns.NamedCounter.Registry)
        iex> {:ok, counter} = Ch11.OtpPatterns.NamedCounter.start_link("my_counter", 0)
        iex> Ch11.OtpPatterns.NamedCounter.get("my_counter")
        0
        iex> Ch11.OtpPatterns.NamedCounter.increment("my_counter")
        iex> Ch11.OtpPatterns.NamedCounter.get("my_counter")
        1
        iex> GenServer.stop(counter)

    """
    use GenServer

    @registry __MODULE__.Registry

    # クライアント API

    @spec start_link(String.t(), integer()) :: GenServer.on_start()
    def start_link(name, initial_value \\ 0) do
      GenServer.start_link(__MODULE__, initial_value, name: via_tuple(name))
    end

    @spec get(String.t()) :: integer()
    def get(name) do
      GenServer.call(via_tuple(name), :get)
    end

    @spec increment(String.t()) :: :ok
    def increment(name) do
      GenServer.cast(via_tuple(name), :increment)
    end

    @spec whereis(String.t()) :: pid() | nil
    def whereis(name) do
      case Registry.lookup(@registry, name) do
        [{pid, _}] -> pid
        [] -> nil
      end
    end

    defp via_tuple(name) do
      {:via, Registry, {@registry, name}}
    end

    # サーバーコールバック

    @impl true
    def init(initial_value) do
      {:ok, initial_value}
    end

    @impl true
    def handle_call(:get, _from, state) do
      {:reply, state, state}
    end

    @impl true
    def handle_cast(:increment, state) do
      {:noreply, state + 1}
    end
  end

  # =============================================================================
  # PubSub パターン
  # =============================================================================

  defmodule PubSub do
    @moduledoc """
    GenServer を使った Pub/Sub パターン

    ## Examples

        iex> {:ok, pubsub} = Ch11.OtpPatterns.PubSub.start_link()
        iex> Ch11.OtpPatterns.PubSub.subscribe(pubsub, "events")
        iex> Ch11.OtpPatterns.PubSub.publish(pubsub, "events", :hello)
        iex> receive do msg -> msg after 100 -> :timeout end
        {:pubsub, "events", :hello}
        iex> GenServer.stop(pubsub)

    """
    use GenServer

    # クライアント API

    @spec start_link() :: GenServer.on_start()
    def start_link do
      GenServer.start_link(__MODULE__, %{})
    end

    @spec subscribe(GenServer.server(), String.t()) :: :ok
    def subscribe(server, topic) do
      GenServer.call(server, {:subscribe, topic, self()})
    end

    @spec unsubscribe(GenServer.server(), String.t()) :: :ok
    def unsubscribe(server, topic) do
      GenServer.call(server, {:unsubscribe, topic, self()})
    end

    @spec publish(GenServer.server(), String.t(), any()) :: :ok
    def publish(server, topic, message) do
      GenServer.cast(server, {:publish, topic, message})
    end

    @spec subscribers(GenServer.server(), String.t()) :: [pid()]
    def subscribers(server, topic) do
      GenServer.call(server, {:subscribers, topic})
    end

    # サーバーコールバック

    @impl true
    def init(state) do
      {:ok, state}
    end

    @impl true
    def handle_call({:subscribe, topic, pid}, _from, state) do
      subscribers = Map.get(state, topic, MapSet.new())
      new_subscribers = MapSet.put(subscribers, pid)
      {:reply, :ok, Map.put(state, topic, new_subscribers)}
    end

    @impl true
    def handle_call({:unsubscribe, topic, pid}, _from, state) do
      subscribers = Map.get(state, topic, MapSet.new())
      new_subscribers = MapSet.delete(subscribers, pid)
      {:reply, :ok, Map.put(state, topic, new_subscribers)}
    end

    @impl true
    def handle_call({:subscribers, topic}, _from, state) do
      subscribers = Map.get(state, topic, MapSet.new())
      {:reply, MapSet.to_list(subscribers), state}
    end

    @impl true
    def handle_cast({:publish, topic, message}, state) do
      subscribers = Map.get(state, topic, MapSet.new())

      Enum.each(subscribers, fn pid ->
        send(pid, {:pubsub, topic, message})
      end)

      {:noreply, state}
    end
  end
end
