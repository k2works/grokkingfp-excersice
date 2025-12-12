defmodule Ch11.OtpPatternsTest do
  use ExUnit.Case, async: false
  doctest Ch11.OtpPatterns.Counter
  doctest Ch11.OtpPatterns.CheckInStore
  doctest Ch11.OtpPatterns.PeriodicTask
  doctest Ch11.OtpPatterns.TrafficLight
  doctest Ch11.OtpPatterns.JobQueue

  alias Ch11.OtpPatterns.{
    Counter,
    CheckInStore,
    PeriodicTask,
    TrafficLight,
    JobQueue,
    CounterSupervisor,
    WorkerSupervisor,
    TaskRunner,
    PubSub
  }

  describe "Counter GenServer" do
    test "基本操作" do
      {:ok, counter} = Counter.start_link(0)

      assert Counter.get(counter) == 0

      Counter.increment(counter)
      assert Counter.get(counter) == 1

      Counter.add(counter, 5)
      assert Counter.get(counter) == 6

      Counter.reset(counter)
      assert Counter.get(counter) == 0

      GenServer.stop(counter)
    end

    test "初期値を指定" do
      {:ok, counter} = Counter.start_link(100)
      assert Counter.get(counter) == 100
      GenServer.stop(counter)
    end
  end

  describe "CheckInStore GenServer" do
    test "基本操作" do
      {:ok, store} = CheckInStore.start_link()

      assert CheckInStore.get_count(store, "Tokyo") == 0

      CheckInStore.check_in(store, "Tokyo")
      CheckInStore.check_in(store, "Tokyo")
      CheckInStore.check_in(store, "Sydney")

      assert CheckInStore.get_count(store, "Tokyo") == 2
      assert CheckInStore.get_count(store, "Sydney") == 1

      all = CheckInStore.get_all(store)
      assert all["Tokyo"] == 2
      assert all["Sydney"] == 1

      GenServer.stop(store)
    end

    test "トップ都市を取得" do
      {:ok, store} = CheckInStore.start_link()

      CheckInStore.check_in(store, "Tokyo")
      CheckInStore.check_in(store, "Tokyo")
      CheckInStore.check_in(store, "Tokyo")
      CheckInStore.check_in(store, "Sydney")
      CheckInStore.check_in(store, "Sydney")
      CheckInStore.check_in(store, "Dublin")

      top = CheckInStore.top_cities(store, 2)
      assert length(top) == 2
      assert hd(top) == {"Tokyo", 3}

      GenServer.stop(store)
    end

    test "リセット" do
      {:ok, store} = CheckInStore.start_link()

      CheckInStore.check_in(store, "Tokyo")
      CheckInStore.reset(store)

      assert CheckInStore.get_all(store) == %{}

      GenServer.stop(store)
    end
  end

  describe "PeriodicTask GenServer" do
    test "定期的にタスクを実行" do
      {:ok, agent} = Agent.start_link(fn -> 0 end)

      {:ok, task} =
        PeriodicTask.start_link(
          fn -> Agent.update(agent, &(&1 + 1)) end,
          10
        )

      Process.sleep(50)
      count = Agent.get(agent, & &1)
      assert count > 0

      GenServer.stop(task)
      Agent.stop(agent)
    end
  end

  describe "TrafficLight GenServer" do
    test "状態遷移" do
      {:ok, light} = TrafficLight.start_link()

      assert TrafficLight.current(light) == :red

      TrafficLight.next(light)
      assert TrafficLight.current(light) == :green

      TrafficLight.next(light)
      assert TrafficLight.current(light) == :yellow

      TrafficLight.next(light)
      assert TrafficLight.current(light) == :red

      GenServer.stop(light)
    end
  end

  describe "JobQueue GenServer" do
    test "基本操作" do
      {:ok, queue} = JobQueue.start_link()

      assert JobQueue.is_empty?(queue)
      assert JobQueue.size(queue) == 0
      assert JobQueue.dequeue(queue) == :empty

      JobQueue.enqueue(queue, :job1)
      JobQueue.enqueue(queue, :job2)

      refute JobQueue.is_empty?(queue)
      assert JobQueue.size(queue) == 2

      assert JobQueue.dequeue(queue) == {:ok, :job1}
      assert JobQueue.dequeue(queue) == {:ok, :job2}
      assert JobQueue.dequeue(queue) == :empty

      GenServer.stop(queue)
    end
  end

  describe "CounterSupervisor" do
    test "カウンターを監視" do
      {:ok, sup} = CounterSupervisor.start_link()

      counter = CounterSupervisor.get_counter(sup)
      assert is_pid(counter)

      Counter.increment(counter)
      assert Counter.get(counter) == 1

      Supervisor.stop(sup)
    end
  end

  describe "WorkerSupervisor" do
    test "動的にワーカーを管理" do
      {:ok, sup} = WorkerSupervisor.start_link()

      assert WorkerSupervisor.count_workers(sup) == 0

      {:ok, worker1} = WorkerSupervisor.start_worker(sup, 0)
      {:ok, worker2} = WorkerSupervisor.start_worker(sup, 100)

      assert WorkerSupervisor.count_workers(sup) == 2

      Counter.increment(worker1)
      assert Counter.get(worker1) == 1
      assert Counter.get(worker2) == 100

      WorkerSupervisor.stop_worker(sup, worker1)
      assert WorkerSupervisor.count_workers(sup) == 1

      DynamicSupervisor.stop(sup)
    end
  end

  describe "TaskRunner" do
    test "並行タスクを実行" do
      {:ok, runner} = TaskRunner.start_link()

      results = TaskRunner.run_all(runner, [fn -> 1 end, fn -> 2 end, fn -> 3 end])
      assert Enum.sort(results) == [1, 2, 3]

      Supervisor.stop(runner)
    end

    test "非同期タスクを実行" do
      {:ok, runner} = TaskRunner.start_link()

      task = TaskRunner.run_async(runner, fn -> 42 end)
      assert Task.await(task) == 42

      Supervisor.stop(runner)
    end

    test "タイムアウト付き実行 - 成功" do
      {:ok, runner} = TaskRunner.start_link()

      result = TaskRunner.run_with_timeout(runner, fn -> 42 end, 1000)
      assert result == {:ok, 42}

      Supervisor.stop(runner)
    end

    test "タイムアウト付き実行 - タイムアウト" do
      {:ok, runner} = TaskRunner.start_link()

      result =
        TaskRunner.run_with_timeout(
          runner,
          fn -> Process.sleep(1000) end,
          10
        )

      assert result == {:error, :timeout}

      Supervisor.stop(runner)
    end
  end

  describe "PubSub" do
    test "購読と発行" do
      {:ok, pubsub} = PubSub.start_link()

      PubSub.subscribe(pubsub, "events")
      PubSub.publish(pubsub, "events", :hello)

      assert_receive {:pubsub, "events", :hello}, 100

      GenServer.stop(pubsub)
    end

    test "複数の購読者" do
      {:ok, pubsub} = PubSub.start_link()

      PubSub.subscribe(pubsub, "events")

      subscribers = PubSub.subscribers(pubsub, "events")
      assert length(subscribers) == 1

      GenServer.stop(pubsub)
    end

    test "購読解除" do
      {:ok, pubsub} = PubSub.start_link()

      PubSub.subscribe(pubsub, "events")
      assert length(PubSub.subscribers(pubsub, "events")) == 1

      PubSub.unsubscribe(pubsub, "events")
      assert length(PubSub.subscribers(pubsub, "events")) == 0

      GenServer.stop(pubsub)
    end

    test "異なるトピック" do
      {:ok, pubsub} = PubSub.start_link()

      PubSub.subscribe(pubsub, "topic1")
      PubSub.publish(pubsub, "topic2", :message)

      refute_receive {:pubsub, _, _}, 50

      GenServer.stop(pubsub)
    end
  end
end
