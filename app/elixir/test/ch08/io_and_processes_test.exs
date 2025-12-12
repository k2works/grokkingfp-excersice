defmodule Ch08.IoAndProcessesTest do
  use ExUnit.Case, async: true
  doctest Ch08.IoAndProcesses

  alias Ch08.IoAndProcesses
  alias Ch08.IoAndProcesses.MeetingTime

  describe "delay/1 and run/1" do
    test "遅延実行" do
      io = IoAndProcesses.delay(fn -> 42 end)
      assert is_function(io)
      assert IoAndProcesses.run(io) == 42
    end

    test "副作用のある関数を遅延" do
      {:ok, agent} = Agent.start_link(fn -> 0 end)

      io =
        IoAndProcesses.delay(fn ->
          Agent.update(agent, &(&1 + 1))
          Agent.get(agent, & &1)
        end)

      # まだ実行されていない
      assert Agent.get(agent, & &1) == 0

      # 実行
      result = IoAndProcesses.run(io)
      assert result == 1
      assert Agent.get(agent, & &1) == 1

      Agent.stop(agent)
    end
  end

  describe "pure/1" do
    test "純粋な値をラップ" do
      io = IoAndProcesses.pure(42)
      assert IoAndProcesses.run(io) == 42
    end

    test "複雑な値をラップ" do
      io = IoAndProcesses.pure(%{key: "value"})
      assert IoAndProcesses.run(io) == %{key: "value"}
    end
  end

  describe "flat_map/2" do
    test "IO をチェーン" do
      io1 = IoAndProcesses.pure(10)
      io2 = IoAndProcesses.flat_map(io1, fn x -> IoAndProcesses.pure(x * 2) end)
      assert IoAndProcesses.run(io2) == 20
    end

    test "複数の flat_map をチェーン" do
      result =
        IoAndProcesses.pure(1)
        |> IoAndProcesses.flat_map(fn x -> IoAndProcesses.pure(x + 1) end)
        |> IoAndProcesses.flat_map(fn x -> IoAndProcesses.pure(x * 2) end)
        |> IoAndProcesses.run()

      assert result == 4
    end
  end

  describe "map/2" do
    test "IO の結果を変換" do
      io = IoAndProcesses.pure(5)
      mapped = IoAndProcesses.map(io, &(&1 * 2))
      assert IoAndProcesses.run(mapped) == 10
    end
  end

  describe "sequence/1" do
    test "複数の IO を順番に実行" do
      ios = [
        IoAndProcesses.pure(1),
        IoAndProcesses.pure(2),
        IoAndProcesses.pure(3)
      ]

      io = IoAndProcesses.sequence(ios)
      assert IoAndProcesses.run(io) == [1, 2, 3]
    end

    test "空リスト" do
      io = IoAndProcesses.sequence([])
      assert IoAndProcesses.run(io) == []
    end
  end

  describe "or_else/2" do
    test "成功時は最初の IO の結果" do
      success = IoAndProcesses.pure(42)
      fallback = IoAndProcesses.pure(0)
      result = IoAndProcesses.run(IoAndProcesses.or_else(success, fallback))
      assert result == 42
    end

    test "失敗時はフォールバック" do
      failing = fn -> raise "error" end
      fallback = IoAndProcesses.pure(0)
      result = IoAndProcesses.run(IoAndProcesses.or_else(failing, fallback))
      assert result == 0
    end
  end

  describe "retry/2" do
    test "成功するまでリトライ" do
      counter = :counters.new(1, [:atomics])

      action = fn ->
        count = :counters.get(counter, 1)
        :counters.add(counter, 1, 1)
        if count < 3, do: raise("fail"), else: count
      end

      result = IoAndProcesses.retry(action, 5)
      assert IoAndProcesses.run(result) == 3
    end

    test "リトライ回数超過で例外" do
      action = fn -> raise "always fails" end

      assert_raise RuntimeError, fn ->
        IoAndProcesses.run(IoAndProcesses.retry(action, 3))
      end
    end
  end

  describe "retry_with_default/3" do
    test "成功時は結果を返す" do
      action = IoAndProcesses.pure(42)
      result = IoAndProcesses.retry_with_default(action, 3, :default)
      assert IoAndProcesses.run(result) == 42
    end

    test "失敗時はデフォルト値" do
      action = fn -> raise "fails" end
      result = IoAndProcesses.retry_with_default(action, 3, :default)
      assert IoAndProcesses.run(result) == :default
    end
  end

  describe "cast_the_die/0" do
    test "1から6の値を返す" do
      io = IoAndProcesses.cast_the_die()
      assert is_function(io)

      for _ <- 1..100 do
        result = IoAndProcesses.run(io)
        assert result >= 1 and result <= 6
      end
    end
  end

  describe "cast_the_die_twice/0" do
    test "2から12の値を返す" do
      io = IoAndProcesses.cast_the_die_twice()

      for _ <- 1..100 do
        result = IoAndProcesses.run(io)
        assert result >= 2 and result <= 12
      end
    end
  end

  describe "calendar_entries/1" do
    test "Alice の予定を取得" do
      io = IoAndProcesses.calendar_entries("Alice")
      entries = IoAndProcesses.run(io)
      assert length(entries) == 2
      assert hd(entries).start_hour == 9
    end

    test "存在しない人の予定は空" do
      io = IoAndProcesses.calendar_entries("Unknown")
      entries = IoAndProcesses.run(io)
      assert entries == []
    end
  end

  describe "scheduled_meetings/2" do
    test "2人の予定を結合" do
      io = IoAndProcesses.scheduled_meetings("Alice", "Bob")
      meetings = IoAndProcesses.run(io)
      assert length(meetings) == 4
    end
  end

  describe "scheduled_meetings_for/1" do
    test "複数人の予定を結合" do
      io = IoAndProcesses.scheduled_meetings_for(["Alice", "Bob"])
      meetings = IoAndProcesses.run(io)
      assert length(meetings) == 4
    end

    test "空リストは空の予定" do
      io = IoAndProcesses.scheduled_meetings_for([])
      meetings = IoAndProcesses.run(io)
      assert meetings == []
    end
  end

  describe "meetings_overlap?/2" do
    test "重ならない場合" do
      m1 = %MeetingTime{start_hour: 9, end_hour: 10}
      m2 = %MeetingTime{start_hour: 10, end_hour: 11}
      refute IoAndProcesses.meetings_overlap?(m1, m2)
    end

    test "重なる場合" do
      m1 = %MeetingTime{start_hour: 9, end_hour: 11}
      m2 = %MeetingTime{start_hour: 10, end_hour: 12}
      assert IoAndProcesses.meetings_overlap?(m1, m2)
    end

    test "完全に含まれる場合" do
      m1 = %MeetingTime{start_hour: 9, end_hour: 12}
      m2 = %MeetingTime{start_hour: 10, end_hour: 11}
      assert IoAndProcesses.meetings_overlap?(m1, m2)
    end
  end

  describe "possible_meetings/4" do
    test "空き時間を計算" do
      existing = [
        %MeetingTime{start_hour: 9, end_hour: 10},
        %MeetingTime{start_hour: 14, end_hour: 15}
      ]

      possible = IoAndProcesses.possible_meetings(existing, 8, 17, 1)
      start_hours = Enum.map(possible, & &1.start_hour)
      assert start_hours == [8, 10, 11, 12, 13, 15, 16]
    end

    test "既存の予定がない場合" do
      possible = IoAndProcesses.possible_meetings([], 9, 17, 1)
      assert length(possible) == 8
    end

    test "2時間のミーティング" do
      existing = [%MeetingTime{start_hour: 12, end_hour: 14}]
      possible = IoAndProcesses.possible_meetings(existing, 9, 17, 2)
      start_hours = Enum.map(possible, & &1.start_hour)
      assert start_hours == [9, 10, 14, 15]
    end
  end

  describe "Agent を使ったカウンター" do
    test "カウンターの基本操作" do
      {:ok, counter} = IoAndProcesses.create_counter(0)

      assert IoAndProcesses.get_count(counter) == 0

      IoAndProcesses.increment(counter)
      assert IoAndProcesses.get_count(counter) == 1

      IoAndProcesses.increment(counter)
      assert IoAndProcesses.get_count(counter) == 2

      Agent.stop(counter)
    end

    test "add で指定値を加算" do
      {:ok, counter} = IoAndProcesses.create_counter(10)

      IoAndProcesses.add(counter, 5)
      assert IoAndProcesses.get_count(counter) == 15

      IoAndProcesses.add(counter, -3)
      assert IoAndProcesses.get_count(counter) == 12

      Agent.stop(counter)
    end
  end

  describe "Task を使った非同期処理" do
    test "非同期でカレンダーを取得" do
      task = IoAndProcesses.async_calendar_entries("Alice")
      entries = Task.await(task)
      assert length(entries) == 2
    end

    test "並行でカレンダーを取得" do
      entries = IoAndProcesses.parallel_calendar_entries(["Alice", "Bob"])
      assert length(entries) == 4
    end
  end

  describe "combine_io/3" do
    test "2つの IO を合成" do
      io =
        IoAndProcesses.combine_io(
          IoAndProcesses.pure(1),
          IoAndProcesses.pure(2),
          &(&1 + &2)
        )

      assert IoAndProcesses.run(io) == 3
    end

    test "文字列の結合" do
      io =
        IoAndProcesses.combine_io(
          IoAndProcesses.pure("Hello"),
          IoAndProcesses.pure("World"),
          &"#{&1} #{&2}"
        )

      assert IoAndProcesses.run(io) == "Hello World"
    end
  end
end
