defmodule Ch08.IoAndProcesses do
  @moduledoc """
  第8章: IO とプロセス

  Elixir での副作用の扱い方:
  - 無名関数による遅延実行（IO モナドの代替）
  - Agent による状態管理
  - Task による非同期処理
  """

  # =============================================================================
  # 遅延実行パターン（IO モナドの Elixir 版）
  # =============================================================================

  @doc """
  副作用を遅延実行するための関数を作成

  Scala の IO.delay に相当。関数を返すことで実行を遅延させる。

  ## Examples

      iex> io = Ch08.IoAndProcesses.delay(fn -> 42 end)
      iex> is_function(io)
      true
      iex> Ch08.IoAndProcesses.run(io)
      42

  """
  @spec delay((-> any())) :: (-> any())
  def delay(f) when is_function(f, 0), do: f

  @doc """
  遅延実行された関数を実行

  Scala の unsafeRunSync() に相当。

  ## Examples

      iex> io = Ch08.IoAndProcesses.delay(fn -> "hello" end)
      iex> Ch08.IoAndProcesses.run(io)
      "hello"

  """
  @spec run((-> any())) :: any()
  def run(f) when is_function(f, 0), do: f.()

  @doc """
  純粋な値を IO にラップ

  Scala の IO.pure に相当。

  ## Examples

      iex> io = Ch08.IoAndProcesses.pure(42)
      iex> Ch08.IoAndProcesses.run(io)
      42

  """
  @spec pure(any()) :: (-> any())
  def pure(value), do: fn -> value end

  @doc """
  2つの IO を順番に実行して結果を合成

  Scala の flatMap に相当。

  ## Examples

      iex> io1 = Ch08.IoAndProcesses.pure(10)
      iex> io2 = Ch08.IoAndProcesses.flat_map(io1, fn x -> Ch08.IoAndProcesses.pure(x * 2) end)
      iex> Ch08.IoAndProcesses.run(io2)
      20

  """
  @spec flat_map((-> any()), (any() -> (-> any()))) :: (-> any())
  def flat_map(io, f) do
    fn ->
      result = run(io)
      run(f.(result))
    end
  end

  @doc """
  IO の結果を変換

  ## Examples

      iex> io = Ch08.IoAndProcesses.pure(5)
      iex> mapped = Ch08.IoAndProcesses.map(io, &(&1 * 2))
      iex> Ch08.IoAndProcesses.run(mapped)
      10

  """
  @spec map((-> any()), (any() -> any())) :: (-> any())
  def map(io, f) do
    fn -> f.(run(io)) end
  end

  @doc """
  複数の IO を順番に実行

  Scala の for 内包表記に相当。

  ## Examples

      iex> io = Ch08.IoAndProcesses.sequence([
      ...>   Ch08.IoAndProcesses.pure(1),
      ...>   Ch08.IoAndProcesses.pure(2),
      ...>   Ch08.IoAndProcesses.pure(3)
      ...> ])
      iex> Ch08.IoAndProcesses.run(io)
      [1, 2, 3]

  """
  @spec sequence([(-> any())]) :: (-> [any()])
  def sequence(ios) do
    fn -> Enum.map(ios, &run/1) end
  end

  @doc """
  失敗時に代替を実行

  Scala の orElse に相当。

  ## Examples

      iex> success = Ch08.IoAndProcesses.pure(42)
      iex> fallback = Ch08.IoAndProcesses.pure(0)
      iex> Ch08.IoAndProcesses.run(Ch08.IoAndProcesses.or_else(success, fallback))
      42

  """
  @spec or_else((-> any()), (-> any())) :: (-> any())
  def or_else(io, fallback) do
    fn ->
      try do
        run(io)
      rescue
        _ -> run(fallback)
      end
    end
  end

  @doc """
  リトライ付きで実行

  ## Examples

      iex> counter = :counters.new(1, [:atomics])
      iex> action = fn ->
      ...>   count = :counters.get(counter, 1)
      ...>   :counters.add(counter, 1, 1)
      ...>   if count < 3, do: raise("fail"), else: count
      ...> end
      iex> result = Ch08.IoAndProcesses.retry(action, 5)
      iex> Ch08.IoAndProcesses.run(result)
      3

  """
  @spec retry((-> any()), non_neg_integer()) :: (-> any())
  def retry(io, max_retries) do
    fn ->
      do_retry(io, max_retries, nil)
    end
  end

  defp do_retry(_io, 0, last_error), do: raise(last_error || "Max retries exceeded")

  defp do_retry(io, retries_left, _last_error) do
    try do
      run(io)
    rescue
      e -> do_retry(io, retries_left - 1, e)
    end
  end

  @doc """
  リトライ付きで実行（失敗時はデフォルト値）

  ## Examples

      iex> failing = fn -> raise "always fails" end
      iex> result = Ch08.IoAndProcesses.retry_with_default(failing, 3, :default)
      iex> Ch08.IoAndProcesses.run(result)
      :default

  """
  @spec retry_with_default((-> any()), non_neg_integer(), any()) :: (-> any())
  def retry_with_default(io, max_retries, default) do
    fn ->
      try do
        run(retry(io, max_retries))
      rescue
        _ -> default
      end
    end
  end

  # =============================================================================
  # サイコロの例
  # =============================================================================

  @doc """
  サイコロを振る（不純な関数）

  ## Examples

      iex> result = Ch08.IoAndProcesses.cast_the_die_impure()
      iex> result >= 1 and result <= 6
      true

  """
  @spec cast_the_die_impure() :: integer()
  def cast_the_die_impure do
    :rand.uniform(6)
  end

  @doc """
  サイコロを振る（IO版）

  ## Examples

      iex> io = Ch08.IoAndProcesses.cast_the_die()
      iex> is_function(io)
      true
      iex> result = Ch08.IoAndProcesses.run(io)
      iex> result >= 1 and result <= 6
      true

  """
  @spec cast_the_die() :: (-> integer())
  def cast_the_die do
    delay(fn -> cast_the_die_impure() end)
  end

  @doc """
  サイコロを2回振って合計

  ## Examples

      iex> io = Ch08.IoAndProcesses.cast_the_die_twice()
      iex> result = Ch08.IoAndProcesses.run(io)
      iex> result >= 2 and result <= 12
      true

  """
  @spec cast_the_die_twice() :: (-> integer())
  def cast_the_die_twice do
    flat_map(cast_the_die(), fn first ->
      map(cast_the_die(), fn second ->
        first + second
      end)
    end)
  end

  # =============================================================================
  # ミーティングスケジューリングの例
  # =============================================================================

  defmodule MeetingTime do
    @moduledoc "ミーティング時間"
    defstruct [:start_hour, :end_hour]

    @type t :: %__MODULE__{
            start_hour: integer(),
            end_hour: integer()
          }
  end

  @doc """
  カレンダーエントリを取得（モック）

  ## Examples

      iex> io = Ch08.IoAndProcesses.calendar_entries("Alice")
      iex> entries = Ch08.IoAndProcesses.run(io)
      iex> is_list(entries)
      true

  """
  @spec calendar_entries(String.t()) :: (-> [MeetingTime.t()])
  def calendar_entries(name) do
    delay(fn ->
      # モックデータ
      case name do
        "Alice" ->
          [
            %MeetingTime{start_hour: 9, end_hour: 10},
            %MeetingTime{start_hour: 14, end_hour: 15}
          ]

        "Bob" ->
          [
            %MeetingTime{start_hour: 10, end_hour: 11},
            %MeetingTime{start_hour: 15, end_hour: 16}
          ]

        _ ->
          []
      end
    end)
  end

  @doc """
  2人の予定を取得

  ## Examples

      iex> io = Ch08.IoAndProcesses.scheduled_meetings("Alice", "Bob")
      iex> meetings = Ch08.IoAndProcesses.run(io)
      iex> length(meetings)
      4

  """
  @spec scheduled_meetings(String.t(), String.t()) :: (-> [MeetingTime.t()])
  def scheduled_meetings(person1, person2) do
    flat_map(calendar_entries(person1), fn entries1 ->
      map(calendar_entries(person2), fn entries2 ->
        entries1 ++ entries2
      end)
    end)
  end

  @doc """
  複数人の予定を取得

  ## Examples

      iex> io = Ch08.IoAndProcesses.scheduled_meetings_for(["Alice", "Bob"])
      iex> meetings = Ch08.IoAndProcesses.run(io)
      iex> length(meetings)
      4

  """
  @spec scheduled_meetings_for([String.t()]) :: (-> [MeetingTime.t()])
  def scheduled_meetings_for(attendees) do
    attendees
    |> Enum.map(&calendar_entries/1)
    |> sequence()
    |> map(&List.flatten/1)
  end

  @doc """
  ミーティングが重なるかチェック

  ## Examples

      iex> m1 = %Ch08.IoAndProcesses.MeetingTime{start_hour: 9, end_hour: 10}
      iex> m2 = %Ch08.IoAndProcesses.MeetingTime{start_hour: 10, end_hour: 11}
      iex> Ch08.IoAndProcesses.meetings_overlap?(m1, m2)
      false

      iex> m1 = %Ch08.IoAndProcesses.MeetingTime{start_hour: 9, end_hour: 11}
      iex> m2 = %Ch08.IoAndProcesses.MeetingTime{start_hour: 10, end_hour: 12}
      iex> Ch08.IoAndProcesses.meetings_overlap?(m1, m2)
      true

  """
  @spec meetings_overlap?(MeetingTime.t(), MeetingTime.t()) :: boolean()
  def meetings_overlap?(%MeetingTime{} = m1, %MeetingTime{} = m2) do
    m1.start_hour < m2.end_hour && m2.start_hour < m1.end_hour
  end

  @doc """
  空き時間を計算（純粋関数）

  ## Examples

      iex> existing = [
      ...>   %Ch08.IoAndProcesses.MeetingTime{start_hour: 9, end_hour: 10},
      ...>   %Ch08.IoAndProcesses.MeetingTime{start_hour: 14, end_hour: 15}
      ...> ]
      iex> possible = Ch08.IoAndProcesses.possible_meetings(existing, 8, 17, 1)
      iex> Enum.map(possible, & &1.start_hour)
      [8, 10, 11, 12, 13, 15, 16]

  """
  @spec possible_meetings([MeetingTime.t()], integer(), integer(), integer()) :: [MeetingTime.t()]
  def possible_meetings(existing_meetings, start_hour, end_hour, length_hours) do
    slots =
      start_hour..(end_hour - length_hours)
      |> Enum.map(fn start ->
        %MeetingTime{start_hour: start, end_hour: start + length_hours}
      end)

    Enum.filter(slots, fn slot ->
      Enum.all?(existing_meetings, fn meeting ->
        not meetings_overlap?(meeting, slot)
      end)
    end)
  end

  # =============================================================================
  # Agent を使った状態管理
  # =============================================================================

  @doc """
  カウンターを作成

  ## Examples

      iex> {:ok, counter} = Ch08.IoAndProcesses.create_counter(0)
      iex> Ch08.IoAndProcesses.get_count(counter)
      0
      iex> Ch08.IoAndProcesses.increment(counter)
      iex> Ch08.IoAndProcesses.get_count(counter)
      1

  """
  @spec create_counter(integer()) :: {:ok, pid()}
  def create_counter(initial_value) do
    Agent.start_link(fn -> initial_value end)
  end

  @doc """
  カウンターの値を取得

  ## Examples

      iex> {:ok, counter} = Ch08.IoAndProcesses.create_counter(42)
      iex> Ch08.IoAndProcesses.get_count(counter)
      42

  """
  @spec get_count(pid()) :: integer()
  def get_count(counter) do
    Agent.get(counter, & &1)
  end

  @doc """
  カウンターをインクリメント

  ## Examples

      iex> {:ok, counter} = Ch08.IoAndProcesses.create_counter(0)
      iex> Ch08.IoAndProcesses.increment(counter)
      iex> Ch08.IoAndProcesses.increment(counter)
      iex> Ch08.IoAndProcesses.get_count(counter)
      2

  """
  @spec increment(pid()) :: :ok
  def increment(counter) do
    Agent.update(counter, &(&1 + 1))
  end

  @doc """
  カウンターを指定値だけ増加

  ## Examples

      iex> {:ok, counter} = Ch08.IoAndProcesses.create_counter(0)
      iex> Ch08.IoAndProcesses.add(counter, 5)
      iex> Ch08.IoAndProcesses.get_count(counter)
      5

  """
  @spec add(pid(), integer()) :: :ok
  def add(counter, value) do
    Agent.update(counter, &(&1 + value))
  end

  # =============================================================================
  # Task を使った非同期処理
  # =============================================================================

  @doc """
  非同期でカレンダーを取得

  ## Examples

      iex> task = Ch08.IoAndProcesses.async_calendar_entries("Alice")
      iex> entries = Task.await(task)
      iex> is_list(entries)
      true

  """
  @spec async_calendar_entries(String.t()) :: Task.t()
  def async_calendar_entries(name) do
    Task.async(fn -> run(calendar_entries(name)) end)
  end

  @doc """
  複数人のカレンダーを並行取得

  ## Examples

      iex> entries = Ch08.IoAndProcesses.parallel_calendar_entries(["Alice", "Bob"])
      iex> length(entries)
      4

  """
  @spec parallel_calendar_entries([String.t()]) :: [MeetingTime.t()]
  def parallel_calendar_entries(names) do
    names
    |> Enum.map(&async_calendar_entries/1)
    |> Task.await_many()
    |> List.flatten()
  end

  # =============================================================================
  # 印刷の例
  # =============================================================================

  @doc """
  メッセージを表示して返す

  ## Examples

      iex> io = Ch08.IoAndProcesses.print_and_return("hello")
      iex> is_function(io)
      true

  """
  @spec print_and_return(String.t()) :: (-> String.t())
  def print_and_return(message) do
    delay(fn ->
      IO.puts(message)
      message
    end)
  end

  @doc """
  2つの IO を合成

  ## Examples

      iex> io = Ch08.IoAndProcesses.combine_io(
      ...>   Ch08.IoAndProcesses.pure(1),
      ...>   Ch08.IoAndProcesses.pure(2),
      ...>   &(&1 + &2)
      ...> )
      iex> Ch08.IoAndProcesses.run(io)
      3

  """
  @spec combine_io((-> any()), (-> any()), (any(), any() -> any())) :: (-> any())
  def combine_io(io1, io2, f) do
    flat_map(io1, fn a ->
      map(io2, fn b ->
        f.(a, b)
      end)
    end)
  end
end
