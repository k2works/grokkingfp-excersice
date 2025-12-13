# frozen_string_literal: true

# Chapter 8: IO Monad - Managing Side Effects
# IO モナド - 副作用の管理
module Ch08IO
  # ===========================================================================
  # 8.1 IO Monad Implementation
  # ===========================================================================

  # IO monad represents a computation that may have side effects
  # IO モナドは副作用を持つ可能性のある計算を表す
  class IO
    attr_reader :computation

    def initialize(&computation)
      @computation = computation
    end

    # Create IO from a side-effectful computation (lazy)
    # 副作用のある計算から IO を作成（遅延評価）
    def self.delay(&block)
      new(&block)
    end

    # Create IO from a pure value (no side effect)
    # 純粋な値から IO を作成（副作用なし）
    def self.pure(value)
      new { value }
    end

    # IO that does nothing and returns nil
    # 何もせず nil を返す IO
    def self.unit
      pure(nil)
    end

    # Run the IO and return the result (unsafe!)
    # IO を実行して結果を返す（安全でない！）
    def run!
      @computation.call
    end

    alias unsafe_run_sync run!

    # Map: transform the result
    # Map: 結果を変換
    def fmap(&fn)
      IO.new { fn.call(run!) }
    end

    alias map fmap

    # Bind/FlatMap: chain IO operations
    # Bind/FlatMap: IO 操作を連鎖
    def bind(&fn)
      IO.new { fn.call(run!).run! }
    end

    alias flat_map bind

    # OrElse: provide fallback on failure
    # OrElse: 失敗時のフォールバック
    def or_else(fallback_io)
      IO.new do
        run!
      rescue StandardError
        fallback_io.run!
      end
    end

    # Handle errors with a recovery function
    # エラーを回復関数で処理
    def recover(&fn)
      IO.new do
        run!
      rescue StandardError => e
        fn.call(e)
      end
    end

    # Combine two IOs
    # 2つの IO を組み合わせる
    def product(other)
      bind do |a|
        other.fmap { |b| [a, b] }
      end
    end

    # Inspect (for debugging)
    def inspect
      'IO(...)'
    end

    def to_s
      inspect
    end
  end

  # ===========================================================================
  # 8.2 Casting Die Example
  # ===========================================================================

  # Impure function - has side effect (random)
  # 不純な関数 - 副作用あり（乱数）
  def self.cast_the_die_impure
    rand(1..6)
  end

  # Pure function using IO monad
  # IO モナドを使った純粋な関数
  def self.cast_the_die
    IO.delay { cast_the_die_impure }
  end

  # Cast die twice and sum the results
  # サイコロを2回振って合計を返す
  def self.cast_the_die_twice
    cast_the_die.bind do |first|
      cast_the_die.fmap { |second| first + second }
    end
  end

  # ===========================================================================
  # 8.3 IO Creation Methods
  # ===========================================================================

  # Delayed print (side effect deferred)
  # 遅延プリント（副作用は遅延される）
  def self.delayed_print(message)
    IO.delay { puts message }
  end

  # Pure value wrapped in IO
  # 純粋な値を IO でラップ
  def self.pure_value(value)
    IO.pure(value)
  end

  # ===========================================================================
  # 8.4 Meeting Scheduling Example
  # ===========================================================================

  MeetingTime = Struct.new(:start_hour, :end_hour, keyword_init: true)

  # Simulated API call (impure)
  # シミュレートされた API 呼び出し（不純）
  def self.calendar_entries_api_call(name)
    # Simulate API response
    entries = {
      'Alice' => [
        MeetingTime.new(start_hour: 8, end_hour: 10),
        MeetingTime.new(start_hour: 14, end_hour: 15)
      ],
      'Bob' => [
        MeetingTime.new(start_hour: 9, end_hour: 11),
        MeetingTime.new(start_hour: 16, end_hour: 17)
      ],
      'Charlie' => [
        MeetingTime.new(start_hour: 10, end_hour: 12)
      ]
    }
    entries.fetch(name, [])
  end

  # Wrap API call in IO
  # API 呼び出しを IO でラップ
  def self.calendar_entries(name)
    IO.delay { calendar_entries_api_call(name) }
  end

  # Get scheduled meetings for two people
  # 2人の予定されたミーティングを取得
  def self.scheduled_meetings(person1, person2)
    calendar_entries(person1).bind do |entries1|
      calendar_entries(person2).fmap do |entries2|
        entries1 + entries2
      end
    end
  end

  # Check if two meetings overlap (pure function)
  # 2つのミーティングが重複するか確認（純粋関数）
  def self.meetings_overlap?(meeting1, meeting2)
    meeting1.end_hour > meeting2.start_hour &&
      meeting2.end_hour > meeting1.start_hour
  end

  # Find possible meeting slots (pure function)
  # 利用可能なミーティングスロットを見つける（純粋関数）
  def self.possible_meetings(existing_meetings, start_hour, end_hour, length_hours)
    slots = (start_hour..(end_hour - length_hours)).map do |start|
      MeetingTime.new(start_hour: start, end_hour: start + length_hours)
    end

    slots.select do |slot|
      existing_meetings.none? { |meeting| meetings_overlap?(meeting, slot) }
    end
  end

  # ===========================================================================
  # 8.5 orElse for Error Handling
  # ===========================================================================

  # Simulated failing API call
  # 失敗をシミュレートする API 呼び出し
  def self.failing_api_call
    IO.delay { raise 'API Error' }
  end

  # Successful API call
  # 成功する API 呼び出し
  def self.successful_api_call(value)
    IO.pure(value)
  end

  # ===========================================================================
  # 8.6 Retry Strategy
  # ===========================================================================

  # Retry an IO action up to max_retries times
  # IO アクションを最大 max_retries 回リトライ
  def self.retry_action(action, max_retries)
    (0...max_retries).reduce(action) do |program, _|
      program.or_else(action)
    end
  end

  # Retry with default value
  # デフォルト値付きリトライ
  def self.retry_with_default(action, max_retries, default)
    retry_action(action, max_retries).or_else(IO.pure(default))
  end

  # ===========================================================================
  # 8.7 Sequence - List[IO[A]] to IO[List[A]]
  # ===========================================================================

  # Convert array of IOs to IO of array
  # IO の配列を配列の IO に変換
  def self.sequence(io_array)
    io_array.reduce(IO.pure([])) do |acc, io|
      acc.bind do |results|
        io.fmap { |result| results + [result] }
      end
    end
  end

  # Map over array and sequence results
  # 配列をマップしてシーケンス
  def self.traverse(array, &fn)
    sequence(array.map(&fn))
  end

  # Get scheduled meetings for multiple attendees
  # 複数の参加者の予定されたミーティングを取得
  def self.scheduled_meetings_for_all(attendees)
    traverse(attendees) do |attendee|
      retry_with_default(calendar_entries(attendee), 3, [])
    end.fmap(&:flatten)
  end

  # ===========================================================================
  # 8.8 Combining IOs
  # ===========================================================================

  # Combine two IOs with a function
  # 2つの IO を関数で結合
  def self.combine_io(io1, io2, &fn)
    io1.bind do |a|
      io2.fmap { |b| fn.call(a, b) }
    end
  end

  # Map over two IOs
  # 2つの IO をマップ
  def self.map2(io1, io2, &fn)
    combine_io(io1, io2, &fn)
  end

  # ===========================================================================
  # 8.9 Practical Examples
  # ===========================================================================

  # Print and return a message
  # メッセージを表示して返す
  def self.print_and_return(message)
    IO.delay do
      puts message
      message
    end
  end

  # Read from console (simulated)
  # コンソールから読み取り（シミュレート）
  def self.read_line(prompt, input_value = nil)
    IO.delay do
      puts prompt
      input_value || gets&.chomp
    end
  end

  # Get current time
  # 現在時刻を取得
  def self.current_time
    IO.delay { Time.now }
  end

  # Simulate random delay
  # ランダムな遅延をシミュレート
  def self.random_delay(max_seconds)
    IO.delay { sleep(rand(0.0..max_seconds)) }
  end

  # ===========================================================================
  # 8.10 For-comprehension style (using bind chains)
  # ===========================================================================

  # Schedule a meeting (using bind chains like for-comprehension)
  # ミーティングをスケジュール（for 内包表記のような bind チェーン）
  def self.schedule_meeting(person1, person2, length_hours)
    scheduled_meetings(person1, person2).fmap do |existing|
      possible_meetings(existing, 8, 18, length_hours)
    end
  end

  # Complete meeting scheduling workflow
  # 完全なミーティングスケジュールワークフロー
  def self.meeting_workflow(person1, person2, length_hours)
    schedule_meeting(person1, person2, length_hours).bind do |possible|
      if possible.empty?
        IO.pure(nil)
      else
        IO.pure(possible.first)
      end
    end
  end
end
