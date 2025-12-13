# frozen_string_literal: true

require_relative 'ch08_io'

# Chapter 10: Concurrency - Parallel and Concurrent Processing
# 並行処理 - 並列・並行プログラミング
module Ch10Concurrency
  # ===========================================================================
  # 10.1 Ref - Atomic Shared State
  # ===========================================================================

  # Ref provides thread-safe atomic operations on a value
  # Ref はスレッドセーフなアトミック操作を提供
  class Ref
    def initialize(initial_value)
      @value = initial_value
      @mutex = Mutex.new
    end

    # Create a new Ref with initial value (returns IO)
    # 初期値で新しい Ref を作成（IO を返す）
    def self.of(initial_value)
      Ch08IO::IO.delay { new(initial_value) }
    end

    # Get current value (returns IO)
    # 現在の値を取得（IO を返す）
    def get
      Ch08IO::IO.delay { @mutex.synchronize { @value } }
    end

    # Set value (returns IO)
    # 値を設定（IO を返す）
    def set(new_value)
      Ch08IO::IO.delay { @mutex.synchronize { @value = new_value } }
    end

    # Update value atomically (returns IO)
    # 値をアトミックに更新（IO を返す）
    def update(&fn)
      Ch08IO::IO.delay do
        @mutex.synchronize { @value = fn.call(@value) }
      end
    end

    # Modify and return old value (returns IO)
    # 変更して古い値を返す（IO を返す）
    def modify(&fn)
      Ch08IO::IO.delay do
        @mutex.synchronize do
          old_value = @value
          new_value, result = fn.call(old_value)
          @value = new_value
          result
        end
      end
    end

    # Update and get new value (returns IO)
    # 更新して新しい値を取得（IO を返す）
    def update_and_get(&fn)
      Ch08IO::IO.delay do
        @mutex.synchronize do
          @value = fn.call(@value)
          @value
        end
      end
    end

    # Get and update (returns IO with old value)
    # 取得して更新（古い値を返す IO）
    def get_and_update(&fn)
      Ch08IO::IO.delay do
        @mutex.synchronize do
          old_value = @value
          @value = fn.call(@value)
          old_value
        end
      end
    end
  end

  # ===========================================================================
  # 10.2 Parallel Execution
  # ===========================================================================

  # Execute IOs in parallel and collect results
  # IO を並列実行して結果を収集
  def self.par_sequence(ios)
    Ch08IO::IO.delay do
      threads = ios.map do |io|
        Thread.new { io.run! }
      end
      threads.map(&:value)
    end
  end

  # Map over array in parallel
  # 配列を並列にマップ
  def self.par_traverse(array, &fn)
    par_sequence(array.map(&fn))
  end

  # Execute two IOs in parallel and combine results
  # 2つの IO を並列実行して結果を結合
  def self.par_map2(io1, io2, &fn)
    Ch08IO::IO.delay do
      t1 = Thread.new { io1.run! }
      t2 = Thread.new { io2.run! }
      fn.call(t1.value, t2.value)
    end
  end

  # Execute two IOs in parallel and return both results
  # 2つの IO を並列実行して両方の結果を返す
  def self.par_both(io1, io2)
    par_map2(io1, io2) { |a, b| [a, b] }
  end

  # Race two IOs, return the first to complete
  # 2つの IO を競争させ、最初に完了したものを返す
  def self.race(io1, io2)
    Ch08IO::IO.delay do
      result = nil
      done = false
      mutex = Mutex.new
      cv = ConditionVariable.new

      [io1, io2].each do |io|
        Thread.new do
          value = io.run!
          mutex.synchronize do
            unless done
              result = value
              done = true
              cv.signal
            end
          end
        end
      end

      mutex.synchronize do
        cv.wait(mutex) until done
        result
      end
    end
  end

  # ===========================================================================
  # 10.3 Fiber - Lightweight Thread Simulation
  # ===========================================================================

  # FiberHandle represents a running background computation
  # FiberHandle はバックグラウンドで実行中の計算を表す
  class FiberHandle
    attr_reader :thread

    def initialize(thread, cancel_flag)
      @thread = thread
      @cancel_flag = cancel_flag
    end

    # Wait for fiber to complete and get result
    # Fiber の完了を待ち、結果を取得
    def join
      Ch08IO::IO.delay { @thread.value }
    end

    # Cancel the fiber
    # Fiber をキャンセル
    def cancel
      Ch08IO::IO.delay do
        @cancel_flag[:cancelled] = true
        @thread.kill if @thread.alive?
      end
    end

    # Check if fiber is still running
    # Fiber がまだ実行中か確認
    def alive?
      @thread.alive?
    end
  end

  # Start an IO as a background fiber
  # IO をバックグラウンド Fiber として開始
  def self.start(io)
    Ch08IO::IO.delay do
      cancel_flag = { cancelled: false }
      thread = Thread.new do
        io.run! unless cancel_flag[:cancelled]
      end
      FiberHandle.new(thread, cancel_flag)
    end
  end

  # ===========================================================================
  # 10.4 Forever and Repeat
  # ===========================================================================

  # Run an IO forever (until cancelled)
  # IO を永遠に実行（キャンセルされるまで）
  def self.forever_m(io)
    Ch08IO::IO.delay do
      loop do
        io.run!
      end
    end
  end

  # Run an IO n times
  # IO を n 回実行
  def self.repeat_n(io, n)
    Ch08IO::IO.delay do
      n.times.map { io.run! }
    end
  end

  # ===========================================================================
  # 10.5 Sleep
  # ===========================================================================

  # Sleep for specified seconds (non-blocking for other threads)
  # 指定秒数スリープ（他のスレッドをブロックしない）
  def self.sleep_io(seconds)
    Ch08IO::IO.delay { sleep(seconds) }
  end

  # Sleep for specified milliseconds
  # 指定ミリ秒スリープ
  def self.sleep_millis(millis)
    sleep_io(millis / 1000.0)
  end

  # ===========================================================================
  # 10.6 Check-ins Example
  # ===========================================================================

  City = Struct.new(:name, keyword_init: true)
  CityStats = Struct.new(:city, :check_ins, keyword_init: true)

  # Calculate top N cities from check-in counts (pure function)
  # チェックイン数からトップ N 都市を計算（純粋関数）
  def self.top_cities(city_check_ins, n = 3)
    city_check_ins
      .map { |city, check_ins| CityStats.new(city: city, check_ins: check_ins) }
      .sort_by { |stats| -stats.check_ins }
      .take(n)
  end

  # Store a check-in (updates Ref)
  # チェックインを保存（Ref を更新）
  def self.store_check_in(stored_check_ins, city)
    stored_check_ins.update do |check_ins|
      current = check_ins[city] || 0
      check_ins.merge(city => current + 1)
    end
  end

  # Update ranking from check-ins (pure function wrapped in IO)
  # チェックインからランキングを更新（IO でラップした純粋関数）
  def self.update_ranking(stored_check_ins, stored_ranking)
    stored_check_ins.get.bind do |check_ins|
      stored_ranking.set(top_cities(check_ins))
    end
  end

  # Process check-ins sequentially
  # チェックインを逐次処理
  def self.process_check_ins_sequential(check_ins, stored_check_ins)
    Ch08IO::IO.delay do
      check_ins.each do |city|
        store_check_in(stored_check_ins, city).run!
      end
    end
  end

  # ===========================================================================
  # 10.7 Concurrent Check-in Processing
  # ===========================================================================

  # Result of processing check-ins
  # チェックイン処理の結果
  ProcessingResult = Struct.new(:current_ranking, :stop, keyword_init: true)

  # Process check-ins concurrently with ranking updates
  # ランキング更新と並行してチェックインを処理
  def self.process_check_ins_concurrent(check_ins)
    Ch08IO::IO.delay do
      stored_check_ins = Ref.new({})
      stored_ranking = Ref.new([])

      cancel_flag = { cancelled: false }

      # Ranking update thread (runs continuously)
      ranking_thread = Thread.new do
        until cancel_flag[:cancelled]
          update_ranking(stored_check_ins, stored_ranking).run!
          sleep(0.01) # Small delay to prevent busy-waiting
        end
      end

      # Check-in processing thread
      checkin_thread = Thread.new do
        check_ins.each do |city|
          break if cancel_flag[:cancelled]

          store_check_in(stored_check_ins, city).run!
        end
      end

      # Wait for check-ins to complete
      checkin_thread.join

      # Allow final ranking update
      sleep(0.05)

      # Signal cancellation
      cancel_flag[:cancelled] = true
      ranking_thread.join

      stored_ranking.get.run!
    end
  end

  # Start processing and return handle for async control
  # 処理を開始し、非同期制御用のハンドルを返す
  def self.start_processing(check_ins)
    Ch08IO::IO.delay do
      stored_check_ins = Ref.new({})
      stored_ranking = Ref.new([])

      cancel_flag = { cancelled: false }

      # Start ranking update thread
      ranking_thread = Thread.new do
        until cancel_flag[:cancelled]
          update_ranking(stored_check_ins, stored_ranking).run!
          sleep(0.01)
        end
      end

      # Start check-in processing thread
      checkin_thread = Thread.new do
        check_ins.each do |city|
          break if cancel_flag[:cancelled]

          store_check_in(stored_check_ins, city).run!
        end
      end

      # Return control interface
      current_ranking = Ch08IO::IO.delay { stored_ranking.get.run! }
      stop = Ch08IO::IO.delay do
        cancel_flag[:cancelled] = true
        checkin_thread.kill if checkin_thread.alive?
        ranking_thread.join
      end

      ProcessingResult.new(current_ranking: current_ranking, stop: stop)
    end
  end

  # ===========================================================================
  # 10.8 Parallel Die Casting
  # ===========================================================================

  # Cast die twice in parallel and sum
  # サイコロを並列に2回振って合計
  def self.cast_die_twice_parallel
    par_map2(
      Ch08IO.cast_the_die,
      Ch08IO.cast_the_die
    ) { |a, b| a + b }
  end

  # Cast die n times in parallel
  # サイコロを並列に n 回振る
  def self.cast_die_n_parallel(n)
    ios = Array.new(n) { Ch08IO.cast_the_die }
    par_sequence(ios)
  end

  # Cast die n times in parallel and store in Ref
  # サイコロを並列に n 回振って Ref に保存
  def self.cast_die_n_with_ref(n)
    Ref.of([]).bind do |stored_casts|
      single_cast = Ch08IO.cast_the_die.bind do |result|
        stored_casts.update { |casts| casts + [result] }
      end
      ios = Array.new(n) { single_cast }
      par_sequence(ios).bind { stored_casts.get }
    end
  end

  # ===========================================================================
  # 10.9 Timeout Operations
  # ===========================================================================

  # Run IO with timeout
  # タイムアウト付きで IO を実行
  def self.with_timeout(io, timeout_seconds)
    Ch08IO::IO.delay do
      result = nil
      completed = false

      thread = Thread.new do
        result = io.run!
        completed = true
      end

      thread.join(timeout_seconds)

      if completed
        { success: true, value: result }
      else
        thread.kill
        { success: false, error: 'Timeout' }
      end
    end
  end

  # Collect results for a duration then stop
  # 一定時間結果を収集して停止
  def self.collect_for(producer_io, duration_seconds)
    Ch08IO::IO.delay do
      collected = Ref.new([])
      cancel_flag = { cancelled: false }

      producer_thread = Thread.new do
        until cancel_flag[:cancelled]
          result = producer_io.run!
          collected.update { |arr| arr + [result] }.run!
        end
      end

      sleep(duration_seconds)
      cancel_flag[:cancelled] = true
      producer_thread.kill if producer_thread.alive?

      collected.get.run!
    end
  end

  # ===========================================================================
  # 10.10 Parallel Map Updates
  # ===========================================================================

  Update = Struct.new(:key, :value, keyword_init: true)

  # Apply updates to a map in parallel
  # マップに並列で更新を適用
  def self.apply_updates(updates)
    Ref.of({}).bind do |map_ref|
      ios = updates.map do |update|
        map_ref.update { |m| m.merge(update.key => update.value) }
      end
      par_sequence(ios).bind { map_ref.get }
    end
  end

  # ===========================================================================
  # 10.11 Counter Examples
  # ===========================================================================

  # Increment counter n times sequentially
  # カウンターを順次 n 回インクリメント
  def self.increment_sequential(n)
    Ref.of(0).bind do |counter|
      n.times { counter.update { |x| x + 1 }.run! }
      counter.get
    end
  end

  # Increment counter n times in parallel
  # カウンターを並列に n 回インクリメント
  def self.increment_parallel(n)
    Ref.of(0).bind do |counter|
      ios = Array.new(n) { counter.update { |x| x + 1 } }
      par_sequence(ios).bind { counter.get }
    end
  end

  # Count evens from parallel IO executions
  # 並列 IO 実行から偶数をカウント
  def self.count_evens(ios)
    Ref.of(0).bind do |counter|
      check_ios = ios.map do |io|
        io.bind do |n|
          if n.even?
            counter.update { |c| c + 1 }
          else
            Ch08IO::IO.unit
          end
        end
      end
      par_sequence(check_ios).bind { counter.get }
    end
  end

  # ===========================================================================
  # 10.12 Producer-Consumer Pattern
  # ===========================================================================

  # Simple queue using Ref
  # Ref を使用したシンプルなキュー
  class RefQueue
    def initialize
      @ref = Ref.new([])
    end

    def enqueue(value)
      @ref.update { |arr| arr + [value] }
    end

    def dequeue
      @ref.modify do |arr|
        if arr.empty?
          [arr, nil]
        else
          [arr[1..], arr[0]]
        end
      end
    end

    def size
      @ref.get.fmap(&:size)
    end

    def to_array
      @ref.get
    end
  end

  # Create a producer-consumer pair
  # プロデューサー・コンシューマーのペアを作成
  def self.producer_consumer(producer_io, consumer_fn, duration_seconds)
    Ch08IO::IO.delay do
      queue = RefQueue.new
      results = Ref.new([])
      cancel_flag = { cancelled: false }

      # Producer thread
      producer_thread = Thread.new do
        until cancel_flag[:cancelled]
          value = producer_io.run!
          queue.enqueue(value).run!
        end
      end

      # Consumer thread
      consumer_thread = Thread.new do
        until cancel_flag[:cancelled]
          value = queue.dequeue.run!
          if value
            result = consumer_fn.call(value)
            results.update { |arr| arr + [result] }.run!
          else
            sleep(0.001) # Wait for producer
          end
        end
      end

      sleep(duration_seconds)
      cancel_flag[:cancelled] = true
      producer_thread.kill if producer_thread.alive?
      consumer_thread.kill if consumer_thread.alive?

      results.get.run!
    end
  end
end
