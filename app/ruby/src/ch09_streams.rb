# frozen_string_literal: true

require_relative 'ch08_io'

# Chapter 9: Stream Processing - Lazy Sequences
# ストリーム処理 - 遅延シーケンス
module Ch09Streams
  # ===========================================================================
  # 9.1 Stream Wrapper (using Ruby's Enumerator::Lazy)
  # ===========================================================================

  # LazyStream wraps Ruby's Enumerator::Lazy for a more functional interface
  # LazyStream は Ruby の Enumerator::Lazy をラップしてより関数型のインターフェースを提供
  class LazyStream
    attr_reader :enumerator

    def initialize(enumerator)
      @enumerator = enumerator.lazy
    end

    # Create from array
    # 配列から作成
    def self.from_array(array)
      new(array.each)
    end

    # Create from range
    # 範囲から作成
    def self.from_range(range)
      new(range.each)
    end

    # Create from a single value
    # 単一の値から作成
    def self.of(*values)
      new(values.each)
    end

    # Create an infinite stream by repeating values
    # 値を繰り返す無限ストリームを作成
    def self.repeat(*values)
      enum = Enumerator.new do |yielder|
        loop do
          values.each { |v| yielder << v }
        end
      end
      new(enum)
    end

    # Create an infinite stream from a generator function
    # ジェネレータ関数から無限ストリームを作成
    def self.continually(&block)
      enum = Enumerator.new do |yielder|
        loop { yielder << block.call }
      end
      new(enum)
    end

    # Create a stream from an iterate function (like Scala's iterate)
    # iterate 関数からストリームを作成
    def self.iterate(initial, &fn)
      enum = Enumerator.new do |yielder|
        value = initial
        loop do
          yielder << value
          value = fn.call(value)
        end
      end
      new(enum)
    end

    # Create empty stream
    # 空のストリームを作成
    def self.empty
      new([].each)
    end

    # Create stream of integers from start
    # start からの整数ストリームを作成
    def self.from(start)
      iterate(start) { |n| n + 1 }
    end

    # ===========================================================================
    # 9.2 Stream Operations
    # ===========================================================================

    # Take first n elements
    # 最初の n 要素を取得
    def take(n)
      LazyStream.new(@enumerator.take(n))
    end

    # Drop first n elements
    # 最初の n 要素をスキップ
    def drop(n)
      LazyStream.new(@enumerator.drop(n))
    end

    # Filter elements
    # 要素をフィルタリング
    def filter(&predicate)
      LazyStream.new(@enumerator.select(&predicate))
    end

    alias select filter

    # Map over elements
    # 要素をマップ
    def fmap(&fn)
      LazyStream.new(@enumerator.map(&fn))
    end

    alias map fmap

    # FlatMap (bind)
    # FlatMap (bind)
    def flat_map(&fn)
      enum = Enumerator.new do |yielder|
        @enumerator.each do |element|
          result = fn.call(element)
          if result.is_a?(LazyStream)
            result.to_array.each { |v| yielder << v }
          elsif result.is_a?(Array)
            result.each { |v| yielder << v }
          else
            yielder << result
          end
        end
      end
      LazyStream.new(enum)
    end

    alias bind flat_map

    # Take while predicate is true
    # 述語が真の間取得
    def take_while(&predicate)
      LazyStream.new(@enumerator.take_while(&predicate))
    end

    # Drop while predicate is true
    # 述語が真の間スキップ
    def drop_while(&predicate)
      LazyStream.new(@enumerator.drop_while(&predicate))
    end

    # Repeat this stream infinitely
    # このストリームを無限に繰り返す
    def repeat
      enum = Enumerator.new do |yielder|
        loop do
          @enumerator.each { |v| yielder << v }
          @enumerator.rewind if @enumerator.respond_to?(:rewind)
        end
      end
      LazyStream.new(enum)
    end

    # Append another stream
    # 別のストリームを追加
    def append(other)
      enum = Enumerator.new do |yielder|
        @enumerator.each { |v| yielder << v }
        other.enumerator.each { |v| yielder << v }
      end
      LazyStream.new(enum)
    end

    alias concat append

    # Zip with another stream (stops at shorter stream)
    # 別のストリームと結合（短い方で停止）
    def zip(other)
      enum = Enumerator.new do |yielder|
        enum1 = @enumerator.to_enum
        enum2 = other.enumerator.to_enum
        loop do
          val1 = enum1.next
          val2 = enum2.next
          yielder << [val1, val2]
        end
      end
      LazyStream.new(enum)
    end

    # Zip but keep only left values (pace controlled by right)
    # 左の値のみ保持（ペースは右で制御）
    def zip_left(other)
      zip(other).fmap { |pair| pair[0] }
    end

    # Zip but keep only right values (pace controlled by left)
    # 右の値のみ保持（ペースは左で制御）
    def zip_right(other)
      zip(other).fmap { |pair| pair[1] }
    end

    # Sliding window
    # スライディングウィンドウ
    def sliding(n)
      enum = Enumerator.new do |yielder|
        window = []
        @enumerator.each do |element|
          window << element
          if window.size == n
            yielder << window.dup
            window.shift
          end
        end
      end
      LazyStream.new(enum)
    end

    # Chunk by predicate
    # 述語でチャンク
    def chunk_by(&fn)
      LazyStream.new(@enumerator.chunk(&fn).lazy)
    end

    # ===========================================================================
    # 9.3 Terminal Operations
    # ===========================================================================

    # Convert to array (forces evaluation)
    # 配列に変換（評価を強制）
    def to_array
      @enumerator.to_a
    end

    alias to_list to_array
    alias force to_array

    # Get first element
    # 最初の要素を取得
    def head
      @enumerator.first
    end

    alias first head

    # Get first element or nil
    # 最初の要素または nil を取得
    def head_option
      @enumerator.first
    end

    # Get last element (warning: infinite streams will hang!)
    # 最後の要素を取得（警告：無限ストリームはハングする！）
    def last
      to_array.last
    end

    # Reduce/fold
    # Reduce/fold
    def fold(initial, &fn)
      @enumerator.reduce(initial, &fn)
    end

    alias reduce fold

    # Check if any element satisfies predicate
    # いずれかの要素が述語を満たすか確認
    def any?(&predicate)
      @enumerator.any?(&predicate)
    end

    # Check if all elements satisfy predicate
    # すべての要素が述語を満たすか確認
    def all?(&predicate)
      @enumerator.all?(&predicate)
    end

    # Find first element satisfying predicate
    # 述語を満たす最初の要素を見つける
    def find(&predicate)
      @enumerator.find(&predicate)
    end

    # Count elements
    # 要素を数える
    def count
      @enumerator.count
    end

    # Sum elements
    # 要素を合計
    def sum
      fold(0) { |acc, x| acc + x }
    end

    # For each (side effect)
    # For each（副作用）
    def for_each(&fn)
      @enumerator.each(&fn)
      nil
    end
  end

  # ===========================================================================
  # 9.4 IOStream - Streams with IO Effects
  # ===========================================================================

  # IOStream represents a stream that produces IO effects
  # IOStream は IO エフェクトを生成するストリームを表す
  class IOStream
    attr_reader :stream_fn

    def initialize(&stream_fn)
      @stream_fn = stream_fn
    end

    # Create from an IO action (single element)
    # IO アクションから作成（単一要素）
    def self.eval(io_action)
      new { [io_action.run!] }
    end

    # Create infinite stream from an IO action
    # IO アクションから無限ストリームを作成
    def self.repeat_eval(io_action)
      enum = Enumerator.new do |yielder|
        loop { yielder << io_action.run! }
      end
      new { enum.lazy }
    end

    # Take first n elements
    # 最初の n 要素を取得
    def take(n)
      IOStream.new { @stream_fn.call.take(n) }
    end

    # Filter elements
    # 要素をフィルタリング
    def filter(&predicate)
      IOStream.new { @stream_fn.call.select(&predicate) }
    end

    # Map over elements
    # 要素をマップ
    def fmap(&fn)
      IOStream.new { @stream_fn.call.map(&fn) }
    end

    alias map fmap

    # Compile to list (returns IO)
    # リストにコンパイル（IO を返す）
    def compile_to_list
      Ch08IO::IO.delay { @stream_fn.call.to_a }
    end

    # Compile to first element (returns IO)
    # 最初の要素にコンパイル（IO を返す）
    def compile_first
      Ch08IO::IO.delay { @stream_fn.call.first }
    end

    # Compile to last element (returns IO)
    # 最後の要素にコンパイル（IO を返す）
    def compile_last
      Ch08IO::IO.delay { @stream_fn.call.to_a.last }
    end
  end

  # ===========================================================================
  # 9.5 Die Casting with Streams
  # ===========================================================================

  # Infinite stream of die casts
  # サイコロを振る無限ストリーム
  def self.infinite_die_casts
    IOStream.repeat_eval(Ch08IO.cast_the_die)
  end

  # Get first n die casts
  # 最初の n 回のサイコロを取得
  def self.first_n_die_casts(n)
    infinite_die_casts.take(n).compile_to_list
  end

  # Get die casts until 6 appears
  # 6 が出るまでサイコロを振る
  def self.die_casts_until_six
    IOStream.repeat_eval(Ch08IO.cast_the_die)
      .filter { |n| n == 6 }
      .take(1)
      .compile_to_list
  end

  # ===========================================================================
  # 9.6 Currency Exchange Example
  # ===========================================================================

  Currency = Struct.new(:code, keyword_init: true)

  # Simulated exchange rates (impure - could fail)
  # シミュレートされた為替レート（不純 - 失敗する可能性）
  def self.exchange_table_api_call(from_currency)
    # Simulate API response with some randomness
    base_rates = {
      'USD' => { 'EUR' => 0.85, 'GBP' => 0.73, 'JPY' => 110.0 },
      'EUR' => { 'USD' => 1.18, 'GBP' => 0.86, 'JPY' => 129.0 },
      'GBP' => { 'USD' => 1.37, 'EUR' => 1.16, 'JPY' => 150.0 }
    }

    rates = base_rates.fetch(from_currency.code, {})
    # Add small random variation
    rates.transform_values { |rate| rate * (1 + rand(-0.02..0.02)) }
  end

  # Get exchange table as IO
  # 為替テーブルを IO として取得
  def self.exchange_table(from_currency)
    Ch08IO::IO.delay { exchange_table_api_call(from_currency) }
  end

  # Extract single currency rate
  # 単一の通貨レートを抽出
  def self.extract_single_currency_rate(to_currency, table)
    table[to_currency.code]
  end

  # Stream of exchange rates
  # 為替レートのストリーム
  def self.rates_stream(from_currency, to_currency)
    IOStream.repeat_eval(exchange_table(from_currency))
      .fmap { |table| extract_single_currency_rate(to_currency, table) }
      .filter { |rate| !rate.nil? }
  end

  # ===========================================================================
  # 9.7 Trend Detection (Pure Functions)
  # ===========================================================================

  # Check if rates are trending up
  # レートが上昇トレンドかチェック
  def self.trending?(rates)
    return false if rates.size <= 1

    rates.each_cons(2).all? { |prev, curr| curr > prev }
  end

  # Check if rates are trending down
  # レートが下降トレンドかチェック
  def self.trending_down?(rates)
    return false if rates.size <= 1

    rates.each_cons(2).all? { |prev, curr| curr < prev }
  end

  # Check if rates are stable (all same)
  # レートが安定しているかチェック（すべて同じ）
  def self.stable?(rates)
    return false if rates.size < 3

    rates.uniq.size == 1
  end

  # Calculate average of rates
  # レートの平均を計算
  def self.average_rate(rates)
    return nil if rates.empty?

    rates.sum.to_f / rates.size
  end

  # ===========================================================================
  # 9.8 Pure Stream Examples
  # ===========================================================================

  # Evens from 1 to n
  # 1 から n までの偶数
  def self.evens(n)
    LazyStream.from_range(1..n).filter(&:even?)
  end

  # Fibonacci sequence
  # フィボナッチ数列
  def self.fibonacci
    enum = Enumerator.new do |yielder|
      a, b = 0, 1
      loop do
        yielder << a
        a, b = b, a + b
      end
    end
    LazyStream.new(enum)
  end

  # Prime numbers (simple sieve)
  # 素数（単純なふるい）
  def self.primes
    LazyStream.from(2).filter { |n| prime?(n) }
  end

  # Check if number is prime
  # 数が素数かチェック
  def self.prime?(n)
    return false if n < 2

    (2..Math.sqrt(n).to_i).none? { |i| (n % i).zero? }
  end

  # Natural numbers
  # 自然数
  def self.natural_numbers
    LazyStream.from(1)
  end

  # Alternating true/false
  # 交互の true/false
  def self.alternating_booleans
    LazyStream.repeat(true, false)
  end

  # ===========================================================================
  # 9.9 Sliding Window Examples
  # ===========================================================================

  # Detect trend in a stream using sliding window
  # スライディングウィンドウを使ってストリームのトレンドを検出
  def self.detect_trend(values, window_size)
    LazyStream.from_array(values)
      .sliding(window_size)
      .fmap { |window| { window: window, trending: trending?(window) } }
  end

  # Moving average
  # 移動平均
  def self.moving_average(values, window_size)
    LazyStream.from_array(values)
      .sliding(window_size)
      .fmap { |window| window.sum.to_f / window.size }
  end

  # ===========================================================================
  # 9.10 Stream Utilities
  # ===========================================================================

  # Interleave two streams
  # 2つのストリームをインターリーブ
  def self.interleave(stream1, stream2)
    enum = Enumerator.new do |yielder|
      enum1 = stream1.enumerator.to_enum
      enum2 = stream2.enumerator.to_enum
      loop do
        begin
          yielder << enum1.next
        rescue StopIteration
          # stream1 exhausted
        end
        begin
          yielder << enum2.next
        rescue StopIteration
          # stream2 exhausted
        end
        break if enum1.peek.nil? && enum2.peek.nil?
      rescue StopIteration
        break
      end
    end
    LazyStream.new(enum)
  end

  # Scan (like fold but returns intermediate results)
  # スキャン（fold のようだが中間結果を返す）
  def self.scan(stream, initial, &fn)
    enum = Enumerator.new do |yielder|
      acc = initial
      yielder << acc
      stream.enumerator.each do |element|
        acc = fn.call(acc, element)
        yielder << acc
      end
    end
    LazyStream.new(enum)
  end

  # Running sum
  # 累積和
  def self.running_sum(stream)
    scan(stream, 0) { |acc, x| acc + x }
  end

  # Running product
  # 累積積
  def self.running_product(stream)
    scan(stream, 1) { |acc, x| acc * x }
  end
end
