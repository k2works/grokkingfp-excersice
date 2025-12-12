defmodule Ch09.Streams do
  @moduledoc """
  第9章: ストリーム処理

  Elixir の Stream を使った遅延評価とストリーム処理:
  - 無限ストリーム
  - 遅延評価
  - トレンド検出
  """

  # =============================================================================
  # 基本的なストリーム操作
  # =============================================================================

  @doc """
  有限ストリームを作成

  ## Examples

      iex> stream = Ch09.Streams.finite_stream([1, 2, 3])
      iex> Enum.to_list(stream)
      [1, 2, 3]

  """
  @spec finite_stream([any()]) :: Enumerable.t()
  def finite_stream(list), do: Stream.map(list, & &1)

  @doc """
  無限に繰り返すストリーム

  ## Examples

      iex> stream = Ch09.Streams.repeat([1, 2, 3])
      iex> stream |> Enum.take(8)
      [1, 2, 3, 1, 2, 3, 1, 2]

  """
  @spec repeat([any()]) :: Enumerable.t()
  def repeat(list), do: Stream.cycle(list)

  @doc """
  無限ストリームを作成（生成関数を使用）

  ## Examples

      iex> stream = Ch09.Streams.generate(fn -> :rand.uniform(6) end)
      iex> results = stream |> Enum.take(5)
      iex> length(results)
      5
      iex> Enum.all?(results, fn x -> x >= 1 and x <= 6 end)
      true

  """
  @spec generate((-> any())) :: Enumerable.t()
  def generate(f), do: Stream.repeatedly(f)

  @doc """
  無限にカウントアップするストリーム

  ## Examples

      iex> stream = Ch09.Streams.count_from(1)
      iex> stream |> Enum.take(5)
      [1, 2, 3, 4, 5]

  """
  @spec count_from(integer()) :: Enumerable.t()
  def count_from(start), do: Stream.iterate(start, &(&1 + 1))

  @doc """
  フィルタリング

  ## Examples

      iex> stream = Ch09.Streams.filter_stream([1, 2, 3, 4, 5], &(rem(&1, 2) == 0))
      iex> Enum.to_list(stream)
      [2, 4]

  """
  @spec filter_stream(Enumerable.t(), (any() -> boolean())) :: Enumerable.t()
  def filter_stream(stream, predicate), do: Stream.filter(stream, predicate)

  @doc """
  マッピング

  ## Examples

      iex> stream = Ch09.Streams.map_stream([1, 2, 3], &(&1 * 2))
      iex> Enum.to_list(stream)
      [2, 4, 6]

  """
  @spec map_stream(Enumerable.t(), (any() -> any())) :: Enumerable.t()
  def map_stream(stream, f), do: Stream.map(stream, f)

  @doc """
  最初の n 要素を取得

  ## Examples

      iex> stream = Ch09.Streams.take_stream([1, 2, 3, 4, 5], 3)
      iex> Enum.to_list(stream)
      [1, 2, 3]

  """
  @spec take_stream(Enumerable.t(), non_neg_integer()) :: Enumerable.t()
  def take_stream(stream, n), do: Stream.take(stream, n)

  @doc """
  条件を満たす間取得

  ## Examples

      iex> stream = Ch09.Streams.take_while_stream([1, 2, 3, 4, 5], &(&1 < 4))
      iex> Enum.to_list(stream)
      [1, 2, 3]

  """
  @spec take_while_stream(Enumerable.t(), (any() -> boolean())) :: Enumerable.t()
  def take_while_stream(stream, predicate), do: Stream.take_while(stream, predicate)

  @doc """
  最初の n 要素をスキップ

  ## Examples

      iex> stream = Ch09.Streams.drop_stream([1, 2, 3, 4, 5], 2)
      iex> Enum.to_list(stream)
      [3, 4, 5]

  """
  @spec drop_stream(Enumerable.t(), non_neg_integer()) :: Enumerable.t()
  def drop_stream(stream, n), do: Stream.drop(stream, n)

  # =============================================================================
  # サイコロの例
  # =============================================================================

  @doc """
  サイコロを振るストリーム

  ## Examples

      iex> stream = Ch09.Streams.die_casts()
      iex> results = stream |> Enum.take(5)
      iex> Enum.all?(results, fn x -> x >= 1 and x <= 6 end)
      true

  """
  @spec die_casts() :: Enumerable.t()
  def die_casts do
    Stream.repeatedly(fn -> :rand.uniform(6) end)
  end

  @doc """
  指定の値が出るまでサイコロを振る

  ## Examples

      iex> results = Ch09.Streams.roll_until(6)
      iex> List.last(results)
      6

  """
  @spec roll_until(integer()) :: [integer()]
  def roll_until(target) do
    die_casts()
    |> Stream.take_while(fn x -> x != target end)
    |> Enum.to_list()
    |> Kernel.++([target])
  end

  @doc """
  指定回数サイコロを振って合計

  ## Examples

      iex> sum = Ch09.Streams.roll_and_sum(3)
      iex> sum >= 3 and sum <= 18
      true

  """
  @spec roll_and_sum(non_neg_integer()) :: integer()
  def roll_and_sum(n) do
    die_casts()
    |> Enum.take(n)
    |> Enum.sum()
  end

  # =============================================================================
  # スライディングウィンドウ
  # =============================================================================

  @doc """
  スライディングウィンドウを作成

  ## Examples

      iex> stream = Ch09.Streams.sliding([1, 2, 3, 4, 5], 3)
      iex> Enum.to_list(stream)
      [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

  """
  @spec sliding(Enumerable.t(), pos_integer()) :: Enumerable.t()
  def sliding(stream, size) do
    Stream.chunk_every(stream, size, 1, :discard)
  end

  @doc """
  スライディングウィンドウ（末尾の不完全なチャンクも含む）

  ## Examples

      iex> stream = Ch09.Streams.sliding_with_remainder([1, 2, 3, 4], 3)
      iex> Enum.to_list(stream)
      [[1, 2, 3], [2, 3, 4], [3, 4], [4]]

  """
  @spec sliding_with_remainder(Enumerable.t(), pos_integer()) :: Enumerable.t()
  def sliding_with_remainder(stream, size) do
    Stream.chunk_every(stream, size, 1, :discard)
    |> Stream.concat(
      stream
      |> Enum.to_list()
      |> Enum.drop(Enum.count(stream) - size + 1)
      |> Enum.with_index()
      |> Enum.map(fn {_, i} ->
        stream
        |> Enum.to_list()
        |> Enum.drop(Enum.count(stream) - size + 1 + i)
      end)
      |> Enum.filter(fn chunk -> length(chunk) > 0 and length(chunk) < size end)
    )
  end

  # =============================================================================
  # トレンド検出
  # =============================================================================

  @doc """
  上昇トレンドかどうか判定（純粋関数）

  ## Examples

      iex> Ch09.Streams.trending?([0.81, 0.82, 0.83])
      true

      iex> Ch09.Streams.trending?([0.81, 0.84, 0.83])
      false

      iex> Ch09.Streams.trending?([0.81])
      false

  """
  @spec trending?([number()]) :: boolean()
  def trending?(rates) when length(rates) <= 1, do: false

  def trending?(rates) do
    rates
    |> Enum.zip(Enum.drop(rates, 1))
    |> Enum.all?(fn {prev, curr} -> curr > prev end)
  end

  @doc """
  下降トレンドかどうか判定

  ## Examples

      iex> Ch09.Streams.declining?([0.83, 0.82, 0.81])
      true

      iex> Ch09.Streams.declining?([0.81, 0.82, 0.83])
      false

  """
  @spec declining?([number()]) :: boolean()
  def declining?(rates) when length(rates) <= 1, do: false

  def declining?(rates) do
    rates
    |> Enum.zip(Enum.drop(rates, 1))
    |> Enum.all?(fn {prev, curr} -> curr < prev end)
  end

  @doc """
  安定しているかどうか判定（全ての値が同じ）

  ## Examples

      iex> Ch09.Streams.stable?([5, 5, 5])
      true

      iex> Ch09.Streams.stable?([5, 5, 6])
      false

      iex> Ch09.Streams.stable?([5])
      false

  """
  @spec stable?([any()]) :: boolean()
  def stable?(values) when length(values) < 3, do: false

  def stable?(values) do
    values
    |> Enum.uniq()
    |> length() == 1
  end

  # =============================================================================
  # 為替レートの例
  # =============================================================================

  defmodule Currency do
    @moduledoc "通貨"
    @type t :: :usd | :eur | :gbp | :jpy
  end

  @doc """
  為替レートを取得（モック）

  ## Examples

      iex> rate = Ch09.Streams.exchange_rate(:usd, :eur)
      iex> rate > 0
      true

  """
  @spec exchange_rate(Currency.t(), Currency.t()) :: float()
  def exchange_rate(from, to) do
    # モックの為替レート（実際にはAPIを呼び出す）
    base_rates = %{
      {:usd, :eur} => 0.85,
      {:usd, :gbp} => 0.73,
      {:usd, :jpy} => 110.0,
      {:eur, :usd} => 1.18,
      {:eur, :gbp} => 0.86,
      {:eur, :jpy} => 129.0,
      {:gbp, :usd} => 1.37,
      {:gbp, :eur} => 1.16,
      {:gbp, :jpy} => 151.0,
      {:jpy, :usd} => 0.0091,
      {:jpy, :eur} => 0.0078,
      {:jpy, :gbp} => 0.0066
    }

    base_rate = Map.get(base_rates, {from, to}, 1.0)
    # 小さな変動を追加
    variation = (:rand.uniform() - 0.5) * 0.02
    base_rate * (1 + variation)
  end

  @doc """
  為替レートのストリーム

  ## Examples

      iex> stream = Ch09.Streams.rate_stream(:usd, :eur)
      iex> rates = stream |> Enum.take(3)
      iex> length(rates)
      3

  """
  @spec rate_stream(Currency.t(), Currency.t()) :: Enumerable.t()
  def rate_stream(from, to) do
    Stream.repeatedly(fn -> exchange_rate(from, to) end)
  end

  @doc """
  上昇トレンドを検出したら交換

  ウィンドウサイズ分の連続上昇トレンドを検出したら交換を実行。

  ## Examples

      iex> {:ok, rate} = Ch09.Streams.exchange_if_trending(100.0, :usd, :eur, 3, 100)
      iex> rate > 0
      true

  """
  @spec exchange_if_trending(float(), Currency.t(), Currency.t(), pos_integer(), pos_integer()) ::
          {:ok, float()} | {:error, String.t()}
  def exchange_if_trending(amount, from, to, window_size, max_attempts) do
    result =
      rate_stream(from, to)
      |> sliding(window_size)
      |> Stream.filter(&trending?/1)
      |> Stream.map(&List.last/1)
      |> Stream.take(1)
      |> Enum.take(max_attempts)
      |> List.first()

    case result do
      nil -> {:error, "No trending pattern found within #{max_attempts} attempts"}
      rate -> {:ok, rate * amount}
    end
  end

  # =============================================================================
  # ストリームの結合
  # =============================================================================

  @doc """
  2つのストリームを zip

  ## Examples

      iex> s1 = [1, 2, 3]
      iex> s2 = ["a", "b", "c"]
      iex> Ch09.Streams.zip_streams(s1, s2) |> Enum.to_list()
      [{1, "a"}, {2, "b"}, {3, "c"}]

  """
  @spec zip_streams(Enumerable.t(), Enumerable.t()) :: Enumerable.t()
  def zip_streams(stream1, stream2) do
    Stream.zip(stream1, stream2)
  end

  @doc """
  2つのストリームを zip して左側の値だけ返す

  ## Examples

      iex> values = [1, 2, 3]
      iex> ticks = [:tick, :tick, :tick]
      iex> Ch09.Streams.zip_left(values, ticks) |> Enum.to_list()
      [1, 2, 3]

  """
  @spec zip_left(Enumerable.t(), Enumerable.t()) :: Enumerable.t()
  def zip_left(stream1, stream2) do
    Stream.zip(stream1, stream2)
    |> Stream.map(fn {left, _right} -> left end)
  end

  @doc """
  インターバル付きでストリームを処理

  ## Examples

      iex> stream = Ch09.Streams.with_interval([1, 2, 3], 10)
      iex> results = stream |> Enum.take(3)
      iex> results
      [1, 2, 3]

  """
  @spec with_interval(Enumerable.t(), pos_integer()) :: Enumerable.t()
  def with_interval(stream, interval_ms) do
    Stream.map(stream, fn item ->
      Process.sleep(interval_ms)
      item
    end)
  end

  # =============================================================================
  # 集計操作
  # =============================================================================

  @doc """
  ストリームの合計を計算

  ## Examples

      iex> Ch09.Streams.sum_stream([1, 2, 3, 4, 5])
      15

  """
  @spec sum_stream(Enumerable.t()) :: number()
  def sum_stream(stream), do: Enum.sum(stream)

  @doc """
  ストリームの平均を計算

  ## Examples

      iex> Ch09.Streams.average_stream([1, 2, 3, 4, 5])
      3.0

  """
  @spec average_stream(Enumerable.t()) :: float()
  def average_stream(stream) do
    {sum, count} =
      Enum.reduce(stream, {0, 0}, fn x, {sum, count} ->
        {sum + x, count + 1}
      end)

    if count == 0, do: 0.0, else: sum / count
  end

  @doc """
  移動平均を計算

  ## Examples

      iex> stream = Ch09.Streams.moving_average([1, 2, 3, 4, 5], 3)
      iex> Enum.to_list(stream)
      [2.0, 3.0, 4.0]

  """
  @spec moving_average(Enumerable.t(), pos_integer()) :: Enumerable.t()
  def moving_average(stream, window_size) do
    stream
    |> sliding(window_size)
    |> Stream.map(&average_stream/1)
  end

  # =============================================================================
  # unfold によるストリーム生成
  # =============================================================================

  @doc """
  フィボナッチ数列のストリーム

  ## Examples

      iex> stream = Ch09.Streams.fibonacci()
      iex> stream |> Enum.take(10)
      [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

  """
  @spec fibonacci() :: Enumerable.t()
  def fibonacci do
    Stream.unfold({0, 1}, fn {a, b} ->
      {a, {b, a + b}}
    end)
  end

  @doc """
  等比数列のストリーム

  ## Examples

      iex> stream = Ch09.Streams.geometric_series(1, 2)
      iex> stream |> Enum.take(5)
      [1, 2, 4, 8, 16]

  """
  @spec geometric_series(number(), number()) :: Enumerable.t()
  def geometric_series(start, ratio) do
    Stream.unfold(start, fn current ->
      {current, current * ratio}
    end)
  end

  @doc """
  等差数列のストリーム

  ## Examples

      iex> stream = Ch09.Streams.arithmetic_series(1, 3)
      iex> stream |> Enum.take(5)
      [1, 4, 7, 10, 13]

  """
  @spec arithmetic_series(number(), number()) :: Enumerable.t()
  def arithmetic_series(start, diff) do
    Stream.unfold(start, fn current ->
      {current, current + diff}
    end)
  end
end
