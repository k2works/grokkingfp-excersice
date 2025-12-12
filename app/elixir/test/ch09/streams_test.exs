defmodule Ch09.StreamsTest do
  use ExUnit.Case, async: true
  doctest Ch09.Streams

  alias Ch09.Streams

  describe "finite_stream/1" do
    test "有限ストリームを作成" do
      stream = Streams.finite_stream([1, 2, 3])
      assert Enum.to_list(stream) == [1, 2, 3]
    end

    test "空リスト" do
      stream = Streams.finite_stream([])
      assert Enum.to_list(stream) == []
    end
  end

  describe "repeat/1" do
    test "無限に繰り返す" do
      stream = Streams.repeat([1, 2, 3])
      assert stream |> Enum.take(8) == [1, 2, 3, 1, 2, 3, 1, 2]
    end

    test "単一要素の繰り返し" do
      stream = Streams.repeat([1])
      assert stream |> Enum.take(5) == [1, 1, 1, 1, 1]
    end
  end

  describe "generate/1" do
    test "生成関数を使用" do
      counter = :counters.new(1, [:atomics])

      stream =
        Streams.generate(fn ->
          :counters.add(counter, 1, 1)
          :counters.get(counter, 1)
        end)

      assert stream |> Enum.take(5) == [1, 2, 3, 4, 5]
    end
  end

  describe "count_from/1" do
    test "カウントアップ" do
      stream = Streams.count_from(1)
      assert stream |> Enum.take(5) == [1, 2, 3, 4, 5]
    end

    test "負の数からカウント" do
      stream = Streams.count_from(-2)
      assert stream |> Enum.take(5) == [-2, -1, 0, 1, 2]
    end
  end

  describe "filter_stream/2" do
    test "偶数をフィルタ" do
      stream = Streams.filter_stream([1, 2, 3, 4, 5], &(rem(&1, 2) == 0))
      assert Enum.to_list(stream) == [2, 4]
    end
  end

  describe "map_stream/2" do
    test "各要素を変換" do
      stream = Streams.map_stream([1, 2, 3], &(&1 * 2))
      assert Enum.to_list(stream) == [2, 4, 6]
    end
  end

  describe "take_stream/2" do
    test "最初の n 要素を取得" do
      stream = Streams.take_stream([1, 2, 3, 4, 5], 3)
      assert Enum.to_list(stream) == [1, 2, 3]
    end
  end

  describe "take_while_stream/2" do
    test "条件を満たす間取得" do
      stream = Streams.take_while_stream([1, 2, 3, 4, 5], &(&1 < 4))
      assert Enum.to_list(stream) == [1, 2, 3]
    end
  end

  describe "drop_stream/2" do
    test "最初の n 要素をスキップ" do
      stream = Streams.drop_stream([1, 2, 3, 4, 5], 2)
      assert Enum.to_list(stream) == [3, 4, 5]
    end
  end

  describe "die_casts/0" do
    test "サイコロの値は1から6" do
      results = Streams.die_casts() |> Enum.take(100)
      assert Enum.all?(results, fn x -> x >= 1 and x <= 6 end)
    end
  end

  describe "roll_until/1" do
    test "指定の値が出るまで" do
      results = Streams.roll_until(6)
      assert List.last(results) == 6
      assert Enum.all?(Enum.drop(results, -1), fn x -> x != 6 end)
    end
  end

  describe "roll_and_sum/1" do
    test "指定回数の合計" do
      for _ <- 1..100 do
        sum = Streams.roll_and_sum(3)
        assert sum >= 3 and sum <= 18
      end
    end
  end

  describe "sliding/2" do
    test "スライディングウィンドウ" do
      stream = Streams.sliding([1, 2, 3, 4, 5], 3)
      assert Enum.to_list(stream) == [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
    end

    test "ウィンドウサイズ 2" do
      stream = Streams.sliding([1, 2, 3, 4], 2)
      assert Enum.to_list(stream) == [[1, 2], [2, 3], [3, 4]]
    end

    test "要素数よりウィンドウが大きい場合は空" do
      stream = Streams.sliding([1, 2], 3)
      assert Enum.to_list(stream) == []
    end
  end

  describe "sliding_with_remainder/2" do
    test "末尾の不完全なチャンクも含む" do
      stream = Streams.sliding_with_remainder([1, 2, 3, 4], 3)
      assert Enum.to_list(stream) == [[1, 2, 3], [2, 3, 4], [3, 4], [4]]
    end
  end

  describe "trending?/1" do
    test "上昇トレンド" do
      assert Streams.trending?([0.81, 0.82, 0.83])
    end

    test "上昇していない" do
      refute Streams.trending?([0.81, 0.84, 0.83])
    end

    test "単一要素は false" do
      refute Streams.trending?([0.81])
    end

    test "空リストは false" do
      refute Streams.trending?([])
    end

    test "同じ値は false" do
      refute Streams.trending?([1, 1, 1])
    end
  end

  describe "declining?/1" do
    test "下降トレンド" do
      assert Streams.declining?([0.83, 0.82, 0.81])
    end

    test "下降していない" do
      refute Streams.declining?([0.81, 0.82, 0.83])
    end

    test "単一要素は false" do
      refute Streams.declining?([0.81])
    end
  end

  describe "stable?/1" do
    test "安定している" do
      assert Streams.stable?([5, 5, 5])
    end

    test "安定していない" do
      refute Streams.stable?([5, 5, 6])
    end

    test "2要素未満は false" do
      refute Streams.stable?([5])
      refute Streams.stable?([5, 5])
    end
  end

  describe "exchange_rate/2" do
    test "為替レートを取得" do
      rate = Streams.exchange_rate(:usd, :eur)
      # 基準レート 0.85 ± 1%
      assert rate > 0.8 and rate < 0.9
    end
  end

  describe "rate_stream/2" do
    test "為替レートのストリーム" do
      rates = Streams.rate_stream(:usd, :eur) |> Enum.take(5)
      assert length(rates) == 5
      assert Enum.all?(rates, fn r -> r > 0 end)
    end
  end

  describe "exchange_if_trending/5" do
    test "上昇トレンドで交換" do
      # 多くの試行で成功する可能性が高い
      result = Streams.exchange_if_trending(100.0, :usd, :eur, 3, 1000)

      case result do
        {:ok, value} -> assert value > 0
        {:error, _} -> :ok
      end
    end
  end

  describe "zip_streams/2" do
    test "2つのストリームを zip" do
      result = Streams.zip_streams([1, 2, 3], ["a", "b", "c"]) |> Enum.to_list()
      assert result == [{1, "a"}, {2, "b"}, {3, "c"}]
    end

    test "長さが異なる場合は短い方に合わせる" do
      result = Streams.zip_streams([1, 2], ["a", "b", "c"]) |> Enum.to_list()
      assert result == [{1, "a"}, {2, "b"}]
    end
  end

  describe "zip_left/2" do
    test "左側の値だけ返す" do
      result = Streams.zip_left([1, 2, 3], [:tick, :tick, :tick]) |> Enum.to_list()
      assert result == [1, 2, 3]
    end
  end

  describe "sum_stream/1" do
    test "合計を計算" do
      assert Streams.sum_stream([1, 2, 3, 4, 5]) == 15
    end

    test "空リストは 0" do
      assert Streams.sum_stream([]) == 0
    end
  end

  describe "average_stream/1" do
    test "平均を計算" do
      assert Streams.average_stream([1, 2, 3, 4, 5]) == 3.0
    end

    test "空リストは 0.0" do
      assert Streams.average_stream([]) == 0.0
    end
  end

  describe "moving_average/2" do
    test "移動平均を計算" do
      result = Streams.moving_average([1, 2, 3, 4, 5], 3) |> Enum.to_list()
      assert result == [2.0, 3.0, 4.0]
    end
  end

  describe "fibonacci/0" do
    test "フィボナッチ数列" do
      result = Streams.fibonacci() |> Enum.take(10)
      assert result == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    end
  end

  describe "geometric_series/2" do
    test "等比数列" do
      result = Streams.geometric_series(1, 2) |> Enum.take(5)
      assert result == [1, 2, 4, 8, 16]
    end

    test "比率 0.5" do
      result = Streams.geometric_series(16, 0.5) |> Enum.take(5)
      assert result == [16, 8.0, 4.0, 2.0, 1.0]
    end
  end

  describe "arithmetic_series/2" do
    test "等差数列" do
      result = Streams.arithmetic_series(1, 3) |> Enum.take(5)
      assert result == [1, 4, 7, 10, 13]
    end

    test "負の差" do
      result = Streams.arithmetic_series(10, -2) |> Enum.take(5)
      assert result == [10, 8, 6, 4, 2]
    end
  end
end
