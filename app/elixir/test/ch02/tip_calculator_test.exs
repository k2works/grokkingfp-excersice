defmodule Ch02.TipCalculatorTest do
  use ExUnit.Case, async: true
  doctest Ch02.TipCalculator

  describe "get_tip_percentage/1" do
    test "6人以上のグループは 20%" do
      names = ["Alice", "Bob", "Charlie", "David", "Eve", "Frank"]
      assert Ch02.TipCalculator.get_tip_percentage(names) == 20
    end

    test "7人のグループも 20%" do
      names = ["A", "B", "C", "D", "E", "F", "G"]
      assert Ch02.TipCalculator.get_tip_percentage(names) == 20
    end

    test "5人のグループは 10%" do
      names = ["Alice", "Bob", "Charlie", "David", "Eve"]
      assert Ch02.TipCalculator.get_tip_percentage(names) == 10
    end

    test "1人のグループは 10%" do
      assert Ch02.TipCalculator.get_tip_percentage(["Alice"]) == 10
    end

    test "空のグループは 0%" do
      assert Ch02.TipCalculator.get_tip_percentage([]) == 0
    end
  end

  describe "calculate_tip/2" do
    test "6人以上のグループのチップ額" do
      names = ["Alice", "Bob", "Charlie", "David", "Eve", "Frank"]
      assert Ch02.TipCalculator.calculate_tip(1000, names) == 200.0
    end

    test "1-5人のグループのチップ額" do
      names = ["Alice", "Bob"]
      assert Ch02.TipCalculator.calculate_tip(1000, names) == 100.0
    end

    test "空のグループのチップ額" do
      assert Ch02.TipCalculator.calculate_tip(1000, []) == 0.0
    end
  end

  describe "calculate_total_with_tip/2" do
    test "チップ込みの合計額" do
      names = ["Alice", "Bob"]
      # 1000 + 1000 * 0.1 = 1100
      assert Ch02.TipCalculator.calculate_total_with_tip(1000, names) == 1100.0
    end

    test "6人以上のグループのチップ込み合計" do
      names = ["A", "B", "C", "D", "E", "F"]
      # 1000 + 1000 * 0.2 = 1200
      assert Ch02.TipCalculator.calculate_total_with_tip(1000, names) == 1200.0
    end
  end

  describe "calculate_per_person/2" do
    test "1人あたりの支払額" do
      names = ["Alice", "Bob"]
      # (1000 + 100) / 2 = 550
      assert Ch02.TipCalculator.calculate_per_person(1000, names) == 550.0
    end

    test "6人グループの1人あたりの支払額" do
      names = ["A", "B", "C", "D", "E", "F"]
      # (1000 + 200) / 6 = 200
      assert Ch02.TipCalculator.calculate_per_person(1000, names) == 200.0
    end

    test "空のグループの1人あたりの支払額" do
      assert Ch02.TipCalculator.calculate_per_person(1000, []) == 0.0
    end
  end

  describe "calculate_custom_tip/2" do
    test "カスタムチップ率で計算" do
      assert Ch02.TipCalculator.calculate_custom_tip(1000, 15) == 150.0
    end

    test "0% のチップ" do
      assert Ch02.TipCalculator.calculate_custom_tip(1000, 0) == 0.0
    end

    test "100% のチップ" do
      assert Ch02.TipCalculator.calculate_custom_tip(1000, 100) == 1000.0
    end
  end

  describe "純粋関数の特性" do
    test "同じ入力に対して常に同じ出力を返す" do
      names = ["Alice", "Bob", "Charlie"]

      for _ <- 1..100 do
        assert Ch02.TipCalculator.get_tip_percentage(names) == 10
        assert Ch02.TipCalculator.calculate_tip(1000, names) == 100.0
      end
    end
  end
end
