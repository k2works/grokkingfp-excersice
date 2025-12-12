defmodule Ch02.ShoppingCartTest do
  use ExUnit.Case, async: true
  doctest Ch02.ShoppingCart

  describe "get_discount_percentage/1" do
    test "Book を含む場合は 5% 割引" do
      assert Ch02.ShoppingCart.get_discount_percentage(["Apple", "Book", "Banana"]) == 5
    end

    test "Book を含まない場合は 0% 割引" do
      assert Ch02.ShoppingCart.get_discount_percentage(["Apple", "Banana"]) == 0
    end

    test "空のカートは 0% 割引" do
      assert Ch02.ShoppingCart.get_discount_percentage([]) == 0
    end

    test "Book だけの場合は 5% 割引" do
      assert Ch02.ShoppingCart.get_discount_percentage(["Book"]) == 5
    end
  end

  describe "get_max_discount/1" do
    test "Book を含む場合は 5% 割引" do
      assert Ch02.ShoppingCart.get_max_discount(["Apple", "Book"]) == 5
    end

    test "Laptop を含む場合は 10% 割引" do
      assert Ch02.ShoppingCart.get_max_discount(["Laptop"]) == 10
    end

    test "10個以上のアイテムで 15% 割引" do
      items = Enum.map(1..15, &Integer.to_string/1)
      assert Ch02.ShoppingCart.get_max_discount(items) == 15
    end

    test "Book と Laptop の両方を含む場合は最大の 10% 割引" do
      assert Ch02.ShoppingCart.get_max_discount(["Book", "Laptop"]) == 10
    end

    test "すべての条件を満たす場合は最大の 15% 割引" do
      items = ["Book", "Laptop"] ++ Enum.map(1..10, &Integer.to_string/1)
      assert Ch02.ShoppingCart.get_max_discount(items) == 15
    end

    test "空のカートは 0% 割引" do
      assert Ch02.ShoppingCart.get_max_discount([]) == 0
    end
  end

  describe "add_item/2" do
    test "カートにアイテムを追加" do
      assert Ch02.ShoppingCart.add_item(["Apple"], "Book") == ["Apple", "Book"]
    end

    test "空のカートにアイテムを追加" do
      assert Ch02.ShoppingCart.add_item([], "Book") == ["Book"]
    end

    test "元のリストは変更されない（イミュータブル）" do
      original = ["Apple"]
      _new = Ch02.ShoppingCart.add_item(original, "Book")
      assert original == ["Apple"]
    end
  end

  describe "remove_item/2" do
    test "カートからアイテムを削除" do
      assert Ch02.ShoppingCart.remove_item(["Apple", "Book", "Banana"], "Book") ==
               ["Apple", "Banana"]
    end

    test "存在しないアイテムを削除しても変化なし" do
      assert Ch02.ShoppingCart.remove_item(["Apple"], "Book") == ["Apple"]
    end

    test "元のリストは変更されない（イミュータブル）" do
      original = ["Apple", "Book"]
      _new = Ch02.ShoppingCart.remove_item(original, "Book")
      assert original == ["Apple", "Book"]
    end
  end

  describe "calculate_total/2" do
    test "複数アイテムの合計金額" do
      prices = %{"Apple" => 100, "Book" => 1500, "Banana" => 80}
      assert Ch02.ShoppingCart.calculate_total(["Apple", "Book"], prices) == 1600
    end

    test "空のカートの合計金額" do
      prices = %{"Apple" => 100}
      assert Ch02.ShoppingCart.calculate_total([], prices) == 0
    end

    test "価格が設定されていないアイテムは 0" do
      prices = %{"Apple" => 100}
      assert Ch02.ShoppingCart.calculate_total(["Apple", "Unknown"], prices) == 100
    end
  end

  describe "calculate_final_total/2" do
    test "Book を含む場合は 5% 割引が適用される" do
      prices = %{"Apple" => 100, "Book" => 1500}
      # (100 + 1500) * 0.95 = 1520
      assert Ch02.ShoppingCart.calculate_final_total(["Apple", "Book"], prices) == 1520.0
    end

    test "割引なしの場合は合計金額そのまま" do
      prices = %{"Apple" => 100, "Banana" => 80}
      # (100 + 80) * 1.0 = 180
      assert Ch02.ShoppingCart.calculate_final_total(["Apple", "Banana"], prices) == 180.0
    end
  end

  describe "純粋関数の特性" do
    test "同じ入力に対して常に同じ出力を返す" do
      items = ["Apple", "Book"]

      for _ <- 1..100 do
        assert Ch02.ShoppingCart.get_discount_percentage(items) == 5
      end
    end

    test "関数の実行順序に依存しない" do
      items1 = ["Apple", "Book"]
      items2 = ["Apple"]

      # どの順序で呼び出しても結果は同じ
      result1 = Ch02.ShoppingCart.get_discount_percentage(items1)
      result2 = Ch02.ShoppingCart.get_discount_percentage(items2)
      result1_again = Ch02.ShoppingCart.get_discount_percentage(items1)

      assert result1 == result1_again
      assert result1 == 5
      assert result2 == 0
    end
  end
end
