defmodule Ch03.ImmutableDataTest do
  use ExUnit.Case, async: true
  doctest Ch03.ImmutableData

  describe "append/2" do
    test "リストに要素を追加" do
      assert Ch03.ImmutableData.append(["Apple", "Book"], "Mango") ==
               ["Apple", "Book", "Mango"]
    end

    test "空リストに追加" do
      assert Ch03.ImmutableData.append([], "Apple") == ["Apple"]
    end

    test "元のリストは変更されない" do
      original = ["Apple", "Book"]
      _modified = Ch03.ImmutableData.append(original, "Mango")
      assert original == ["Apple", "Book"]
    end
  end

  describe "prepend/2" do
    test "リストの先頭に追加" do
      assert Ch03.ImmutableData.prepend(["Apple", "Book"], "Mango") ==
               ["Mango", "Apple", "Book"]
    end
  end

  describe "slice/3" do
    test "リストの一部を切り出し" do
      assert Ch03.ImmutableData.slice(["a", "b", "c", "d"], 1, 3) == ["b", "c"]
    end

    test "先頭から切り出し" do
      assert Ch03.ImmutableData.slice(["a", "b", "c"], 0, 2) == ["a", "b"]
    end
  end

  describe "first_n/2" do
    test "最初のN要素を取得" do
      assert Ch03.ImmutableData.first_n(["a", "b", "c", "d"], 2) == ["a", "b"]
    end

    test "リストより多く要求した場合" do
      assert Ch03.ImmutableData.first_n(["a", "b"], 5) == ["a", "b"]
    end
  end

  describe "last_n/2" do
    test "最後のN要素を取得" do
      assert Ch03.ImmutableData.last_n(["a", "b", "c", "d"], 2) == ["c", "d"]
    end
  end

  describe "drop_first_n/2" do
    test "最初のN要素を除く" do
      assert Ch03.ImmutableData.drop_first_n(["a", "b", "c", "d"], 2) == ["c", "d"]
    end
  end

  describe "move_first_two_to_end/1" do
    test "最初の2要素を末尾に移動" do
      assert Ch03.ImmutableData.move_first_two_to_end(["a", "b", "c"]) == ["c", "a", "b"]
    end

    test "4要素のリスト" do
      assert Ch03.ImmutableData.move_first_two_to_end(["a", "b", "c", "d"]) ==
               ["c", "d", "a", "b"]
    end
  end

  describe "insert_before_last/2" do
    test "最後の要素の前に挿入" do
      assert Ch03.ImmutableData.insert_before_last(["a", "b"], "c") == ["a", "c", "b"]
    end
  end

  describe "insert_at/3" do
    test "指定位置に挿入" do
      assert Ch03.ImmutableData.insert_at(["a", "b", "d"], 2, "c") == ["a", "b", "c", "d"]
    end

    test "先頭に挿入" do
      assert Ch03.ImmutableData.insert_at(["a", "b"], 0, "x") == ["x", "a", "b"]
    end
  end

  describe "insert_at_middle/2" do
    test "中央に挿入（偶数要素）" do
      assert Ch03.ImmutableData.insert_at_middle(["a", "b", "c", "d"], "X") ==
               ["a", "b", "X", "c", "d"]
    end

    test "中央に挿入（奇数要素）" do
      assert Ch03.ImmutableData.insert_at_middle(["a", "b"], "X") == ["a", "X", "b"]
    end
  end

  describe "replan/3" do
    test "旅程に新しい都市を追加" do
      assert Ch03.ImmutableData.replan(["Paris", "Berlin", "Kraków"], "Vienna", "Kraków") ==
               ["Paris", "Berlin", "Vienna", "Kraków"]
    end

    test "元の旅程は変更されない" do
      original = ["Paris", "Berlin", "Kraków"]
      _new_plan = Ch03.ImmutableData.replan(original, "Vienna", "Kraków")
      assert original == ["Paris", "Berlin", "Kraków"]
    end
  end

  describe "abbreviate/1" do
    test "名前を省略形に変換" do
      assert Ch03.ImmutableData.abbreviate("Alonzo Church") == "A. Church"
    end

    test "すでに省略形の場合" do
      assert Ch03.ImmutableData.abbreviate("A. Church") == "A. Church"
    end
  end

  describe "concat_strings/2" do
    test "文字列を結合" do
      assert Ch03.ImmutableData.concat_strings("Hello", " World") == "Hello World"
    end
  end

  describe "substring/3" do
    test "文字列の一部を切り出し" do
      assert Ch03.ImmutableData.substring("Hello World", 0, 5) == "Hello"
    end
  end

  describe "concat_lists/2" do
    test "リストを結合" do
      assert Ch03.ImmutableData.concat_lists(["a", "b"], ["c", "d"]) == ["a", "b", "c", "d"]
    end
  end

  describe "demonstrate_immutability/0" do
    test "イミュータブルの実演" do
      {original, modified} = Ch03.ImmutableData.demonstrate_immutability()
      assert original == [1, 2, 3]
      assert modified == [1, 2, 3, 4]
    end
  end
end
