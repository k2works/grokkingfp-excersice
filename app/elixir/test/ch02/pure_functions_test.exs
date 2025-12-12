defmodule Ch02.PureFunctionsTest do
  use ExUnit.Case, async: true
  doctest Ch02.PureFunctions

  describe "increment/1" do
    test "正の数を増加" do
      assert Ch02.PureFunctions.increment(5) == 6
    end

    test "ゼロを増加" do
      assert Ch02.PureFunctions.increment(0) == 1
    end

    test "負の数を増加" do
      assert Ch02.PureFunctions.increment(-3) == -2
    end

    test "同じ入力に対して常に同じ出力（参照透過性）" do
      assert Ch02.PureFunctions.increment(5) == Ch02.PureFunctions.increment(5)
    end
  end

  describe "add/2" do
    test "2つの正の数を加算" do
      assert Ch02.PureFunctions.add(2, 3) == 5
    end

    test "負の数を加算" do
      assert Ch02.PureFunctions.add(-2, 3) == 1
    end

    test "交換法則" do
      assert Ch02.PureFunctions.add(2, 3) == Ch02.PureFunctions.add(3, 2)
    end
  end

  describe "double/1" do
    test "正の数を2倍" do
      assert Ch02.PureFunctions.double(7) == 14
    end

    test "ゼロを2倍" do
      assert Ch02.PureFunctions.double(0) == 0
    end

    test "負の数を2倍" do
      assert Ch02.PureFunctions.double(-3) == -6
    end
  end

  describe "get_first_character/1" do
    test "通常の文字列" do
      assert Ch02.PureFunctions.get_first_character("Elixir") == "E"
    end

    test "空文字列" do
      assert Ch02.PureFunctions.get_first_character("") == nil
    end
  end

  describe "word_score/1" do
    test "通常の文字列" do
      assert Ch02.PureFunctions.word_score("Elixir") == 6
    end

    test "空文字列" do
      assert Ch02.PureFunctions.word_score("") == 0
    end
  end

  describe "word_score_without_a/1" do
    test "'a' を含む文字列" do
      assert Ch02.PureFunctions.word_score_without_a("Scala") == 3
    end

    test "複数の 'a' を含む文字列" do
      assert Ch02.PureFunctions.word_score_without_a("banana") == 3
    end

    test "'a' を含まない文字列" do
      assert Ch02.PureFunctions.word_score_without_a("xyz") == 3
    end

    test "空文字列" do
      assert Ch02.PureFunctions.word_score_without_a("") == 0
    end
  end

  describe "word_score_without_char/2" do
    test "特定の文字を除外" do
      assert Ch02.PureFunctions.word_score_without_char("Elixir", "i") == 4
    end
  end

  describe "total_score/2" do
    test "2つの単語の合計スコア" do
      assert Ch02.PureFunctions.total_score("Elixir", "Scala") == 9
    end
  end

  describe "even?/1" do
    test "偶数" do
      assert Ch02.PureFunctions.even?(0) == true
      assert Ch02.PureFunctions.even?(2) == true
      assert Ch02.PureFunctions.even?(4) == true
      assert Ch02.PureFunctions.even?(-2) == true
    end

    test "奇数" do
      assert Ch02.PureFunctions.even?(1) == false
      assert Ch02.PureFunctions.even?(3) == false
      assert Ch02.PureFunctions.even?(-3) == false
    end
  end

  describe "odd?/1" do
    test "奇数" do
      assert Ch02.PureFunctions.odd?(1) == true
      assert Ch02.PureFunctions.odd?(3) == true
    end

    test "偶数" do
      assert Ch02.PureFunctions.odd?(2) == false
      assert Ch02.PureFunctions.odd?(0) == false
    end
  end

  describe "absolute/1" do
    test "正の数の絶対値" do
      assert Ch02.PureFunctions.absolute(5) == 5
    end

    test "負の数の絶対値" do
      assert Ch02.PureFunctions.absolute(-5) == 5
    end

    test "ゼロの絶対値" do
      assert Ch02.PureFunctions.absolute(0) == 0
    end
  end

  describe "reverse_string/1" do
    test "通常の文字列を逆順" do
      assert Ch02.PureFunctions.reverse_string("hello") == "olleh"
    end

    test "空文字列を逆順" do
      assert Ch02.PureFunctions.reverse_string("") == ""
    end

    test "1文字の文字列を逆順" do
      assert Ch02.PureFunctions.reverse_string("a") == "a"
    end
  end

  describe "palindrome?/1" do
    test "回文" do
      assert Ch02.PureFunctions.palindrome?("radar") == true
      assert Ch02.PureFunctions.palindrome?("level") == true
      assert Ch02.PureFunctions.palindrome?("") == true
      assert Ch02.PureFunctions.palindrome?("a") == true
    end

    test "回文でない" do
      assert Ch02.PureFunctions.palindrome?("hello") == false
    end
  end

  describe "list_length/1" do
    test "リストの長さ" do
      assert Ch02.PureFunctions.list_length([1, 2, 3]) == 3
    end

    test "空リストの長さ" do
      assert Ch02.PureFunctions.list_length([]) == 0
    end
  end

  describe "sum/1" do
    test "数値リストの合計" do
      assert Ch02.PureFunctions.sum([1, 2, 3, 4, 5]) == 15
    end

    test "空リストの合計" do
      assert Ch02.PureFunctions.sum([]) == 0
    end

    test "負の数を含むリストの合計" do
      assert Ch02.PureFunctions.sum([-1, 2, -3]) == -2
    end
  end

  describe "average/1" do
    test "数値リストの平均" do
      assert Ch02.PureFunctions.average([1, 2, 3, 4, 5]) == 3.0
    end

    test "単一要素の平均" do
      assert Ch02.PureFunctions.average([10]) == 10.0
    end

    test "空リストの平均" do
      assert Ch02.PureFunctions.average([]) == nil
    end
  end

  describe "max_value/1" do
    test "数値リストの最大値" do
      assert Ch02.PureFunctions.max_value([3, 1, 4, 1, 5, 9]) == {:ok, 9}
    end

    test "負の数を含むリストの最大値" do
      assert Ch02.PureFunctions.max_value([-3, -1, -4]) == {:ok, -1}
    end

    test "空リストの最大値" do
      assert Ch02.PureFunctions.max_value([]) == {:error, :empty_list}
    end
  end

  describe "min_value/1" do
    test "数値リストの最小値" do
      assert Ch02.PureFunctions.min_value([3, 1, 4, 1, 5, 9]) == {:ok, 1}
    end

    test "空リストの最小値" do
      assert Ch02.PureFunctions.min_value([]) == {:error, :empty_list}
    end
  end

  describe "参照透過性" do
    test "純粋関数は同じ入力に対して常に同じ出力を返す" do
      # 複数回呼び出しても同じ結果
      for _ <- 1..100 do
        assert Ch02.PureFunctions.increment(5) == 6
        assert Ch02.PureFunctions.word_score("test") == 4
        assert Ch02.PureFunctions.even?(4) == true
      end
    end
  end
end
