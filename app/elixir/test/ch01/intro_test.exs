defmodule Ch01.IntroTest do
  use ExUnit.Case, async: true
  doctest Ch01.Intro

  describe "increment/1" do
    test "正の数を増加" do
      assert Ch01.Intro.increment(5) == 6
    end

    test "ゼロを増加" do
      assert Ch01.Intro.increment(0) == 1
    end

    test "負の数を増加" do
      assert Ch01.Intro.increment(-3) == -2
    end
  end

  describe "get_first_character/1" do
    test "文字列の最初の文字を取得" do
      assert Ch01.Intro.get_first_character("Elixir") == "E"
    end

    test "1文字の文字列" do
      assert Ch01.Intro.get_first_character("A") == "A"
    end

    test "空文字列" do
      assert Ch01.Intro.get_first_character("") == nil
    end
  end

  describe "word_score/1" do
    test "通常の文字列のスコア" do
      assert Ch01.Intro.word_score("Elixir") == 6
    end

    test "空文字列のスコア" do
      assert Ch01.Intro.word_score("") == 0
    end

    test "日本語文字列のスコア" do
      assert Ch01.Intro.word_score("関数型") == 3
    end
  end

  describe "sum_imperative_style/1" do
    test "数値リストの合計" do
      assert Ch01.Intro.sum_imperative_style([1, 2, 3, 4, 5]) == 15
    end

    test "空リストの合計" do
      assert Ch01.Intro.sum_imperative_style([]) == 0
    end

    test "単一要素の合計" do
      assert Ch01.Intro.sum_imperative_style([42]) == 42
    end
  end

  describe "sum_functional_style/1" do
    test "数値リストの合計" do
      assert Ch01.Intro.sum_functional_style([1, 2, 3, 4, 5]) == 15
    end

    test "命令型と関数型が同じ結果を返す" do
      numbers = [1, 2, 3, 4, 5]

      assert Ch01.Intro.sum_imperative_style(numbers) ==
               Ch01.Intro.sum_functional_style(numbers)
    end
  end

  describe "extract_name/1" do
    test "タプルから名前を抽出" do
      assert Ch01.Intro.extract_name({:user, "Alice", 30}) == "Alice"
    end
  end

  describe "head/1" do
    test "リストの先頭を取得" do
      assert Ch01.Intro.head([1, 2, 3]) == {:ok, 1}
    end

    test "空リストの先頭" do
      assert Ch01.Intro.head([]) == {:error, :empty_list}
    end
  end

  describe "tail/1" do
    test "リストの末尾を取得" do
      assert Ch01.Intro.tail([1, 2, 3]) == {:ok, [2, 3]}
    end

    test "空リストの末尾" do
      assert Ch01.Intro.tail([]) == {:error, :empty_list}
    end

    test "単一要素リストの末尾" do
      assert Ch01.Intro.tail([1]) == {:ok, []}
    end
  end

  describe "count_words/1" do
    test "複数単語をカウント" do
      assert Ch01.Intro.count_words("hello world elixir") == 3
    end

    test "単一単語" do
      assert Ch01.Intro.count_words("hello") == 1
    end

    test "空文字列" do
      assert Ch01.Intro.count_words("") == 0
    end
  end

  describe "count_words_without_pipe/1" do
    test "パイプありと同じ結果" do
      text = "hello world elixir"

      assert Ch01.Intro.count_words(text) ==
               Ch01.Intro.count_words_without_pipe(text)
    end
  end

  describe "apply_operation/3" do
    test "加算を適用" do
      assert Ch01.Intro.apply_operation(5, 3, fn a, b -> a + b end) == 8
    end

    test "乗算を適用" do
      assert Ch01.Intro.apply_operation(5, 3, fn a, b -> a * b end) == 15
    end

    test "減算を適用" do
      assert Ch01.Intro.apply_operation(5, 3, fn a, b -> a - b end) == 2
    end
  end

  describe "double_all/1" do
    test "リストの全要素を2倍" do
      assert Ch01.Intro.double_all([1, 2, 3]) == [2, 4, 6]
    end

    test "空リスト" do
      assert Ch01.Intro.double_all([]) == []
    end
  end

  describe "describe_number/1" do
    test "正の数" do
      assert Ch01.Intro.describe_number(5) == "positive"
    end

    test "負の数" do
      assert Ch01.Intro.describe_number(-3) == "negative"
    end

    test "ゼロ" do
      assert Ch01.Intro.describe_number(0) == "zero"
    end
  end

  describe "describe_type/1" do
    test "文字列の型" do
      assert Ch01.Intro.describe_type("hello") == "string"
    end

    test "整数の型" do
      assert Ch01.Intro.describe_type(42) == "integer"
    end

    test "浮動小数点数の型" do
      assert Ch01.Intro.describe_type(3.14) == "float"
    end

    test "リストの型" do
      assert Ch01.Intro.describe_type([1, 2, 3]) == "list"
    end

    test "不明な型" do
      assert Ch01.Intro.describe_type({:tuple, 1}) == "unknown"
    end
  end
end
