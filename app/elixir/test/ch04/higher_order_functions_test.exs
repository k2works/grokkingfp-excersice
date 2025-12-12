defmodule Ch04.HigherOrderFunctionsTest do
  use ExUnit.Case, async: true
  doctest Ch04.HigherOrderFunctions

  describe "score/1" do
    test "'a' を除外してスコア計算" do
      assert Ch04.HigherOrderFunctions.score("rust") == 4
      assert Ch04.HigherOrderFunctions.score("java") == 2
      assert Ch04.HigherOrderFunctions.score("scala") == 3
    end
  end

  describe "bonus/1" do
    test "'c' を含む場合ボーナス" do
      assert Ch04.HigherOrderFunctions.bonus("scala") == 5
      assert Ch04.HigherOrderFunctions.bonus("rust") == 0
    end
  end

  describe "penalty/1" do
    test "'s' を含む場合ペナルティ" do
      assert Ch04.HigherOrderFunctions.penalty("scala") == 7
      assert Ch04.HigherOrderFunctions.penalty("java") == 0
    end
  end

  describe "map_example/2" do
    test "各要素を変換" do
      assert Ch04.HigherOrderFunctions.map_example(["elixir", "rust", "ada"], &String.length/1) ==
               [6, 4, 3]
    end

    test "数値を2倍" do
      assert Ch04.HigherOrderFunctions.map_example([5, 1, 2, 4, 0], &(&1 * 2)) ==
               [10, 2, 4, 8, 0]
    end
  end

  describe "double_all/1" do
    test "全要素を2倍" do
      assert Ch04.HigherOrderFunctions.double_all([1, 2, 3]) == [2, 4, 6]
    end

    test "空リスト" do
      assert Ch04.HigherOrderFunctions.double_all([]) == []
    end
  end

  describe "filter_example/2" do
    test "奇数をフィルタ" do
      assert Ch04.HigherOrderFunctions.filter_example([5, 1, 2, 4, 0], &(rem(&1, 2) == 1)) ==
               [5, 1]
    end

    test "4より大きい要素" do
      assert Ch04.HigherOrderFunctions.filter_example([5, 1, 2, 4, 0], &(&1 > 4)) == [5]
    end
  end

  describe "odds/1" do
    test "奇数を抽出" do
      assert Ch04.HigherOrderFunctions.odds([5, 1, 2, 4, 0]) == [5, 1]
    end
  end

  describe "evens/1" do
    test "偶数を抽出" do
      assert Ch04.HigherOrderFunctions.evens([5, 1, 2, 4, 0]) == [2, 4, 0]
    end
  end

  describe "reduce_example/3" do
    test "合計を計算" do
      assert Ch04.HigherOrderFunctions.reduce_example([5, 1, 2, 4, 100], 0, &(&1 + &2)) == 112
    end
  end

  describe "sum/1" do
    test "リストの合計" do
      assert Ch04.HigherOrderFunctions.sum([5, 1, 2, 4, 100]) == 112
    end

    test "空リストの合計" do
      assert Ch04.HigherOrderFunctions.sum([]) == 0
    end
  end

  describe "max_value/1" do
    test "最大値を取得" do
      assert Ch04.HigherOrderFunctions.max_value([5, 1, 2, 4, 15]) == 15
    end

    test "負の数を含む場合" do
      assert Ch04.HigherOrderFunctions.max_value([-5, -1, -10]) == -1
    end
  end

  describe "product/1" do
    test "積を計算" do
      assert Ch04.HigherOrderFunctions.product([1, 2, 3, 4]) == 24
    end
  end

  describe "sort_by_score/1" do
    test "スコアでソート" do
      assert Ch04.HigherOrderFunctions.sort_by_score(["rust", "java"]) == ["java", "rust"]
    end
  end

  describe "sort_by_example/2" do
    test "長さでソート" do
      assert Ch04.HigherOrderFunctions.sort_by_example(["elixir", "rust", "go"], &String.length/1) ==
               ["go", "rust", "elixir"]
    end
  end

  describe "larger_than/1" do
    test "関数を返す" do
      larger = Ch04.HigherOrderFunctions.larger_than(4)
      assert larger.(5) == true
      assert larger.(3) == false
      assert larger.(4) == false
    end
  end

  describe "divisible_by/1" do
    test "割り切れるか判定する関数を返す" do
      divisible = Ch04.HigherOrderFunctions.divisible_by(3)
      assert divisible.(9) == true
      assert divisible.(7) == false
    end
  end

  describe "filter_larger_than/2" do
    test "指定値より大きい要素をフィルタ" do
      assert Ch04.HigherOrderFunctions.filter_larger_than([5, 1, 2, 4, 0], 4) == [5]
      assert Ch04.HigherOrderFunctions.filter_larger_than([5, 1, 2, 4, 0], 1) == [5, 2, 4]
    end
  end

  describe "ranked_words/2" do
    test "スコア関数でランキング" do
      words = ["ada", "haskell", "scala", "java", "rust"]

      assert Ch04.HigherOrderFunctions.ranked_words(&Ch04.HigherOrderFunctions.score/1, words) ==
               ["haskell", "rust", "scala", "java", "ada"]
    end
  end

  describe "ranking_basic/1" do
    test "基本スコアでランキング" do
      words = ["ada", "haskell", "scala", "java", "rust"]
      assert Ch04.HigherOrderFunctions.ranking_basic(words) == ["haskell", "rust", "scala", "java", "ada"]
    end
  end

  describe "ranking_with_bonus/1" do
    test "ボーナス付きランキング" do
      words = ["ada", "haskell", "scala", "java", "rust"]
      assert Ch04.HigherOrderFunctions.ranking_with_bonus(words) == ["scala", "haskell", "rust", "java", "ada"]
    end
  end

  describe "ranking_with_bonus_and_penalty/1" do
    test "ボーナスとペナルティ付きランキング" do
      words = ["ada", "haskell", "scala", "java", "rust"]

      assert Ch04.HigherOrderFunctions.ranking_with_bonus_and_penalty(words) ==
               ["java", "scala", "ada", "haskell", "rust"]
    end
  end

  describe "language_names/1" do
    test "言語名を抽出" do
      languages = [%{name: "Java", year: 1995}, %{name: "Scala", year: 2004}]
      assert Ch04.HigherOrderFunctions.language_names(languages) == ["Java", "Scala"]
    end
  end

  describe "languages_after/2" do
    test "指定年以降の言語をフィルタ" do
      languages = [%{name: "Java", year: 1995}, %{name: "Scala", year: 2004}]
      assert Ch04.HigherOrderFunctions.languages_after(languages, 2000) == [%{name: "Scala", year: 2004}]
    end
  end

  describe "count_where/2" do
    test "条件を満たす要素数" do
      assert Ch04.HigherOrderFunctions.count_where([1, 2, 3, 4, 5], &(&1 > 3)) == 2
    end

    test "文字列の長さで条件" do
      assert Ch04.HigherOrderFunctions.count_where(["a", "bb", "ccc"], &(String.length(&1) > 1)) == 2
    end
  end
end
