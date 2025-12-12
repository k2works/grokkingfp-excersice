defmodule Ch05.FlatMapAndComprehensionsTest do
  use ExUnit.Case, async: true
  doctest Ch05.FlatMapAndComprehensions

  alias Ch05.FlatMapAndComprehensions
  alias Ch05.FlatMapAndComprehensions.{Book, Point}

  describe "flatten/1" do
    test "ネストしたリストを平坦化" do
      assert FlatMapAndComprehensions.flatten([[1, 2], [3], [4, 5]]) == [1, 2, 3, 4, 5]
    end

    test "空リストを含む" do
      assert FlatMapAndComprehensions.flatten([[1], [], [2, 3]]) == [1, 2, 3]
    end
  end

  describe "get_all_authors/1" do
    test "全著者を抽出" do
      books = [
        %Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
        %Book{title: "The Hobbit", authors: ["Tolkien"]}
      ]

      assert FlatMapAndComprehensions.get_all_authors(books) == ["Chiusano", "Bjarnason", "Tolkien"]
    end
  end

  describe "flat_map_example/2" do
    test "要素数が増える" do
      assert FlatMapAndComprehensions.flat_map_example([1, 2, 3], fn i -> [i, i + 10] end) ==
               [1, 11, 2, 12, 3, 13]
    end
  end

  describe "get_all_authors_flat_map/1" do
    test "flat_map版も同じ結果" do
      books = [
        %Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
        %Book{title: "The Hobbit", authors: ["Tolkien"]}
      ]

      assert FlatMapAndComprehensions.get_all_authors_flat_map(books) ==
               FlatMapAndComprehensions.get_all_authors(books)
    end
  end

  describe "duplicate_elements/1" do
    test "各要素を複製" do
      assert FlatMapAndComprehensions.duplicate_elements([1, 2, 3]) == [1, 1, 2, 2, 3, 3]
    end
  end

  describe "filter_even/1" do
    test "偶数のみ抽出" do
      assert FlatMapAndComprehensions.filter_even([1, 2, 3, 4, 5]) == [2, 4]
    end
  end

  describe "filter_by_flat_map/2" do
    test "flat_map によるフィルタリング" do
      assert FlatMapAndComprehensions.filter_by_flat_map([1, 2, 3, 4, 5], &(&1 > 3)) == [4, 5]
    end
  end

  describe "book_adaptations/1" do
    test "Tolkien の映画化作品" do
      movies = FlatMapAndComprehensions.book_adaptations("Tolkien")
      assert length(movies) == 2
      assert hd(movies).title == "An Unexpected Journey"
    end

    test "未知の著者は空リスト" do
      assert FlatMapAndComprehensions.book_adaptations("Unknown") == []
    end
  end

  describe "get_recommendations/1" do
    test "映画リコメンデーションを生成" do
      books = [
        %Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
        %Book{title: "The Hobbit", authors: ["Tolkien"]}
      ]

      recommendations = FlatMapAndComprehensions.get_recommendations(books)
      assert length(recommendations) == 2
      assert hd(recommendations) =~ "You may like"
      assert hd(recommendations) =~ "Tolkien"
    end
  end

  describe "for 内包表記" do
    test "基本例" do
      assert FlatMapAndComprehensions.for_basic_example() == [2, 4, 6, 8, 10]
    end

    test "フィルタリング" do
      assert FlatMapAndComprehensions.for_filter_example() == [2, 4]
    end

    test "組み合わせ" do
      assert FlatMapAndComprehensions.for_combination_example() ==
               [{1, "a"}, {1, "b"}, {2, "a"}, {2, "b"}]
    end
  end

  describe "get_recommendations_for/1" do
    test "for 内包表記版も同じ結果" do
      books = [
        %Book{title: "FP in Scala", authors: ["Chiusano", "Bjarnason"]},
        %Book{title: "The Hobbit", authors: ["Tolkien"]}
      ]

      assert FlatMapAndComprehensions.get_recommendations_for(books) ==
               FlatMapAndComprehensions.get_recommendations(books)
    end
  end

  describe "inside?/2" do
    test "点が円の内側" do
      assert FlatMapAndComprehensions.inside?(%Point{x: 1, y: 1}, 2) == true
    end

    test "点が円の外側" do
      assert FlatMapAndComprehensions.inside?(%Point{x: 5, y: 2}, 2) == false
    end

    test "境界上" do
      # 1^2 + 0^2 = 1 <= 1^2
      assert FlatMapAndComprehensions.inside?(%Point{x: 1, y: 0}, 1) == true
    end
  end

  describe "all_combinations/2" do
    test "全組み合わせを生成" do
      points = [%Point{x: 5, y: 2}, %Point{x: 1, y: 1}]
      radiuses = [2, 1]
      results = FlatMapAndComprehensions.all_combinations(points, radiuses)
      assert length(results) == 4
    end
  end

  describe "inside_points/2" do
    test "円内の点のみ抽出" do
      points = [%Point{x: 5, y: 2}, %Point{x: 1, y: 1}]
      radiuses = [2, 1]
      results = FlatMapAndComprehensions.inside_points(points, radiuses)
      assert results == ["Point(1, 1) is within a radius of 2"]
    end
  end

  describe "for_into_set/1" do
    test "MapSet を生成" do
      result = FlatMapAndComprehensions.for_into_set([1, 2, 2, 3])
      assert result == MapSet.new([1, 2, 3])
    end
  end

  describe "for_into_map/1" do
    test "Map を生成" do
      result = FlatMapAndComprehensions.for_into_map(["a", "bb", "ccc"])
      assert result == %{"a" => 1, "bb" => 2, "ccc" => 3}
    end
  end

  describe "three_list_combinations/0" do
    test "3リストの組み合わせ" do
      assert FlatMapAndComprehensions.three_list_combinations() ==
               [111, 211, 121, 221, 112, 212, 122, 222]
    end
  end

  describe "three_list_combinations_for/0" do
    test "for 版も同じ結果" do
      assert FlatMapAndComprehensions.three_list_combinations_for() ==
               FlatMapAndComprehensions.three_list_combinations()
    end
  end
end
