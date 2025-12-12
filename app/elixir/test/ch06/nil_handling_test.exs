defmodule Ch06.NilHandlingTest do
  use ExUnit.Case, async: true
  doctest Ch06.NilHandling

  alias Ch06.NilHandling
  alias Ch06.NilHandling.TvShow

  describe "sort_shows/1" do
    test "放送期間でソート" do
      shows = [
        %TvShow{title: "Breaking Bad", start: 2008, end_year: 2013},
        %TvShow{title: "The Wire", start: 2002, end_year: 2008},
        %TvShow{title: "Mad Men", start: 2007, end_year: 2015}
      ]

      sorted = NilHandling.sort_shows(shows)
      assert Enum.map(sorted, & &1.title) == ["Mad Men", "The Wire", "Breaking Bad"]
    end
  end

  describe "parse_show_unsafe!/1" do
    test "正常な入力をパース" do
      result = NilHandling.parse_show_unsafe!("Breaking Bad (2008-2013)")
      assert result == %TvShow{title: "Breaking Bad", start: 2008, end_year: 2013}
    end

    test "不正な入力で例外" do
      assert_raise FunctionClauseError, fn ->
        NilHandling.parse_show_unsafe!("Invalid Show")
      end
    end
  end

  describe "extract_name/1" do
    test "正常な番組名を抽出" do
      assert NilHandling.extract_name("Breaking Bad (2008-2013)") == "Breaking Bad"
      assert NilHandling.extract_name("The Wire (2002-2008)") == "The Wire"
    end

    test "名前がない場合はnil" do
      assert NilHandling.extract_name("(2008-2013)") == nil
      assert NilHandling.extract_name("") == nil
    end
  end

  describe "extract_year_start/1" do
    test "開始年を抽出" do
      assert NilHandling.extract_year_start("Breaking Bad (2008-2013)") == 2008
      assert NilHandling.extract_year_start("(2002- N/A ) The Wire") == 2002
    end

    test "開始年がない場合はnil" do
      assert NilHandling.extract_year_start("Mad Men (-2015)") == nil
      assert NilHandling.extract_year_start("Invalid") == nil
    end

    test "パースできない場合はnil" do
      assert NilHandling.extract_year_start("Show (abc-2013)") == nil
    end
  end

  describe "extract_year_end/1" do
    test "終了年を抽出" do
      assert NilHandling.extract_year_end("Breaking Bad (2008-2013)") == 2013
    end

    test "終了年がない場合はnil" do
      assert NilHandling.extract_year_end("Stranger Things (2016-)") == nil
      assert NilHandling.extract_year_end("Invalid") == nil
    end
  end

  describe "extract_single_year/1" do
    test "単一年を抽出" do
      assert NilHandling.extract_single_year("Chernobyl (2019)") == 2019
    end

    test "ダッシュがある場合はnil" do
      assert NilHandling.extract_single_year("Breaking Bad (2008-2013)") == nil
    end

    test "括弧がない場合はnil" do
      assert NilHandling.extract_single_year("Chernobyl 2019") == nil
    end
  end

  describe "or_else/2" do
    test "値がある場合はその値" do
      assert NilHandling.or_else(10, 42) == 10
      assert NilHandling.or_else("hello", "world") == "hello"
    end

    test "nilの場合は代替値" do
      assert NilHandling.or_else(nil, 42) == 42
      assert NilHandling.or_else(nil, "fallback") == "fallback"
    end

    test "代替値が関数の場合" do
      assert NilHandling.or_else(nil, fn -> 100 end) == 100
      assert NilHandling.or_else(10, fn -> 100 end) == 10
    end
  end

  describe "parse_show/1" do
    test "通常のTV番組をパース" do
      result = NilHandling.parse_show("Breaking Bad (2008-2013)")
      assert result == %TvShow{title: "Breaking Bad", start: 2008, end_year: 2013}
    end

    test "ミニシリーズをパース" do
      result = NilHandling.parse_show("Chernobyl (2019)")
      assert result == %TvShow{title: "Chernobyl", start: 2019, end_year: 2019}
    end

    test "不正な入力はnil" do
      assert NilHandling.parse_show("Invalid Show") == nil
      assert NilHandling.parse_show("(2019)") == nil
      assert NilHandling.parse_show("Show (-)") == nil
    end
  end

  describe "parse_shows_best_effort/1" do
    test "有効な番組のみを返す" do
      raw = ["Breaking Bad (2008-2013)", "Invalid", "Chernobyl (2019)"]
      shows = NilHandling.parse_shows_best_effort(raw)
      assert length(shows) == 2
      assert hd(shows).title == "Breaking Bad"
    end

    test "すべて無効な場合は空リスト" do
      raw = ["Invalid1", "Invalid2"]
      assert NilHandling.parse_shows_best_effort(raw) == []
    end

    test "空リストの場合は空リスト" do
      assert NilHandling.parse_shows_best_effort([]) == []
    end
  end

  describe "parse_shows_all_or_nothing/1" do
    test "すべて有効な場合は成功" do
      raw = ["Breaking Bad (2008-2013)", "Chernobyl (2019)"]
      {:ok, shows} = NilHandling.parse_shows_all_or_nothing(raw)
      assert length(shows) == 2
    end

    test "1つでも無効な場合はnil" do
      raw = ["Breaking Bad (2008-2013)", "Invalid"]
      assert NilHandling.parse_shows_all_or_nothing(raw) == nil
    end

    test "空リストの場合は成功" do
      assert NilHandling.parse_shows_all_or_nothing([]) == {:ok, []}
    end
  end

  describe "nil_map/2" do
    test "値がある場合は関数を適用" do
      assert NilHandling.nil_map(5, &(&1 * 2)) == 10
      assert NilHandling.nil_map("hello", &String.upcase/1) == "HELLO"
    end

    test "nilの場合はnil" do
      assert NilHandling.nil_map(nil, &(&1 * 2)) == nil
    end
  end

  describe "nil_flat_map/2" do
    test "値がある場合は関数を適用" do
      assert NilHandling.nil_flat_map(5, fn x -> x * 2 end) == 10
    end

    test "関数がnilを返す場合" do
      assert NilHandling.nil_flat_map(5, fn _x -> nil end) == nil
    end

    test "nilの場合はnil" do
      assert NilHandling.nil_flat_map(nil, fn x -> x * 2 end) == nil
    end
  end

  describe "extract_single_year_or_year_end/1" do
    test "単一年を優先" do
      assert NilHandling.extract_single_year_or_year_end("B (2002)") == 2002
    end

    test "単一年がなければ終了年" do
      assert NilHandling.extract_single_year_or_year_end("C (-2012)") == 2012
    end

    test "どちらもない場合はnil" do
      assert NilHandling.extract_single_year_or_year_end("A (1992-)") == nil
    end
  end

  describe "extract_any_year/1" do
    test "開始年を優先" do
      assert NilHandling.extract_any_year("A (1992-)") == 1992
    end

    test "単一年を抽出" do
      assert NilHandling.extract_any_year("B (2002)") == 2002
    end

    test "終了年を抽出" do
      assert NilHandling.extract_any_year("C (-2012)") == 2012
    end

    test "どれもない場合はnil" do
      assert NilHandling.extract_any_year("E (-)") == nil
    end
  end

  describe "extract_single_year_if_name_exists/1" do
    test "名前と単一年がある場合" do
      assert NilHandling.extract_single_year_if_name_exists("B (2002)") == 2002
    end

    test "名前がない場合はnil" do
      assert NilHandling.extract_single_year_if_name_exists("(2022)") == nil
    end

    test "単一年がない場合はnil" do
      assert NilHandling.extract_single_year_if_name_exists("A (1992-)") == nil
    end
  end

  describe "extract_any_year_if_name_exists/1" do
    test "名前と年がある場合" do
      assert NilHandling.extract_any_year_if_name_exists("A (1992-)") == 1992
    end

    test "名前がない場合はnil" do
      assert NilHandling.extract_any_year_if_name_exists("(2022)") == nil
    end
  end

  describe "to_list/1" do
    test "値をリストに変換" do
      assert NilHandling.to_list(42) == [42]
      assert NilHandling.to_list("hello") == ["hello"]
    end

    test "nilを空リストに変換" do
      assert NilHandling.to_list(nil) == []
    end
  end

  describe "list_head/1" do
    test "最初の要素を取得" do
      assert NilHandling.list_head([1, 2, 3]) == 1
    end

    test "空リストはnil" do
      assert NilHandling.list_head([]) == nil
    end
  end

  describe "nil_forall/2" do
    test "nilは常にtrue" do
      assert NilHandling.nil_forall(nil, fn x -> x > 0 end) == true
    end

    test "条件を満たす場合はtrue" do
      assert NilHandling.nil_forall(5, fn x -> x > 0 end) == true
    end

    test "条件を満たさない場合はfalse" do
      assert NilHandling.nil_forall(-1, fn x -> x > 0 end) == false
    end
  end

  describe "nil_exists/2" do
    test "nilは常にfalse" do
      assert NilHandling.nil_exists(nil, fn x -> x > 0 end) == false
    end

    test "条件を満たす場合はtrue" do
      assert NilHandling.nil_exists(5, fn x -> x > 0 end) == true
    end

    test "条件を満たさない場合はfalse" do
      assert NilHandling.nil_exists(-1, fn x -> x > 0 end) == false
    end
  end
end
