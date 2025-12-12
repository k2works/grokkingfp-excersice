defmodule Ch07.ErrorHandlingTest do
  use ExUnit.Case, async: true
  doctest Ch07.ErrorHandling

  alias Ch07.ErrorHandling
  alias Ch07.ErrorHandling.{TvShow, Artist, Song, Playlist}

  describe "extract_name/1" do
    test "正常な番組名を抽出" do
      assert ErrorHandling.extract_name("Breaking Bad (2008-2013)") == {:ok, "Breaking Bad"}
    end

    test "名前がない場合はエラー" do
      {:error, msg} = ErrorHandling.extract_name("(2008-2013)")
      assert String.contains?(msg, "Can't extract name")
    end
  end

  describe "extract_year_start/1" do
    test "開始年を抽出" do
      assert ErrorHandling.extract_year_start("Breaking Bad (2008-2013)") == {:ok, 2008}
    end

    test "開始年がない場合はエラー" do
      {:error, msg} = ErrorHandling.extract_year_start("Mad Men (-2015)")
      assert String.contains?(msg, "Can't extract start year")
    end

    test "パースできない場合はエラー" do
      {:error, msg} = ErrorHandling.extract_year_start("The Wire (oops-2008)")
      assert String.contains?(msg, "Can't parse")
    end
  end

  describe "extract_year_end/1" do
    test "終了年を抽出" do
      assert ErrorHandling.extract_year_end("Breaking Bad (2008-2013)") == {:ok, 2013}
    end

    test "終了年がない場合はエラー" do
      {:error, msg} = ErrorHandling.extract_year_end("Stranger Things (2016-)")
      assert String.contains?(msg, "Can't extract end year")
    end
  end

  describe "extract_single_year/1" do
    test "単一年を抽出" do
      assert ErrorHandling.extract_single_year("Chernobyl (2019)") == {:ok, 2019}
    end

    test "ダッシュがある場合はエラー" do
      {:error, msg} = ErrorHandling.extract_single_year("Breaking Bad (2008-2013)")
      assert String.contains?(msg, "Can't extract single year")
    end
  end

  describe "or_else/2" do
    test "成功時はそのまま" do
      assert ErrorHandling.or_else({:ok, 1}, fn -> {:ok, 2} end) == {:ok, 1}
    end

    test "失敗時は代替を実行" do
      assert ErrorHandling.or_else({:error, "failed"}, fn -> {:ok, 2} end) == {:ok, 2}
    end

    test "代替も失敗する場合" do
      assert ErrorHandling.or_else({:error, "first"}, fn -> {:error, "second"} end) ==
               {:error, "second"}
    end
  end

  describe "result_map/2" do
    test "成功時は関数を適用" do
      assert ErrorHandling.result_map({:ok, 5}, &(&1 * 2)) == {:ok, 10}
    end

    test "失敗時はそのまま" do
      assert ErrorHandling.result_map({:error, "oops"}, &(&1 * 2)) == {:error, "oops"}
    end
  end

  describe "result_flat_map/2" do
    test "成功時は関数を適用" do
      assert ErrorHandling.result_flat_map({:ok, 5}, fn x -> {:ok, x * 2} end) == {:ok, 10}
    end

    test "関数がエラーを返す場合" do
      assert ErrorHandling.result_flat_map({:ok, 5}, fn _x -> {:error, "failed"} end) ==
               {:error, "failed"}
    end

    test "失敗時はそのまま" do
      assert ErrorHandling.result_flat_map({:error, "oops"}, fn x -> {:ok, x * 2} end) ==
               {:error, "oops"}
    end
  end

  describe "parse_show/1" do
    test "通常のTV番組をパース" do
      {:ok, result} = ErrorHandling.parse_show("Breaking Bad (2008-2013)")
      assert result == %TvShow{title: "Breaking Bad", start: 2008, end_year: 2013}
    end

    test "ミニシリーズをパース" do
      {:ok, result} = ErrorHandling.parse_show("Chernobyl (2019)")
      assert result == %TvShow{title: "Chernobyl", start: 2019, end_year: 2019}
    end

    test "名前がない場合はエラー" do
      {:error, msg} = ErrorHandling.parse_show("(2019)")
      assert String.contains?(msg, "Can't extract name")
    end

    test "年がない場合はエラー" do
      {:error, msg} = ErrorHandling.parse_show("The Wire (-)")
      assert String.contains?(msg, "Can't extract single year")
    end

    test "パースできない場合はエラー" do
      {:error, msg} = ErrorHandling.parse_show("The Wire (oops)")
      assert String.contains?(msg, "Can't parse")
    end
  end

  describe "parse_shows/1" do
    test "すべて有効な場合は成功" do
      raw = ["Breaking Bad (2008-2013)", "Chernobyl (2019)"]
      {:ok, shows} = ErrorHandling.parse_shows(raw)
      assert length(shows) == 2
    end

    test "1つでも無効な場合はエラー" do
      raw = ["Breaking Bad (2008-2013)", "Invalid"]
      {:error, msg} = ErrorHandling.parse_shows(raw)
      assert String.contains?(msg, "Can't extract")
    end

    test "空リストの場合は成功" do
      assert ErrorHandling.parse_shows([]) == {:ok, []}
    end
  end

  describe "parse_shows_best_effort/1" do
    test "有効な番組のみを返す" do
      raw = ["Breaking Bad (2008-2013)", "Invalid", "Chernobyl (2019)"]
      shows = ErrorHandling.parse_shows_best_effort(raw)
      assert length(shows) == 2
    end
  end

  describe "was_artist_active?/3" do
    test "現在も活動中のアーティスト" do
      metallica = %Artist{
        name: "Metallica",
        genre: :heavy_metal,
        origin: "U.S.",
        years_active: {:still_active, 1981}
      }

      assert ErrorHandling.was_artist_active?(metallica, 2019, 2022) == true
      assert ErrorHandling.was_artist_active?(metallica, 1970, 1975) == false
    end

    test "過去に活動していたアーティスト" do
      led_zeppelin = %Artist{
        name: "Led Zeppelin",
        genre: :hard_rock,
        origin: "England",
        years_active: {:active_between, 1968, 1980}
      }

      assert ErrorHandling.was_artist_active?(led_zeppelin, 1970, 1975) == true
      assert ErrorHandling.was_artist_active?(led_zeppelin, 1990, 2000) == false
    end
  end

  describe "active_length/2" do
    test "現在も活動中のアーティスト" do
      metallica = %Artist{
        name: "Metallica",
        genre: :heavy_metal,
        origin: "U.S.",
        years_active: {:still_active, 1981}
      }

      assert ErrorHandling.active_length(metallica, 2022) == 41
    end

    test "過去に活動していたアーティスト" do
      led_zeppelin = %Artist{
        name: "Led Zeppelin",
        genre: :hard_rock,
        origin: "England",
        years_active: {:active_between, 1968, 1980}
      }

      assert ErrorHandling.active_length(led_zeppelin, 2022) == 12
    end
  end

  describe "search_artists/2" do
    setup do
      artists = [
        %Artist{
          name: "Metallica",
          genre: :heavy_metal,
          origin: "U.S.",
          years_active: {:still_active, 1981}
        },
        %Artist{
          name: "Led Zeppelin",
          genre: :hard_rock,
          origin: "England",
          years_active: {:active_between, 1968, 1980}
        },
        %Artist{
          name: "Bee Gees",
          genre: :pop,
          origin: "England",
          years_active: {:active_between, 1958, 2003}
        }
      ]

      {:ok, artists: artists}
    end

    test "ジャンルで検索", %{artists: artists} do
      result = ErrorHandling.search_artists(artists, [{:by_genre, [:pop]}])
      assert length(result) == 1
      assert hd(result).name == "Bee Gees"
    end

    test "出身地で検索", %{artists: artists} do
      result = ErrorHandling.search_artists(artists, [{:by_origin, ["England"]}])
      assert length(result) == 2
    end

    test "活動期間で検索", %{artists: artists} do
      result = ErrorHandling.search_artists(artists, [{:by_active_years, 2019, 2022}])
      assert length(result) == 1
      assert hd(result).name == "Metallica"
    end

    test "複数条件で検索", %{artists: artists} do
      conditions = [
        {:by_genre, [:pop]},
        {:by_origin, ["England"]}
      ]

      result = ErrorHandling.search_artists(artists, conditions)
      assert length(result) == 1
      assert hd(result).name == "Bee Gees"
    end

    test "条件なしはすべて返す", %{artists: artists} do
      result = ErrorHandling.search_artists(artists, [])
      assert length(result) == 3
    end
  end

  describe "nil_to_result/2" do
    test "値をResultに変換" do
      assert ErrorHandling.nil_to_result(42, "no value") == {:ok, 42}
    end

    test "nilをエラーに変換" do
      assert ErrorHandling.nil_to_result(nil, "no value") == {:error, "no value"}
    end
  end

  describe "result_to_nil/1" do
    test "成功を値に変換" do
      assert ErrorHandling.result_to_nil({:ok, 42}) == 42
    end

    test "エラーをnilに変換" do
      assert ErrorHandling.result_to_nil({:error, "failed"}) == nil
    end
  end

  describe "gather_songs/3" do
    test "アーティストベースのプレイリストから収集" do
      foo_fighters = "Foo Fighters"

      playlist = %Playlist{
        name: "This is Foo Fighters",
        kind: {:based_on_artist, foo_fighters},
        songs: [
          %Song{artist: foo_fighters, title: "Breakout"},
          %Song{artist: foo_fighters, title: "Learn To Fly"}
        ]
      }

      songs = ErrorHandling.gather_songs([playlist], foo_fighters, :funk)
      assert length(songs) == 2
    end

    test "ジャンルベースのプレイリストから収集" do
      playlist = %Playlist{
        name: "Deep Focus",
        kind: {:based_on_genres, MapSet.new([:house, :funk])},
        songs: [
          %Song{artist: "Daft Punk", title: "One More Time"}
        ]
      }

      songs = ErrorHandling.gather_songs([playlist], "Other", :funk)
      assert length(songs) == 1
    end

    test "ユーザーキュレーションのプレイリストから収集" do
      foo_fighters = "Foo Fighters"

      playlist = %Playlist{
        name: "My Playlist",
        kind: {:curated_by_user, "User1"},
        songs: [
          %Song{artist: foo_fighters, title: "My Hero"},
          %Song{artist: "Iron Maiden", title: "The Trooper"}
        ]
      }

      songs = ErrorHandling.gather_songs([playlist], foo_fighters, :rock)
      assert length(songs) == 1
      assert hd(songs).title == "My Hero"
    end

    test "マッチしない場合は空" do
      playlist = %Playlist{
        name: "Rock Playlist",
        kind: {:based_on_artist, "Led Zeppelin"},
        songs: [%Song{artist: "Led Zeppelin", title: "Stairway to Heaven"}]
      }

      songs = ErrorHandling.gather_songs([playlist], "Other Artist", :pop)
      assert songs == []
    end
  end

  describe "extract_single_year_or_year_end/1" do
    test "単一年を優先" do
      assert ErrorHandling.extract_single_year_or_year_end("B (2002)") == {:ok, 2002}
    end

    test "単一年がなければ終了年" do
      assert ErrorHandling.extract_single_year_or_year_end("C (-2012)") == {:ok, 2012}
    end

    test "どちらもない場合はエラー" do
      {:error, _msg} = ErrorHandling.extract_single_year_or_year_end("A (1992-)")
    end
  end

  describe "extract_any_year/1" do
    test "開始年を優先" do
      assert ErrorHandling.extract_any_year("A (1992-)") == {:ok, 1992}
    end

    test "単一年を抽出" do
      assert ErrorHandling.extract_any_year("B (2002)") == {:ok, 2002}
    end

    test "終了年を抽出" do
      assert ErrorHandling.extract_any_year("C (-2012)") == {:ok, 2012}
    end
  end
end
