package ch07;

import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static ch07.MusicArtist.*;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第7章: 音楽アーティストのテスト
 */
@DisplayName("第7章: 音楽アーティスト")
class MusicArtistTest {

    private final Artist metallica = new Artist(
            "Metallica", MusicGenre.HEAVY_METAL, "U.S.", new StillActive(1981));
    private final Artist ledZeppelin = new Artist(
            "Led Zeppelin", MusicGenre.HARD_ROCK, "England", new ActiveBetween(1968, 1980));
    private final Artist beatles = new Artist(
            "The Beatles", MusicGenre.POP, "England", new ActiveBetween(1960, 1970));
    private final Artist queen = new Artist(
            "Queen", MusicGenre.HARD_ROCK, "England", new StillActive(1970));

    private final List<Artist> artists = List.of(metallica, ledZeppelin, beatles, queen);

    @Nested
    @DisplayName("wasArtistActive")
    class WasArtistActiveTest {

        @Test
        @DisplayName("StillActive: 指定期間より前から活動していれば true")
        void stillActiveBeforePeriod() {
            assertThat(wasArtistActive(metallica, 1990, 2000)).isTrue();
            assertThat(wasArtistActive(metallica, 2020, 2024)).isTrue();
        }

        @Test
        @DisplayName("StillActive: 指定期間より後から活動開始は false")
        void stillActiveAfterPeriod() {
            assertThat(wasArtistActive(metallica, 1970, 1975)).isFalse();
        }

        @Test
        @DisplayName("ActiveBetween: 期間が重なれば true")
        void activeBetweenOverlap() {
            assertThat(wasArtistActive(ledZeppelin, 1975, 1985)).isTrue();
            assertThat(wasArtistActive(ledZeppelin, 1965, 1970)).isTrue();
        }

        @Test
        @DisplayName("ActiveBetween: 期間が重ならなければ false")
        void activeBetweenNoOverlap() {
            assertThat(wasArtistActive(ledZeppelin, 1985, 1990)).isFalse();
            assertThat(wasArtistActive(ledZeppelin, 1950, 1960)).isFalse();
        }
    }

    @Nested
    @DisplayName("activeLength")
    class ActiveLengthTest {

        @Test
        @DisplayName("StillActive: 現在年との差")
        void stillActiveLength() {
            assertThat(activeLength(metallica, 2024)).isEqualTo(43); // 2024 - 1981
        }

        @Test
        @DisplayName("ActiveBetween: 終了年と開始年の差")
        void activeBetweenLength() {
            assertThat(activeLength(ledZeppelin, 2024)).isEqualTo(12); // 1980 - 1968
        }
    }

    @Nested
    @DisplayName("getEndYear")
    class GetEndYearTest {

        @Test
        @DisplayName("StillActive: None")
        void stillActiveNoEndYear() {
            assertThat(getEndYear(metallica)).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("ActiveBetween: Some(終了年)")
        void activeBetweenEndYear() {
            assertThat(getEndYear(ledZeppelin)).isEqualTo(Option.some(1980));
        }
    }

    @Nested
    @DisplayName("describeActivity")
    class DescribeActivityTest {

        @Test
        @DisplayName("StillActive の説明")
        void describeStillActive() {
            String desc = describeActivity(metallica);
            assertThat(desc).contains("Metallica").contains("since 1981");
        }

        @Test
        @DisplayName("ActiveBetween の説明")
        void describeActiveBetween() {
            String desc = describeActivity(ledZeppelin);
            assertThat(desc).contains("Led Zeppelin").contains("1968").contains("1980");
        }
    }

    @Nested
    @DisplayName("searchArtists")
    class SearchArtistsTest {

        @Test
        @DisplayName("ジャンルで検索")
        void searchByGenre() {
            List<Artist> result = searchArtists(artists,
                    List.of(new SearchByGenre(List.of(MusicGenre.HEAVY_METAL))));

            assertThat(result).hasSize(1);
            assertThat(result.get(0).name()).isEqualTo("Metallica");
        }

        @Test
        @DisplayName("複数ジャンルで検索")
        void searchByMultipleGenres() {
            List<Artist> result = searchArtists(artists,
                    List.of(new SearchByGenre(List.of(MusicGenre.HEAVY_METAL, MusicGenre.HARD_ROCK))));

            assertThat(result).hasSize(3);
        }

        @Test
        @DisplayName("出身地で検索")
        void searchByOrigin() {
            List<Artist> result = searchArtists(artists,
                    List.of(new SearchByOrigin(List.of("England"))));

            assertThat(result).hasSize(3);
            assertThat(result.map(Artist::name)).containsExactlyInAnyOrder(
                    "Led Zeppelin", "The Beatles", "Queen");
        }

        @Test
        @DisplayName("活動期間で検索")
        void searchByActiveYears() {
            List<Artist> result = searchArtists(artists,
                    List.of(new SearchByActiveYears(1965, 1969)));

            assertThat(result).hasSize(2);
            assertThat(result.map(Artist::name)).containsExactlyInAnyOrder(
                    "Led Zeppelin", "The Beatles");
        }

        @Test
        @DisplayName("複数条件の AND 検索")
        void searchMultipleConditions() {
            List<Artist> result = searchArtists(artists, List.of(
                    new SearchByOrigin(List.of("England")),
                    new SearchByActiveYears(1970, 1979)
            ));

            // England 出身で 1970年代に活動
            assertThat(result).hasSize(3);
        }

        @Test
        @DisplayName("条件を満たすアーティストがない場合")
        void noMatch() {
            List<Artist> result = searchArtists(artists,
                    List.of(new SearchByGenre(List.of(MusicGenre.JAZZ))));

            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("searchArtistsAny")
    class SearchArtistsAnyTest {

        @Test
        @DisplayName("いずれかの条件を満たすアーティストを検索")
        void searchAny() {
            List<Artist> result = searchArtistsAny(artists, List.of(
                    new SearchByGenre(List.of(MusicGenre.POP)),
                    new SearchByOrigin(List.of("U.S."))
            ));

            assertThat(result).hasSize(2);
            assertThat(result.map(Artist::name)).containsExactlyInAnyOrder(
                    "Metallica", "The Beatles");
        }
    }
}
