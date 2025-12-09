package ch07;

import io.vavr.collection.List;
import io.vavr.control.Option;

/**
 * 第7章: 代数的データ型（ADT）を使った音楽アーティストのモデリング
 *
 * sealed interface と record を使って ADT を表現します。
 */
public class MusicArtist {

    // ============================================
    // 直和型（Sum Type）- sealed interface
    // ============================================

    /**
     * 音楽ジャンル
     */
    public enum MusicGenre {
        HEAVY_METAL,
        POP,
        HARD_ROCK,
        JAZZ,
        CLASSICAL
    }

    /**
     * 活動期間を表す直和型
     */
    public sealed interface YearsActive permits StillActive, ActiveBetween {}

    /**
     * まだ活動中
     */
    public record StillActive(int since) implements YearsActive {}

    /**
     * 特定期間のみ活動
     */
    public record ActiveBetween(int start, int end) implements YearsActive {}

    // ============================================
    // 直積型（Product Type）- record
    // ============================================

    /**
     * アーティスト
     */
    public record Artist(
            String name,
            MusicGenre genre,
            String origin,
            YearsActive yearsActive
    ) {}

    // ============================================
    // パターンマッチング
    // ============================================

    /**
     * アーティストが指定期間に活動していたか判定
     */
    public static boolean wasArtistActive(Artist artist, int yearStart, int yearEnd) {
        return switch (artist.yearsActive()) {
            case StillActive(int since) -> since <= yearEnd;
            case ActiveBetween(int start, int end) -> start <= yearEnd && end >= yearStart;
        };
    }

    /**
     * 活動期間の長さを計算
     */
    public static int activeLength(Artist artist, int currentYear) {
        return switch (artist.yearsActive()) {
            case StillActive(int since) -> currentYear - since;
            case ActiveBetween(int start, int end) -> end - start;
        };
    }

    /**
     * 活動終了年を取得（活動中なら None）
     */
    public static Option<Integer> getEndYear(Artist artist) {
        return switch (artist.yearsActive()) {
            case StillActive sa -> Option.none();
            case ActiveBetween(int start, int end) -> Option.some(end);
        };
    }

    /**
     * アーティストの活動状態を文字列で表現
     */
    public static String describeActivity(Artist artist) {
        return switch (artist.yearsActive()) {
            case StillActive(int since) ->
                    String.format("%s has been active since %d", artist.name(), since);
            case ActiveBetween(int start, int end) ->
                    String.format("%s was active from %d to %d", artist.name(), start, end);
        };
    }

    // ============================================
    // 検索条件（直和型）
    // ============================================

    /**
     * 検索条件を表す直和型
     */
    public sealed interface SearchCondition permits SearchByGenre, SearchByOrigin, SearchByActiveYears {}

    public record SearchByGenre(List<MusicGenre> genres) implements SearchCondition {}
    public record SearchByOrigin(List<String> locations) implements SearchCondition {}
    public record SearchByActiveYears(int start, int end) implements SearchCondition {}

    /**
     * 検索条件に一致するか判定
     */
    public static boolean matchesCondition(Artist artist, SearchCondition condition) {
        return switch (condition) {
            case SearchByGenre(List<MusicGenre> genres) ->
                    genres.contains(artist.genre());
            case SearchByOrigin(List<String> locations) ->
                    locations.contains(artist.origin());
            case SearchByActiveYears(int start, int end) ->
                    wasArtistActive(artist, start, end);
        };
    }

    /**
     * 全ての条件を満たすアーティストを検索
     */
    public static List<Artist> searchArtists(
            List<Artist> artists,
            List<SearchCondition> requiredConditions) {
        return artists.filter(artist ->
                requiredConditions.forAll(condition -> matchesCondition(artist, condition))
        );
    }

    /**
     * いずれかの条件を満たすアーティストを検索
     */
    public static List<Artist> searchArtistsAny(
            List<Artist> artists,
            List<SearchCondition> conditions) {
        return artists.filter(artist ->
                conditions.exists(condition -> matchesCondition(artist, condition))
        );
    }

    public static void main(String[] args) {
        System.out.println("=== 音楽アーティスト検索 ===\n");

        // サンプルデータ
        List<Artist> artists = List.of(
                new Artist("Metallica", MusicGenre.HEAVY_METAL, "U.S.", new StillActive(1981)),
                new Artist("Led Zeppelin", MusicGenre.HARD_ROCK, "England", new ActiveBetween(1968, 1980)),
                new Artist("Black Sabbath", MusicGenre.HEAVY_METAL, "England", new ActiveBetween(1968, 2017)),
                new Artist("The Beatles", MusicGenre.POP, "England", new ActiveBetween(1960, 1970)),
                new Artist("Queen", MusicGenre.HARD_ROCK, "England", new StillActive(1970))
        );

        System.out.println("--- アーティスト一覧 ---");
        artists.forEach(artist -> System.out.println(describeActivity(artist)));

        // 活動期間
        System.out.println("\n--- 活動期間の長さ（2024年時点）---");
        artists.forEach(artist ->
                System.out.println(artist.name() + ": " + activeLength(artist, 2024) + " years")
        );

        // 検索: ヘビーメタルのアーティスト
        System.out.println("\n--- 検索: Heavy Metal ---");
        List<Artist> heavyMetal = searchArtists(artists,
                List.of(new SearchByGenre(List.of(MusicGenre.HEAVY_METAL))));
        heavyMetal.forEach(a -> System.out.println(a.name()));

        // 検索: イギリス出身で1970年代に活動
        System.out.println("\n--- 検索: England + 1970年代に活動 ---");
        List<Artist> england70s = searchArtists(artists, List.of(
                new SearchByOrigin(List.of("England")),
                new SearchByActiveYears(1970, 1979)
        ));
        england70s.forEach(a -> System.out.println(a.name()));

        // 検索: Heavy Metal または Hard Rock
        System.out.println("\n--- 検索: Heavy Metal または Hard Rock ---");
        List<Artist> rock = searchArtists(artists,
                List.of(new SearchByGenre(List.of(MusicGenre.HEAVY_METAL, MusicGenre.HARD_ROCK))));
        rock.forEach(a -> System.out.println(a.name()));
    }
}
