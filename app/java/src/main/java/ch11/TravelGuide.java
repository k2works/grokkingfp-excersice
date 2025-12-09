package ch11;

import ch08.IO;
import io.vavr.collection.List;
import io.vavr.control.Either;
import io.vavr.control.Option;

import java.util.Objects;

/**
 * 第11章: TravelGuide アプリケーション
 *
 * 旅行ガイドアプリケーションのドメインモデルとビジネスロジックです。
 */
public final class TravelGuide {

    private TravelGuide() {}

    // ============================================
    // ドメインモデル
    // ============================================

    /**
     * ロケーション ID（値オブジェクト）
     */
    public record LocationId(String value) {
        public LocationId {
            Objects.requireNonNull(value, "LocationId value cannot be null");
        }
    }

    /**
     * ロケーション（場所）
     */
    public record Location(LocationId id, String name, int population) {
        public Location {
            Objects.requireNonNull(id, "Location id cannot be null");
            Objects.requireNonNull(name, "Location name cannot be null");
        }
    }

    /**
     * アトラクション（観光地）
     */
    public record Attraction(
            String name,
            Option<String> description,
            Location location
    ) {
        public Attraction {
            Objects.requireNonNull(name, "Attraction name cannot be null");
            Objects.requireNonNull(description, "Attraction description cannot be null");
            Objects.requireNonNull(location, "Attraction location cannot be null");
        }
    }

    /**
     * ポップカルチャーの題材
     */
    public sealed interface PopCultureSubject permits Artist, Movie {
        String name();
    }

    /**
     * アーティスト
     */
    public record Artist(String name, int followers) implements PopCultureSubject {
        public Artist {
            Objects.requireNonNull(name, "Artist name cannot be null");
        }
    }

    /**
     * 映画
     */
    public record Movie(String name, int boxOffice) implements PopCultureSubject {
        public Movie {
            Objects.requireNonNull(name, "Movie name cannot be null");
        }
    }

    /**
     * 旅行ガイド
     */
    public record Guide(
            Attraction attraction,
            List<PopCultureSubject> subjects
    ) {
        public Guide {
            Objects.requireNonNull(attraction, "Guide attraction cannot be null");
            Objects.requireNonNull(subjects, "Guide subjects cannot be null");
        }
    }

    /**
     * 検索レポート（エラーや不十分な結果を報告）
     */
    public record SearchReport(
            List<Guide> badGuides,
            List<String> problems
    ) {
        public SearchReport {
            Objects.requireNonNull(badGuides, "SearchReport badGuides cannot be null");
            Objects.requireNonNull(problems, "SearchReport problems cannot be null");
        }

        public static SearchReport empty() {
            return new SearchReport(List.empty(), List.empty());
        }

        public static SearchReport withProblems(List<String> problems) {
            return new SearchReport(List.empty(), problems);
        }

        public static SearchReport withBadGuides(List<Guide> badGuides) {
            return new SearchReport(badGuides, List.empty());
        }
    }

    // ============================================
    // アトラクションの並び順
    // ============================================

    public enum AttractionOrdering {
        BY_NAME,
        BY_LOCATION_POPULATION
    }

    // ============================================
    // ビジネスロジック（純粋関数）
    // ============================================

    /**
     * ガイドのスコアを計算
     *
     * 要件:
     * - 30 点: 説明がある場合
     * - 10 点/件: アーティストまたは映画（最大 40 点）
     * - 1 点/100,000 フォロワー: 全アーティスト合計（最大 15 点）
     * - 1 点/10,000,000 ドル: 全映画の興行収入合計（最大 15 点）
     */
    public static int guideScore(Guide guide) {
        int descriptionScore = guide.attraction().description().isDefined() ? 30 : 0;
        int quantityScore = Math.min(40, guide.subjects().size() * 10);

        // Long を使用してオーバーフローを防ぐ
        long totalFollowers = guide.subjects()
                .filter(s -> s instanceof Artist)
                .map(s -> (long) ((Artist) s).followers())
                .sum().longValue();

        long totalBoxOffice = guide.subjects()
                .filter(s -> s instanceof Movie)
                .map(s -> (long) ((Movie) s).boxOffice())
                .sum().longValue();

        int followersScore = (int) Math.min(15, totalFollowers / 100_000);
        int boxOfficeScore = (int) Math.min(15, totalBoxOffice / 10_000_000);

        return descriptionScore + quantityScore + followersScore + boxOfficeScore;
    }

    /**
     * ガイドのリストから最良のガイドを選択
     */
    public static Option<Guide> findBestGuide(List<Guide> guides) {
        return guides
                .sortBy(g -> -guideScore(g))
                .headOption();
    }

    /**
     * 良いガイドを見つける（スコアが閾値を超える場合）
     */
    public static Either<SearchReport, Guide> findGoodGuide(List<Guide> guides, int minScore) {
        return findBestGuide(guides)
                .filter(g -> guideScore(g) > minScore)
                .<Either<SearchReport, Guide>>map(Either::right)
                .getOrElse(() -> Either.left(SearchReport.withBadGuides(guides)));
    }

    // ============================================
    // バージョン 1: 基本的な実装
    // ============================================

    /**
     * 旅行ガイドを取得（バージョン 1: 最初の結果のみ）
     */
    public static IO<Option<Guide>> travelGuideV1(
            DataAccess dataAccess,
            String attractionName) {

        return dataAccess.findAttractions(attractionName, AttractionOrdering.BY_LOCATION_POPULATION, 1)
                .flatMap(attractions -> attractions.headOption()
                        .map(attraction ->
                                dataAccess.findArtistsFromLocation(attraction.location().id(), 2)
                                        .flatMap(artists ->
                                                dataAccess.findMoviesAboutLocation(attraction.location().id(), 2)
                                                        .map(movies -> {
                                                            List<PopCultureSubject> subjects = List.<PopCultureSubject>empty()
                                                                    .appendAll(artists)
                                                                    .appendAll(movies);
                                                            return Option.some(new Guide(attraction, subjects));
                                                        })
                                        )
                        )
                        .getOrElse(IO.pure(Option.none()))
                );
    }

    // ============================================
    // バージョン 2: 複数候補からベストを選択
    // ============================================

    /**
     * 旅行ガイドを取得（バージョン 2: 複数候補からベストを選択）
     */
    public static IO<Option<Guide>> travelGuideV2(
            DataAccess dataAccess,
            String attractionName) {

        return dataAccess.findAttractions(attractionName, AttractionOrdering.BY_LOCATION_POPULATION, 3)
                .flatMap(attractions ->
                        IO.traverse(attractions, attraction ->
                                dataAccess.findArtistsFromLocation(attraction.location().id(), 2)
                                        .flatMap(artists ->
                                                dataAccess.findMoviesAboutLocation(attraction.location().id(), 2)
                                                        .map(movies -> {
                                                            List<PopCultureSubject> subjects = List.<PopCultureSubject>empty()
                                                                    .appendAll(artists)
                                                                    .appendAll(movies);
                                                            return new Guide(attraction, subjects);
                                                        })
                                        )
                        ).map(TravelGuide::findBestGuide)
                );
    }

    // ============================================
    // バージョン 3: 並列実行
    // ============================================

    /**
     * アトラクションからガイドを作成（並列実行）
     */
    public static IO<Guide> guideForAttraction(
            DataAccess dataAccess,
            Attraction attraction) {

        return dataAccess.findArtistsFromLocation(attraction.location().id(), 2)
                .flatMap(artists ->
                        dataAccess.findMoviesAboutLocation(attraction.location().id(), 2)
                                .map(movies -> {
                                    List<PopCultureSubject> subjects = List.<PopCultureSubject>empty()
                                            .appendAll(artists)
                                            .appendAll(movies);
                                    return new Guide(attraction, subjects);
                                })
                );
    }

    /**
     * 旅行ガイドを取得（バージョン 3: 並列実行、エラーハンドリング付き）
     */
    public static IO<Either<SearchReport, Guide>> travelGuideV3(
            DataAccess dataAccess,
            String attractionName) {

        return dataAccess.findAttractions(attractionName, AttractionOrdering.BY_LOCATION_POPULATION, 3)
                .attempt()
                .flatMap(attractionsResult ->
                        attractionsResult.fold(
                                error -> IO.pure(Either.left(
                                        SearchReport.withProblems(List.of(error.getMessage())))),
                                attractions ->
                                        IO.traverse(attractions, attraction ->
                                                        guideForAttraction(dataAccess, attraction).attempt()
                                                )
                                                .map(TravelGuide::processGuideResults)
                        )
                );
    }

    /**
     * ガイド取得結果を処理（エラーと成功を分離）
     */
    private static Either<SearchReport, Guide> processGuideResults(
            List<Either<Throwable, Guide>> results) {

        List<Guide> guides = results
                .filter(Either::isRight)
                .map(Either::get);

        List<String> errors = results
                .filter(Either::isLeft)
                .map(e -> e.getLeft().getMessage());

        return findBestGuide(guides)
                .filter(g -> guideScore(g) > 55)
                .<Either<SearchReport, Guide>>map(Either::right)
                .getOrElse(() -> Either.left(new SearchReport(guides, errors)));
    }
}
