package ch12;

import ch08.IO;
import ch11.DataAccess;
import io.vavr.collection.List;

import static ch11.TravelGuide.*;

/**
 * 第12章: テスト用 DataAccess スタブ
 *
 * テスト時に使用するスタブ実装です。
 * IO.pure で即座に結果を返すため、高速にテストできます。
 */
public final class TestDataAccess implements DataAccess {

    private final IO<List<Attraction>> attractions;
    private final IO<List<Artist>> artists;
    private final IO<List<Movie>> movies;

    private TestDataAccess(
            IO<List<Attraction>> attractions,
            IO<List<Artist>> artists,
            IO<List<Movie>> movies) {
        this.attractions = attractions;
        this.artists = artists;
        this.movies = movies;
    }

    // ============================================
    // ビルダー
    // ============================================

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder {
        private IO<List<Attraction>> attractions = IO.pure(List.empty());
        private IO<List<Artist>> artists = IO.pure(List.empty());
        private IO<List<Movie>> movies = IO.pure(List.empty());

        public Builder withAttractions(List<Attraction> attractions) {
            this.attractions = IO.pure(attractions);
            return this;
        }

        public Builder withAttractions(Attraction... attractions) {
            this.attractions = IO.pure(List.of(attractions));
            return this;
        }

        public Builder withAttractionsIO(IO<List<Attraction>> attractions) {
            this.attractions = attractions;
            return this;
        }

        public Builder withArtists(List<Artist> artists) {
            this.artists = IO.pure(artists);
            return this;
        }

        public Builder withArtists(Artist... artists) {
            this.artists = IO.pure(List.of(artists));
            return this;
        }

        public Builder withArtistsIO(IO<List<Artist>> artists) {
            this.artists = artists;
            return this;
        }

        public Builder withMovies(List<Movie> movies) {
            this.movies = IO.pure(movies);
            return this;
        }

        public Builder withMovies(Movie... movies) {
            this.movies = IO.pure(List.of(movies));
            return this;
        }

        public Builder withMoviesIO(IO<List<Movie>> movies) {
            this.movies = movies;
            return this;
        }

        public Builder failOnAttractions(String errorMessage) {
            this.attractions = IO.delay(() -> {
                throw new RuntimeException(errorMessage);
            });
            return this;
        }

        public Builder failOnArtists(String errorMessage) {
            this.artists = IO.delay(() -> {
                throw new RuntimeException(errorMessage);
            });
            return this;
        }

        public Builder failOnMovies(String errorMessage) {
            this.movies = IO.delay(() -> {
                throw new RuntimeException(errorMessage);
            });
            return this;
        }

        public TestDataAccess build() {
            return new TestDataAccess(attractions, artists, movies);
        }
    }

    // ============================================
    // 便利なファクトリメソッド
    // ============================================

    /**
     * 空の DataAccess を作成
     */
    public static DataAccess empty() {
        return builder().build();
    }

    /**
     * アトラクションのみを返す DataAccess を作成
     */
    public static DataAccess withAttractions(Attraction... attractions) {
        return builder().withAttractions(attractions).build();
    }

    /**
     * すべてのデータを指定して DataAccess を作成
     */
    public static DataAccess of(
            List<Attraction> attractions,
            List<Artist> artists,
            List<Movie> movies) {
        return builder()
                .withAttractions(attractions)
                .withArtists(artists)
                .withMovies(movies)
                .build();
    }

    // ============================================
    // DataAccess 実装
    // ============================================

    @Override
    public IO<List<Attraction>> findAttractions(
            String name,
            AttractionOrdering ordering,
            int limit) {
        return attractions.map(list -> list.take(limit));
    }

    @Override
    public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
        return artists.map(list -> list.take(limit));
    }

    @Override
    public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
        return movies.map(list -> list.take(limit));
    }
}
