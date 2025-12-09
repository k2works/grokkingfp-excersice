package ch05;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第5章: 書籍と映画化のテスト
 */
@DisplayName("第5章: 書籍と映画化")
class BookAdaptationsTest {

    private final List<BookAdaptations.Book> books = List.of(
            new BookAdaptations.Book("FP in Scala", List.of("Chiusano", "Bjarnason")),
            new BookAdaptations.Book("The Hobbit", List.of("Tolkien")),
            new BookAdaptations.Book("Modern Java in Action", List.of("Urma", "Fusco", "Mycroft"))
    );

    @Nested
    @DisplayName("bookAdaptations")
    class BookAdaptationsMethodTest {

        @Test
        @DisplayName("Tolkien は映画化作品を持つ")
        void tolkienHasMovies() {
            List<BookAdaptations.Movie> movies = BookAdaptations.bookAdaptations("Tolkien");
            assertThat(movies).hasSize(2);
            assertThat(movies.map(BookAdaptations.Movie::title))
                    .containsExactly("An Unexpected Journey", "The Desolation of Smaug");
        }

        @Test
        @DisplayName("他の著者は映画化作品を持たない")
        void otherAuthorsNoMovies() {
            assertThat(BookAdaptations.bookAdaptations("Chiusano")).isEmpty();
            assertThat(BookAdaptations.bookAdaptations("Unknown")).isEmpty();
        }
    }

    @Nested
    @DisplayName("allAuthors")
    class AllAuthorsTest {

        @Test
        @DisplayName("全著者のリストを取得")
        void getAllAuthors() {
            List<String> authors = BookAdaptations.allAuthors(books);
            assertThat(authors).containsExactly(
                    "Chiusano", "Bjarnason", "Tolkien", "Urma", "Fusco", "Mycroft"
            );
        }

        @Test
        @DisplayName("空の書籍リストは空の著者リスト")
        void emptyBooksEmptyAuthors() {
            assertThat(BookAdaptations.allAuthors(List.empty())).isEmpty();
        }
    }

    @Nested
    @DisplayName("recommendations")
    class RecommendationsTest {

        @Test
        @DisplayName("おすすめ文を生成")
        void generateRecommendations() {
            List<String> recs = BookAdaptations.recommendations(books);
            assertThat(recs).hasSize(2);
            assertThat(recs.get(0)).contains("An Unexpected Journey");
            assertThat(recs.get(0)).contains("Tolkien");
            assertThat(recs.get(0)).contains("The Hobbit");
        }

        @Test
        @DisplayName("映画化がない場合は空リスト")
        void noMoviesNoRecommendations() {
            List<BookAdaptations.Book> noMovieBooks = List.of(
                    new BookAdaptations.Book("FP in Scala", List.of("Chiusano"))
            );
            assertThat(BookAdaptations.recommendations(noMovieBooks)).isEmpty();
        }
    }

    @Nested
    @DisplayName("recommendationsWithFor")
    class RecommendationsWithForTest {

        @Test
        @DisplayName("For版も同じ結果を返す")
        void forVersionSameResult() {
            List<String> recs1 = BookAdaptations.recommendations(books);
            List<String> recs2 = BookAdaptations.recommendationsWithFor(books);
            assertThat(recs2).isEqualTo(recs1);
        }
    }

    @Nested
    @DisplayName("titlesForAuthor")
    class TitlesForAuthorTest {

        @Test
        @DisplayName("著者の書籍タイトルを取得")
        void getTitlesForAuthor() {
            List<String> titles = BookAdaptations.titlesForAuthor(books, "Tolkien");
            assertThat(titles).containsExactly("The Hobbit");
        }

        @Test
        @DisplayName("複数の著者がいる書籍")
        void multipleAuthors() {
            List<String> titles = BookAdaptations.titlesForAuthor(books, "Chiusano");
            assertThat(titles).containsExactly("FP in Scala");
        }

        @Test
        @DisplayName("存在しない著者は空リスト")
        void unknownAuthor() {
            assertThat(BookAdaptations.titlesForAuthor(books, "Unknown")).isEmpty();
        }
    }

    @Nested
    @DisplayName("authorsWithMovies")
    class AuthorsWithMoviesTest {

        @Test
        @DisplayName("映画化された著者を取得")
        void getAuthorsWithMovies() {
            List<String> authors = BookAdaptations.authorsWithMovies(books);
            assertThat(authors).containsExactly("Tolkien");
        }
    }
}
