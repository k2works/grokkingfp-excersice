package ch05;

import io.vavr.collection.List;

/**
 * 第5章: 書籍と映画化
 *
 * ネストした flatMap の実践例
 */
public class BookAdaptations {

    /**
     * 書籍を表す record
     */
    public record Book(String title, List<String> authors) {}

    /**
     * 映画を表す record
     */
    public record Movie(String title) {}

    /**
     * 著者に基づいて映画化作品を取得
     */
    public static List<Movie> bookAdaptations(String author) {
        if ("Tolkien".equals(author)) {
            return List.of(
                    new Movie("An Unexpected Journey"),
                    new Movie("The Desolation of Smaug")
            );
        }
        return List.empty();
    }

    /**
     * 全著者のリストを取得（flatMap を使用）
     */
    public static List<String> allAuthors(List<Book> books) {
        return books.flatMap(Book::authors);
    }

    /**
     * おすすめ文を生成（ネストした flatMap）
     */
    public static List<String> recommendations(List<Book> books) {
        return books.flatMap(book ->
                book.authors().flatMap(author ->
                        bookAdaptations(author).map(movie ->
                                String.format("You may like %s, because you liked %s's %s",
                                        movie.title(), author, book.title())
                        )
                )
        );
    }

    /**
     * Vavr の For を使った同等の実装
     */
    public static List<String> recommendationsWithFor(List<Book> books) {
        return io.vavr.collection.Iterator.ofAll(books)
                .flatMap(book -> io.vavr.collection.Iterator.ofAll(book.authors())
                        .flatMap(author -> io.vavr.collection.Iterator.ofAll(bookAdaptations(author))
                                .map(movie -> String.format(
                                        "You may like %s, because you liked %s's %s",
                                        movie.title(), author, book.title()))))
                .toList();
    }

    /**
     * 著者ごとの書籍タイトルを取得
     */
    public static List<String> titlesForAuthor(List<Book> books, String targetAuthor) {
        return books.filter(book -> book.authors().contains(targetAuthor))
                .map(Book::title);
    }

    /**
     * 映画化された著者のみを取得
     */
    public static List<String> authorsWithMovies(List<Book> books) {
        return books.flatMap(Book::authors)
                .filter(author -> !bookAdaptations(author).isEmpty())
                .distinct();
    }

    public static void main(String[] args) {
        System.out.println("=== 書籍と映画化 ===\n");

        List<Book> books = List.of(
                new Book("FP in Scala", List.of("Chiusano", "Bjarnason")),
                new Book("The Hobbit", List.of("Tolkien")),
                new Book("Modern Java in Action", List.of("Urma", "Fusco", "Mycroft"))
        );

        System.out.println("--- 書籍リスト ---");
        books.forEach(book -> System.out.println(book.title() + " by " + book.authors()));

        // 全著者
        System.out.println("\n--- 全著者 ---");
        System.out.println(allAuthors(books));

        // おすすめ
        System.out.println("\n--- おすすめ ---");
        recommendations(books).forEach(System.out::println);

        // 著者の書籍
        System.out.println("\n--- Tolkien の書籍 ---");
        System.out.println(titlesForAuthor(books, "Tolkien"));

        // 映画化された著者
        System.out.println("\n--- 映画化された著者 ---");
        System.out.println(authorsWithMovies(books));
    }
}
