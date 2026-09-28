package ch05

/**
 * 第5章: 書籍と映画化
 *
 * ネストした flatMap の実践例
 */

data class Book(val title: String, val authors: List<String>)

data class Movie(val title: String)

/** 著者に基づいて映画化作品を取得 */
fun bookAdaptations(author: String): List<Movie> =
    when (author) {
        "Tolkien" -> listOf(Movie("An Unexpected Journey"), Movie("The Desolation of Smaug"))
        else -> emptyList()
    }

/** 全著者のリストを取得 */
fun allAuthors(books: List<Book>): List<String> = books.flatMap(Book::authors)

/** おすすめ文を生成（ネストした flatMap） */
fun recommendations(books: List<Book>): List<String> =
    books.flatMap { book ->
        book.authors.flatMap { author ->
            bookAdaptations(author).map { movie ->
                "You may like ${movie.title}, because you liked $author's ${book.title}"
            }
        }
    }

/** おすすめ文を生成（sequence ビルダー版） */
fun recommendationsWithSequence(books: List<Book>): List<String> =
    sequence {
        for (book in books)
            for (author in book.authors)
                for (movie in bookAdaptations(author))
                    yield("You may like ${movie.title}, because you liked $author's ${book.title}")
    }.toList()

/** 指定した著者の書籍タイトルを取得 */
fun titlesForAuthor(books: List<Book>, author: String): List<String> =
    books.filter { author in it.authors }.map(Book::title)

/** 映画化作品のある著者のみを取得 */
fun authorsWithMovies(books: List<Book>): List<String> =
    allAuthors(books)
        .filter { bookAdaptations(it).isNotEmpty() }
        .distinct()
