package ch05

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class BookAdaptationsTest : FunSpec({
    val books = listOf(
        Book("FP in Scala", listOf("Chiusano", "Bjarnason")),
        Book("The Hobbit", listOf("Tolkien")),
    )
    val expectedRecommendations = listOf(
        "You may like An Unexpected Journey, because you liked Tolkien's The Hobbit",
        "You may like The Desolation of Smaug, because you liked Tolkien's The Hobbit",
    )

    test("bookAdaptations は Tolkien の映画化作品を返す") {
        bookAdaptations("Tolkien") shouldBe listOf(
            Movie("An Unexpected Journey"),
            Movie("The Desolation of Smaug"),
        )
    }

    test("bookAdaptations は映画化作品がない著者には空リストを返す") {
        bookAdaptations("Chiusano") shouldBe emptyList()
    }

    test("map だけだとリストがネストする") {
        books.map(Book::authors) shouldBe listOf(listOf("Chiusano", "Bjarnason"), listOf("Tolkien"))
    }

    test("allAuthors は flatMap で全著者を平坦に返す") {
        allAuthors(books) shouldBe listOf("Chiusano", "Bjarnason", "Tolkien")
    }

    test("recommendations はネストした flatMap でおすすめ文を生成する") {
        recommendations(books) shouldBe expectedRecommendations
    }

    test("recommendationsWithSequence は sequence ビルダーで同じ結果を返す") {
        recommendationsWithSequence(books) shouldBe expectedRecommendations
    }

    test("sequence ビルダー版とネストした flatMap 版は同じ結果になる") {
        recommendationsWithSequence(books) shouldBe recommendations(books)
    }

    test("titlesForAuthor は著者の書籍タイトルを返す") {
        titlesForAuthor(books, "Tolkien") shouldBe listOf("The Hobbit")
    }

    test("authorsWithMovies は映画化作品のある著者のみを返す") {
        authorsWithMovies(books) shouldBe listOf("Tolkien")
    }
})
