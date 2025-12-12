"""第5章: flatMap とネストしたリスト操作のテスト"""

from itertools import islice

from grokking_fp.ch05_flatmap import (
    Book,
    Movie,
    Point,
    Point3d,
    book_adaptations,
    chain_lists,
    duplicate_each,
    filter_even_via_flatmap,
    flat_map,
    flatten,
    generate_points,
    generate_points_3d,
    generate_points_lazy,
    get_all_authors,
    get_all_movies,
    get_recommended_authors,
    group_by_first_char,
    infinite_sequence,
    product_pairs,
    recommendation_feed,
    recommended_books,
    take,
    with_incremented,
)


class TestFlatten:
    """flatten 関数のテスト"""

    def test_numbers(self) -> None:
        assert flatten([[1, 2], [3, 4], [5]]) == [1, 2, 3, 4, 5]

    def test_strings(self) -> None:
        assert flatten([["a", "b"], [], ["c"]]) == ["a", "b", "c"]

    def test_empty(self) -> None:
        assert flatten([]) == []


class TestFlatMap:
    """flat_map 関数のテスト"""

    def test_duplicate_each(self) -> None:
        assert flat_map(lambda x: [x, x * 2], [1, 2, 3]) == [1, 2, 2, 4, 3, 6]

    def test_split_string(self) -> None:
        assert flat_map(lambda s: list(s), ["ab", "cd"]) == ["a", "b", "c", "d"]


class TestBook:
    """Book のテスト"""

    def test_creation(self) -> None:
        book = Book("FP in Scala", ["Chiusano", "Bjarnason"])
        assert book.title == "FP in Scala"
        assert book.authors == ["Chiusano", "Bjarnason"]


class TestMovie:
    """Movie のテスト"""

    def test_creation(self) -> None:
        movie = Movie("An Unexpected Journey")
        assert movie.title == "An Unexpected Journey"


class TestBookAdaptations:
    """book_adaptations 関数のテスト"""

    def test_tolkien(self) -> None:
        result = book_adaptations("Tolkien")
        assert result == [
            Movie("An Unexpected Journey"),
            Movie("The Desolation of Smaug"),
        ]

    def test_other_author(self) -> None:
        assert book_adaptations("Chiusano") == []


class TestGetAllAuthors:
    """get_all_authors 関数のテスト"""

    def test_basic(self) -> None:
        books = [
            Book("FP in Scala", ["Chiusano", "Bjarnason"]),
            Book("The Hobbit", ["Tolkien"]),
        ]
        assert get_all_authors(books) == ["Chiusano", "Bjarnason", "Tolkien"]


class TestGetAllMovies:
    """get_all_movies 関数のテスト"""

    def test_basic(self) -> None:
        result = get_all_movies(["Chiusano", "Tolkien"])
        assert result == [
            Movie("An Unexpected Journey"),
            Movie("The Desolation of Smaug"),
        ]


class TestRecommendationFeed:
    """recommendation_feed 関数のテスト"""

    def test_basic(self) -> None:
        books = [
            Book("FP in Scala", ["Chiusano", "Bjarnason"]),
            Book("The Hobbit", ["Tolkien"]),
        ]
        result = recommendation_feed(books)
        assert result == [
            "You may like An Unexpected Journey, because you liked Tolkien's The Hobbit",
            "You may like The Desolation of Smaug, because you liked Tolkien's The Hobbit",
        ]


class TestRecommendedBooks:
    """recommended_books 関数のテスト"""

    def test_alice(self) -> None:
        result = recommended_books("Alice")
        assert len(result) == 2
        assert result[0].title == "FP in Scala"

    def test_bob(self) -> None:
        result = recommended_books("Bob")
        assert len(result) == 2
        assert result[0].title == "Harry Potter"

    def test_unknown(self) -> None:
        assert recommended_books("Charlie") == []


class TestGetRecommendedAuthors:
    """get_recommended_authors 関数のテスト"""

    def test_basic(self) -> None:
        result = get_recommended_authors(["Alice", "Bob", "Charlie"])
        assert result == ["Chiusano", "Bjarnason", "Sfregola", "Rowling", "Tolkien"]


class TestPoint:
    """Point のテスト"""

    def test_creation(self) -> None:
        p = Point(1, 2)
        assert p.x == 1
        assert p.y == 2


class TestPoint3d:
    """Point3d のテスト"""

    def test_creation(self) -> None:
        p = Point3d(1, 2, 3)
        assert p.x == 1
        assert p.y == 2
        assert p.z == 3


class TestGeneratePoints:
    """generate_points 関数のテスト"""

    def test_single_x(self) -> None:
        result = generate_points([1], [-2, 7])
        assert result == [Point(1, -2), Point(1, 7)]

    def test_multiple_x_y(self) -> None:
        result = generate_points([1, 2], [-2, 7])
        assert result == [
            Point(1, -2),
            Point(1, 7),
            Point(2, -2),
            Point(2, 7),
        ]

    def test_empty_x(self) -> None:
        assert generate_points([], [-2, 7]) == []


class TestGeneratePoints3d:
    """generate_points_3d 関数のテスト"""

    def test_basic(self) -> None:
        result = generate_points_3d([1], [-2, 7], [3, 4])
        assert result == [
            Point3d(1, -2, 3),
            Point3d(1, -2, 4),
            Point3d(1, 7, 3),
            Point3d(1, 7, 4),
        ]


class TestDuplicateEach:
    """duplicate_each 関数のテスト"""

    def test_basic(self) -> None:
        assert duplicate_each([1, 2, 3]) == [1, 1, 2, 2, 3, 3]


class TestWithIncremented:
    """with_incremented 関数のテスト"""

    def test_basic(self) -> None:
        assert with_incremented([1, 2, 3]) == [1, 11, 2, 12, 3, 13]


class TestFilterEvenViaFlatmap:
    """filter_even_via_flatmap 関数のテスト"""

    def test_basic(self) -> None:
        assert filter_even_via_flatmap([1, 2, 3, 4, 5]) == [2, 4]


class TestGeneratePointsLazy:
    """generate_points_lazy 関数のテスト"""

    def test_basic(self) -> None:
        result = list(generate_points_lazy([1], [-2, 7]))
        assert result == [Point(1, -2), Point(1, 7)]


class TestInfiniteSequence:
    """infinite_sequence 関数のテスト"""

    def test_take_from_start(self) -> None:
        result = list(islice(infinite_sequence(5), 3))
        assert result == [5, 6, 7]


class TestTake:
    """take 関数のテスト"""

    def test_basic(self) -> None:
        assert take(3, iter([1, 2, 3, 4, 5])) == [1, 2, 3]


class TestProductPairs:
    """product_pairs 関数のテスト"""

    def test_basic(self) -> None:
        assert product_pairs([1, 2], [3, 4]) == [(1, 3), (1, 4), (2, 3), (2, 4)]


class TestChainLists:
    """chain_lists 関数のテスト"""

    def test_basic(self) -> None:
        assert chain_lists([[1, 2], [3], [4, 5, 6]]) == [1, 2, 3, 4, 5, 6]


class TestGroupByFirstChar:
    """group_by_first_char 関数のテスト"""

    def test_basic(self) -> None:
        result = group_by_first_char(["apple", "apricot", "banana", "blueberry"])
        assert result == {"a": ["apple", "apricot"], "b": ["banana", "blueberry"]}
