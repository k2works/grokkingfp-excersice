"""第5章: flatMap とネストしたリスト操作

flatMap（Python ではリスト内包表記やジェネレータ式で実現）を使って、
ネストしたデータ構造を平坦化しながら変換する方法を学びます。
"""

from collections.abc import Callable, Iterator
from dataclasses import dataclass
from itertools import chain
from typing import TypeVar

T = TypeVar("T")
U = TypeVar("U")


# =============================================================================
# 5.1 基本的な flatten と flatMap
# =============================================================================


def flatten(nested: list[list[T]]) -> list[T]:
    """ネストしたリストを平坦化する。

    Examples:
        >>> flatten([[1, 2], [3, 4], [5]])
        [1, 2, 3, 4, 5]
        >>> flatten([["a", "b"], [], ["c"]])
        ['a', 'b', 'c']
    """
    return [item for sublist in nested for item in sublist]


def flat_map(func: Callable[[T], list[U]], lst: list[T]) -> list[U]:
    """map してから flatten する。

    各要素に関数を適用し、結果のリストを平坦化する。

    Examples:
        >>> flat_map(lambda x: [x, x * 2], [1, 2, 3])
        [1, 2, 2, 4, 3, 6]
        >>> flat_map(lambda s: list(s), ["ab", "cd"])
        ['a', 'b', 'c', 'd']
    """
    return [item for x in lst for item in func(x)]


# =============================================================================
# 5.2 Book と Movie の例（推薦システム）
# =============================================================================


@dataclass(frozen=True)
class Book:
    """本を表すイミュータブルなデータクラス。

    Examples:
        >>> book = Book("FP in Scala", ["Chiusano", "Bjarnason"])
        >>> book.title
        'FP in Scala'
        >>> book.authors
        ['Chiusano', 'Bjarnason']
    """

    title: str
    authors: list[str]


@dataclass(frozen=True)
class Movie:
    """映画を表すイミュータブルなデータクラス。

    Examples:
        >>> movie = Movie("An Unexpected Journey")
        >>> movie.title
        'An Unexpected Journey'
    """

    title: str


def book_adaptations(author: str) -> list[Movie]:
    """著者の作品の映画化作品を取得する。

    Examples:
        >>> book_adaptations("Tolkien")
        [Movie(title='An Unexpected Journey'), Movie(title='The Desolation of Smaug')]
        >>> book_adaptations("Chiusano")
        []
    """
    if author == "Tolkien":
        return [Movie("An Unexpected Journey"), Movie("The Desolation of Smaug")]
    return []


def get_all_authors(books: list[Book]) -> list[str]:
    """全ての本の著者を取得する。

    Examples:
        >>> books = [
        ...     Book("FP in Scala", ["Chiusano", "Bjarnason"]),
        ...     Book("The Hobbit", ["Tolkien"])
        ... ]
        >>> get_all_authors(books)
        ['Chiusano', 'Bjarnason', 'Tolkien']
    """
    return [author for book in books for author in book.authors]


def get_all_movies(authors: list[str]) -> list[Movie]:
    """著者リストから全ての映画化作品を取得する。

    Examples:
        >>> get_all_movies(["Chiusano", "Tolkien"])
        [Movie(title='An Unexpected Journey'), Movie(title='The Desolation of Smaug')]
    """
    return [movie for author in authors for movie in book_adaptations(author)]


def recommendation_feed(books: list[Book]) -> list[str]:
    """本のリストから推薦メッセージを生成する。

    Examples:
        >>> books = [
        ...     Book("FP in Scala", ["Chiusano", "Bjarnason"]),
        ...     Book("The Hobbit", ["Tolkien"])
        ... ]
        >>> recommendation_feed(books)  # doctest: +NORMALIZE_WHITESPACE
        ["You may like An Unexpected Journey, because you liked Tolkien's The Hobbit",
         "You may like The Desolation of Smaug, because you liked Tolkien's The Hobbit"]
    """
    return [
        f"You may like {movie.title}, because you liked {author}'s {book.title}"
        for book in books
        for author in book.authors
        for movie in book_adaptations(author)
    ]


# =============================================================================
# 5.3 友達の推薦本
# =============================================================================


def recommended_books(friend: str) -> list[Book]:
    """友達のおすすめ本を取得する。

    Examples:
        >>> recommended_books("Alice")
        [Book(title='FP in Scala', authors=['Chiusano', 'Bjarnason']), Book(title='Get Programming with Scala', authors=['Sfregola'])]
        >>> recommended_books("Charlie")
        []
    """
    scala_books = [
        Book("FP in Scala", ["Chiusano", "Bjarnason"]),
        Book("Get Programming with Scala", ["Sfregola"]),
    ]
    fiction_books = [
        Book("Harry Potter", ["Rowling"]),
        Book("The Lord of the Rings", ["Tolkien"]),
    ]
    if friend == "Alice":
        return scala_books
    if friend == "Bob":
        return fiction_books
    return []


def get_recommended_authors(friends: list[str]) -> list[str]:
    """友達のおすすめ本の著者を全て取得する。

    Examples:
        >>> get_recommended_authors(["Alice", "Bob", "Charlie"])
        ['Chiusano', 'Bjarnason', 'Sfregola', 'Rowling', 'Tolkien']
    """
    return [
        author
        for friend in friends
        for book in recommended_books(friend)
        for author in book.authors
    ]


# =============================================================================
# 5.4 2D/3D ポイントの生成
# =============================================================================


@dataclass(frozen=True)
class Point:
    """2次元の点。

    Examples:
        >>> Point(1, 2)
        Point(x=1, y=2)
    """

    x: int
    y: int


@dataclass(frozen=True)
class Point3d:
    """3次元の点。

    Examples:
        >>> Point3d(1, 2, 3)
        Point3d(x=1, y=2, z=3)
    """

    x: int
    y: int
    z: int


def generate_points(xs: list[int], ys: list[int]) -> list[Point]:
    """全ての (x, y) の組み合わせから Point を生成する。

    Examples:
        >>> generate_points([1], [-2, 7])
        [Point(x=1, y=-2), Point(x=1, y=7)]
        >>> generate_points([1, 2], [-2, 7])
        [Point(x=1, y=-2), Point(x=1, y=7), Point(x=2, y=-2), Point(x=2, y=7)]
    """
    return [Point(x, y) for x in xs for y in ys]


def generate_points_3d(xs: list[int], ys: list[int], zs: list[int]) -> list[Point3d]:
    """全ての (x, y, z) の組み合わせから Point3d を生成する。

    Examples:
        >>> generate_points_3d([1], [-2, 7], [3, 4])
        [Point3d(x=1, y=-2, z=3), Point3d(x=1, y=-2, z=4), Point3d(x=1, y=7, z=3), Point3d(x=1, y=7, z=4)]
    """
    return [Point3d(x, y, z) for x in xs for y in ys for z in zs]


# =============================================================================
# 5.5 flatMap でリストサイズを変更
# =============================================================================


def duplicate_each(numbers: list[int]) -> list[int]:
    """各要素を2つに複製する。

    Examples:
        >>> duplicate_each([1, 2, 3])
        [1, 1, 2, 2, 3, 3]
    """
    return [n for n in numbers for _ in range(2)]


def with_incremented(numbers: list[int]) -> list[int]:
    """各要素とその +10 を含むリストを生成する。

    Examples:
        >>> with_incremented([1, 2, 3])
        [1, 11, 2, 12, 3, 13]
    """
    return [x for n in numbers for x in [n, n + 10]]


def filter_even_via_flatmap(numbers: list[int]) -> list[int]:
    """flatMap パターンで偶数のみをフィルタする。

    通常は filter を使うが、flatMap でもフィルタリングできることを示す。

    Examples:
        >>> filter_even_via_flatmap([1, 2, 3, 4, 5])
        [2, 4]
    """
    return [n for n in numbers if n % 2 == 0]


# =============================================================================
# 5.6 ジェネレータ（遅延評価）
# =============================================================================


def generate_points_lazy(xs: list[int], ys: list[int]) -> Iterator[Point]:
    """ジェネレータを使って遅延評価で Point を生成する。

    Examples:
        >>> list(generate_points_lazy([1], [-2, 7]))
        [Point(x=1, y=-2), Point(x=1, y=7)]
    """
    return (Point(x, y) for x in xs for y in ys)


def infinite_sequence(start: int = 0) -> Iterator[int]:
    """無限シーケンスを生成する。

    Examples:
        >>> from itertools import islice
        >>> list(islice(infinite_sequence(5), 3))
        [5, 6, 7]
    """
    n = start
    while True:
        yield n
        n += 1


def take(n: int, iterable: Iterator[T]) -> list[T]:
    """イテレータから最初の n 個の要素を取得する。

    Examples:
        >>> take(3, iter([1, 2, 3, 4, 5]))
        [1, 2, 3]
    """
    from itertools import islice
    return list(islice(iterable, n))


# =============================================================================
# 5.7 itertools を使った高度な操作
# =============================================================================


def product_pairs(xs: list[int], ys: list[int]) -> list[tuple[int, int]]:
    """デカルト積（全ての組み合わせ）を生成する。

    Examples:
        >>> product_pairs([1, 2], [3, 4])
        [(1, 3), (1, 4), (2, 3), (2, 4)]
    """
    from itertools import product
    return list(product(xs, ys))


def chain_lists(lists: list[list[T]]) -> list[T]:
    """複数のリストを連結する。

    Examples:
        >>> chain_lists([[1, 2], [3], [4, 5, 6]])
        [1, 2, 3, 4, 5, 6]
    """
    return list(chain.from_iterable(lists))


def group_by_first_char(words: list[str]) -> dict[str, list[str]]:
    """単語を最初の文字でグループ化する。

    Examples:
        >>> group_by_first_char(["apple", "apricot", "banana", "blueberry"])
        {'a': ['apple', 'apricot'], 'b': ['banana', 'blueberry']}
    """
    from itertools import groupby
    sorted_words = sorted(words, key=lambda w: w[0])
    return {
        key: list(group)
        for key, group in groupby(sorted_words, key=lambda w: w[0])
    }
