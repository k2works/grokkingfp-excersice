"""第7章: Result 型と複合的なエラー処理

Python の returns ライブラリを使って、Either 型（Result）による
エラーメッセージを保持した安全なエラーハンドリングを学びます。
Maybe（Option）の限界を超え、失敗理由を伝える方法を習得します。
"""

from dataclasses import dataclass
from enum import Enum, auto
from typing import TypeVar

from returns.result import Failure, Result, Success

T = TypeVar("T")


# =============================================================================
# 7.1 Result の基本
# =============================================================================


def parse_int_result(s: str) -> Result[int, str]:
    """文字列を整数に変換し、エラーメッセージを返す。

    Examples:
        >>> parse_int_result("42")
        <Success: 42>
        >>> parse_int_result("abc")
        <Failure: Cannot parse 'abc' as integer>
    """
    try:
        return Success(int(s))
    except ValueError:
        return Failure(f"Cannot parse '{s}' as integer")


def safe_divide_result(a: int, b: int) -> Result[int, str]:
    """安全な除算（エラーメッセージ付き）。

    Examples:
        >>> safe_divide_result(10, 2)
        <Success: 5>
        >>> safe_divide_result(10, 0)
        <Failure: Division by zero>
    """
    if b == 0:
        return Failure("Division by zero")
    return Success(a // b)


# =============================================================================
# 7.2 TV番組のパース（エラーメッセージ付き）
# =============================================================================


@dataclass(frozen=True)
class TvShow:
    """TV番組を表すイミュータブルなデータクラス。"""

    title: str
    start: int
    end: int


def extract_name_result(raw_show: str) -> Result[str, str]:
    """番組名を抽出する（エラーメッセージ付き）。

    Examples:
        >>> extract_name_result("Breaking Bad (2008-2013)")
        <Success: Breaking Bad>
        >>> extract_name_result("(2008-2013)")
        <Failure: Can't extract name from '(2008-2013)'>
    """
    bracket_open = raw_show.find("(")
    if bracket_open > 0:
        return Success(raw_show[:bracket_open].strip())
    return Failure(f"Can't extract name from '{raw_show}'")


def extract_year_start_result(raw_show: str) -> Result[int, str]:
    """開始年を抽出する（エラーメッセージ付き）。

    Examples:
        >>> extract_year_start_result("Breaking Bad (2008-2013)")
        <Success: 2008>
        >>> extract_year_start_result("Breaking Bad (abc-2013)")
        <Failure: Cannot parse 'abc' as integer>
    """
    bracket_open = raw_show.find("(")
    dash = raw_show.find("-")
    if bracket_open != -1 and dash > bracket_open + 1:
        year_str = raw_show[bracket_open + 1 : dash]
        return parse_int_result(year_str)
    return Failure(f"Can't extract start year from '{raw_show}'")


def extract_year_end_result(raw_show: str) -> Result[int, str]:
    """終了年を抽出する（エラーメッセージ付き）。

    Examples:
        >>> extract_year_end_result("Breaking Bad (2008-2013)")
        <Success: 2013>
        >>> extract_year_end_result("Breaking Bad (2008-)")
        <Failure: Can't extract end year from 'Breaking Bad (2008-)'>
    """
    dash = raw_show.find("-")
    bracket_close = raw_show.find(")")
    if dash != -1 and bracket_close > dash + 1:
        year_str = raw_show[dash + 1 : bracket_close]
        return parse_int_result(year_str)
    return Failure(f"Can't extract end year from '{raw_show}'")


def extract_single_year_result(raw_show: str) -> Result[int, str]:
    """単年の番組から年を抽出する（エラーメッセージ付き）。

    Examples:
        >>> extract_single_year_result("Chernobyl (2019)")
        <Success: 2019>
        >>> extract_single_year_result("Breaking Bad (2008-2013)")
        <Failure: Can't extract single year from 'Breaking Bad (2008-2013)'>
    """
    dash = raw_show.find("-")
    bracket_open = raw_show.find("(")
    bracket_close = raw_show.find(")")
    if dash == -1 and bracket_open != -1 and bracket_close > bracket_open + 1:
        year_str = raw_show[bracket_open + 1 : bracket_close]
        return parse_int_result(year_str)
    return Failure(f"Can't extract single year from '{raw_show}'")


def parse_show_result(raw_show: str) -> Result[TvShow, str]:
    """TV番組の文字列をパースする（エラーメッセージ付き）。

    Examples:
        >>> parse_show_result("Breaking Bad (2008-2013)")
        <Success: TvShow(title='Breaking Bad', start=2008, end=2013)>
        >>> parse_show_result("(2008-2013)")
        <Failure: Can't extract name from '(2008-2013)'>
    """
    name = extract_name_result(raw_show)
    year_start = extract_year_start_result(raw_show).lash(
        lambda _: extract_single_year_result(raw_show)
    )
    year_end = extract_year_end_result(raw_show).lash(
        lambda _: extract_single_year_result(raw_show)
    )

    return name.bind(
        lambda n: year_start.bind(lambda s: year_end.map(lambda e: TvShow(n, s, e)))
    )


# =============================================================================
# 7.3 バリデーション
# =============================================================================


def validate_age(age: int) -> Result[int, str]:
    """年齢を検証する。

    Examples:
        >>> validate_age(25)
        <Success: 25>
        >>> validate_age(-5)
        <Failure: Age cannot be negative>
        >>> validate_age(200)
        <Failure: Age cannot be greater than 150>
    """
    if age < 0:
        return Failure("Age cannot be negative")
    if age > 150:
        return Failure("Age cannot be greater than 150")
    return Success(age)


def validate_email(email: str) -> Result[str, str]:
    """メールアドレスを検証する。

    Examples:
        >>> validate_email("user@example.com")
        <Success: user@example.com>
        >>> validate_email("invalid")
        <Failure: Email must contain '@'>
    """
    if "@" not in email:
        return Failure("Email must contain '@'")
    if not email.endswith((".com", ".org", ".net", ".jp")):
        return Failure("Email must have a valid domain")
    return Success(email)


def validate_password(password: str) -> Result[str, str]:
    """パスワードを検証する。

    Examples:
        >>> validate_password("SecurePass123")
        <Success: SecurePass123>
        >>> validate_password("short")
        <Failure: Password must be at least 8 characters>
    """
    if len(password) < 8:
        return Failure("Password must be at least 8 characters")
    if not any(c.isupper() for c in password):
        return Failure("Password must contain at least one uppercase letter")
    if not any(c.isdigit() for c in password):
        return Failure("Password must contain at least one digit")
    return Success(password)


# =============================================================================
# 7.4 代数的データ型（ADT）
# =============================================================================


class MusicGenre(Enum):
    """音楽ジャンルを表す列挙型。"""

    HEAVY_METAL = auto()
    POP = auto()
    HARD_ROCK = auto()
    JAZZ = auto()
    CLASSICAL = auto()


class Location(Enum):
    """場所を表す列挙型。"""

    US = "U.S."
    UK = "U.K."
    JAPAN = "Japan"
    GERMANY = "Germany"


@dataclass(frozen=True)
class PeriodInYears:
    """年の期間を表すデータクラス。"""

    start: int
    end: int | None  # None はまだアクティブなことを示す


@dataclass(frozen=True)
class Artist:
    """アーティストを表すデータクラス。

    Examples:
        >>> metallica = Artist("Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None))
        >>> metallica.name
        'Metallica'
    """

    name: str
    genre: MusicGenre
    origin: Location
    years_active: PeriodInYears


def was_artist_active(artist: Artist, year_start: int, year_end: int) -> bool:
    """アーティストが指定期間中にアクティブだったかを判定する。

    Examples:
        >>> metallica = Artist("Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None))
        >>> was_artist_active(metallica, 2000, 2020)
        True
        >>> led_zeppelin = Artist("Led Zeppelin", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1968, 1980))
        >>> was_artist_active(led_zeppelin, 1990, 2000)
        False
    """
    period = artist.years_active
    if period.end is None:
        # まだアクティブ
        return period.start <= year_end
    # 活動終了済み
    return period.start <= year_end and period.end >= year_start


def active_length(artist: Artist, current_year: int) -> int:
    """アーティストの活動年数を計算する。

    Examples:
        >>> metallica = Artist("Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None))
        >>> active_length(metallica, 2024)
        43
        >>> led_zeppelin = Artist("Led Zeppelin", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1968, 1980))
        >>> active_length(led_zeppelin, 2024)
        12
    """
    period = artist.years_active
    if period.end is None:
        return current_year - period.start
    return period.end - period.start


# =============================================================================
# 7.5 検索条件のモデリング
# =============================================================================


@dataclass(frozen=True)
class SearchByGenre:
    """ジャンルで検索する条件。"""

    genres: list[MusicGenre]


@dataclass(frozen=True)
class SearchByOrigin:
    """出身地で検索する条件。"""

    locations: list[Location]


@dataclass(frozen=True)
class SearchByActiveYears:
    """活動期間で検索する条件。"""

    start: int
    end: int


SearchCondition = SearchByGenre | SearchByOrigin | SearchByActiveYears


def matches_condition(artist: Artist, condition: SearchCondition) -> bool:
    """アーティストが検索条件に一致するかを判定する。

    Examples:
        >>> metallica = Artist("Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None))
        >>> matches_condition(metallica, SearchByGenre([MusicGenre.HEAVY_METAL]))
        True
        >>> matches_condition(metallica, SearchByOrigin([Location.UK]))
        False
    """
    match condition:
        case SearchByGenre(genres):
            return artist.genre in genres
        case SearchByOrigin(locations):
            return artist.origin in locations
        case SearchByActiveYears(start, end):
            return was_artist_active(artist, start, end)


def search_artists(
    artists: list[Artist],
    required_conditions: list[SearchCondition],
) -> list[Artist]:
    """条件を全て満たすアーティストを検索する。

    Examples:
        >>> metallica = Artist("Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None))
        >>> queen = Artist("Queen", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1970, 1995))
        >>> artists = [metallica, queen]
        >>> search_artists(artists, [SearchByOrigin([Location.US])])
        [Artist(name='Metallica', genre=<MusicGenre.HEAVY_METAL: 1>, origin=<Location.US: 'U.S.'>, years_active=PeriodInYears(start=1981, end=None))]
    """
    return [
        artist
        for artist in artists
        if all(matches_condition(artist, cond) for cond in required_conditions)
    ]


# =============================================================================
# 7.6 Result の合成
# =============================================================================


@dataclass(frozen=True)
class User:
    """ユーザーを表すデータクラス。"""

    name: str
    email: str
    age: int


def create_user(name: str, email: str, age: int) -> Result[User, str]:
    """ユーザーを作成する（バリデーション付き）。

    Examples:
        >>> create_user("Alice", "alice@example.com", 25)
        <Success: User(name='Alice', email='alice@example.com', age=25)>
        >>> create_user("Bob", "invalid", 25)
        <Failure: Email must contain '@'>
    """
    return validate_email(email).bind(
        lambda valid_email: validate_age(age).map(
            lambda valid_age: User(name, valid_email, valid_age)
        )
    )


# =============================================================================
# 7.7 エラー型を使ったより表現力の高いエラー処理
# =============================================================================


class ValidationError(Enum):
    """バリデーションエラーの種類。"""

    EMPTY_NAME = "Name cannot be empty"
    INVALID_EMAIL = "Invalid email format"
    AGE_TOO_YOUNG = "Age must be at least 18"
    AGE_TOO_OLD = "Age cannot exceed 120"


def validate_name_typed(name: str) -> Result[str, ValidationError]:
    """名前を検証する（型付きエラー）。

    Examples:
        >>> validate_name_typed("Alice")
        <Success: Alice>
        >>> validate_name_typed("")
        <Failure: ValidationError.EMPTY_NAME>
    """
    if not name.strip():
        return Failure(ValidationError.EMPTY_NAME)
    return Success(name)


def validate_age_typed(age: int) -> Result[int, ValidationError]:
    """年齢を検証する（型付きエラー）。

    Examples:
        >>> validate_age_typed(25)
        <Success: 25>
        >>> validate_age_typed(15)
        <Failure: ValidationError.AGE_TOO_YOUNG>
    """
    if age < 18:
        return Failure(ValidationError.AGE_TOO_YOUNG)
    if age > 120:
        return Failure(ValidationError.AGE_TOO_OLD)
    return Success(age)


# =============================================================================
# 7.8 Result のユーティリティ
# =============================================================================


def result_to_maybe(result: Result[T, str]):
    """Result を Maybe に変換する。

    Examples:
        >>> from returns.maybe import Some, Nothing
        >>> result_to_maybe(Success(42))
        <Some: 42>
        >>> result_to_maybe(Failure("error"))
        <Nothing>
    """
    from returns.maybe import Nothing, Some

    if isinstance(result, Success):
        return Some(result.unwrap())
    return Nothing


def get_or_else(result: Result[T, str], default: T) -> T:
    """Result から値を取得するか、デフォルト値を返す。

    Examples:
        >>> get_or_else(Success(42), 0)
        42
        >>> get_or_else(Failure("error"), 0)
        0
    """
    if isinstance(result, Success):
        return result.unwrap()
    return default


def map_error(result: Result[T, str], f) -> Result[T, str]:
    """エラーを変換する。

    Examples:
        >>> map_error(Failure("error"), lambda e: f"ERROR: {e}")
        <Failure: ERROR: error>
        >>> map_error(Success(42), lambda e: f"ERROR: {e}")
        <Success: 42>
    """
    if isinstance(result, Failure):
        return Failure(f(result.failure()))
    return result


def sequence_results(results: list[Result[T, str]]) -> Result[list[T], str]:
    """全ての Result が Success なら、値のリストを返す。

    Examples:
        >>> sequence_results([Success(1), Success(2), Success(3)])
        <Success: [1, 2, 3]>
        >>> sequence_results([Success(1), Failure("error"), Success(3)])
        <Failure: error>
    """
    values: list[T] = []
    for r in results:
        if isinstance(r, Success):
            values.append(r.unwrap())
        else:
            return r
    return Success(values)
