"""第3章: イミュータブルなデータ操作のテスト"""

from grokking_fp.ch03_immutable_data import (
    City,
    Point,
    abbreviate,
    add_to_set,
    appended,
    appended_all,
    first_two,
    insert_at_middle,
    insert_before_last,
    last_two,
    move_first_two_to_end,
    remove_from_set,
    remove_key,
    replan,
    substring,
    update_dict,
    with_population,
)


class TestAppended:
    """appended 関数のテスト"""

    def test_basic(self) -> None:
        original = ["Apple", "Book"]
        result = appended(original, "Mango")
        assert original == ["Apple", "Book"]  # 元のリストは変わらない
        assert result == ["Apple", "Book", "Mango"]

    def test_empty_list(self) -> None:
        assert appended([], "a") == ["a"]


class TestAppendedAll:
    """appended_all 関数のテスト"""

    def test_basic(self) -> None:
        assert appended_all(["a", "b"], ["c", "d"]) == ["a", "b", "c", "d"]

    def test_empty_elements(self) -> None:
        assert appended_all(["a", "b"], []) == ["a", "b"]

    def test_empty_list(self) -> None:
        assert appended_all([], ["a", "b"]) == ["a", "b"]


class TestFirstTwo:
    """first_two 関数のテスト"""

    def test_basic(self) -> None:
        assert first_two(["a", "b", "c"]) == ["a", "b"]

    def test_single_element(self) -> None:
        assert first_two(["a"]) == ["a"]

    def test_empty(self) -> None:
        assert first_two([]) == []


class TestLastTwo:
    """last_two 関数のテスト"""

    def test_basic(self) -> None:
        assert last_two(["a", "b", "c"]) == ["b", "c"]

    def test_single_element(self) -> None:
        assert last_two(["a"]) == ["a"]

    def test_empty(self) -> None:
        assert last_two([]) == []


class TestMoveFirstTwoToEnd:
    """move_first_two_to_end 関数のテスト"""

    def test_three_elements(self) -> None:
        assert move_first_two_to_end(["a", "b", "c"]) == ["c", "a", "b"]

    def test_four_elements(self) -> None:
        assert move_first_two_to_end(["a", "b", "c", "d"]) == ["c", "d", "a", "b"]


class TestInsertBeforeLast:
    """insert_before_last 関数のテスト"""

    def test_basic(self) -> None:
        assert insert_before_last(["a", "b"], "c") == ["a", "c", "b"]

    def test_single_element(self) -> None:
        assert insert_before_last(["a"], "b") == ["b", "a"]

    def test_empty(self) -> None:
        assert insert_before_last([], "a") == ["a"]


class TestInsertAtMiddle:
    """insert_at_middle 関数のテスト"""

    def test_even_elements(self) -> None:
        assert insert_at_middle(["a", "b", "c", "d"], "X") == ["a", "b", "X", "c", "d"]

    def test_two_elements(self) -> None:
        assert insert_at_middle(["a", "b"], "X") == ["a", "X", "b"]


class TestReplan:
    """replan 関数のテスト"""

    def test_insert_before_city(self) -> None:
        plan = ["Paris", "Berlin", "Kraków"]
        result = replan(plan, "Vienna", "Kraków")
        assert plan == ["Paris", "Berlin", "Kraków"]  # 元の計画は変わらない
        assert result == ["Paris", "Berlin", "Vienna", "Kraków"]

    def test_city_not_found(self) -> None:
        plan = ["Paris", "Berlin"]
        result = replan(plan, "Vienna", "Tokyo")
        assert result == ["Paris", "Berlin", "Vienna"]


class TestAbbreviate:
    """abbreviate 関数のテスト"""

    def test_full_name(self) -> None:
        assert abbreviate("Alonzo Church") == "A. Church"

    def test_already_abbreviated(self) -> None:
        assert abbreviate("A. Church") == "A. Church"

    def test_single_name(self) -> None:
        assert abbreviate("Alonzo") == "Alonzo"


class TestSubstring:
    """substring 関数のテスト"""

    def test_basic(self) -> None:
        assert substring("hello", 1, 4) == "ell"

    def test_from_start(self) -> None:
        assert substring("Python", 0, 2) == "Py"


class TestPoint:
    """Point のテスト"""

    def test_creation(self) -> None:
        p = Point(1, 2)
        assert p.x == 1
        assert p.y == 2

    def test_immutability(self) -> None:
        p = Point(1, 2)
        p2 = Point(p.x + 1, p.y)
        assert p == Point(1, 2)
        assert p2 == Point(2, 2)


class TestCity:
    """City のテスト"""

    def test_creation(self) -> None:
        city = City("Tokyo", 13960000)
        assert city.name == "Tokyo"
        assert city.population == 13960000


class TestWithPopulation:
    """with_population 関数のテスト"""

    def test_update_population(self) -> None:
        tokyo = City("Tokyo", 13960000)
        tokyo_updated = with_population(tokyo, 14000000)
        assert tokyo.population == 13960000  # 元の都市は変わらない
        assert tokyo_updated.population == 14000000
        assert tokyo_updated.name == "Tokyo"


class TestFrozensetOperations:
    """frozenset 操作のテスト"""

    def test_add_to_set(self) -> None:
        s = frozenset({"a", "b"})
        s2 = add_to_set(s, "c")
        assert "c" not in s  # 元の集合は変わらない
        assert "c" in s2

    def test_remove_from_set(self) -> None:
        s = frozenset({"a", "b", "c"})
        s2 = remove_from_set(s, "b")
        assert "b" in s  # 元の集合は変わらない
        assert "b" not in s2


class TestDictOperations:
    """辞書操作のテスト"""

    def test_update_dict(self) -> None:
        d = {"a": 1, "b": 2}
        d2 = update_dict(d, "c", 3)
        assert d == {"a": 1, "b": 2}  # 元の辞書は変わらない
        assert d2 == {"a": 1, "b": 2, "c": 3}

    def test_remove_key(self) -> None:
        d = {"a": 1, "b": 2, "c": 3}
        d2 = remove_key(d, "b")
        assert d == {"a": 1, "b": 2, "c": 3}  # 元の辞書は変わらない
        assert d2 == {"a": 1, "c": 3}
