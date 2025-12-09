package ch07;

import io.vavr.control.Either;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第7章: Either の基本操作のテスト
 */
@DisplayName("第7章: Either の基本操作")
class EitherBasicsTest {

    @Nested
    @DisplayName("Either の作成")
    class CreationTest {

        @Test
        @DisplayName("Right を作成")
        void createRight() {
            Either<String, Integer> either = EitherBasics.rightValue();
            assertThat(either.isRight()).isTrue();
            assertThat(either.get()).isEqualTo(42);
        }

        @Test
        @DisplayName("Left を作成")
        void createLeft() {
            Either<String, Integer> either = EitherBasics.leftValue();
            assertThat(either.isLeft()).isTrue();
            assertThat(either.getLeft()).isEqualTo("Error occurred");
        }
    }

    @Nested
    @DisplayName("validateAge")
    class ValidateAgeTest {

        @Test
        @DisplayName("正常な年齢")
        void validAge() {
            assertThat(EitherBasics.validateAge(25)).isEqualTo(Either.right(25));
            assertThat(EitherBasics.validateAge(0)).isEqualTo(Either.right(0));
            assertThat(EitherBasics.validateAge(150)).isEqualTo(Either.right(150));
        }

        @Test
        @DisplayName("負の年齢はエラー")
        void negativeAge() {
            Either<String, Integer> result = EitherBasics.validateAge(-5);
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).isEqualTo("Age cannot be negative");
        }

        @Test
        @DisplayName("150歳超はエラー")
        void tooOld() {
            Either<String, Integer> result = EitherBasics.validateAge(200);
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).isEqualTo("Age cannot be greater than 150");
        }
    }

    @Nested
    @DisplayName("validateEmail")
    class ValidateEmailTest {

        @Test
        @DisplayName("正常なメールアドレス")
        void validEmail() {
            assertThat(EitherBasics.validateEmail("test@example.com"))
                    .isEqualTo(Either.right("test@example.com"));
        }

        @Test
        @DisplayName("空のメールアドレスはエラー")
        void emptyEmail() {
            Either<String, String> result = EitherBasics.validateEmail("");
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).isEqualTo("Email cannot be empty");
        }

        @Test
        @DisplayName("@がないメールアドレスはエラー")
        void noAtSign() {
            Either<String, String> result = EitherBasics.validateEmail("invalid");
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).isEqualTo("Email must contain @");
        }
    }

    @Nested
    @DisplayName("map")
    class MapTest {

        @Test
        @DisplayName("Right を変換")
        void mapRight() {
            assertThat(EitherBasics.mapExample(Either.right(5))).isEqualTo(Either.right(10));
        }

        @Test
        @DisplayName("Left は Left のまま")
        void mapLeft() {
            Either<String, Integer> left = Either.left("error");
            assertThat(EitherBasics.mapExample(left)).isEqualTo(Either.left("error"));
        }
    }

    @Nested
    @DisplayName("flatMap")
    class FlatMapTest {

        @Test
        @DisplayName("Right で成功する関数を適用")
        void flatMapSuccess() {
            assertThat(EitherBasics.flatMapExample(Either.right(10))).isEqualTo(Either.right(10));
        }

        @Test
        @DisplayName("Right で失敗する関数を適用")
        void flatMapFailure() {
            Either<String, Integer> result = EitherBasics.flatMapExample(Either.right(0));
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).isEqualTo("Cannot divide by zero");
        }

        @Test
        @DisplayName("Left は Left のまま")
        void flatMapLeft() {
            Either<String, Integer> left = Either.left("original error");
            assertThat(EitherBasics.flatMapExample(left)).isEqualTo(Either.left("original error"));
        }
    }

    @Nested
    @DisplayName("orElse")
    class OrElseTest {

        @Test
        @DisplayName("Right は代替を使わない")
        void orElseRight() {
            assertThat(EitherBasics.orElseExample(Either.right(1), Either.right(2)))
                    .isEqualTo(Either.right(1));
        }

        @Test
        @DisplayName("Left は代替を使う")
        void orElseLeft() {
            assertThat(EitherBasics.orElseExample(Either.left("err"), Either.right(2)))
                    .isEqualTo(Either.right(2));
        }
    }

    @Nested
    @DisplayName("fold")
    class FoldTest {

        @Test
        @DisplayName("Right を fold")
        void foldRight() {
            assertThat(EitherBasics.foldExample(Either.right(42))).isEqualTo("Success: 42");
        }

        @Test
        @DisplayName("Left を fold")
        void foldLeft() {
            assertThat(EitherBasics.foldExample(Either.left("oops"))).isEqualTo("Error: oops");
        }
    }

    @Nested
    @DisplayName("toOption")
    class ToOptionTest {

        @Test
        @DisplayName("Right は Some")
        void rightToSome() {
            assertThat(EitherBasics.toOptionExample(Either.right(42))).isEqualTo(Option.some(42));
        }

        @Test
        @DisplayName("Left は None")
        void leftToNone() {
            assertThat(EitherBasics.toOptionExample(Either.left("err"))).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("fromOption")
    class FromOptionTest {

        @Test
        @DisplayName("Some は Right")
        void someToRight() {
            assertThat(EitherBasics.fromOption(Option.some(42), "error"))
                    .isEqualTo(Either.right(42));
        }

        @Test
        @DisplayName("None は Left")
        void noneToLeft() {
            assertThat(EitherBasics.fromOption(Option.none(), "no value"))
                    .isEqualTo(Either.left("no value"));
        }
    }

    @Nested
    @DisplayName("複合バリデーション")
    class CompositeValidationTest {

        @Test
        @DisplayName("全て有効なら User を作成")
        void allValid() {
            Either<String, EitherBasics.User> result =
                    EitherBasics.validateUser("alice", "alice@example.com", 25);

            assertThat(result.isRight()).isTrue();
            assertThat(result.get().username()).isEqualTo("alice");
            assertThat(result.get().email()).isEqualTo("alice@example.com");
            assertThat(result.get().age()).isEqualTo(25);
        }

        @Test
        @DisplayName("ユーザー名が無効")
        void invalidUsername() {
            Either<String, EitherBasics.User> result =
                    EitherBasics.validateUser("ab", "alice@example.com", 25);

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Username");
        }

        @Test
        @DisplayName("メールアドレスが無効")
        void invalidEmail() {
            Either<String, EitherBasics.User> result =
                    EitherBasics.validateUser("alice", "invalid", 25);

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("@");
        }

        @Test
        @DisplayName("年齢が無効")
        void invalidAge() {
            Either<String, EitherBasics.User> result =
                    EitherBasics.validateUser("alice", "alice@example.com", -5);

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Age");
        }
    }
}
