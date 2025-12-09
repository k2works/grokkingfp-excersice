package ch08;

import io.vavr.collection.List;
import io.vavr.control.Try;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 第8章: IO モナドのテスト
 */
@DisplayName("第8章: IO モナド")
class IOTest {

    @Nested
    @DisplayName("IO の作成")
    class CreationTest {

        @Test
        @DisplayName("delay は副作用を遅延実行する")
        void delayDefersExecution() {
            AtomicInteger counter = new AtomicInteger(0);

            IO<Integer> io = IO.delay(() -> {
                counter.incrementAndGet();
                return 42;
            });

            // IO を作成しただけでは実行されない
            assertThat(counter.get()).isEqualTo(0);

            // unsafeRun で実行される
            int result = io.unsafeRun();
            assertThat(result).isEqualTo(42);
            assertThat(counter.get()).isEqualTo(1);
        }

        @Test
        @DisplayName("pure は値を即座にラップする")
        void pureWrapsValue() {
            IO<Integer> io = IO.pure(42);
            assertThat(io.unsafeRun()).isEqualTo(42);
        }

        @Test
        @DisplayName("unit は何もしない IO を返す")
        void unitReturnsNull() {
            IO<Void> io = IO.unit();
            assertThat(io.unsafeRun()).isNull();
        }

        @Test
        @DisplayName("effect は副作用のみを実行する")
        void effectRunsSideEffect() {
            AtomicInteger counter = new AtomicInteger(0);
            IO<Void> io = IO.effect(counter::incrementAndGet);

            assertThat(counter.get()).isEqualTo(0);
            io.unsafeRun();
            assertThat(counter.get()).isEqualTo(1);
        }
    }

    @Nested
    @DisplayName("IO の変換")
    class TransformationTest {

        @Test
        @DisplayName("map で結果を変換")
        void mapTransformsResult() {
            IO<Integer> io = IO.pure(10).map(x -> x * 2);
            assertThat(io.unsafeRun()).isEqualTo(20);
        }

        @Test
        @DisplayName("flatMap で IO をチェーン")
        void flatMapChainsIO() {
            IO<Integer> io = IO.pure(10)
                    .flatMap(x -> IO.pure(x + 5))
                    .flatMap(x -> IO.pure(x * 2));

            assertThat(io.unsafeRun()).isEqualTo(30);
        }

        @Test
        @DisplayName("andThen で順番に実行")
        void andThenRunsSequentially() {
            AtomicInteger counter = new AtomicInteger(0);

            IO<Integer> io = IO.effect(counter::incrementAndGet)
                    .andThen(IO.effect(counter::incrementAndGet))
                    .andThen(IO.pure(counter.get()));

            // まだ実行されていない
            assertThat(counter.get()).isEqualTo(0);

            // 実行
            io.unsafeRun();

            // 2回 increment された後の値を取得
            assertThat(counter.get()).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("エラーハンドリング")
    class ErrorHandlingTest {

        @Test
        @DisplayName("orElse で失敗時にフォールバック（Supplier版）")
        void orElseWithSupplier() {
            IO<Integer> failing = IO.delay(() -> {
                throw new RuntimeException("error");
            });

            IO<Integer> io = failing.orElse(() -> IO.pure(42));
            assertThat(io.unsafeRun()).isEqualTo(42);
        }

        @Test
        @DisplayName("orElse で成功時はそのまま")
        void orElseOnSuccess() {
            IO<Integer> success = IO.pure(10);
            IO<Integer> io = success.orElse(42);
            assertThat(io.unsafeRun()).isEqualTo(10);
        }

        @Test
        @DisplayName("orElse でデフォルト値を返す")
        void orElseWithDefaultValue() {
            IO<Integer> failing = IO.delay(() -> {
                throw new RuntimeException("error");
            });

            IO<Integer> io = failing.orElse(99);
            assertThat(io.unsafeRun()).isEqualTo(99);
        }

        @Test
        @DisplayName("unsafeRunTry で Try にラップ")
        void unsafeRunTryWrapsInTry() {
            IO<Integer> success = IO.pure(42);
            IO<Integer> failing = IO.delay(() -> {
                throw new RuntimeException("error");
            });

            Try<Integer> successTry = success.unsafeRunTry();
            assertThat(successTry.isSuccess()).isTrue();
            assertThat(successTry.get()).isEqualTo(42);

            Try<Integer> failureTry = failing.unsafeRunTry();
            assertThat(failureTry.isFailure()).isTrue();
        }

        @Test
        @DisplayName("retry で複数回リトライ")
        void retryMultipleTimes() {
            AtomicInteger counter = new AtomicInteger(0);

            IO<Integer> failingTwice = IO.delay(() -> {
                int count = counter.incrementAndGet();
                if (count < 3) {
                    throw new RuntimeException("not yet");
                }
                return count;
            });

            IO<Integer> io = failingTwice.retry(5);
            assertThat(io.unsafeRun()).isEqualTo(3);
        }

        @Test
        @DisplayName("retryWithDefault でリトライ後デフォルト値")
        void retryWithDefaultValue() {
            IO<Integer> alwaysFailing = IO.delay(() -> {
                throw new RuntimeException("always fail");
            });

            IO<Integer> io = alwaysFailing.retryWithDefault(3, -1);
            assertThat(io.unsafeRun()).isEqualTo(-1);
        }
    }

    @Nested
    @DisplayName("ユーティリティ")
    class UtilityTest {

        @Test
        @DisplayName("sequence で IO のリストを実行")
        void sequenceExecutesList() {
            List<IO<Integer>> ios = List.of(
                    IO.pure(1),
                    IO.pure(2),
                    IO.pure(3)
            );

            IO<List<Integer>> combined = IO.sequence(ios);
            assertThat(combined.unsafeRun()).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("sequence は順番を保つ")
        void sequencePreservesOrder() {
            AtomicInteger counter = new AtomicInteger(0);

            List<IO<Integer>> ios = List.of(
                    IO.delay(counter::incrementAndGet),
                    IO.delay(counter::incrementAndGet),
                    IO.delay(counter::incrementAndGet)
            );

            IO<List<Integer>> combined = IO.sequence(ios);
            assertThat(combined.unsafeRun()).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("traverse でリストの各要素に IO を適用")
        void traverseAppliesFunction() {
            List<Integer> numbers = List.of(1, 2, 3);

            IO<List<Integer>> io = IO.traverse(numbers, n -> IO.pure(n * 2));
            assertThat(io.unsafeRun()).containsExactly(2, 4, 6);
        }
    }

    @Nested
    @DisplayName("参照透過性")
    class ReferentialTransparencyTest {

        @Test
        @DisplayName("IO は参照透過性を保つ")
        void ioIsReferentiallyTransparent() {
            AtomicInteger counter = new AtomicInteger(0);

            // 同じ IO を2回参照しても
            IO<Integer> io = IO.delay(counter::incrementAndGet);
            IO<Integer> combined = io.flatMap(x -> io.map(y -> x + y));

            // 実行すると2回カウントされる
            int result = combined.unsafeRun();
            assertThat(counter.get()).isEqualTo(2);
            assertThat(result).isEqualTo(3); // 1 + 2
        }
    }
}
