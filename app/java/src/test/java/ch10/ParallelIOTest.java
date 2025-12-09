package ch10;

import ch08.IO;
import io.vavr.Tuple2;
import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第10章: 並列 IO のテスト
 */
@DisplayName("第10章: 並列 IO")
class ParallelIOTest {

    @Nested
    @DisplayName("parSequence")
    class ParSequenceTest {

        @Test
        @DisplayName("IO のリストを並列実行")
        void executesInParallel() {
            List<IO<Integer>> ios = List.of(
                    IO.pure(1),
                    IO.pure(2),
                    IO.pure(3)
            );

            List<Integer> result = ParallelIO.parSequence(ios).unsafeRun();

            assertThat(result).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("空のリストは空のリストを返す")
        void emptyListReturnsEmpty() {
            List<IO<Integer>> ios = List.empty();

            List<Integer> result = ParallelIO.parSequence(ios).unsafeRun();

            assertThat(result).isEmpty();
        }

        @Test
        @DisplayName("並列実行で時間が短縮される")
        void parallelReducesTime() {
            List<IO<Integer>> ios = List.of(
                    Fiber.sleep(100).andThen(IO.pure(1)),
                    Fiber.sleep(100).andThen(IO.pure(2)),
                    Fiber.sleep(100).andThen(IO.pure(3))
            );

            long start = System.currentTimeMillis();
            ParallelIO.parSequence(ios).unsafeRun();
            long elapsed = System.currentTimeMillis() - start;

            // 並列なので約100ms（順次なら約300ms）
            assertThat(elapsed).isLessThan(200);
        }
    }

    @Nested
    @DisplayName("parTraverse")
    class ParTraverseTest {

        @Test
        @DisplayName("各要素に関数を適用して並列実行")
        void traversesInParallel() {
            List<Integer> numbers = List.of(1, 2, 3, 4, 5);

            List<Integer> result = ParallelIO.parTraverse(
                    numbers,
                    n -> IO.pure(n * 2)
            ).unsafeRun();

            assertThat(result).containsExactly(2, 4, 6, 8, 10);
        }
    }

    @Nested
    @DisplayName("parTuple")
    class ParTupleTest {

        @Test
        @DisplayName("parTuple2 で2つの IO を並列実行")
        void parTuple2() {
            Tuple2<Integer, String> result = ParallelIO.parTuple2(
                    IO.pure(42),
                    IO.pure("hello")
            ).unsafeRun();

            assertThat(result._1()).isEqualTo(42);
            assertThat(result._2()).isEqualTo("hello");
        }

        @Test
        @DisplayName("parTuple3 で3つの IO を並列実行")
        void parTuple3() {
            var result = ParallelIO.parTuple3(
                    IO.pure(1),
                    IO.pure("two"),
                    IO.pure(3.0)
            ).unsafeRun();

            assertThat(result._1()).isEqualTo(1);
            assertThat(result._2()).isEqualTo("two");
            assertThat(result._3()).isEqualTo(3.0);
        }
    }

    @Nested
    @DisplayName("parMap2")
    class ParMap2Test {

        @Test
        @DisplayName("2つの IO を並列実行して結合")
        void mapsTwoIOs() {
            IO<Integer> result = ParallelIO.parMap2(
                    IO.pure(10),
                    IO.pure(20),
                    Integer::sum
            );

            assertThat(result.unsafeRun()).isEqualTo(30);
        }
    }

    @Nested
    @DisplayName("parFold")
    class ParFoldTest {

        @Test
        @DisplayName("並列実行して結果を集約")
        void foldsResults() {
            List<IO<Integer>> ios = List.of(
                    IO.pure(1),
                    IO.pure(2),
                    IO.pure(3),
                    IO.pure(4)
            );

            Integer sum = ParallelIO.parFold(ios, 0, Integer::sum).unsafeRun();

            assertThat(sum).isEqualTo(10);
        }
    }

    @Nested
    @DisplayName("race")
    class RaceTest {

        @Test
        @DisplayName("最初に完了した方の結果を返す")
        void returnsFirstCompleted() {
            IO<String> slow = Fiber.sleep(200).andThen(IO.pure("slow"));
            IO<String> fast = Fiber.sleep(50).andThen(IO.pure("fast"));

            String result = ParallelIO.race(slow, fast).unsafeRun();

            assertThat(result).isEqualTo("fast");
        }
    }

    @Nested
    @DisplayName("timeout")
    class TimeoutTest {

        @Test
        @DisplayName("時間内に完了すれば Some")
        void completesWithinTimeout() {
            IO<Integer> fast = Fiber.sleep(50).andThen(IO.pure(42));

            Option<Integer> result = ParallelIO.timeout(fast, 200).unsafeRun();

            assertThat(result.isDefined()).isTrue();
            assertThat(result.get()).isEqualTo(42);
        }

        @Test
        @DisplayName("タイムアウトすれば None")
        void timesOut() {
            IO<Integer> slow = Fiber.sleep(500).andThen(IO.pure(42));

            Option<Integer> result = ParallelIO.timeout(slow, 100).unsafeRun();

            assertThat(result.isEmpty()).isTrue();
        }
    }

    @Nested
    @DisplayName("Ref との組み合わせ")
    class RefIntegrationTest {

        @Test
        @DisplayName("並列に Ref を更新")
        void parallelRefUpdate() {
            Ref<Integer> ref = Ref.unsafe(0);

            List<IO<Void>> updates = List.range(0, 100)
                    .map(i -> ref.update(x -> x + 1));

            ParallelIO.parSequence(updates).unsafeRun();

            assertThat(ref.unsafeGet()).isEqualTo(100);
        }

        @Test
        @DisplayName("並列にカウント")
        void parallelCount() {
            AtomicInteger javaCounter = new AtomicInteger(0);

            List<IO<Void>> increments = List.range(0, 1000)
                    .map(i -> IO.effect(javaCounter::incrementAndGet));

            ParallelIO.parSequence(increments).unsafeRun();

            assertThat(javaCounter.get()).isEqualTo(1000);
        }
    }
}
