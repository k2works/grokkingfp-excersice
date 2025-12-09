package ch10;

import ch08.IO;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第10章: Fiber のテスト
 */
@DisplayName("第10章: Fiber（軽量スレッド）")
class FiberTest {

    @Nested
    @DisplayName("Fiber の起動")
    class StartTest {

        @Test
        @DisplayName("start でバックグラウンド実行")
        void startRunsInBackground() throws InterruptedException {
            AtomicInteger counter = new AtomicInteger(0);

            IO<Fiber<Void>> startProgram = Fiber.start(
                    Fiber.sleep(50).andThen(IO.effect(counter::incrementAndGet))
            );

            Fiber<Void> fiber = startProgram.unsafeRun();

            // 起動直後はまだ実行されていない
            assertThat(counter.get()).isEqualTo(0);

            // 完了を待機
            fiber.join().unsafeRun();

            // 実行された
            assertThat(counter.get()).isEqualTo(1);
        }

        @Test
        @DisplayName("start は即座に制御を返す")
        void startReturnsImmediately() {
            long start = System.currentTimeMillis();

            Fiber<Void> fiber = Fiber.start(
                    Fiber.sleep(1000).andThen(IO.unit())
            ).unsafeRun();

            long elapsed = System.currentTimeMillis() - start;

            // 1000ms 待たずに戻る
            assertThat(elapsed).isLessThan(100);

            // クリーンアップ
            fiber.cancel().unsafeRun();
        }
    }

    @Nested
    @DisplayName("Fiber のキャンセル")
    class CancelTest {

        @Test
        @DisplayName("cancel で Fiber を停止")
        void cancelStopsFiber() throws InterruptedException {
            AtomicInteger counter = new AtomicInteger(0);

            IO<Fiber<Void>> startProgram = Fiber.startForever(
                    Fiber.sleep(10).andThen(IO.effect(counter::incrementAndGet))
            );

            Fiber<Void> fiber = startProgram.unsafeRun();

            // しばらく実行
            Thread.sleep(100);
            int countBefore = counter.get();
            assertThat(countBefore).isGreaterThan(0);

            // キャンセル
            fiber.cancel().unsafeRun();
            Thread.sleep(50);

            int countAfter = counter.get();

            // キャンセル後は増加しない（または少しだけ増加）
            assertThat(countAfter).isLessThanOrEqualTo(countBefore + 2);
            assertThat(fiber.isCancelled()).isTrue();
        }
    }

    @Nested
    @DisplayName("Fiber.sleep")
    class SleepTest {

        @Test
        @DisplayName("sleep で指定時間待機")
        void sleepWaitsSpecifiedTime() {
            long start = System.currentTimeMillis();

            Fiber.sleep(100).unsafeRun();

            long elapsed = System.currentTimeMillis() - start;
            assertThat(elapsed).isBetween(90L, 200L);
        }
    }

    @Nested
    @DisplayName("Fiber.delay")
    class DelayTest {

        @Test
        @DisplayName("delay で指定時間後に実行")
        void delayExecutesAfterTime() {
            AtomicInteger counter = new AtomicInteger(0);

            long start = System.currentTimeMillis();
            Fiber.delay(100, IO.effect(counter::incrementAndGet)).unsafeRun();
            long elapsed = System.currentTimeMillis() - start;

            assertThat(elapsed).isBetween(90L, 200L);
            assertThat(counter.get()).isEqualTo(1);
        }
    }

    @Nested
    @DisplayName("Fiber の状態")
    class StateTest {

        @Test
        @DisplayName("isDone は完了後に true")
        void isDoneAfterCompletion() {
            Fiber<Integer> fiber = Fiber.start(IO.pure(42)).unsafeRun();

            // 完了を待機
            fiber.join().unsafeRun();

            assertThat(fiber.isDone()).isTrue();
        }

        @Test
        @DisplayName("isCancelled はキャンセル後に true")
        void isCancelledAfterCancel() {
            Fiber<Void> fiber = Fiber.startForever(Fiber.sleep(10)).unsafeRun();

            assertThat(fiber.isCancelled()).isFalse();

            fiber.cancel().unsafeRun();

            assertThat(fiber.isCancelled()).isTrue();
        }
    }

    @Nested
    @DisplayName("結果の取得")
    class JoinTest {

        @Test
        @DisplayName("join で結果を取得")
        void joinGetsResult() {
            Fiber<Integer> fiber = Fiber.start(
                    IO.pure(10).map(x -> x * 2)
            ).unsafeRun();

            Integer result = fiber.join().unsafeRun();

            assertThat(result).isEqualTo(20);
        }
    }
}
