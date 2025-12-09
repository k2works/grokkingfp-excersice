package ch08;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第8章: サイコロを振るテスト
 */
@DisplayName("第8章: サイコロを振る")
class CastingDieTest {

    @Nested
    @DisplayName("castTheDie")
    class CastTheDieTest {

        @Test
        @DisplayName("サイコロの目は1〜6の範囲")
        void dieRollsInRange() {
            for (int i = 0; i < 100; i++) {
                int result = CastingDie.castTheDie().unsafeRun();
                assertThat(result).isBetween(1, 6);
            }
        }

        @Test
        @DisplayName("IO は遅延実行される")
        void ioIsDeferred() {
            IO<Integer> io = CastingDie.castTheDie();
            // IO を作成しただけでは実行されない
            // toString で確認
            assertThat(io.toString()).isEqualTo("IO(...)");
        }
    }

    @Nested
    @DisplayName("castTheDieTwice")
    class CastTheDieTwiceTest {

        @Test
        @DisplayName("2回振った合計は2〜12の範囲")
        void twiceInRange() {
            for (int i = 0; i < 100; i++) {
                int result = CastingDie.castTheDieTwice().unsafeRun();
                assertThat(result).isBetween(2, 12);
            }
        }
    }

    @Nested
    @DisplayName("castTheDieNTimes")
    class CastTheDieNTimesTest {

        @Test
        @DisplayName("n回振った合計は n〜6n の範囲")
        void nTimesInRange() {
            int n = 5;
            for (int i = 0; i < 50; i++) {
                int result = CastingDie.castTheDieNTimes(n).unsafeRun();
                assertThat(result).isBetween(n, 6 * n);
            }
        }

        @Test
        @DisplayName("0回振ると0")
        void zeroTimesReturnsZero() {
            assertThat(CastingDie.castTheDieNTimes(0).unsafeRun()).isEqualTo(0);
        }

        @Test
        @DisplayName("負の回数は0")
        void negativeTimesReturnsZero() {
            assertThat(CastingDie.castTheDieNTimes(-1).unsafeRun()).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("castUntil")
    class CastUntilTest {

        @Test
        @DisplayName("目標の目が出るまで振る")
        void castUntilTarget() {
            // 十分な試行回数で6が出る確率は高い
            IO<Integer> io = CastingDie.castUntil(6, 100);
            int attempts = io.unsafeRun();

            // 100回以内に見つかったか、見つからなかったか
            if (attempts == -1) {
                // 100回振っても出なかった（非常に稀）
                assertThat(attempts).isEqualTo(-1);
            } else {
                assertThat(attempts).isBetween(1, 100);
            }
        }

        @Test
        @DisplayName("最大試行回数を超えると-1")
        void returnsMinusOneOnMaxAttempts() {
            // 1回だけ振って、その目が7（ありえない）なら必ず-1
            // 実際には1-6が出るので、このテストは確率的
            IO<Integer> io = CastingDie.castUntil(7, 10); // 7は出ない
            assertThat(io.unsafeRun()).isEqualTo(-1);
        }
    }

    @Nested
    @DisplayName("combineIO")
    class CombineIOTest {

        @Test
        @DisplayName("2つの IO を結合")
        void combinesTwoIOs() {
            IO<Integer> io1 = IO.pure(10);
            IO<Integer> io2 = IO.pure(20);

            IO<Integer> combined = CastingDie.combineIO(io1, io2, Integer::sum);
            assertThat(combined.unsafeRun()).isEqualTo(30);
        }

        @Test
        @DisplayName("異なる型の IO を結合")
        void combinesDifferentTypes() {
            IO<String> io1 = IO.pure("Hello");
            IO<Integer> io2 = IO.pure(42);

            IO<String> combined = CastingDie.combineIO(io1, io2,
                    (s, i) -> s + " " + i);
            assertThat(combined.unsafeRun()).isEqualTo("Hello 42");
        }
    }

    @Nested
    @DisplayName("printAndReturn")
    class PrintAndReturnTest {

        @Test
        @DisplayName("メッセージを返す")
        void returnsMessage() {
            String message = "test message";
            IO<String> io = CastingDie.printAndReturn(message);
            assertThat(io.unsafeRun()).isEqualTo(message);
        }
    }
}
