package ch08;

import io.vavr.collection.List;

import java.util.Random;

/**
 * 第8章: サイコロを振る例
 *
 * 不純な関数（副作用あり）と IO を使った純粋な記述の比較
 */
public final class CastingDie {

    private static final Random random = new Random();

    private CastingDie() {}

    // ============================================
    // 不純な関数（副作用あり）
    // ============================================

    /**
     * サイコロを振る（不純）
     * 呼び出すたびに異なる値が返る
     */
    public static int castTheDieImpure() {
        return random.nextInt(6) + 1;
    }

    // ============================================
    // IO を使った純粋な記述
    // ============================================

    /**
     * サイコロを振る（IO でラップ）
     * IO 値を作成しただけでは実行されない
     */
    public static IO<Integer> castTheDie() {
        return IO.delay(CastingDie::castTheDieImpure);
    }

    /**
     * サイコロを2回振って合計を返す
     */
    public static IO<Integer> castTheDieTwice() {
        return castTheDie()
                .flatMap(first -> castTheDie()
                        .map(second -> first + second));
    }

    /**
     * サイコロを n 回振って合計を返す
     */
    public static IO<Integer> castTheDieNTimes(int n) {
        if (n <= 0) {
            return IO.pure(0);
        }
        return IO.sequence(List.fill(n, castTheDie()))
                .map(results -> results.foldLeft(0, Integer::sum));
    }

    /**
     * 指定した目が出るまで振り続ける（最大 maxAttempts 回）
     */
    public static IO<Integer> castUntil(int target, int maxAttempts) {
        return castUntilHelper(target, maxAttempts, 0);
    }

    private static IO<Integer> castUntilHelper(int target, int maxAttempts, int attempts) {
        if (attempts >= maxAttempts) {
            return IO.pure(-1); // 見つからなかった
        }
        return castTheDie().flatMap(result -> {
            if (result == target) {
                return IO.pure(attempts + 1);
            } else {
                return castUntilHelper(target, maxAttempts, attempts + 1);
            }
        });
    }

    // ============================================
    // コンソール出力との組み合わせ
    // ============================================

    /**
     * サイコロを振って結果を表示
     */
    public static IO<Integer> castAndPrint() {
        return castTheDie().flatMap(result -> {
            return IO.effect(() -> System.out.println("Rolled: " + result))
                    .andThen(IO.pure(result));
        });
    }

    /**
     * メッセージを表示してから値を返す
     */
    public static IO<String> printAndReturn(String message) {
        return IO.delay(() -> {
            System.out.println(message);
            return message;
        });
    }

    // ============================================
    // IO の合成ユーティリティ
    // ============================================

    /**
     * 2つの IO を結合
     */
    public static <A, B, C> IO<C> combineIO(IO<A> io1, IO<B> io2, java.util.function.BiFunction<A, B, C> f) {
        return io1.flatMap(a -> io2.map(b -> f.apply(a, b)));
    }
}
