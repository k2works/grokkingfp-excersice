package ch08;

import io.vavr.collection.List;
import io.vavr.control.Either;
import io.vavr.control.Try;

import java.util.function.Function;
import java.util.function.Supplier;

/**
 * 第8章: IO モナドの簡易実装
 *
 * 副作用を持つ計算の「記述」を表す型です。
 * IO 値を作成しただけでは副作用は発生しません。
 * unsafeRun() で実際に実行されます。
 */
public final class IO<A> {

    private final Supplier<A> thunk;

    private IO(Supplier<A> thunk) {
        this.thunk = thunk;
    }

    // ============================================
    // IO の作成
    // ============================================

    /**
     * 副作用のある式を遅延実行する IO を作成
     */
    public static <A> IO<A> delay(Supplier<A> supplier) {
        return new IO<>(supplier);
    }

    /**
     * 既存の値をラップ（副作用なし）
     */
    public static <A> IO<A> pure(A value) {
        return new IO<>(() -> value);
    }

    /**
     * 何もしない IO
     */
    public static IO<Void> unit() {
        return new IO<>(() -> null);
    }

    /**
     * 副作用のみを実行する IO を作成
     */
    public static IO<Void> effect(Runnable action) {
        return new IO<>(() -> {
            action.run();
            return null;
        });
    }

    // ============================================
    // IO の実行
    // ============================================

    /**
     * IO を実行して結果を取得（副作用が発生する）
     */
    public A unsafeRun() {
        return thunk.get();
    }

    /**
     * IO を実行して Try でラップ
     */
    public Try<A> unsafeRunTry() {
        return Try.of(thunk::get);
    }

    // ============================================
    // IO の変換
    // ============================================

    /**
     * 結果を変換する
     */
    public <B> IO<B> map(Function<A, B> f) {
        return new IO<>(() -> f.apply(thunk.get()));
    }

    /**
     * IO を返す関数を適用してフラット化
     */
    public <B> IO<B> flatMap(Function<A, IO<B>> f) {
        return new IO<>(() -> f.apply(thunk.get()).unsafeRun());
    }

    /**
     * 2つの IO を順番に実行
     */
    public <B> IO<B> andThen(IO<B> next) {
        return flatMap(ignored -> next);
    }

    /**
     * 失敗時のフォールバック
     */
    public IO<A> orElse(Supplier<IO<A>> fallback) {
        return new IO<>(() -> {
            try {
                return thunk.get();
            } catch (Exception e) {
                return fallback.get().unsafeRun();
            }
        });
    }

    /**
     * 失敗時にデフォルト値を返す
     */
    public IO<A> orElse(A defaultValue) {
        return orElse(() -> IO.pure(defaultValue));
    }

    // ============================================
    // ユーティリティ
    // ============================================

    /**
     * IO のリストを実行して結果のリストを返す
     */
    public static <A> IO<List<A>> sequence(List<IO<A>> ios) {
        return ios.foldLeft(
                IO.pure(List.empty()),
                (acc, io) -> acc.flatMap(list -> io.map(list::append))
        );
    }

    /**
     * リストの各要素に IO を適用して結果のリストを返す
     */
    public static <A, B> IO<List<B>> traverse(List<A> list, Function<A, IO<B>> f) {
        return sequence(list.map(f));
    }

    /**
     * 複数回リトライ
     */
    public IO<A> retry(int maxRetries) {
        IO<A> result = this;
        for (int i = 0; i < maxRetries; i++) {
            result = result.orElse(() -> this);
        }
        return result;
    }

    /**
     * 複数回リトライしてデフォルト値を返す
     */
    public IO<A> retryWithDefault(int maxRetries, A defaultValue) {
        return retry(maxRetries).orElse(defaultValue);
    }

    /**
     * 例外をキャッチして Either に変換
     */
    public IO<Either<Throwable, A>> attempt() {
        return new IO<>(() -> {
            try {
                return Either.right(thunk.get());
            } catch (Throwable e) {
                return Either.left(e);
            }
        });
    }

    /**
     * 成功・失敗に関わらず、最後に必ず実行するアクション
     */
    public IO<A> guarantee(IO<Void> finalizer) {
        return new IO<>(() -> {
            try {
                return thunk.get();
            } finally {
                try {
                    finalizer.unsafeRun();
                } catch (Exception ignored) {
                    // ファイナライザーの例外は無視
                }
            }
        });
    }

    /**
     * 指定時間後にタイムアウト
     */
    public IO<A> timeout(long millis) {
        return new IO<>(() -> {
            java.util.concurrent.CompletableFuture<A> future =
                    java.util.concurrent.CompletableFuture.supplyAsync(thunk::get);
            try {
                return future.get(millis, java.util.concurrent.TimeUnit.MILLISECONDS);
            } catch (java.util.concurrent.TimeoutException e) {
                future.cancel(true);
                throw new RuntimeException("IO timed out after " + millis + "ms", e);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
    }

    @Override
    public String toString() {
        return "IO(...)";
    }
}
