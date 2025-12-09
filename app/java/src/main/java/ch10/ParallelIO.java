package ch10;

import ch08.IO;
import io.vavr.collection.List;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * 第10章: 並列 IO 操作
 *
 * IO のリストを並列に実行するユーティリティです。
 * cats-effect の parSequence に相当する機能を提供します。
 */
public final class ParallelIO {

    private static final ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

    private ParallelIO() {}

    // ============================================
    // 並列実行
    // ============================================

    /**
     * IO のリストを並列に実行し、結果のリストを返す
     * (parSequence に相当)
     */
    public static <A> IO<List<A>> parSequence(List<IO<A>> ios) {
        return IO.delay(() -> {
            // 各 IO を CompletableFuture として非同期実行
            java.util.List<CompletableFuture<A>> futures = ios
                    .map(io -> CompletableFuture.supplyAsync(io::unsafeRun, executor))
                    .toJavaList();

            // すべての完了を待機
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

            // 結果を収集
            return List.ofAll(futures.stream().map(CompletableFuture::join).toList());
        });
    }

    /**
     * リストの各要素に IO を返す関数を適用し、並列に実行
     * (parTraverse に相当)
     */
    public static <A, B> IO<List<B>> parTraverse(List<A> list, java.util.function.Function<A, IO<B>> f) {
        return parSequence(list.map(f));
    }

    /**
     * 2つの IO を並列に実行し、結果のタプルを返す
     */
    public static <A, B> IO<io.vavr.Tuple2<A, B>> parTuple2(IO<A> io1, IO<B> io2) {
        return IO.delay(() -> {
            CompletableFuture<A> f1 = CompletableFuture.supplyAsync(io1::unsafeRun, executor);
            CompletableFuture<B> f2 = CompletableFuture.supplyAsync(io2::unsafeRun, executor);

            CompletableFuture.allOf(f1, f2).join();

            return io.vavr.Tuple.of(f1.join(), f2.join());
        });
    }

    /**
     * 3つの IO を並列に実行し、結果のタプルを返す
     */
    public static <A, B, C> IO<io.vavr.Tuple3<A, B, C>> parTuple3(IO<A> io1, IO<B> io2, IO<C> io3) {
        return IO.delay(() -> {
            CompletableFuture<A> f1 = CompletableFuture.supplyAsync(io1::unsafeRun, executor);
            CompletableFuture<B> f2 = CompletableFuture.supplyAsync(io2::unsafeRun, executor);
            CompletableFuture<C> f3 = CompletableFuture.supplyAsync(io3::unsafeRun, executor);

            CompletableFuture.allOf(f1, f2, f3).join();

            return io.vavr.Tuple.of(f1.join(), f2.join(), f3.join());
        });
    }

    /**
     * 2つの IO を並列に実行し、結果を結合
     */
    public static <A, B, C> IO<C> parMap2(
            IO<A> io1,
            IO<B> io2,
            java.util.function.BiFunction<A, B, C> f) {
        return parTuple2(io1, io2).map(t -> f.apply(t._1(), t._2()));
    }

    /**
     * IO のリストを並列に実行し、結果を集約
     */
    public static <A, B> IO<B> parFold(
            List<IO<A>> ios,
            B initial,
            java.util.function.BiFunction<B, A, B> f) {
        return parSequence(ios).map(results -> results.foldLeft(initial, f));
    }

    // ============================================
    // 競争 (Race)
    // ============================================

    /**
     * 2つの IO を並列に実行し、最初に完了した方の結果を返す
     */
    public static <A> IO<A> race(IO<A> io1, IO<A> io2) {
        return IO.delay(() -> {
            CompletableFuture<A> f1 = CompletableFuture.supplyAsync(io1::unsafeRun, executor);
            CompletableFuture<A> f2 = CompletableFuture.supplyAsync(io2::unsafeRun, executor);

            return CompletableFuture.anyOf(f1, f2)
                    .thenApply(result -> {
                        f1.cancel(true);
                        f2.cancel(true);
                        @SuppressWarnings("unchecked")
                        A typedResult = (A) result;
                        return typedResult;
                    })
                    .join();
        });
    }

    // ============================================
    // タイムアウト
    // ============================================

    /**
     * IO にタイムアウトを設定
     */
    public static <A> IO<io.vavr.control.Option<A>> timeout(IO<A> action, long millis) {
        return IO.delay(() -> {
            CompletableFuture<A> future = CompletableFuture.supplyAsync(action::unsafeRun, executor);

            try {
                A result = future.get(millis, java.util.concurrent.TimeUnit.MILLISECONDS);
                return io.vavr.control.Option.some(result);
            } catch (java.util.concurrent.TimeoutException e) {
                future.cancel(true);
                return io.vavr.control.Option.none();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
    }
}
