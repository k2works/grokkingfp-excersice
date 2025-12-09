package ch10;

import ch08.IO;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * 第10章: Fiber - 軽量スレッド
 *
 * IO の実行をバックグラウンドで行う軽量な実行単位です。
 * cats-effect の Fiber に相当する機能を提供します。
 *
 * @param <A> 結果の型
 */
public final class Fiber<A> {

    private static final ExecutorService executor = Executors.newVirtualThreadPerTaskExecutor();

    private final CompletableFuture<A> future;
    private final AtomicBoolean cancelled;
    private final Future<?> task;

    private Fiber(CompletableFuture<A> future, AtomicBoolean cancelled, Future<?> task) {
        this.future = future;
        this.cancelled = cancelled;
        this.task = task;
    }

    // ============================================
    // Fiber の起動
    // ============================================

    /**
     * IO をバックグラウンドで起動し、Fiber を返す
     */
    public static <A> IO<Fiber<A>> start(IO<A> io) {
        return IO.delay(() -> {
            CompletableFuture<A> future = new CompletableFuture<>();
            AtomicBoolean cancelled = new AtomicBoolean(false);

            Future<?> task = executor.submit(() -> {
                try {
                    if (!cancelled.get()) {
                        A result = io.unsafeRun();
                        if (!cancelled.get()) {
                            future.complete(result);
                        }
                    }
                } catch (Exception e) {
                    if (!cancelled.get()) {
                        future.completeExceptionally(e);
                    }
                }
            });

            return new Fiber<>(future, cancelled, task);
        });
    }

    /**
     * 無限ループする IO をバックグラウンドで起動
     * キャンセルされるまで繰り返し実行する
     */
    public static IO<Fiber<Void>> startForever(IO<Void> io) {
        return IO.delay(() -> {
            CompletableFuture<Void> future = new CompletableFuture<>();
            AtomicBoolean cancelled = new AtomicBoolean(false);

            Future<?> task = executor.submit(() -> {
                try {
                    while (!cancelled.get()) {
                        io.unsafeRun();
                    }
                    future.complete(null);
                } catch (Exception e) {
                    if (!cancelled.get()) {
                        future.completeExceptionally(e);
                    }
                }
            });

            return new Fiber<>(future, cancelled, task);
        });
    }

    // ============================================
    // Fiber の操作
    // ============================================

    /**
     * Fiber をキャンセル
     */
    public IO<Void> cancel() {
        return IO.effect(() -> {
            cancelled.set(true);
            task.cancel(true);
        });
    }

    /**
     * Fiber の完了を待機し、結果を取得
     */
    public IO<A> join() {
        return IO.delay(() -> {
            try {
                return future.get();
            } catch (Exception e) {
                throw new RuntimeException("Fiber failed", e);
            }
        });
    }

    /**
     * Fiber がキャンセルされたかどうか
     */
    public boolean isCancelled() {
        return cancelled.get();
    }

    /**
     * Fiber が完了したかどうか
     */
    public boolean isDone() {
        return future.isDone();
    }

    // ============================================
    // ユーティリティ
    // ============================================

    /**
     * 指定時間スリープ（Fiber フレンドリー）
     */
    public static IO<Void> sleep(long millis) {
        return IO.delay(() -> {
            try {
                Thread.sleep(millis);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            return null;
        });
    }

    /**
     * 指定時間後にアクションを実行
     */
    public static <A> IO<A> delay(long millis, IO<A> action) {
        return sleep(millis).andThen(action);
    }

    @Override
    public String toString() {
        String status = cancelled.get() ? "cancelled" : (future.isDone() ? "done" : "running");
        return "Fiber(" + status + ")";
    }
}
