package ch11;

import ch08.IO;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * 第11章: Resource - 安全なリソース管理
 *
 * リソースの取得と解放を安全に行うための型です。
 * cats-effect の Resource に相当する機能を提供します。
 *
 * @param <A> リソースの型
 */
public final class Resource<A> {

    private final IO<A> acquire;
    private final Consumer<A> release;

    private Resource(IO<A> acquire, Consumer<A> release) {
        this.acquire = acquire;
        this.release = release;
    }

    // ============================================
    // Resource の作成
    // ============================================

    /**
     * リソースを作成
     *
     * @param acquire 取得処理
     * @param release 解放処理
     */
    public static <A> Resource<A> make(IO<A> acquire, Consumer<A> release) {
        return new Resource<>(acquire, release);
    }

    /**
     * リソースを作成（解放処理が IO を返す場合）
     */
    public static <A> Resource<A> makeIO(IO<A> acquire, Function<A, IO<Void>> release) {
        return new Resource<>(acquire, a -> release.apply(a).unsafeRun());
    }

    /**
     * Supplier からリソースを作成
     */
    public static <A> Resource<A> fromSupplier(Supplier<A> acquire, Consumer<A> release) {
        return new Resource<>(IO.delay(acquire), release);
    }

    /**
     * AutoCloseable からリソースを作成
     */
    public static <A extends AutoCloseable> Resource<A> fromAutoCloseable(IO<A> acquire) {
        return new Resource<>(acquire, a -> {
            try {
                a.close();
            } catch (Exception e) {
                throw new RuntimeException("Failed to close resource", e);
            }
        });
    }

    /**
     * AutoCloseable からリソースを作成（Supplier 版）
     */
    public static <A extends AutoCloseable> Resource<A> fromAutoCloseable(Supplier<A> acquire) {
        return fromAutoCloseable(IO.delay(acquire));
    }

    // ============================================
    // Resource の使用
    // ============================================

    /**
     * リソースを使用して処理を実行
     * リソースは処理完了後（成功・失敗問わず）に必ず解放される
     *
     * @param f リソースを使用する処理
     * @return 処理結果（IO でラップ）
     */
    public <B> IO<B> use(Function<A, IO<B>> f) {
        return acquire.flatMap(resource ->
                f.apply(resource)
                        .guarantee(IO.effect(() -> release.accept(resource)))
        );
    }

    /**
     * リソースを使用して同期処理を実行
     */
    public <B> IO<B> useSync(Function<A, B> f) {
        return use(resource -> IO.delay(() -> f.apply(resource)));
    }

    // ============================================
    // Resource の変換
    // ============================================

    /**
     * リソースを変換
     */
    public <B> Resource<B> map(Function<A, B> f) {
        // 変換後のリソースは、元のリソースの解放処理を引き継ぐ
        // ただし、変換後の値に対しては何も解放しない
        return new Resource<>(
                acquire.map(f),
                b -> {} // 変換後の値は解放不要
        );
    }

    /**
     * リソースを合成
     */
    public <B> Resource<B> flatMap(Function<A, Resource<B>> f) {
        return new Resource<>(
                acquire.flatMap(a -> {
                    Resource<B> resourceB = f.apply(a);
                    return resourceB.acquire;
                }),
                b -> {} // flatMap で作成されたリソースは use 時に適切に解放される
        );
    }

    // ============================================
    // ユーティリティ
    // ============================================

    /**
     * 複数のリソースを組み合わせる
     */
    public static <A, B> Resource<io.vavr.Tuple2<A, B>> both(
            Resource<A> ra,
            Resource<B> rb) {
        return new Resource<>(
                ra.acquire.flatMap(a ->
                        rb.acquire.map(b -> io.vavr.Tuple.of(a, b))
                ),
                tuple -> {
                    try {
                        rb.release.accept(tuple._2());
                    } finally {
                        ra.release.accept(tuple._1());
                    }
                }
        );
    }

    /**
     * 解放処理なしのリソースを作成
     */
    public static <A> Resource<A> pure(A value) {
        return new Resource<>(IO.pure(value), a -> {});
    }

    /**
     * IO からリソースを作成（解放処理なし）
     */
    public static <A> Resource<A> eval(IO<A> io) {
        return new Resource<>(io, a -> {});
    }
}
