package ch10;

import ch08.IO;

import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.UnaryOperator;

/**
 * 第10章: Ref - アトミックな共有状態
 *
 * 複数の並行処理から安全にアクセスできるアトミックな参照です。
 * cats-effect の Ref に相当する機能を提供します。
 *
 * @param <A> 保持する値の型
 */
public final class Ref<A> {

    private final AtomicReference<A> value;

    private Ref(A initial) {
        this.value = new AtomicReference<>(initial);
    }

    // ============================================
    // Ref の作成
    // ============================================

    /**
     * 初期値で Ref を作成（IO でラップ）
     */
    public static <A> IO<Ref<A>> of(A initial) {
        return IO.delay(() -> new Ref<>(initial));
    }

    /**
     * 初期値で Ref を直接作成（テスト用）
     */
    public static <A> Ref<A> unsafe(A initial) {
        return new Ref<>(initial);
    }

    // ============================================
    // 値の取得
    // ============================================

    /**
     * 現在の値を取得（IO でラップ）
     */
    public IO<A> get() {
        return IO.delay(value::get);
    }

    /**
     * 現在の値を直接取得（テスト用）
     */
    public A unsafeGet() {
        return value.get();
    }

    // ============================================
    // 値の設定
    // ============================================

    /**
     * 値を設定（IO でラップ）
     */
    public IO<Void> set(A newValue) {
        return IO.effect(() -> value.set(newValue));
    }

    /**
     * 値を直接設定（テスト用）
     */
    public void unsafeSet(A newValue) {
        value.set(newValue);
    }

    // ============================================
    // アトミックな更新
    // ============================================

    /**
     * アトミックに値を更新（IO でラップ）
     */
    public IO<Void> update(UnaryOperator<A> f) {
        return IO.effect(() -> value.updateAndGet(f));
    }

    /**
     * アトミックに値を更新し、古い値を返す
     */
    public IO<A> getAndUpdate(UnaryOperator<A> f) {
        return IO.delay(() -> value.getAndUpdate(f));
    }

    /**
     * アトミックに値を更新し、新しい値を返す
     */
    public IO<A> updateAndGet(UnaryOperator<A> f) {
        return IO.delay(() -> value.updateAndGet(f));
    }

    /**
     * アトミックに値を更新し、結果を返す
     *
     * @param f 現在の値を受け取り、(新しい値, 戻り値) のペアを返す関数
     */
    public <B> IO<B> modify(Function<A, ModifyResult<A, B>> f) {
        return IO.delay(() -> {
            while (true) {
                A current = value.get();
                ModifyResult<A, B> result = f.apply(current);
                if (value.compareAndSet(current, result.newValue())) {
                    return result.returnValue();
                }
                // CAS failed, retry
            }
        });
    }

    /**
     * modify の結果を表すレコード
     */
    public record ModifyResult<A, B>(A newValue, B returnValue) {
        public static <A, B> ModifyResult<A, B> of(A newValue, B returnValue) {
            return new ModifyResult<>(newValue, returnValue);
        }
    }

    // ============================================
    // ユーティリティ
    // ============================================

    /**
     * 値を直接更新（テスト用）
     */
    public void unsafeUpdate(UnaryOperator<A> f) {
        value.updateAndGet(f);
    }

    @Override
    public String toString() {
        return "Ref(" + value.get() + ")";
    }
}
