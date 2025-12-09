package ch09;

import ch08.IO;
import io.vavr.collection.List;
import io.vavr.collection.Queue;
import io.vavr.control.Option;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * 第9章: 遅延評価ストリーム
 *
 * 潜在的に無限のシーケンスを遅延評価で処理するためのストリーム実装。
 * fs2 ライブラリの Stream に相当する機能を提供。
 *
 * @param <A> 要素の型
 */
public final class LazyStream<A> implements Iterable<A> {

    private final Supplier<Option<Cons<A>>> headTail;

    private record Cons<A>(A head, LazyStream<A> tail) {}

    private LazyStream(Supplier<Option<Cons<A>>> headTail) {
        this.headTail = headTail;
    }

    // ============================================
    // ストリームの作成
    // ============================================

    /**
     * 空のストリーム
     */
    public static <A> LazyStream<A> empty() {
        return new LazyStream<>(Option::none);
    }

    /**
     * 単一要素のストリーム
     */
    public static <A> LazyStream<A> of(A value) {
        return new LazyStream<>(() -> Option.some(new Cons<>(value, empty())));
    }

    /**
     * 複数要素のストリーム
     */
    @SafeVarargs
    public static <A> LazyStream<A> of(A... values) {
        return fromList(List.of(values));
    }

    /**
     * リストからストリームを作成
     */
    public static <A> LazyStream<A> fromList(List<A> list) {
        if (list.isEmpty()) {
            return empty();
        }
        return new LazyStream<>(() ->
                Option.some(new Cons<>(list.head(), fromList(list.tail()))));
    }

    /**
     * 範囲のストリーム
     */
    public static LazyStream<Integer> range(int start, int endExclusive) {
        if (start >= endExclusive) {
            return empty();
        }
        return new LazyStream<>(() ->
                Option.some(new Cons<>(start, range(start + 1, endExclusive))));
    }

    /**
     * 無限に値を繰り返すストリーム
     */
    public static <A> LazyStream<A> repeat(A value) {
        return new LazyStream<>(() -> Option.some(new Cons<>(value, repeat(value))));
    }

    /**
     * Supplier を使って無限ストリームを生成
     */
    public static <A> LazyStream<A> continually(Supplier<A> supplier) {
        return new LazyStream<>(() -> Option.some(new Cons<>(supplier.get(), continually(supplier))));
    }

    /**
     * IO を含むストリームを生成
     */
    public static <A> LazyStream<IO<A>> eval(IO<A> io) {
        return of(io);
    }

    // ============================================
    // ストリームの変換
    // ============================================

    /**
     * 各要素に関数を適用
     */
    public <B> LazyStream<B> map(Function<A, B> f) {
        return new LazyStream<>(() -> headTail.get()
                .map(cons -> new Cons<>(f.apply(cons.head()), cons.tail().map(f))));
    }

    /**
     * 条件を満たす要素のみを抽出
     */
    public LazyStream<A> filter(Predicate<A> p) {
        return new LazyStream<>(() -> {
            Option<Cons<A>> current = headTail.get();
            while (current.isDefined()) {
                Cons<A> cons = current.get();
                if (p.test(cons.head())) {
                    return Option.some(new Cons<>(cons.head(), cons.tail().filter(p)));
                }
                current = cons.tail().headTail.get();
            }
            return Option.none();
        });
    }

    /**
     * flatMap
     */
    public <B> LazyStream<B> flatMap(Function<A, LazyStream<B>> f) {
        return new LazyStream<>(() -> headTail.get()
                .flatMap(cons -> {
                    LazyStream<B> mapped = f.apply(cons.head());
                    LazyStream<B> rest = cons.tail().flatMap(f);
                    return mapped.append(rest).headTail.get();
                }));
    }

    /**
     * 最初の n 要素を取得
     */
    public LazyStream<A> take(int n) {
        if (n <= 0) {
            return empty();
        }
        return new LazyStream<>(() -> headTail.get()
                .map(cons -> new Cons<>(cons.head(), cons.tail().take(n - 1))));
    }

    /**
     * 最初の n 要素をスキップ
     */
    public LazyStream<A> drop(int n) {
        if (n <= 0) {
            return this;
        }
        return new LazyStream<>(() -> {
            Option<Cons<A>> current = headTail.get();
            int remaining = n;
            while (current.isDefined() && remaining > 0) {
                current = current.get().tail().headTail.get();
                remaining--;
            }
            return current;
        });
    }

    /**
     * 条件を満たす間だけ要素を取得
     */
    public LazyStream<A> takeWhile(Predicate<A> p) {
        return new LazyStream<>(() -> headTail.get()
                .filter(cons -> p.test(cons.head()))
                .map(cons -> new Cons<>(cons.head(), cons.tail().takeWhile(p))));
    }

    /**
     * 条件を満たす間だけ要素をスキップ
     */
    public LazyStream<A> dropWhile(Predicate<A> p) {
        return new LazyStream<>(() -> {
            Option<Cons<A>> current = headTail.get();
            while (current.isDefined() && p.test(current.get().head())) {
                current = current.get().tail().headTail.get();
            }
            return current;
        });
    }

    /**
     * 無限に繰り返す
     */
    public LazyStream<A> repeat() {
        LazyStream<A> self = this;
        return new LazyStream<>(() -> headTail.get()
                .map(cons -> {
                    LazyStream<A> restThenRepeat = cons.tail().append(self.repeat());
                    return new Cons<>(cons.head(), restThenRepeat);
                }));
    }

    /**
     * 別のストリームを結合
     */
    public LazyStream<A> append(LazyStream<A> other) {
        return new LazyStream<>(() -> headTail.get()
                .map(cons -> new Cons<>(cons.head(), cons.tail().append(other)))
                .orElse(other.headTail));
    }

    /**
     * スライディングウィンドウ
     */
    public LazyStream<Queue<A>> sliding(int windowSize) {
        return slidingHelper(Queue.empty(), windowSize);
    }

    private LazyStream<Queue<A>> slidingHelper(Queue<A> window, int windowSize) {
        return new LazyStream<>(() -> headTail.get()
                .flatMap(cons -> {
                    Queue<A> newWindow = window.enqueue(cons.head());
                    if (newWindow.size() < windowSize) {
                        return cons.tail().slidingHelper(newWindow, windowSize).headTail.get();
                    } else if (newWindow.size() == windowSize) {
                        return Option.some(new Cons<>(newWindow,
                                cons.tail().slidingHelper(newWindow.dequeue()._2(), windowSize)));
                    } else {
                        Queue<A> trimmed = newWindow.dequeue()._2();
                        return Option.some(new Cons<>(trimmed,
                                cons.tail().slidingHelper(trimmed.dequeue()._2(), windowSize)));
                    }
                }));
    }

    // ============================================
    // ストリームの終端操作
    // ============================================

    /**
     * リストに変換
     */
    public List<A> toList() {
        List<A> result = List.empty();
        for (A a : this) {
            result = result.append(a);
        }
        return result;
    }

    /**
     * 最初の要素を取得
     */
    public Option<A> headOption() {
        return headTail.get().map(Cons::head);
    }

    /**
     * 最後の要素を取得
     */
    public Option<A> lastOption() {
        Option<A> result = Option.none();
        for (A a : this) {
            result = Option.some(a);
        }
        return result;
    }

    /**
     * すべての要素が条件を満たすか
     */
    public boolean forAll(Predicate<A> p) {
        for (A a : this) {
            if (!p.test(a)) {
                return false;
            }
        }
        return true;
    }

    /**
     * いずれかの要素が条件を満たすか
     */
    public boolean exists(Predicate<A> p) {
        for (A a : this) {
            if (p.test(a)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 畳み込み
     */
    public <B> B foldLeft(B initial, java.util.function.BiFunction<B, A, B> f) {
        B result = initial;
        for (A a : this) {
            result = f.apply(result, a);
        }
        return result;
    }

    /**
     * 要素数を数える
     */
    public int size() {
        return foldLeft(0, (acc, elem) -> acc + 1);
    }

    /**
     * ストリームが空かどうか
     */
    public boolean isEmpty() {
        return headTail.get().isEmpty();
    }

    // ============================================
    // Iterator の実装
    // ============================================

    @Override
    public Iterator<A> iterator() {
        return new Iterator<>() {
            private Option<Cons<A>> current = headTail.get();

            @Override
            public boolean hasNext() {
                return current.isDefined();
            }

            @Override
            public A next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                Cons<A> cons = current.get();
                current = cons.tail().headTail.get();
                return cons.head();
            }
        };
    }

    @Override
    public String toString() {
        return "LazyStream(...)";
    }
}
