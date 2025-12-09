package ch09;

import io.vavr.collection.List;
import io.vavr.collection.Queue;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第9章: 遅延評価ストリームのテスト
 */
@DisplayName("第9章: 遅延評価ストリーム")
class LazyStreamTest {

    @Nested
    @DisplayName("ストリームの作成")
    class CreationTest {

        @Test
        @DisplayName("empty は空のストリーム")
        void emptyStream() {
            LazyStream<Integer> stream = LazyStream.empty();
            assertThat(stream.isEmpty()).isTrue();
            assertThat(stream.toList()).isEmpty();
        }

        @Test
        @DisplayName("of で単一要素のストリーム")
        void singleElement() {
            LazyStream<Integer> stream = LazyStream.of(42);
            assertThat(stream.toList()).containsExactly(42);
        }

        @Test
        @DisplayName("of で複数要素のストリーム")
        void multipleElements() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3);
            assertThat(stream.toList()).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("fromList でリストからストリーム")
        void fromList() {
            LazyStream<String> stream = LazyStream.fromList(List.of("a", "b", "c"));
            assertThat(stream.toList()).containsExactly("a", "b", "c");
        }

        @Test
        @DisplayName("range で範囲のストリーム")
        void rangeStream() {
            LazyStream<Integer> stream = LazyStream.range(1, 5);
            assertThat(stream.toList()).containsExactly(1, 2, 3, 4);
        }

        @Test
        @DisplayName("repeat で無限に繰り返す")
        void repeatInfinite() {
            LazyStream<String> stream = LazyStream.repeat("x");
            assertThat(stream.take(5).toList()).containsExactly("x", "x", "x", "x", "x");
        }

        @Test
        @DisplayName("continually で Supplier から生成")
        void continuallyFromSupplier() {
            int[] counter = {0};
            LazyStream<Integer> stream = LazyStream.continually(() -> ++counter[0]);
            assertThat(stream.take(3).toList()).containsExactly(1, 2, 3);
        }
    }

    @Nested
    @DisplayName("ストリームの変換")
    class TransformationTest {

        @Test
        @DisplayName("map で各要素を変換")
        void mapElements() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3).map(x -> x * 2);
            assertThat(stream.toList()).containsExactly(2, 4, 6);
        }

        @Test
        @DisplayName("filter で条件を満たす要素のみ")
        void filterElements() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3, 4, 5).filter(x -> x % 2 == 0);
            assertThat(stream.toList()).containsExactly(2, 4);
        }

        @Test
        @DisplayName("flatMap で展開")
        void flatMapElements() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3)
                    .flatMap(x -> LazyStream.of(x, x * 10));
            assertThat(stream.toList()).containsExactly(1, 10, 2, 20, 3, 30);
        }

        @Test
        @DisplayName("take で最初の n 要素")
        void takeElements() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3, 4, 5).take(3);
            assertThat(stream.toList()).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("take で要素数より多く指定")
        void takeMoreThanSize() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3).take(10);
            assertThat(stream.toList()).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("drop で最初の n 要素をスキップ")
        void dropElements() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3, 4, 5).drop(2);
            assertThat(stream.toList()).containsExactly(3, 4, 5);
        }

        @Test
        @DisplayName("takeWhile で条件を満たす間")
        void takeWhileCondition() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3, 4, 5).takeWhile(x -> x < 4);
            assertThat(stream.toList()).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("dropWhile で条件を満たす間スキップ")
        void dropWhileCondition() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3, 4, 5).dropWhile(x -> x < 3);
            assertThat(stream.toList()).containsExactly(3, 4, 5);
        }

        @Test
        @DisplayName("repeat でストリームを繰り返す")
        void repeatStream() {
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3).repeat().take(8);
            assertThat(stream.toList()).containsExactly(1, 2, 3, 1, 2, 3, 1, 2);
        }

        @Test
        @DisplayName("append で結合")
        void appendStreams() {
            LazyStream<Integer> stream1 = LazyStream.of(1, 2);
            LazyStream<Integer> stream2 = LazyStream.of(3, 4);
            assertThat(stream1.append(stream2).toList()).containsExactly(1, 2, 3, 4);
        }
    }

    @Nested
    @DisplayName("スライディングウィンドウ")
    class SlidingTest {

        @Test
        @DisplayName("sliding でウィンドウを作成")
        void slidingWindow() {
            LazyStream<Queue<Integer>> windows = LazyStream.of(1, 2, 3, 4, 5).sliding(3);
            List<List<Integer>> result = windows.toList().map(Queue::toList);

            assertThat(result).hasSize(3);
            assertThat(result.get(0)).containsExactly(1, 2, 3);
            assertThat(result.get(1)).containsExactly(2, 3, 4);
            assertThat(result.get(2)).containsExactly(3, 4, 5);
        }

        @Test
        @DisplayName("ウィンドウサイズより小さいストリーム")
        void slidingSmallerStream() {
            LazyStream<Queue<Integer>> windows = LazyStream.of(1, 2).sliding(3);
            assertThat(windows.toList()).isEmpty();
        }

        @Test
        @DisplayName("ウィンドウサイズ1")
        void slidingWindowSizeOne() {
            LazyStream<Queue<Integer>> windows = LazyStream.of(1, 2, 3).sliding(1);
            List<List<Integer>> result = windows.toList().map(Queue::toList);

            assertThat(result).hasSize(3);
            assertThat(result.get(0)).containsExactly(1);
            assertThat(result.get(1)).containsExactly(2);
            assertThat(result.get(2)).containsExactly(3);
        }
    }

    @Nested
    @DisplayName("終端操作")
    class TerminalOperationsTest {

        @Test
        @DisplayName("toList でリストに変換")
        void toListConversion() {
            List<Integer> list = LazyStream.of(1, 2, 3).toList();
            assertThat(list).isEqualTo(List.of(1, 2, 3));
        }

        @Test
        @DisplayName("headOption で最初の要素")
        void headOptionFirst() {
            assertThat(LazyStream.of(1, 2, 3).headOption()).contains(1);
            assertThat(LazyStream.<Integer>empty().headOption()).isEmpty();
        }

        @Test
        @DisplayName("lastOption で最後の要素")
        void lastOptionLast() {
            assertThat(LazyStream.of(1, 2, 3).lastOption()).contains(3);
            assertThat(LazyStream.<Integer>empty().lastOption()).isEmpty();
        }

        @Test
        @DisplayName("forAll で全要素が条件を満たすか")
        void forAllPredicate() {
            assertThat(LazyStream.of(2, 4, 6).forAll(x -> x % 2 == 0)).isTrue();
            assertThat(LazyStream.of(2, 3, 6).forAll(x -> x % 2 == 0)).isFalse();
            assertThat(LazyStream.<Integer>empty().forAll(x -> false)).isTrue();
        }

        @Test
        @DisplayName("exists でいずれかの要素が条件を満たすか")
        void existsPredicate() {
            assertThat(LazyStream.of(1, 2, 3).exists(x -> x == 2)).isTrue();
            assertThat(LazyStream.of(1, 2, 3).exists(x -> x == 5)).isFalse();
            assertThat(LazyStream.<Integer>empty().exists(x -> true)).isFalse();
        }

        @Test
        @DisplayName("foldLeft で畳み込み")
        void foldLeftAggregation() {
            int sum = LazyStream.of(1, 2, 3, 4).foldLeft(0, Integer::sum);
            assertThat(sum).isEqualTo(10);

            String concat = LazyStream.of("a", "b", "c").foldLeft("", String::concat);
            assertThat(concat).isEqualTo("abc");
        }

        @Test
        @DisplayName("size で要素数")
        void sizeCount() {
            assertThat(LazyStream.of(1, 2, 3).size()).isEqualTo(3);
            assertThat(LazyStream.empty().size()).isEqualTo(0);
        }

        @Test
        @DisplayName("isEmpty で空かどうか")
        void isEmptyCheck() {
            assertThat(LazyStream.empty().isEmpty()).isTrue();
            assertThat(LazyStream.of(1).isEmpty()).isFalse();
        }
    }

    @Nested
    @DisplayName("遅延評価")
    class LazyEvaluationTest {

        @Test
        @DisplayName("無限ストリームを遅延評価で処理")
        void infiniteStreamLazy() {
            // 無限ストリームを作成しても take で有限にすれば処理可能
            LazyStream<Integer> infinite = LazyStream.range(1, Integer.MAX_VALUE);
            List<Integer> result = infinite.take(5).toList();
            assertThat(result).containsExactly(1, 2, 3, 4, 5);
        }

        @Test
        @DisplayName("filter と take の組み合わせ")
        void filterTakeCombination() {
            // 無限ストリームから条件を満たす最初の3つを取得
            LazyStream<Integer> stream = LazyStream.range(1, Integer.MAX_VALUE)
                    .filter(x -> x % 7 == 0)
                    .take(3);
            assertThat(stream.toList()).containsExactly(7, 14, 21);
        }

        @Test
        @DisplayName("map は遅延評価される")
        void mapIsLazy() {
            int[] counter = {0};
            LazyStream<Integer> stream = LazyStream.of(1, 2, 3, 4, 5)
                    .map(x -> {
                        counter[0]++;
                        return x * 2;
                    });

            // まだ map は実行されていない
            assertThat(counter[0]).isEqualTo(0);

            // take(2) で最初の2要素のみ評価
            stream.take(2).toList();
            assertThat(counter[0]).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("Iterator")
    class IteratorTest {

        @Test
        @DisplayName("for-each で反復")
        void forEachIteration() {
            int sum = 0;
            for (int n : LazyStream.of(1, 2, 3)) {
                sum += n;
            }
            assertThat(sum).isEqualTo(6);
        }
    }
}
