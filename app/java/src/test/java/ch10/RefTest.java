package ch10;

import ch08.IO;
import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第10章: Ref のテスト
 */
@DisplayName("第10章: Ref（アトミック参照）")
class RefTest {

    @Nested
    @DisplayName("Ref の作成")
    class CreationTest {

        @Test
        @DisplayName("初期値で Ref を作成")
        void createWithInitialValue() {
            IO<Integer> program = Ref.of(42).flatMap(Ref::get);
            assertThat(program.unsafeRun()).isEqualTo(42);
        }

        @Test
        @DisplayName("unsafe で直接作成")
        void unsafeCreate() {
            Ref<String> ref = Ref.unsafe("hello");
            assertThat(ref.unsafeGet()).isEqualTo("hello");
        }
    }

    @Nested
    @DisplayName("値の取得と設定")
    class GetSetTest {

        @Test
        @DisplayName("get で値を取得")
        void getValue() {
            Ref<Integer> ref = Ref.unsafe(10);
            assertThat(ref.get().unsafeRun()).isEqualTo(10);
        }

        @Test
        @DisplayName("set で値を設定")
        void setValue() {
            Ref<Integer> ref = Ref.unsafe(10);
            ref.set(20).unsafeRun();
            assertThat(ref.unsafeGet()).isEqualTo(20);
        }
    }

    @Nested
    @DisplayName("アトミック更新")
    class UpdateTest {

        @Test
        @DisplayName("update でアトミックに更新")
        void updateAtomically() {
            Ref<Integer> ref = Ref.unsafe(10);
            ref.update(x -> x + 5).unsafeRun();
            assertThat(ref.unsafeGet()).isEqualTo(15);
        }

        @Test
        @DisplayName("getAndUpdate で古い値を取得")
        void getAndUpdate() {
            Ref<Integer> ref = Ref.unsafe(10);
            Integer oldValue = ref.getAndUpdate(x -> x * 2).unsafeRun();
            assertThat(oldValue).isEqualTo(10);
            assertThat(ref.unsafeGet()).isEqualTo(20);
        }

        @Test
        @DisplayName("updateAndGet で新しい値を取得")
        void updateAndGet() {
            Ref<Integer> ref = Ref.unsafe(10);
            Integer newValue = ref.updateAndGet(x -> x * 2).unsafeRun();
            assertThat(newValue).isEqualTo(20);
            assertThat(ref.unsafeGet()).isEqualTo(20);
        }

        @Test
        @DisplayName("modify で更新して結果を返す")
        void modifyAndReturn() {
            Ref<Integer> ref = Ref.unsafe(10);
            String result = ref.modify(x ->
                    Ref.ModifyResult.of(x + 1, "was " + x)
            ).unsafeRun();

            assertThat(result).isEqualTo("was 10");
            assertThat(ref.unsafeGet()).isEqualTo(11);
        }
    }

    @Nested
    @DisplayName("チェーン操作")
    class ChainTest {

        @Test
        @DisplayName("複数の更新をチェーン")
        void chainUpdates() {
            IO<Integer> program = Ref.of(0)
                    .flatMap(ref ->
                            ref.update(x -> x + 1)
                                    .andThen(ref.update(x -> x + 1))
                                    .andThen(ref.update(x -> x + 1))
                                    .andThen(ref.get())
                    );

            assertThat(program.unsafeRun()).isEqualTo(3);
        }

        @Test
        @DisplayName("flatMap でチェーン")
        void flatMapChain() {
            IO<Integer> program = Ref.of(1)
                    .flatMap(ref ->
                            ref.update(x -> x * 2)
                                    .flatMap(ignored -> ref.update(x -> x * 2))
                                    .flatMap(ignored -> ref.update(x -> x * 2))
                                    .flatMap(ignored -> ref.get())
                    );

            assertThat(program.unsafeRun()).isEqualTo(8);  // 1 * 2 * 2 * 2
        }
    }

    @Nested
    @DisplayName("リスト操作")
    class ListRefTest {

        @Test
        @DisplayName("リストに要素を追加")
        void appendToList() {
            Ref<List<String>> ref = Ref.unsafe(List.empty());

            ref.update(list -> list.append("a")).unsafeRun();
            ref.update(list -> list.append("b")).unsafeRun();
            ref.update(list -> list.append("c")).unsafeRun();

            assertThat(ref.unsafeGet()).containsExactly("a", "b", "c");
        }

        @Test
        @DisplayName("リストに要素を先頭に追加")
        void prependToList() {
            Ref<List<Integer>> ref = Ref.unsafe(List.empty());

            ref.update(list -> list.prepend(1)).unsafeRun();
            ref.update(list -> list.prepend(2)).unsafeRun();
            ref.update(list -> list.prepend(3)).unsafeRun();

            assertThat(ref.unsafeGet()).containsExactly(3, 2, 1);
        }
    }

    @Nested
    @DisplayName("マップ操作")
    class MapRefTest {

        @Test
        @DisplayName("マップに要素を追加")
        void putToMap() {
            Ref<io.vavr.collection.Map<String, Integer>> ref =
                    Ref.unsafe(io.vavr.collection.HashMap.empty());

            ref.update(map -> map.put("a", 1)).unsafeRun();
            ref.update(map -> map.put("b", 2)).unsafeRun();

            io.vavr.collection.Map<String, Integer> result = ref.unsafeGet();
            assertThat(result.get("a").get()).isEqualTo(1);
            assertThat(result.get("b").get()).isEqualTo(2);
        }

        @Test
        @DisplayName("マップの値を更新")
        void updateMapValue() {
            Ref<io.vavr.collection.Map<String, Integer>> ref =
                    Ref.unsafe(io.vavr.collection.HashMap.of("count", 0));

            ref.update(map ->
                    map.put("count", map.get("count").getOrElse(0) + 1)
            ).unsafeRun();
            ref.update(map ->
                    map.put("count", map.get("count").getOrElse(0) + 1)
            ).unsafeRun();

            assertThat(ref.unsafeGet().get("count").get()).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("toString")
    class ToStringTest {

        @Test
        @DisplayName("toString で値を表示")
        void toStringShowsValue() {
            Ref<Integer> ref = Ref.unsafe(42);
            assertThat(ref.toString()).isEqualTo("Ref(42)");
        }
    }
}
