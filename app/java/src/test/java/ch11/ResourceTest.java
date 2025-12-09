package ch11;

import ch08.IO;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 第11章: Resource のテスト
 */
@DisplayName("第11章: Resource")
class ResourceTest {

    @Nested
    @DisplayName("Resource の作成と使用")
    class BasicUsageTest {

        @Test
        @DisplayName("use でリソースを取得して使用")
        void useAcquiresAndReleasesResource() {
            AtomicBoolean released = new AtomicBoolean(false);

            Resource<String> resource = Resource.make(
                    IO.pure("hello"),
                    s -> released.set(true)
            );

            String result = resource.use(s -> IO.pure(s.toUpperCase())).unsafeRun();

            assertThat(result).isEqualTo("HELLO");
            assertThat(released.get()).isTrue();
        }

        @Test
        @DisplayName("処理が失敗してもリソースは解放される")
        void resourceReleasedOnFailure() {
            AtomicBoolean released = new AtomicBoolean(false);

            Resource<String> resource = Resource.make(
                    IO.pure("hello"),
                    s -> released.set(true)
            );

            assertThatThrownBy(() ->
                    resource.use(s -> IO.delay(() -> {
                        throw new RuntimeException("error");
                    })).unsafeRun()
            ).isInstanceOf(RuntimeException.class);

            assertThat(released.get()).isTrue();
        }

        @Test
        @DisplayName("useSync で同期処理を実行")
        void useSyncExecutesSynchronously() {
            AtomicInteger counter = new AtomicInteger(0);

            Resource<Integer> resource = Resource.make(
                    IO.delay(counter::incrementAndGet),
                    i -> counter.decrementAndGet()
            );

            Integer result = resource.useSync(i -> i * 2).unsafeRun();

            assertThat(result).isEqualTo(2);
            assertThat(counter.get()).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("AutoCloseable のサポート")
    class AutoCloseableTest {

        @Test
        @DisplayName("AutoCloseable リソースは自動的に close される")
        void autoCloseableIsClosed() {
            AtomicBoolean closed = new AtomicBoolean(false);

            AutoCloseable closeable = () -> closed.set(true);

            Resource<AutoCloseable> resource = Resource.fromAutoCloseable(IO.pure(closeable));

            resource.use(c -> IO.pure("done")).unsafeRun();

            assertThat(closed.get()).isTrue();
        }

        @Test
        @DisplayName("Supplier から AutoCloseable を作成")
        void autoCloseableFromSupplier() {
            AtomicBoolean closed = new AtomicBoolean(false);

            Resource<AutoCloseable> resource = Resource.fromAutoCloseable(
                    () -> () -> closed.set(true)
            );

            resource.use(c -> IO.pure("done")).unsafeRun();

            assertThat(closed.get()).isTrue();
        }
    }

    @Nested
    @DisplayName("Resource の変換")
    class TransformationTest {

        @Test
        @DisplayName("map でリソースを変換")
        void mapTransformsResource() {
            Resource<Integer> intResource = Resource.make(
                    IO.pure(42),
                    i -> {}
            );

            Resource<String> stringResource = intResource.map(String::valueOf);

            String result = stringResource.use(s -> IO.pure(s + "!")).unsafeRun();

            assertThat(result).isEqualTo("42!");
        }

        @Test
        @DisplayName("flatMap でリソースを合成")
        void flatMapComposesResources() {
            AtomicInteger releaseOrder = new AtomicInteger(0);
            AtomicInteger firstReleased = new AtomicInteger(-1);
            AtomicInteger secondReleased = new AtomicInteger(-1);

            Resource<String> first = Resource.make(
                    IO.pure("first"),
                    s -> firstReleased.set(releaseOrder.incrementAndGet())
            );

            Resource<String> combined = first.flatMap(f ->
                    Resource.make(
                            IO.pure(f + "+second"),
                            s -> secondReleased.set(releaseOrder.incrementAndGet())
                    )
            );

            String result = combined.use(s -> IO.pure(s)).unsafeRun();

            assertThat(result).isEqualTo("first+second");
        }
    }

    @Nested
    @DisplayName("複数リソースの組み合わせ")
    class MultipleResourcesTest {

        @Test
        @DisplayName("both で2つのリソースを組み合わせる")
        void bothCombinesTwoResources() {
            AtomicBoolean firstReleased = new AtomicBoolean(false);
            AtomicBoolean secondReleased = new AtomicBoolean(false);

            Resource<Integer> first = Resource.make(
                    IO.pure(1),
                    i -> firstReleased.set(true)
            );

            Resource<String> second = Resource.make(
                    IO.pure("two"),
                    s -> secondReleased.set(true)
            );

            var result = Resource.both(first, second)
                    .use(tuple -> IO.pure(tuple._1() + tuple._2()))
                    .unsafeRun();

            assertThat(result).isEqualTo("1two");
            assertThat(firstReleased.get()).isTrue();
            assertThat(secondReleased.get()).isTrue();
        }
    }

    @Nested
    @DisplayName("ユーティリティ")
    class UtilityTest {

        @Test
        @DisplayName("pure は解放処理なしのリソースを作成")
        void pureCreatesNoReleaseResource() {
            String result = Resource.pure("hello")
                    .use(s -> IO.pure(s.toUpperCase()))
                    .unsafeRun();

            assertThat(result).isEqualTo("HELLO");
        }

        @Test
        @DisplayName("eval は IO からリソースを作成")
        void evalCreatesResourceFromIO() {
            AtomicInteger counter = new AtomicInteger(0);

            String result = Resource.eval(IO.delay(() -> {
                        counter.incrementAndGet();
                        return "computed";
                    }))
                    .use(s -> IO.pure(s))
                    .unsafeRun();

            assertThat(result).isEqualTo("computed");
            assertThat(counter.get()).isEqualTo(1);
        }
    }
}
