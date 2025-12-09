package ch05;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第5章: 円内の点のテスト
 */
@DisplayName("第5章: 円内の点")
class PointsInsideCirclesTest {

    private final List<PointsInsideCircles.Point> points = List.of(
            new PointsInsideCircles.Point(0, 0),
            new PointsInsideCircles.Point(1, 1),
            new PointsInsideCircles.Point(3, 3),
            new PointsInsideCircles.Point(5, 2)
    );

    @Nested
    @DisplayName("isInside")
    class IsInsideTest {

        @Test
        @DisplayName("原点は全ての円に含まれる")
        void originInside() {
            PointsInsideCircles.Point origin = new PointsInsideCircles.Point(0, 0);
            assertThat(PointsInsideCircles.isInside(origin, 1)).isTrue();
            assertThat(PointsInsideCircles.isInside(origin, 0)).isTrue();
        }

        @Test
        @DisplayName("円の境界上の点")
        void pointOnBoundary() {
            PointsInsideCircles.Point point = new PointsInsideCircles.Point(1, 0);
            assertThat(PointsInsideCircles.isInside(point, 1)).isTrue();
        }

        @Test
        @DisplayName("円の外の点")
        void pointOutside() {
            PointsInsideCircles.Point point = new PointsInsideCircles.Point(5, 2);
            assertThat(PointsInsideCircles.isInside(point, 2)).isFalse();
            assertThat(PointsInsideCircles.isInside(point, 5)).isFalse();
        }

        @Test
        @DisplayName("(1, 1) は半径2の円に含まれる")
        void point11InRadius2() {
            PointsInsideCircles.Point point = new PointsInsideCircles.Point(1, 1);
            // 1*1 + 1*1 = 2 <= 2*2 = 4
            assertThat(PointsInsideCircles.isInside(point, 2)).isTrue();
        }
    }

    @Nested
    @DisplayName("allCombinations")
    class AllCombinationsTest {

        @Test
        @DisplayName("全組み合わせを生成")
        void generateAllCombinations() {
            List<PointsInsideCircles.Point> testPoints = List.of(
                    new PointsInsideCircles.Point(1, 1),
                    new PointsInsideCircles.Point(5, 2)
            );
            List<Integer> radiuses = List.of(2, 5);

            List<String> combinations = PointsInsideCircles.allCombinations(testPoints, radiuses);

            assertThat(combinations).hasSize(4);
            assertThat(combinations.get(0)).contains("(1, 1)").contains("radius 2");
        }
    }

    @Nested
    @DisplayName("pointsInsideCircle")
    class PointsInsideCircleTest {

        @Test
        @DisplayName("円内の点のみを取得")
        void getPointsInside() {
            List<String> inside = PointsInsideCircles.pointsInsideCircle(points, List.of(2));
            // (0, 0) と (1, 1) が半径2の円に含まれる
            assertThat(inside).hasSize(2);
        }
    }

    @Nested
    @DisplayName("countPointsPerRadius")
    class CountPointsPerRadiusTest {

        @Test
        @DisplayName("各半径の点の数をカウント")
        void countPerRadius() {
            List<String> counts = PointsInsideCircles.countPointsPerRadius(points, List.of(2, 5, 10));
            assertThat(counts).hasSize(3);
            assertThat(counts.get(0)).contains("Radius 2").contains("2 points");
            assertThat(counts.get(1)).contains("Radius 5").contains("3 points");
            assertThat(counts.get(2)).contains("Radius 10").contains("4 points");
        }
    }

    @Nested
    @DisplayName("generatePairs")
    class GeneratePairsTest {

        @Test
        @DisplayName("点と半径のペアを生成")
        void generatePairs() {
            List<PointsInsideCircles.Point> testPoints = List.of(
                    new PointsInsideCircles.Point(1, 1)
            );
            List<Integer> radiuses = List.of(2, 3);

            List<String> pairs = PointsInsideCircles.generatePairs(testPoints, radiuses);

            assertThat(pairs).containsExactly(
                    "Point (1, 1), Radius 2",
                    "Point (1, 1), Radius 3"
            );
        }
    }

    @Nested
    @DisplayName("pointsInAnyCircle")
    class PointsInAnyCircleTest {

        @Test
        @DisplayName("いずれかの円に含まれる点を取得")
        void getPointsInAny() {
            List<PointsInsideCircles.Point> inAny =
                    PointsInsideCircles.pointsInAnyCircle(points, List.of(2, 5));
            // (0, 0), (1, 1), (3, 3) が半径5の円に含まれる
            assertThat(inAny).hasSize(3);
        }

        @Test
        @DisplayName("どの円にも含まれない点は除外")
        void excludePointsOutside() {
            List<PointsInsideCircles.Point> inAny =
                    PointsInsideCircles.pointsInAnyCircle(points, List.of(1));
            // (0, 0) のみ
            assertThat(inAny).hasSize(1);
            assertThat(inAny.get(0).x()).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("pointsInAllCircles")
    class PointsInAllCirclesTest {

        @Test
        @DisplayName("すべての円に含まれる点を取得")
        void getPointsInAll() {
            List<PointsInsideCircles.Point> inAll =
                    PointsInsideCircles.pointsInAllCircles(points, List.of(5, 10));
            // (0, 0), (1, 1), (3, 3) が両方に含まれる
            assertThat(inAll).hasSize(3);
        }

        @Test
        @DisplayName("小さい円に制限される")
        void constrainedBySmallCircle() {
            List<PointsInsideCircles.Point> inAll =
                    PointsInsideCircles.pointsInAllCircles(points, List.of(2, 10));
            // (0, 0), (1, 1) のみが半径2に含まれる
            assertThat(inAll).hasSize(2);
        }
    }
}
