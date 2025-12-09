package ch05;

import io.vavr.collection.List;

/**
 * 第5章: 円内の点の判定
 *
 * for 内包表記の代替パターンと flatMap の組み合わせ
 */
public class PointsInsideCircles {

    /**
     * 点を表す record
     */
    public record Point(int x, int y) {
        @Override
        public String toString() {
            return String.format("(%d, %d)", x, y);
        }
    }

    /**
     * 点が円内にあるか判定
     */
    public static boolean isInside(Point point, int radius) {
        return radius * radius >= point.x() * point.x() + point.y() * point.y();
    }

    /**
     * 全組み合わせを生成
     */
    public static List<String> allCombinations(List<Point> points, List<Integer> radiuses) {
        return radiuses.flatMap(r ->
                points.map(point ->
                        String.format("%s is within radius %d: %s",
                                point, r, isInside(point, r))
                )
        );
    }

    /**
     * 円内の点のみを取得（ガード式相当）
     */
    public static List<String> pointsInsideCircle(List<Point> points, List<Integer> radiuses) {
        return radiuses.flatMap(r ->
                points.filter(point -> isInside(point, r))
                        .map(point -> String.format("%s is inside circle with radius %d", point, r))
        );
    }

    /**
     * 各半径について、内部の点の数をカウント
     */
    public static List<String> countPointsPerRadius(List<Point> points, List<Integer> radiuses) {
        return radiuses.map(r -> {
            long count = points.count(point -> isInside(point, r));
            return String.format("Radius %d: %d points inside", r, count);
        });
    }

    /**
     * 点と半径のペアを生成（直積）
     */
    public static List<String> generatePairs(List<Point> points, List<Integer> radiuses) {
        return points.flatMap(point ->
                radiuses.map(r -> String.format("Point %s, Radius %d", point, r))
        );
    }

    /**
     * 少なくとも1つの円に含まれる点を取得
     */
    public static List<Point> pointsInAnyCircle(List<Point> points, List<Integer> radiuses) {
        return points.filter(point ->
                radiuses.exists(r -> isInside(point, r))
        );
    }

    /**
     * すべての円に含まれる点を取得
     */
    public static List<Point> pointsInAllCircles(List<Point> points, List<Integer> radiuses) {
        return points.filter(point ->
                radiuses.forAll(r -> isInside(point, r))
        );
    }

    public static void main(String[] args) {
        System.out.println("=== 円内の点の判定 ===\n");

        List<Point> points = List.of(
                new Point(5, 2),
                new Point(1, 1),
                new Point(0, 0),
                new Point(3, 3)
        );
        List<Integer> radiuses = List.of(2, 5, 10);

        System.out.println("点: " + points);
        System.out.println("半径: " + radiuses);

        // 全組み合わせ
        System.out.println("\n--- 全組み合わせ ---");
        allCombinations(points, radiuses).forEach(System.out::println);

        // 円内の点のみ
        System.out.println("\n--- 円内の点のみ ---");
        pointsInsideCircle(points, radiuses).forEach(System.out::println);

        // 各半径の点の数
        System.out.println("\n--- 各半径の点の数 ---");
        countPointsPerRadius(points, radiuses).forEach(System.out::println);

        // いずれかの円に含まれる点
        System.out.println("\n--- いずれかの円に含まれる点（半径2または5）---");
        System.out.println(pointsInAnyCircle(points, List.of(2, 5)));

        // すべての円に含まれる点
        System.out.println("\n--- すべての円に含まれる点（半径5以上）---");
        System.out.println(pointsInAllCircles(points, List.of(5, 10)));
    }
}
