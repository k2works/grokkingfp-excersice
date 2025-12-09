package ch03;

import io.vavr.collection.List;

/**
 * 第3章: イミュータブルなデータ操作
 *
 * Vavr の List を使用したイミュータブルなリスト操作を学びます。
 */
public class ImmutableList {

    // ============================================
    // 基本的な List 操作
    // ============================================

    /**
     * 最初の2要素を取得
     */
    public static List<String> firstTwo(List<String> list) {
        return list.take(2);
    }

    /**
     * 最後の2要素を取得
     */
    public static List<String> lastTwo(List<String> list) {
        return list.takeRight(2);
    }

    /**
     * 指定範囲を取得（slice相当）
     */
    public static <T> List<T> slice(List<T> list, int start, int end) {
        return list.drop(start).take(end - start);
    }

    // ============================================
    // リストの変換
    // ============================================

    /**
     * 最初の2要素を末尾に移動
     */
    public static List<String> movedFirstTwoToTheEnd(List<String> list) {
        List<String> firstTwo = list.take(2);
        List<String> withoutFirstTwo = list.drop(2);
        return withoutFirstTwo.appendAll(firstTwo);
    }

    /**
     * 最後の要素の前に挿入
     */
    public static List<String> insertedBeforeLast(List<String> list, String element) {
        List<String> withoutLast = list.dropRight(1);
        List<String> last = list.takeRight(1);
        return withoutLast.append(element).appendAll(last);
    }

    /**
     * 指定位置に要素を挿入
     */
    public static List<String> insertAt(List<String> list, int index, String element) {
        List<String> before = list.take(index);
        List<String> after = list.drop(index);
        return before.append(element).appendAll(after);
    }

    /**
     * リストの中央に要素を挿入
     */
    public static List<String> insertAtMiddle(List<String> list, String element) {
        int middle = list.size() / 2;
        return insertAt(list, middle, element);
    }

    // ============================================
    // メインメソッド
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== 第3章: イミュータブルなデータ操作 ===\n");

        // 基本操作
        System.out.println("--- 基本操作 ---");
        List<String> fruits = List.of("Apple", "Banana", "Cherry", "Date");
        System.out.println("元のリスト: " + fruits);
        System.out.println("最初の2つ: " + firstTwo(fruits));
        System.out.println("最後の2つ: " + lastTwo(fruits));
        System.out.println("slice(1, 3): " + slice(fruits, 1, 3));

        // イミュータブルな操作
        System.out.println("\n--- イミュータブルな操作 ---");
        List<String> list1 = List.of("a", "b", "c");
        List<String> list2 = list1.append("d");
        System.out.println("list1: " + list1 + " (変更されていない)");
        System.out.println("list2: " + list2 + " (新しいリスト)");

        // 変換操作
        System.out.println("\n--- 変換操作 ---");
        List<String> abc = List.of("a", "b", "c");
        System.out.println("元のリスト: " + abc);
        System.out.println("最初の2つを末尾に: " + movedFirstTwoToTheEnd(abc));
        System.out.println("最後の前に挿入: " + insertedBeforeLast(abc, "X"));
        System.out.println("中央に挿入: " + insertAtMiddle(List.of("a", "b", "c", "d"), "X"));
    }
}
