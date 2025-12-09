package ch02;

import io.vavr.collection.List;

/**
 * 第2章: チップ計算の例
 *
 * 純粋関数によるチップ計算を示します。
 */
public class TipCalculator {

    /**
     * チップ率を計算する純粋関数
     *
     * @param names グループのメンバー名リスト
     * @return チップ率（6人以上: 20%, 1-5人: 10%, 0人: 0%）
     */
    public static int getTipPercentage(List<String> names) {
        if (names.size() > 5) {
            return 20;
        } else if (names.size() > 0) {
            return 10;
        } else {
            return 0;
        }
    }

    /**
     * より関数型らしい実装（パターンマッチング風）
     */
    public static int getTipPercentageFunctional(List<String> names) {
        int size = names.size();
        return size > 5 ? 20 : size > 0 ? 10 : 0;
    }

    // ============================================
    // メインメソッド
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== チップ計算（純粋関数版） ===\n");

        // 空のグループ
        List<String> emptyGroup = List.empty();
        System.out.println("空のグループ: " + emptyGroup);
        System.out.println("チップ率: " + getTipPercentage(emptyGroup) + "%");

        // 3人のグループ
        List<String> smallGroup = List.of("Alice", "Bob", "Charlie");
        System.out.println("\n3人のグループ: " + smallGroup);
        System.out.println("チップ率: " + getTipPercentage(smallGroup) + "%");

        // 5人のグループ
        List<String> mediumGroup = List.of("Alice", "Bob", "Charlie", "David", "Eve");
        System.out.println("\n5人のグループ: " + mediumGroup);
        System.out.println("チップ率: " + getTipPercentage(mediumGroup) + "%");

        // 6人のグループ
        List<String> largeGroup = List.of("Alice", "Bob", "Charlie", "David", "Eve", "Frank");
        System.out.println("\n6人のグループ: " + largeGroup);
        System.out.println("チップ率: " + getTipPercentage(largeGroup) + "%");

        // イミュータブルな操作
        System.out.println("\n--- イミュータブルな操作 ---");
        List<String> group1 = List.of("Alice", "Bob");
        List<String> group2 = group1.append("Charlie");
        List<String> group3 = group2.appendAll(List.of("David", "Eve", "Frank"));

        System.out.println("group1: " + group1 + " → チップ: " + getTipPercentage(group1) + "%");
        System.out.println("group2: " + group2 + " → チップ: " + getTipPercentage(group2) + "%");
        System.out.println("group3: " + group3 + " → チップ: " + getTipPercentage(group3) + "%");

        // group1 は変更されていないことを確認
        System.out.println("\ngroup1 は変更されていない: " + group1);
    }
}
