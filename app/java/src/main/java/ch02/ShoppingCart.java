package ch02;

import io.vavr.collection.List;

/**
 * 第2章: ショッピングカートの例
 *
 * 状態を持つクラスの問題点と、純粋関数による解決策を示します。
 */
public class ShoppingCart {

    // ============================================
    // 純粋関数版: 状態を持たない
    // ============================================

    /**
     * 割引率を計算する純粋関数
     *
     * @param items カート内のアイテムリスト
     * @return 割引率（Book が含まれていれば 5%、そうでなければ 0%）
     */
    public static int getDiscountPercentage(List<String> items) {
        if (items.contains("Book")) {
            return 5;
        } else {
            return 0;
        }
    }

    /**
     * Vavr の List を使用したより関数型らしい実装
     */
    public static int getDiscountPercentageFunctional(List<String> items) {
        return items.contains("Book") ? 5 : 0;
    }

    // ============================================
    // メインメソッド
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== ショッピングカート（純粋関数版） ===\n");

        // 空のカート
        List<String> emptyCart = List.empty();
        System.out.println("空のカート: " + emptyCart);
        System.out.println("割引率: " + getDiscountPercentage(emptyCart) + "%");

        // Apple のみのカート
        List<String> appleCart = List.of("Apple");
        System.out.println("\nApple のみ: " + appleCart);
        System.out.println("割引率: " + getDiscountPercentage(appleCart) + "%");

        // Apple と Book のカート
        List<String> appleAndBookCart = List.of("Apple", "Book");
        System.out.println("\nApple と Book: " + appleAndBookCart);
        System.out.println("割引率: " + getDiscountPercentage(appleAndBookCart) + "%");

        // イミュータブルな操作
        System.out.println("\n--- イミュータブルな操作 ---");
        List<String> cart1 = List.of("Apple");
        List<String> cart2 = cart1.append("Book");  // 新しいリストを作成
        List<String> cart3 = cart2.append("Lemon"); // 新しいリストを作成

        System.out.println("cart1: " + cart1 + " → 割引: " + getDiscountPercentage(cart1) + "%");
        System.out.println("cart2: " + cart2 + " → 割引: " + getDiscountPercentage(cart2) + "%");
        System.out.println("cart3: " + cart3 + " → 割引: " + getDiscountPercentage(cart3) + "%");

        // cart1 は変更されていないことを確認
        System.out.println("\ncart1 は変更されていない: " + cart1);
    }
}
