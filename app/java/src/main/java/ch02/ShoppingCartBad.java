package ch02;

import java.util.ArrayList;
import java.util.List;

/**
 * 第2章: 問題のあるショッピングカート（アンチパターン）
 *
 * 状態を持つクラスの問題点を示します。
 * このコードはアンチパターンの例示目的です。
 */
public class ShoppingCartBad {
    private List<String> items = new ArrayList<>();
    private boolean bookAdded = false;

    /**
     * アイテムを追加
     * 問題: 内部状態を変更する副作用がある
     */
    public void addItem(String item) {
        items.add(item);
        if (item.equals("Book")) {
            bookAdded = true;
        }
    }

    /**
     * 割引率を取得
     * 問題: bookAdded フラグに依存しており、実際のカート内容と一致しない可能性がある
     */
    public int getDiscountPercentage() {
        if (bookAdded) {
            return 5;
        } else {
            return 0;
        }
    }

    /**
     * アイテムリストを取得
     * 問題: 内部の List への参照を返しているため、外部から変更可能
     */
    public List<String> getItems() {
        return items;  // 危険: 内部状態への参照を返している
    }

    // ============================================
    // メインメソッド: 問題点のデモ
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== ショッピングカート（問題のあるコード） ===\n");

        ShoppingCartBad cart = new ShoppingCartBad();

        // 通常の操作
        cart.addItem("Apple");
        System.out.println("Apple を追加: " + cart.getItems());
        System.out.println("割引率: " + cart.getDiscountPercentage() + "%");

        cart.addItem("Book");
        System.out.println("\nBook を追加: " + cart.getItems());
        System.out.println("割引率: " + cart.getDiscountPercentage() + "%");

        // 問題1: 外部からの変更
        System.out.println("\n--- 問題1: 外部からの変更 ---");
        List<String> itemsRef = cart.getItems();
        itemsRef.remove("Book");  // 外部から直接変更!

        System.out.println("外部から Book を削除した後:");
        System.out.println("カート内容: " + cart.getItems());
        System.out.println("割引率: " + cart.getDiscountPercentage() + "% (まだ 5%!)");
        System.out.println("→ カートに Book がないのに割引が適用されている!");

        // 問題2: 複数の Book を追加して1つ削除
        System.out.println("\n--- 問題2: 複数の Book の問題 ---");
        ShoppingCartBad cart2 = new ShoppingCartBad();
        cart2.addItem("Book");
        cart2.addItem("Book");
        System.out.println("Book を2つ追加: " + cart2.getItems());
        System.out.println("割引率: " + cart2.getDiscountPercentage() + "%");

        // removeItem がないため、直接操作
        cart2.getItems().remove("Book");
        System.out.println("\n1つの Book を削除: " + cart2.getItems());
        System.out.println("割引率: " + cart2.getDiscountPercentage() + "%");
        System.out.println("→ Book がまだあるのに、bookAdded フラグは正しく管理されていない可能性");
    }
}
