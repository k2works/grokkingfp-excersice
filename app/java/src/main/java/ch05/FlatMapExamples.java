package ch05;

import io.vavr.collection.List;

/**
 * 第5章: flatMap とネスト構造
 *
 * flatten と flatMap によるネストしたリストの操作を学びます。
 */
public class FlatMapExamples {

    // ============================================
    // flatten - ネストしたリストを平坦化
    // ============================================

    /**
     * ネストしたリストを平坦化
     */
    public static <T> List<T> flatten(List<List<T>> nested) {
        return nested.flatMap(x -> x);
    }

    // ============================================
    // flatMap の基本
    // ============================================

    /**
     * 各要素を複製（要素数が増える例）
     */
    public static List<Integer> duplicate(List<Integer> numbers) {
        return numbers.flatMap(i -> List.of(i, i + 10));
    }

    /**
     * 偶数のみを抽出（要素数が減る例 - filter の代替）
     */
    public static List<Integer> filterEvens(List<Integer> numbers) {
        return numbers.flatMap(i -> i % 2 == 0 ? List.of(i) : List.empty());
    }

    /**
     * 各単語を文字のリストに変換
     */
    public static List<Character> toChars(List<String> words) {
        return words.flatMap(word -> List.ofAll(word.toCharArray()));
    }

    // ============================================
    // 実用的な例
    // ============================================

    /**
     * 文字列をスペースで分割して平坦化
     */
    public static List<String> splitAndFlatten(List<String> sentences) {
        return sentences.flatMap(s -> List.of(s.split(" ")));
    }

    /**
     * 数値の範囲を生成して平坦化
     */
    public static List<Integer> ranges(List<Integer> ends) {
        return ends.flatMap(n -> List.range(1, n + 1));
    }

    /**
     * 直積（デカルト積）を計算
     */
    public static <A, B> List<String> cartesianProduct(List<A> as, List<B> bs) {
        return as.flatMap(a ->
                bs.map(b -> "(" + a + ", " + b + ")")
        );
    }

    public static void main(String[] args) {
        System.out.println("=== 第5章: flatMap とネスト構造 ===\n");

        // flatten の例
        System.out.println("--- flatten ---");
        List<List<Integer>> nested = List.of(
                List.of(1, 2),
                List.of(3, 4, 5),
                List.of(6)
        );
        System.out.println("ネストしたリスト: " + nested);
        System.out.println("平坦化: " + flatten(nested));

        // flatMap の基本
        System.out.println("\n--- flatMap（要素数の変化）---");
        List<Integer> numbers = List.of(1, 2, 3);
        System.out.println("元のリスト: " + numbers);
        System.out.println("duplicate: " + duplicate(numbers));
        System.out.println("filterEvens: " + filterEvens(List.of(1, 2, 3, 4, 5)));

        // 文字への変換
        System.out.println("\n--- 文字への変換 ---");
        List<String> words = List.of("hi", "bye");
        System.out.println("単語: " + words);
        System.out.println("文字: " + toChars(words));

        // 文字列の分割
        System.out.println("\n--- 文字列の分割 ---");
        List<String> sentences = List.of("hello world", "foo bar baz");
        System.out.println("文: " + sentences);
        System.out.println("単語: " + splitAndFlatten(sentences));

        // 範囲の生成
        System.out.println("\n--- 範囲の生成 ---");
        System.out.println("ranges([2, 3]): " + ranges(List.of(2, 3)));

        // 直積
        System.out.println("\n--- 直積 ---");
        List<String> colors = List.of("Red", "Blue");
        List<Integer> sizes = List.of(1, 2, 3);
        System.out.println("colors × sizes: " + cartesianProduct(colors, sizes));
    }
}
