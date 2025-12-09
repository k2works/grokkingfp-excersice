package ch04;

import io.vavr.collection.List;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * 第4章: 高階関数
 *
 * 関数を引数として受け取る、または関数を返す関数を学びます。
 */
public class HigherOrderFunctions {

    // ============================================
    // map - 各要素を変換
    // ============================================

    /**
     * 文字列の長さを取得
     */
    public static int len(String s) {
        return s.length();
    }

    /**
     * 整数を2倍にする
     */
    public static int doubleValue(int i) {
        return 2 * i;
    }

    /**
     * 文字列リストの各要素の長さを取得
     */
    public static List<Integer> lengths(List<String> words) {
        return words.map(HigherOrderFunctions::len);
    }

    /**
     * 整数リストの各要素を2倍にする
     */
    public static List<Integer> doubles(List<Integer> numbers) {
        return numbers.map(HigherOrderFunctions::doubleValue);
    }

    // ============================================
    // filter - 条件に合う要素を抽出
    // ============================================

    /**
     * 奇数判定
     */
    public static boolean isOdd(int i) {
        return i % 2 == 1;
    }

    /**
     * 4より大きいか判定
     */
    public static boolean isLargerThan4(int i) {
        return i > 4;
    }

    /**
     * 奇数のみを抽出
     */
    public static List<Integer> filterOdds(List<Integer> numbers) {
        return numbers.filter(HigherOrderFunctions::isOdd);
    }

    /**
     * 4より大きい数のみを抽出
     */
    public static List<Integer> filterLargerThan4(List<Integer> numbers) {
        return numbers.filter(HigherOrderFunctions::isLargerThan4);
    }

    // ============================================
    // 関数を返す関数
    // ============================================

    /**
     * 指定値より大きいか判定する関数を返す
     */
    public static Predicate<Integer> largerThan(int n) {
        return i -> i > n;
    }

    /**
     * 指定値で割り切れるか判定する関数を返す
     */
    public static Predicate<Integer> divisibleBy(int n) {
        return i -> i % n == 0;
    }

    /**
     * 指定文字を含むか判定する関数を返す
     */
    public static Predicate<String> contains(String substr) {
        return s -> s.contains(substr);
    }

    // ============================================
    // foldLeft - 畳み込み
    // ============================================

    /**
     * 合計を計算
     */
    public static int sum(List<Integer> numbers) {
        return numbers.foldLeft(0, (acc, i) -> acc + i);
    }

    /**
     * 最大値を取得
     */
    public static int max(List<Integer> numbers) {
        return numbers.foldLeft(Integer.MIN_VALUE, (m, i) -> i > m ? i : m);
    }

    /**
     * 文字列を連結
     */
    public static String concatenate(List<String> strings) {
        return strings.foldLeft("", (acc, s) -> acc + s);
    }

    /**
     * 区切り文字で連結
     */
    public static String join(List<String> strings, String delimiter) {
        if (strings.isEmpty()) return "";
        return strings.tail().foldLeft(strings.head(), (acc, s) -> acc + delimiter + s);
    }

    // ============================================
    // sortBy - ソート基準を関数で指定
    // ============================================

    /**
     * 指定した関数でソート
     */
    public static <T, U extends Comparable<U>> List<T> sortByFunction(
            List<T> list, Function<T, U> keyExtractor) {
        return list.sortBy(keyExtractor);
    }

    public static void main(String[] args) {
        System.out.println("=== 第4章: 高階関数 ===\n");

        // map の例
        System.out.println("--- map ---");
        List<String> words = List.of("scala", "rust", "ada");
        System.out.println("単語: " + words);
        System.out.println("長さ: " + lengths(words));

        List<Integer> numbers = List.of(5, 1, 2, 4, 0);
        System.out.println("数値: " + numbers);
        System.out.println("2倍: " + doubles(numbers));

        // filter の例
        System.out.println("\n--- filter ---");
        System.out.println("奇数: " + filterOdds(numbers));
        System.out.println("4より大きい: " + filterLargerThan4(numbers));

        // 関数を返す関数
        System.out.println("\n--- 関数を返す関数 ---");
        System.out.println("3より大きい: " + numbers.filter(largerThan(3)));
        System.out.println("2で割り切れる: " + numbers.filter(divisibleBy(2)));

        // foldLeft の例
        System.out.println("\n--- foldLeft ---");
        System.out.println("合計: " + sum(numbers));
        System.out.println("最大: " + max(numbers));
        System.out.println("連結: " + concatenate(List.of("a", "b", "c")));
        System.out.println("カンマ区切り: " + join(List.of("a", "b", "c"), ", "));

        // sortBy の例
        System.out.println("\n--- sortBy ---");
        System.out.println("長さでソート: " + sortByFunction(words, String::length));
    }
}
