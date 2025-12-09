package ch06;

import io.vavr.collection.List;
import io.vavr.control.Option;

/**
 * 第6章: Option の基本操作
 *
 * Option の主要メソッドを学びます。
 */
public class OptionBasics {

    // ============================================
    // Option の作成
    // ============================================

    /**
     * Some: 値が存在する
     */
    public static Option<Integer> someValue() {
        return Option.some(42);
    }

    /**
     * None: 値が存在しない
     */
    public static Option<Integer> noValue() {
        return Option.none();
    }

    /**
     * of: null なら None、それ以外は Some
     */
    public static Option<String> fromNullable(String value) {
        return Option.of(value);
    }

    // ============================================
    // 安全な除算
    // ============================================

    /**
     * 0 で割ると None を返す安全な除算
     */
    public static Option<Integer> safeDivide(int a, int b) {
        if (b == 0) {
            return Option.none();
        }
        return Option.some(a / b);
    }

    // ============================================
    // 文字列の加算
    // ============================================

    /**
     * 2つの数値文字列を加算
     */
    public static Option<Integer> addStrings(String a, String b) {
        return parseInt(a).flatMap(x ->
                parseInt(b).map(y -> x + y)
        );
    }

    /**
     * 文字列を整数にパース
     */
    public static Option<Integer> parseInt(String s) {
        try {
            return Option.some(Integer.parseInt(s.trim()));
        } catch (NumberFormatException e) {
            return Option.none();
        }
    }

    // ============================================
    // Option の主要メソッド
    // ============================================

    /**
     * map: 値があれば変換
     */
    public static Option<Integer> mapExample(Option<Integer> opt) {
        return opt.map(x -> x * 2);
    }

    /**
     * flatMap: 値があれば Option を返す関数を適用
     */
    public static Option<Integer> flatMapExample(Option<Integer> opt) {
        return opt.flatMap(x -> safeDivide(100, x));
    }

    /**
     * filter: 条件を満たさなければ None
     */
    public static Option<Integer> filterExample(Option<Integer> opt, int threshold) {
        return opt.filter(x -> x > threshold);
    }

    /**
     * orElse: None なら代替を使用
     */
    public static Option<Integer> orElseExample(Option<Integer> opt, Option<Integer> alternative) {
        return opt.orElse(alternative);
    }

    /**
     * getOrElse: None ならデフォルト値
     */
    public static int getOrElseExample(Option<Integer> opt, int defaultValue) {
        return opt.getOrElse(defaultValue);
    }

    /**
     * toList: List に変換
     */
    public static List<Integer> toListExample(Option<Integer> opt) {
        return opt.toList();
    }

    /**
     * isDefined / isEmpty
     */
    public static boolean isDefinedExample(Option<Integer> opt) {
        return opt.isDefined();
    }

    // ============================================
    // forall と exists
    // ============================================

    /**
     * forall: 値がないか、条件を満たす
     */
    public static boolean forallExample(Option<Integer> opt, int max) {
        return opt.forAll(x -> x < max);
    }

    /**
     * exists: 値があり、条件を満たす
     */
    public static boolean existsExample(Option<Integer> opt, int min) {
        return opt.exists(x -> x > min);
    }

    public static void main(String[] args) {
        System.out.println("=== Option の基本操作 ===\n");

        Option<Integer> some = Option.some(996);
        Option<Integer> none = Option.none();

        // map
        System.out.println("--- map ---");
        System.out.println("Some(996).map(_ * 2) = " + mapExample(some));
        System.out.println("None.map(_ * 2) = " + mapExample(none));

        // flatMap
        System.out.println("\n--- flatMap ---");
        System.out.println("Some(996).flatMap(safeDivide(100, _)) = " + flatMapExample(some));
        System.out.println("Some(0).flatMap(safeDivide(100, _)) = " + flatMapExample(Option.some(0)));

        // filter
        System.out.println("\n--- filter ---");
        System.out.println("Some(996).filter(_ > 500) = " + filterExample(some, 500));
        System.out.println("Some(996).filter(_ > 1000) = " + filterExample(some, 1000));

        // orElse
        System.out.println("\n--- orElse ---");
        System.out.println("Some(996).orElse(Some(2020)) = " + orElseExample(some, Option.some(2020)));
        System.out.println("None.orElse(Some(2020)) = " + orElseExample(none, Option.some(2020)));

        // getOrElse
        System.out.println("\n--- getOrElse ---");
        System.out.println("Some(996).getOrElse(0) = " + getOrElseExample(some, 0));
        System.out.println("None.getOrElse(0) = " + getOrElseExample(none, 0));

        // forall / exists
        System.out.println("\n--- forall / exists ---");
        System.out.println("Some(996).forall(_ < 2020) = " + forallExample(some, 2020));
        System.out.println("None.forall(_ < 2020) = " + forallExample(none, 2020));
        System.out.println("Some(996).exists(_ > 500) = " + existsExample(some, 500));
        System.out.println("None.exists(_ > 500) = " + existsExample(none, 500));

        // safeDivide
        System.out.println("\n--- safeDivide ---");
        System.out.println("safeDivide(10, 2) = " + safeDivide(10, 2));
        System.out.println("safeDivide(10, 0) = " + safeDivide(10, 0));

        // addStrings
        System.out.println("\n--- addStrings ---");
        System.out.println("addStrings(\"10\", \"20\") = " + addStrings("10", "20"));
        System.out.println("addStrings(\"10\", \"abc\") = " + addStrings("10", "abc"));
    }
}
