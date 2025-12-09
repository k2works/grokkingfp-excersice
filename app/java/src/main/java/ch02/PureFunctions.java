package ch02;

/**
 * 第2章: 純粋関数
 *
 * 純粋関数の特徴:
 * 1. 同じ入力には常に同じ出力を返す
 * 2. 副作用がない（外部状態を変更しない）
 */
public class PureFunctions {

    // ============================================
    // 純粋関数の例
    // ============================================

    /**
     * 数値をインクリメント（純粋関数）
     */
    public static int increment(int x) {
        return x + 1;
    }

    /**
     * 2つの数値を加算（純粋関数）
     */
    public static int add(int a, int b) {
        return a + b;
    }

    /**
     * 文字列の最初の文字を取得（純粋関数）
     */
    public static char getFirstCharacter(String s) {
        return s.charAt(0);
    }

    /**
     * 割引率を計算（純粋関数）
     * 入力の 95% を返す
     */
    public static double applyDiscount(double x) {
        return x * 95.0 / 100.0;
    }

    /**
     * 文字列の長さに基づくスコア計算（純粋関数）
     */
    public static int calculateStringScore(String s) {
        return s.length() * 3;
    }

    // ============================================
    // 不純な関数の例（アンチパターン）
    // ============================================

    /**
     * 不純な関数 - Math.random() は毎回異なる値を返す
     * 同じ入力に対して異なる出力を返す可能性がある
     */
    public static double randomPart(double x) {
        return x * Math.random();
    }

    /**
     * 不純な関数 - 現在時刻は毎回異なる
     */
    public static long currentTime() {
        return System.currentTimeMillis();
    }

    // ============================================
    // メインメソッド
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== 第2章: 純粋関数 ===\n");

        // 純粋関数のデモ
        System.out.println("--- 純粋関数のデモ ---");
        System.out.println("increment(5) = " + increment(5));
        System.out.println("increment(5) = " + increment(5)); // 常に同じ結果
        System.out.println("add(2, 3) = " + add(2, 3));
        System.out.println("getFirstCharacter(\"Hello\") = " + getFirstCharacter("Hello"));
        System.out.println("applyDiscount(100) = " + applyDiscount(100));
        System.out.println("calculateStringScore(\"Scala\") = " + calculateStringScore("Scala"));

        // 不純な関数のデモ
        System.out.println("\n--- 不純な関数のデモ ---");
        System.out.println("randomPart(10) = " + randomPart(10));
        System.out.println("randomPart(10) = " + randomPart(10)); // 異なる結果
        System.out.println("currentTime() = " + currentTime());
        System.out.println("currentTime() = " + currentTime()); // 異なる結果

        // 純粋関数の参照透過性
        System.out.println("\n--- 参照透過性 ---");
        int score1 = calculateStringScore("Java");
        int score2 = calculateStringScore("Java");
        System.out.println("score1 = " + score1);
        System.out.println("score2 = " + score2);
        System.out.println("score1 == score2: " + (score1 == score2));
        System.out.println("式を値で置き換え可能: calculateStringScore(\"Java\") → " + score1);
    }
}
