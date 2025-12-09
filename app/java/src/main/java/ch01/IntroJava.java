package ch01;

/**
 * 第1章: 関数型プログラミング入門
 *
 * 命令型プログラミングと関数型プログラミングの違いを学びます。
 */
public class IntroJava {

    // ============================================
    // 命令型プログラミング: HOW（どうやるか）を記述
    // ============================================

    /**
     * 命令型スタイルでワードスコアを計算
     * ステップバイステップで処理を記述
     */
    public static int calculateScoreImperative(String word) {
        int score = 0;
        for (char c : word.toCharArray()) {
            score++;
        }
        return score;
    }

    /**
     * 命令型スタイルで 'a' を除外したスコアを計算
     */
    public static int calculateScoreWithoutAImperative(String word) {
        int score = 0;
        for (char c : word.toCharArray()) {
            if (c != 'a') {
                score++;
            }
        }
        return score;
    }

    // ============================================
    // 関数型プログラミング: WHAT（何をするか）を記述
    // ============================================

    /**
     * 関数型スタイルでワードスコアを計算
     * 「文字列の長さを返す」という宣言的な記述
     */
    public static int wordScore(String word) {
        return word.length();
    }

    /**
     * 関数型スタイルで 'a' を除外したスコアを計算
     * 「'a' を除去した文字列の長さを返す」という宣言的な記述
     */
    public static int wordScoreWithoutA(String word) {
        return word.replace("a", "").length();
    }

    // ============================================
    // 基本的な純粋関数の例
    // ============================================

    /**
     * 数値をインクリメント
     */
    public static int increment(int x) {
        return x + 1;
    }

    /**
     * 2つの数値を加算
     */
    public static int add(int a, int b) {
        return a + b;
    }

    /**
     * 文字列の最初の文字を取得
     */
    public static char getFirstCharacter(String s) {
        return s.charAt(0);
    }

    /**
     * 2つの文字列を連結
     */
    public static String concatenate(String a, String b) {
        return a + b;
    }

    /**
     * 数値を2倍にする
     */
    public static int doubleValue(int x) {
        return x * 2;
    }

    /**
     * 挨拶文を生成
     */
    public static String greet(String name) {
        return "Hello, " + name + "!";
    }

    /**
     * 数値が偶数かどうかを判定
     */
    public static boolean isEven(int n) {
        return n % 2 == 0;
    }

    // ============================================
    // メインメソッド
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== 第1章: 関数型プログラミング入門 ===\n");

        // 命令型 vs 関数型
        System.out.println("--- 命令型 vs 関数型 ---");
        String word = "functional";

        System.out.println("単語: " + word);
        System.out.println("命令型スコア: " + calculateScoreImperative(word));
        System.out.println("関数型スコア: " + wordScore(word));

        // 'a' を除外したスコア
        System.out.println("\n--- 'a' を除外したスコア ---");
        String wordWithA = "Scala";
        System.out.println("単語: " + wordWithA);
        System.out.println("命令型（'a'除外）: " + calculateScoreWithoutAImperative(wordWithA));
        System.out.println("関数型（'a'除外）: " + wordScoreWithoutA(wordWithA));

        // 基本的な純粋関数
        System.out.println("\n--- 基本的な純粋関数 ---");
        System.out.println("increment(6) = " + increment(6));
        System.out.println("add(2, 3) = " + add(2, 3));
        System.out.println("getFirstCharacter(\"Java\") = " + getFirstCharacter("Java"));
        System.out.println("concatenate(\"Hello\", \"World\") = " + concatenate("Hello", "World"));
        System.out.println("doubleValue(5) = " + doubleValue(5));
        System.out.println("greet(\"FP\") = " + greet("FP"));
        System.out.println("isEven(4) = " + isEven(4));
        System.out.println("isEven(7) = " + isEven(7));
    }
}
