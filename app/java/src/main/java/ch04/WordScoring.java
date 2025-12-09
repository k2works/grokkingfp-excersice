package ch04;

import io.vavr.collection.List;
import java.util.function.Function;

/**
 * 第4章: ワードスコアリング
 *
 * 複数のスコアリングロジックを組み合わせる例
 */
public class WordScoring {

    /**
     * 基本スコア: 'a' を除いた文字数
     */
    public static int score(String word) {
        return word.replaceAll("a", "").length();
    }

    /**
     * ボーナス: 'c' を含むと +5
     */
    public static int bonus(String word) {
        return word.contains("c") ? 5 : 0;
    }

    /**
     * ペナルティ: 's' を含むと -7
     */
    public static int penalty(String word) {
        return word.contains("s") ? 7 : 0;
    }

    /**
     * スコア関数を受け取ってランキングを作成
     */
    public static List<String> rankedWords(Function<String, Integer> wordScore, List<String> words) {
        return words.sortBy(wordScore).reverse();
    }

    /**
     * スコアと一緒にランキングを返す
     */
    public static List<String> rankedWordsWithScore(Function<String, Integer> wordScore, List<String> words) {
        return words
                .sortBy(wordScore)
                .reverse()
                .map(w -> w + ": " + wordScore.apply(w));
    }

    /**
     * 高スコアの単語のみを抽出
     */
    public static List<String> highScoringWords(
            Function<String, Integer> wordScore,
            List<String> words,
            int threshold) {
        return words.filter(w -> wordScore.apply(w) >= threshold);
    }

    /**
     * スコア合計を計算
     */
    public static int totalScore(Function<String, Integer> wordScore, List<String> words) {
        return words.map(wordScore).foldLeft(0, Integer::sum);
    }

    public static void main(String[] args) {
        System.out.println("=== ワードスコアリング ===\n");

        List<String> words = List.of("ada", "haskell", "scala", "java", "rust");
        System.out.println("単語リスト: " + words);

        // 基本スコア
        System.out.println("\n--- 基本スコア ---");
        words.forEach(w -> System.out.println(w + ": " + score(w)));

        // 基本スコアでランキング
        System.out.println("\n--- 基本スコアでランキング ---");
        System.out.println(rankedWords(WordScoring::score, words));

        // ボーナス付きスコアでランキング
        System.out.println("\n--- ボーナス付きスコア ---");
        Function<String, Integer> scoreWithBonus = w -> score(w) + bonus(w);
        System.out.println(rankedWordsWithScore(scoreWithBonus, words));

        // ボーナスとペナルティ付き
        System.out.println("\n--- ボーナス + ペナルティ ---");
        Function<String, Integer> fullScore = w -> score(w) + bonus(w) - penalty(w);
        System.out.println(rankedWordsWithScore(fullScore, words));

        // 高スコアの単語
        System.out.println("\n--- 高スコアの単語（スコア >= 4）---");
        System.out.println(highScoringWords(WordScoring::score, words, 4));

        // スコア合計
        System.out.println("\n--- スコア合計 ---");
        System.out.println("基本スコア合計: " + totalScore(WordScoring::score, words));
    }
}
