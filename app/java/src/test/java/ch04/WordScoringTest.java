package ch04;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第4章: ワードスコアリングのテスト
 */
@DisplayName("第4章: ワードスコアリング")
class WordScoringTest {

    @Nested
    @DisplayName("スコア計算")
    class ScoreCalculationTest {

        @Test
        @DisplayName("score: 'a' を除いた文字数")
        void scoreRemovesA() {
            assertThat(WordScoring.score("java")).isEqualTo(2); // j, v
            assertThat(WordScoring.score("rust")).isEqualTo(4); // r, u, s, t
            assertThat(WordScoring.score("ada")).isEqualTo(1);  // d
            assertThat(WordScoring.score("aaa")).isEqualTo(0);  // 全部 a
        }

        @Test
        @DisplayName("bonus: 'c' を含むと +5")
        void bonusForC() {
            assertThat(WordScoring.bonus("scala")).isEqualTo(5);
            assertThat(WordScoring.bonus("java")).isEqualTo(0);
        }

        @Test
        @DisplayName("penalty: 's' を含むと -7")
        void penaltyForS() {
            assertThat(WordScoring.penalty("rust")).isEqualTo(7);
            assertThat(WordScoring.penalty("java")).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("rankedWords")
    class RankedWordsTest {

        private final List<String> words = List.of("ada", "haskell", "scala", "java", "rust");

        @Test
        @DisplayName("基本スコアでランキング")
        void rankByBasicScore() {
            List<String> ranked = WordScoring.rankedWords(WordScoring::score, words);
            // haskell: 6, rust: 4, scala: 3, java: 2, ada: 1
            assertThat(ranked).containsExactly("haskell", "rust", "scala", "java", "ada");
        }

        @Test
        @DisplayName("ボーナス付きスコアでランキング")
        void rankWithBonus() {
            Function<String, Integer> scoreWithBonus = w -> WordScoring.score(w) + WordScoring.bonus(w);
            List<String> ranked = WordScoring.rankedWords(scoreWithBonus, words);
            // scala: 3+5=8, haskell: 6+0=6, rust: 4+0=4, java: 2+0=2, ada: 1+0=1
            assertThat(ranked).containsExactly("scala", "haskell", "rust", "java", "ada");
        }

        @Test
        @DisplayName("ボーナスとペナルティ付きスコアでランキング")
        void rankWithBonusAndPenalty() {
            Function<String, Integer> fullScore = w ->
                    WordScoring.score(w) + WordScoring.bonus(w) - WordScoring.penalty(w);
            List<String> ranked = WordScoring.rankedWords(fullScore, words);
            // java: 2+0-0=2, scala: 3+5-7=1, ada: 1+0-0=1, haskell: 6+0-7=-1, rust: 4+0-7=-3
            assertThat(ranked.head()).isEqualTo("java");
        }
    }

    @Nested
    @DisplayName("highScoringWords")
    class HighScoringWordsTest {

        @Test
        @DisplayName("閾値以上のスコアの単語を抽出")
        void filterByThreshold() {
            List<String> words = List.of("ada", "haskell", "scala", "java", "rust");
            List<String> high = WordScoring.highScoringWords(WordScoring::score, words, 4);
            // haskell: 6, rust: 4 が該当
            assertThat(high).containsExactly("haskell", "rust");
        }
    }

    @Nested
    @DisplayName("totalScore")
    class TotalScoreTest {

        @Test
        @DisplayName("スコア合計を計算")
        void calculateTotalScore() {
            List<String> words = List.of("java", "rust");
            // java: 2, rust: 4, 合計: 6
            assertThat(WordScoring.totalScore(WordScoring::score, words)).isEqualTo(6);
        }

        @Test
        @DisplayName("空リストの合計は0")
        void totalScoreOfEmpty() {
            assertThat(WordScoring.totalScore(WordScoring::score, List.empty())).isEqualTo(0);
        }
    }
}
