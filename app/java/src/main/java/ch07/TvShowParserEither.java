package ch07;

import ch06.TvShow;
import io.vavr.collection.List;
import io.vavr.control.Either;

/**
 * 第7章: Either 型と複合的なエラー処理
 *
 * Option の限界を超えて、エラーメッセージを保持できる Either を学びます。
 */
public class TvShowParserEither {

    // ============================================
    // Either を使ったパーサー
    // ============================================

    /**
     * 名前を抽出（エラーメッセージ付き）
     */
    public static Either<String, String> extractName(String rawShow) {
        int bracketOpen = rawShow.indexOf('(');
        if (bracketOpen > 0) {
            return Either.right(rawShow.substring(0, bracketOpen).trim());
        }
        return Either.left("Can't extract name from: " + rawShow);
    }

    /**
     * 開始年を抽出（エラーメッセージ付き）
     */
    public static Either<String, Integer> extractYearStart(String rawShow) {
        int bracketOpen = rawShow.indexOf('(');
        int dash = rawShow.indexOf('-');

        if (bracketOpen != -1 && dash > bracketOpen + 1) {
            String yearStr = rawShow.substring(bracketOpen + 1, dash);
            return parseYear(yearStr, "start year");
        }
        return Either.left("Can't extract start year from: " + rawShow);
    }

    /**
     * 終了年を抽出（エラーメッセージ付き）
     */
    public static Either<String, Integer> extractYearEnd(String rawShow) {
        int dash = rawShow.indexOf('-');
        int bracketClose = rawShow.indexOf(')');

        if (dash != -1 && bracketClose > dash + 1) {
            String yearStr = rawShow.substring(dash + 1, bracketClose);
            return parseYear(yearStr, "end year");
        }
        return Either.left("Can't extract end year from: " + rawShow);
    }

    /**
     * 単年を抽出（エラーメッセージ付き）
     */
    public static Either<String, Integer> extractSingleYear(String rawShow) {
        int dash = rawShow.indexOf('-');
        int bracketOpen = rawShow.indexOf('(');
        int bracketClose = rawShow.indexOf(')');

        if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1) {
            String yearStr = rawShow.substring(bracketOpen + 1, bracketClose);
            return parseYear(yearStr, "single year");
        }
        return Either.left("Can't extract single year from: " + rawShow);
    }

    /**
     * 文字列を年（整数）にパース（エラーメッセージ付き）
     */
    public static Either<String, Integer> parseYear(String yearStr, String context) {
        try {
            return Either.right(Integer.parseInt(yearStr.trim()));
        } catch (NumberFormatException e) {
            return Either.left("Can't parse " + context + ": '" + yearStr + "'");
        }
    }

    /**
     * Either を使った安全なパーサー
     */
    public static Either<String, TvShow> parseShow(String rawShow) {
        return extractName(rawShow).flatMap(name ->
                extractYearStart(rawShow).orElse(() -> extractSingleYear(rawShow)).flatMap(yearStart ->
                        extractYearEnd(rawShow).orElse(() -> extractSingleYear(rawShow)).map(yearEnd ->
                                new TvShow(name, yearStart, yearEnd)
                        )
                )
        );
    }

    // ============================================
    // 複数のパース
    // ============================================

    /**
     * Best-effort 戦略: パースできたものだけ返す（エラーは無視）
     */
    public static List<TvShow> parseShowsBestEffort(List<String> rawShows) {
        return rawShows
                .map(TvShowParserEither::parseShow)
                .flatMap(either -> either.toOption().toList());
    }

    /**
     * All-or-nothing 戦略: 全部成功するか、最初のエラーを返す
     */
    public static Either<String, List<TvShow>> parseShowsAllOrNothing(List<String> rawShows) {
        Either<String, List<TvShow>> initial = Either.right(List.empty());

        return rawShows
                .map(TvShowParserEither::parseShow)
                .foldLeft(initial, (acc, eitherShow) ->
                        acc.flatMap(shows ->
                                eitherShow.map(shows::append)
                        )
                );
    }

    /**
     * 全てのエラーを収集する戦略
     */
    public static Either<List<String>, List<TvShow>> parseShowsCollectErrors(List<String> rawShows) {
        List<Either<String, TvShow>> results = rawShows.map(TvShowParserEither::parseShow);

        List<String> errors = results.filter(Either::isLeft).map(e -> e.getLeft());
        List<TvShow> successes = results.filter(Either::isRight).map(e -> e.get());

        if (errors.isEmpty()) {
            return Either.right(successes);
        } else {
            return Either.left(errors);
        }
    }

    public static void main(String[] args) {
        System.out.println("=== 第7章: Either による詳細なエラーハンドリング ===\n");

        // 正常ケース
        System.out.println("--- 正常ケース ---");
        System.out.println(parseShow("Breaking Bad (2008-2013)"));
        System.out.println(parseShow("Chernobyl (2019)"));

        // 異常ケース（エラーメッセージ付き）
        System.out.println("\n--- 異常ケース（エラーメッセージ付き）---");
        System.out.println(parseShow("The Wire 2002-2008"));
        System.out.println(parseShow("(2008-2013)"));
        System.out.println(parseShow("Bad Show (abc-2020)"));

        // Best-effort 戦略
        System.out.println("\n--- Best-effort 戦略 ---");
        List<String> rawShows = List.of(
                "Breaking Bad (2008-2013)",
                "The Wire 2002 2008",
                "Mad Men (2007-2015)"
        );
        System.out.println("入力: " + rawShows);
        System.out.println("結果: " + parseShowsBestEffort(rawShows));

        // All-or-nothing 戦略
        System.out.println("\n--- All-or-nothing 戦略 ---");
        List<String> validShows = List.of(
                "Breaking Bad (2008-2013)",
                "Mad Men (2007-2015)"
        );
        System.out.println("全て有効: " + parseShowsAllOrNothing(validShows));
        System.out.println("一部無効: " + parseShowsAllOrNothing(rawShows));

        // エラー収集戦略
        System.out.println("\n--- エラー収集戦略 ---");
        System.out.println("一部無効: " + parseShowsCollectErrors(rawShows));
    }
}
