package ch06;

import io.vavr.collection.List;
import io.vavr.control.Option;

/**
 * 第6章: Option 型による安全なエラーハンドリング
 *
 * TV番組のパースを通じて Option の使い方を学びます。
 */
public class TvShowParser {

    // ============================================
    // 例外を使う方法（問題あり）
    // ============================================

    /**
     * 例外をスローする危険なパーサー
     * "Breaking Bad (2008-2013)" -> TvShow("Breaking Bad", 2008, 2013)
     */
    public static TvShow parseShowUnsafe(String rawShow) {
        int bracketOpen = rawShow.indexOf('(');
        int bracketClose = rawShow.indexOf(')');
        int dash = rawShow.indexOf('-');

        String name = rawShow.substring(0, bracketOpen).trim();
        int yearStart = Integer.parseInt(rawShow.substring(bracketOpen + 1, dash));
        int yearEnd = Integer.parseInt(rawShow.substring(dash + 1, bracketClose));

        return new TvShow(name, yearStart, yearEnd);
    }

    // ============================================
    // Option を使う安全な方法
    // ============================================

    /**
     * 名前を抽出
     */
    public static Option<String> extractName(String rawShow) {
        int bracketOpen = rawShow.indexOf('(');
        if (bracketOpen > 0) {
            return Option.some(rawShow.substring(0, bracketOpen).trim());
        }
        return Option.none();
    }

    /**
     * 開始年を抽出
     */
    public static Option<Integer> extractYearStart(String rawShow) {
        int bracketOpen = rawShow.indexOf('(');
        int dash = rawShow.indexOf('-');

        if (bracketOpen != -1 && dash > bracketOpen + 1) {
            String yearStr = rawShow.substring(bracketOpen + 1, dash);
            return parseYear(yearStr);
        }
        return Option.none();
    }

    /**
     * 終了年を抽出
     */
    public static Option<Integer> extractYearEnd(String rawShow) {
        int dash = rawShow.indexOf('-');
        int bracketClose = rawShow.indexOf(')');

        if (dash != -1 && bracketClose > dash + 1) {
            String yearStr = rawShow.substring(dash + 1, bracketClose);
            return parseYear(yearStr);
        }
        return Option.none();
    }

    /**
     * 単年を抽出（"Chernobyl (2019)" のような形式用）
     */
    public static Option<Integer> extractSingleYear(String rawShow) {
        int dash = rawShow.indexOf('-');
        int bracketOpen = rawShow.indexOf('(');
        int bracketClose = rawShow.indexOf(')');

        if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1) {
            String yearStr = rawShow.substring(bracketOpen + 1, bracketClose);
            return parseYear(yearStr);
        }
        return Option.none();
    }

    /**
     * 文字列を年（整数）にパース
     */
    public static Option<Integer> parseYear(String yearStr) {
        try {
            return Option.some(Integer.parseInt(yearStr.trim()));
        } catch (NumberFormatException e) {
            return Option.none();
        }
    }

    /**
     * 安全なパーサー（Option を返す）
     */
    public static Option<TvShow> parseShow(String rawShow) {
        return extractName(rawShow).flatMap(name ->
                extractYearStart(rawShow).orElse(() -> extractSingleYear(rawShow)).flatMap(yearStart ->
                        extractYearEnd(rawShow).orElse(() -> extractSingleYear(rawShow)).map(yearEnd ->
                                new TvShow(name, yearStart, yearEnd)
                        )
                )
        );
    }

    // ============================================
    // エラーハンドリング戦略
    // ============================================

    /**
     * Best-effort 戦略: パースできたものだけ返す
     */
    public static List<TvShow> parseShowsBestEffort(List<String> rawShows) {
        return rawShows
                .map(TvShowParser::parseShow)
                .flatMap(Option::toList);
    }

    /**
     * All-or-nothing 戦略: 全部成功するか、全部失敗
     */
    public static Option<List<TvShow>> parseShowsAllOrNothing(List<String> rawShows) {
        Option<List<TvShow>> initial = Option.some(List.empty());

        return rawShows
                .map(TvShowParser::parseShow)
                .foldLeft(initial, (acc, optShow) ->
                        acc.flatMap(shows ->
                                optShow.map(shows::append)
                        )
                );
    }

    public static void main(String[] args) {
        System.out.println("=== 第6章: Option による安全なエラーハンドリング ===\n");

        // 正常ケース
        System.out.println("--- 正常ケース ---");
        System.out.println(parseShow("Breaking Bad (2008-2013)"));
        System.out.println(parseShow("Chernobyl (2019)"));

        // 異常ケース
        System.out.println("\n--- 異常ケース（None が返る）---");
        System.out.println(parseShow("The Wire 2002-2008"));
        System.out.println(parseShow("Invalid"));
        System.out.println(parseShow("(2008-2013)"));

        // Best-effort 戦略
        System.out.println("\n--- Best-effort 戦略 ---");
        List<String> rawShows = List.of(
                "Breaking Bad (2008-2013)",
                "The Wire 2002 2008",
                "Mad Men (2007-2015)",
                "Chernobyl (2019)"
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
    }
}
