package ch04;

import io.vavr.collection.List;

/**
 * 第4章: プログラミング言語のデータクラス
 *
 * record を使ったイミュータブルなデータクラスの例
 */
public record ProgrammingLanguage(String name, int year) {

    /**
     * 言語名でソート
     */
    public static List<ProgrammingLanguage> sortByName(List<ProgrammingLanguage> languages) {
        return languages.sortBy(ProgrammingLanguage::name);
    }

    /**
     * 年でソート
     */
    public static List<ProgrammingLanguage> sortByYear(List<ProgrammingLanguage> languages) {
        return languages.sortBy(ProgrammingLanguage::year);
    }

    /**
     * 指定年以降の言語をフィルタ
     */
    public static List<ProgrammingLanguage> filterByYearAfter(
            List<ProgrammingLanguage> languages, int year) {
        return languages.filter(lang -> lang.year() > year);
    }

    /**
     * 言語名のリストを取得
     */
    public static List<String> getNames(List<ProgrammingLanguage> languages) {
        return languages.map(ProgrammingLanguage::name);
    }

    /**
     * 平均年を計算
     */
    public static double averageYear(List<ProgrammingLanguage> languages) {
        if (languages.isEmpty()) return 0;
        int sum = languages.map(ProgrammingLanguage::year).foldLeft(0, Integer::sum);
        return (double) sum / languages.size();
    }

    public static void main(String[] args) {
        System.out.println("=== プログラミング言語 ===\n");

        List<ProgrammingLanguage> languages = List.of(
                new ProgrammingLanguage("Java", 1995),
                new ProgrammingLanguage("Scala", 2004),
                new ProgrammingLanguage("Kotlin", 2011),
                new ProgrammingLanguage("Rust", 2010),
                new ProgrammingLanguage("Go", 2009)
        );

        System.out.println("言語リスト: " + languages);

        // 名前でソート
        System.out.println("\n--- 名前でソート ---");
        System.out.println(sortByName(languages));

        // 年でソート
        System.out.println("\n--- 年でソート ---");
        System.out.println(sortByYear(languages));

        // 2005年以降の言語
        System.out.println("\n--- 2005年以降 ---");
        System.out.println(filterByYearAfter(languages, 2005));

        // 言語名のリスト
        System.out.println("\n--- 言語名のみ ---");
        System.out.println(getNames(languages));

        // 平均年
        System.out.println("\n--- 平均年 ---");
        System.out.printf("%.1f%n", averageYear(languages));
    }
}
