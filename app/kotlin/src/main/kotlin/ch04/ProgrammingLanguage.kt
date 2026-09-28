package ch04

/**
 * 第4章: プログラミング言語のデータクラス
 *
 * data class と関数参照（::）の例
 */
data class ProgrammingLanguage(val name: String, val year: Int)

/** 言語名のリストを取得 */
fun names(languages: List<ProgrammingLanguage>): List<String> =
    languages.map(ProgrammingLanguage::name)

/** 指定年より後に登場した言語を取得 */
fun filterByYearAfter(languages: List<ProgrammingLanguage>, year: Int): List<ProgrammingLanguage> =
    languages.filter { it.year > year }

/** 言語名でソート */
fun sortByName(languages: List<ProgrammingLanguage>): List<ProgrammingLanguage> =
    languages.sortedBy(ProgrammingLanguage::name)

/** 新しい順にソート */
fun sortByYearDescending(languages: List<ProgrammingLanguage>): List<ProgrammingLanguage> =
    languages.sortedByDescending { it.year }

/** 平均年を計算（空リストなら 0.0） */
fun averageYear(languages: List<ProgrammingLanguage>): Double =
    if (languages.isEmpty()) 0.0 else languages.map { it.year }.average()

/** 名前だけを変えた新しいインスタンスを返す */
fun renamed(language: ProgrammingLanguage, newName: String): ProgrammingLanguage =
    language.copy(name = newName)
