//! 第4章: 関数を値として扱う
//!
//! 高階関数（Higher-Order Function）を学びます。
//! 関数を引数として受け取る、または関数を戻り値として返す関数です。

// =============================================================================
// 4.1 高階関数とは
// =============================================================================

/// 高階関数の基本: 関数を引数として受け取る
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::apply_twice;
///
/// fn double(x: i32) -> i32 { x * 2 }
///
/// assert_eq!(apply_twice(double, 3), 12); // double(double(3)) = double(6) = 12
/// ```
pub fn apply_twice<T, F>(f: F, x: T) -> T
where
    F: Fn(T) -> T,
{
    f(f(x))
}

// =============================================================================
// 4.2 関数を引数として渡す
// =============================================================================

/// 'a' を除いた文字数でスコアを計算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::score;
///
/// assert_eq!(score("rust"), 4);  // r, u, s, t
/// assert_eq!(score("java"), 2);  // j, v (a を除く)
/// assert_eq!(score("scala"), 3); // s, c, l (a を除く)
/// ```
pub fn score(word: &str) -> usize {
    word.chars().filter(|&c| c != 'a').count()
}

/// スコア順にソート（昇順）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::{score, sort_by_score};
///
/// let words = vec!["rust", "java"];
/// let sorted = sort_by_score(&words, |w| score(w));
///
/// assert_eq!(sorted, vec!["java", "rust"]);
/// // java: 2, rust: 4
/// ```
pub fn sort_by_score<T, F, K>(list: &[T], key_fn: F) -> Vec<T>
where
    T: Clone,
    F: Fn(&T) -> K,
    K: Ord,
{
    let mut result = list.to_vec();
    result.sort_by_key(|item| key_fn(item));
    result
}

/// map: 各要素を変換
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::map;
///
/// let lengths: Vec<usize> = map(&["scala", "rust", "ada"], |s| s.len());
/// assert_eq!(lengths, vec![5, 4, 3]);
///
/// let doubles: Vec<i32> = map(&[5, 1, 2, 4, 0], |&i| i * 2);
/// assert_eq!(doubles, vec![10, 2, 4, 8, 0]);
/// ```
pub fn map<T, U, F>(list: &[T], f: F) -> Vec<U>
where
    F: Fn(&T) -> U,
{
    list.iter().map(f).collect()
}

/// filter: 条件に合う要素を抽出
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::filter;
///
/// let odds: Vec<i32> = filter(&[5, 1, 2, 4, 0], |&i| i % 2 == 1);
/// assert_eq!(odds, vec![5, 1]);
///
/// let large: Vec<i32> = filter(&[5, 1, 2, 4, 0], |&i| i > 4);
/// assert_eq!(large, vec![5]);
/// ```
pub fn filter<T: Clone, F>(list: &[T], predicate: F) -> Vec<T>
where
    F: Fn(&T) -> bool,
{
    list.iter().filter(|item| predicate(item)).cloned().collect()
}

/// fold: 畳み込み（foldLeft 相当）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::fold;
///
/// let sum = fold(&[5, 1, 2, 4, 100], 0, |acc, &i| acc + i);
/// assert_eq!(sum, 112);
///
/// let max = fold(&[5, 1, 2, 4, 15], i32::MIN, |max, &i| if i > max { i } else { max });
/// assert_eq!(max, 15);
///
/// let product = fold(&[1, 2, 3, 4], 1, |acc, &i| acc * i);
/// assert_eq!(product, 24);
/// ```
pub fn fold<T, U, F>(list: &[T], initial: U, f: F) -> U
where
    F: Fn(U, &T) -> U,
{
    list.iter().fold(initial, f)
}

// =============================================================================
// 4.3 構造体とパターン
// =============================================================================

/// プログラミング言語を表す構造体
#[derive(Debug, Clone, PartialEq)]
pub struct ProgrammingLanguage {
    pub name: String,
    pub year: i32,
}

impl ProgrammingLanguage {
    pub fn new(name: &str, year: i32) -> Self {
        Self {
            name: name.to_string(),
            year,
        }
    }
}

/// 構造体のフィールドを使った操作
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::{ProgrammingLanguage, get_names, filter_by_year};
///
/// let java = ProgrammingLanguage::new("Java", 1995);
/// let scala = ProgrammingLanguage::new("Scala", 2004);
/// let languages = vec![java.clone(), scala.clone()];
///
/// let names = get_names(&languages);
/// assert_eq!(names, vec!["Java", "Scala"]);
///
/// let young = filter_by_year(&languages, 2000);
/// assert_eq!(young, vec![scala]);
/// ```
pub fn get_names(languages: &[ProgrammingLanguage]) -> Vec<String> {
    languages.iter().map(|lang| lang.name.clone()).collect()
}

pub fn filter_by_year(languages: &[ProgrammingLanguage], min_year: i32) -> Vec<ProgrammingLanguage> {
    languages
        .iter()
        .filter(|lang| lang.year > min_year)
        .cloned()
        .collect()
}

// =============================================================================
// 4.4 関数を返す関数
// =============================================================================

/// n より大きいかを判定する関数を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::larger_than;
///
/// let larger_than_4 = larger_than(4);
/// assert!(larger_than_4(5));
/// assert!(!larger_than_4(4));
/// assert!(!larger_than_4(3));
///
/// // filter で使う場合は参照を受け取るクロージャでラップ
/// let large: Vec<i32> = vec![5, 1, 2, 4, 0]
///     .into_iter()
///     .filter(|x| larger_than(4)(*x))
///     .collect();
/// assert_eq!(large, vec![5]);
/// ```
pub fn larger_than(n: i32) -> impl Fn(i32) -> bool {
    move |i| i > n
}

/// n より大きいかを判定する関数を返す（参照版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::larger_than_ref;
///
/// let nums = vec![5, 1, 2, 4, 0];
/// let large: Vec<&i32> = nums.iter().filter(larger_than_ref(4)).collect();
/// assert_eq!(large, vec![&5]);
///
/// let medium: Vec<&i32> = nums.iter().filter(larger_than_ref(1)).collect();
/// assert_eq!(medium, vec![&5, &2, &4]);
/// ```
pub fn larger_than_ref(n: i32) -> impl Fn(&&i32) -> bool {
    move |&&i| i > n
}

/// n で割り切れるかを判定する関数を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::divisible_by;
///
/// let divisible_by_3 = divisible_by(3);
/// assert!(divisible_by_3(9));
/// assert!(divisible_by_3(6));
/// assert!(!divisible_by_3(7));
/// ```
pub fn divisible_by(n: i32) -> impl Fn(i32) -> bool {
    move |i| i % n == 0
}

/// 文字を含むかを判定する関数を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::contains_char;
///
/// let contains_s = contains_char('s');
/// assert!(contains_s("rust"));
/// assert!(contains_s("scala"));
/// assert!(!contains_s("java"));
/// ```
pub fn contains_char(c: char) -> impl Fn(&str) -> bool {
    move |s| s.contains(c)
}

// =============================================================================
// 4.5 カリー化
// =============================================================================

/// カリー化された関数: 2つの数を比較
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::curried_larger;
///
/// let larger_than_10 = curried_larger(10);
/// assert!(larger_than_10(15));
/// assert!(!larger_than_10(5));
/// ```
pub fn curried_larger(threshold: i32) -> impl Fn(i32) -> bool {
    move |value| value > threshold
}

/// カリー化された加算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::curried_add;
///
/// let add_10 = curried_add(10);
/// assert_eq!(add_10(5), 15);
/// assert_eq!(add_10(20), 30);
/// ```
pub fn curried_add(a: i32) -> impl Fn(i32) -> i32 {
    move |b| a + b
}

// =============================================================================
// 4.6 ワードスコアリング
// =============================================================================

/// ボーナススコア: 'c' を含む場合は +5
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::bonus;
///
/// assert_eq!(bonus("scala"), 5);
/// assert_eq!(bonus("clojure"), 5);
/// assert_eq!(bonus("rust"), 0);
/// ```
pub fn bonus(word: &str) -> i32 {
    if word.contains('c') { 5 } else { 0 }
}

/// ペナルティ: 's' を含む場合は -7
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::penalty;
///
/// assert_eq!(penalty("scala"), 7);
/// assert_eq!(penalty("rust"), 7);
/// assert_eq!(penalty("java"), 0);
/// ```
pub fn penalty(word: &str) -> i32 {
    if word.contains('s') { 7 } else { 0 }
}

/// スコアリング関数を使ってランキングを作成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::{score, bonus, penalty, ranked_words};
///
/// let words = vec!["ada", "haskell", "scala", "java", "rust"];
///
/// // 基本スコアでランキング
/// let ranking1 = ranked_words(&words, |w| score(w) as i32);
/// assert_eq!(ranking1, vec!["haskell", "rust", "scala", "java", "ada"]);
///
/// // ボーナス付きスコアでランキング
/// let ranking2 = ranked_words(&words, |w| score(w) as i32 + bonus(w));
/// assert_eq!(ranking2, vec!["scala", "haskell", "rust", "java", "ada"]);
///
/// // ボーナスとペナルティ付きスコアでランキング
/// // java: 2+0-0=2, scala: 3+5-7=1, ada: 0+0-0=0, haskell: 6+0-7=-1, rust: 4+0-7=-3
/// let ranking3 = ranked_words(&words, |w| score(w) as i32 + bonus(w) - penalty(w));
/// assert_eq!(ranking3[0], "java");   // 2点
/// assert_eq!(ranking3[4], "rust");   // -3点
/// ```
pub fn ranked_words<'a, F>(words: &[&'a str], word_score: F) -> Vec<&'a str>
where
    F: Fn(&str) -> i32,
{
    let mut result: Vec<&'a str> = words.to_vec();
    result.sort_by(|a, b| word_score(b).cmp(&word_score(a))); // 降順
    result
}

/// 条件を満たす要素の数をカウント
///
/// # Examples
///
/// ```
/// use grokking_fp::ch04_higher_order::count_where;
///
/// assert_eq!(count_where(&[1, 2, 3, 4, 5], |&i| i > 3), 2);
/// assert_eq!(count_where(&["a", "bb", "ccc"], |s| s.len() > 1), 2);
/// ```
pub fn count_where<T, F>(list: &[T], predicate: F) -> usize
where
    F: Fn(&T) -> bool,
{
    list.iter().filter(|item| predicate(item)).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply_twice() {
        let double = |x: i32| x * 2;
        assert_eq!(apply_twice(double, 3), 12);
        assert_eq!(apply_twice(double, 5), 20);
    }

    #[test]
    fn test_score() {
        assert_eq!(score("rust"), 4);
        assert_eq!(score("java"), 2);
        assert_eq!(score("scala"), 3);
        assert_eq!(score("aaa"), 0);
    }

    #[test]
    fn test_sort_by_score() {
        let words = vec!["rust", "java", "ada"];
        let sorted = sort_by_score(&words, |w| score(w));
        assert_eq!(sorted, vec!["ada", "java", "rust"]);
    }

    #[test]
    fn test_map() {
        let nums = vec![1, 2, 3];
        let doubled: Vec<i32> = map(&nums, |&x| x * 2);
        assert_eq!(doubled, vec![2, 4, 6]);
    }

    #[test]
    fn test_filter() {
        let nums = vec![1, 2, 3, 4, 5];
        let evens: Vec<i32> = filter(&nums, |&x| x % 2 == 0);
        assert_eq!(evens, vec![2, 4]);
    }

    #[test]
    fn test_fold() {
        let nums = vec![1, 2, 3, 4, 5];
        let sum = fold(&nums, 0, |acc, &x| acc + x);
        assert_eq!(sum, 15);

        let concat = fold(&["a", "b", "c"], String::new(), |acc, &s| acc + s);
        assert_eq!(concat, "abc");
    }

    #[test]
    fn test_programming_language() {
        let java = ProgrammingLanguage::new("Java", 1995);
        let scala = ProgrammingLanguage::new("Scala", 2004);
        let rust = ProgrammingLanguage::new("Rust", 2010);
        let languages = vec![java, scala.clone(), rust.clone()];

        let names = get_names(&languages);
        assert_eq!(names, vec!["Java", "Scala", "Rust"]);

        let young = filter_by_year(&languages, 2000);
        assert_eq!(young, vec![scala, rust]);
    }

    #[test]
    fn test_larger_than() {
        let larger_than_5 = larger_than(5);
        assert!(larger_than_5(10));
        assert!(!larger_than_5(5));
        assert!(!larger_than_5(3));
    }

    #[test]
    fn test_divisible_by() {
        let div_by_2 = divisible_by(2);
        assert!(div_by_2(4));
        assert!(div_by_2(10));
        assert!(!div_by_2(5));
    }

    #[test]
    fn test_contains_char() {
        let has_r = contains_char('r');
        assert!(has_r("rust"));
        assert!(!has_r("java"));
    }

    #[test]
    fn test_curried_add() {
        let add_5 = curried_add(5);
        assert_eq!(add_5(10), 15);
        assert_eq!(add_5(0), 5);
    }

    #[test]
    fn test_bonus_penalty() {
        assert_eq!(bonus("scala"), 5);
        assert_eq!(bonus("rust"), 0);
        assert_eq!(penalty("scala"), 7);
        assert_eq!(penalty("java"), 0);
    }

    #[test]
    fn test_ranked_words() {
        let words = vec!["ada", "haskell", "scala", "java", "rust"];

        // 基本スコア
        let ranking1 = ranked_words(&words, |w| score(w) as i32);
        assert_eq!(ranking1[0], "haskell"); // 7文字、aなし

        // ボーナス付き
        let ranking2 = ranked_words(&words, |w| score(w) as i32 + bonus(w));
        assert_eq!(ranking2[0], "scala"); // 3 + 5 = 8
    }

    #[test]
    fn test_count_where() {
        assert_eq!(count_where(&[1, 2, 3, 4, 5], |&x| x > 3), 2);
        assert_eq!(count_where(&[1, 2, 3, 4, 5], |&x| x % 2 == 0), 2);
    }
}
