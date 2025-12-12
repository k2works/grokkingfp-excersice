//! 第2章: 純粋関数とテスト
//!
//! 純粋関数の特徴、副作用の排除、テストの容易さを学びます。
//! Rust の所有権システムがどのように純粋性を支援するかも理解します。

// =============================================================================
// 2.1 純粋関数の例
// =============================================================================

/// 純粋関数: 同じ入力に対して常に同じ出力を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::add;
/// // 何度呼び出しても同じ結果
/// assert_eq!(add(2, 3), 5);
/// assert_eq!(add(2, 3), 5);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

/// 純粋関数: 文字列の長さを計算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::string_length;
/// assert_eq!(string_length("hello"), 5);
/// assert_eq!(string_length(""), 0);
/// ```
pub fn string_length(s: &str) -> usize {
    s.len()
}

/// 純粋関数: 文字 'a' を除外したワードスコア
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::word_score;
/// assert_eq!(word_score("Scala"), 3);  // "Scl" → 3文字
/// assert_eq!(word_score("function"), 8);  // 'a' なし → 8文字
/// assert_eq!(word_score("aaa"), 0);  // すべて 'a' → 0文字
/// assert_eq!(word_score(""), 0);
/// ```
pub fn word_score(word: &str) -> usize {
    word.chars().filter(|&c| c != 'a').count()
}

/// 純粋関数: ボーナス付きワードスコア
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::bonus_score;
/// // 'c' が含まれる場合は +5
/// assert_eq!(bonus_score("Scala"), 8);  // 3 + 5 ('a'除外で"Scl"=3, 'c'含む)
/// assert_eq!(bonus_score("cat"), 7);    // 2 + 5 ('a'除外で"ct"=2, 'c'含む)
/// assert_eq!(bonus_score("Rust"), 4);   // 4 + 0 ('c'なし)
/// ```
pub fn bonus_score(word: &str) -> usize {
    let base = word_score(word);
    if word.contains('c') {
        base + 5
    } else {
        base
    }
}

// =============================================================================
// 2.2 ショッピングカートの例（純粋関数版）
// =============================================================================

/// ショッピングカートの割引率を計算（純粋関数）
///
/// - "Book" が含まれていれば 5% 割引
/// - それ以外は 0% 割引
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::get_discount_percentage;
/// assert_eq!(get_discount_percentage(&["Apple", "Book"]), 5);
/// assert_eq!(get_discount_percentage(&["Apple", "Orange"]), 0);
/// assert_eq!(get_discount_percentage(&[]), 0);
/// ```
pub fn get_discount_percentage(items: &[&str]) -> i32 {
    if items.contains(&"Book") {
        5
    } else {
        0
    }
}

/// 割引額を計算（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::calculate_discount;
/// assert_eq!(calculate_discount(100, 10), 10);
/// assert_eq!(calculate_discount(100, 5), 5);
/// assert_eq!(calculate_discount(200, 15), 30);
/// ```
pub fn calculate_discount(total: i32, percentage: i32) -> i32 {
    total * percentage / 100
}

/// 最終価格を計算（純粋関数の合成）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::calculate_final_price;
/// // Book があれば 5% 割引
/// assert_eq!(calculate_final_price(100, &["Apple", "Book"]), 95);
/// assert_eq!(calculate_final_price(100, &["Apple"]), 100);
/// ```
pub fn calculate_final_price(total: i32, items: &[&str]) -> i32 {
    let discount_percentage = get_discount_percentage(items);
    let discount = calculate_discount(total, discount_percentage);
    total - discount
}

// =============================================================================
// 2.3 チップ計算の例
// =============================================================================

/// チップの割合を計算（純粋関数）
///
/// - 6人以上のグループ → 20%
/// - 1-5人のグループ → 10%
/// - 0人（空リスト） → 0%
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::get_tip_percentage;
/// assert_eq!(get_tip_percentage(&["Alice", "Bob", "Charlie", "Dave", "Eve", "Frank"]), 20);
/// assert_eq!(get_tip_percentage(&["Alice", "Bob"]), 10);
/// assert_eq!(get_tip_percentage(&[]), 0);
/// ```
pub fn get_tip_percentage(names: &[&str]) -> i32 {
    match names.len() {
        0 => 0,
        1..=5 => 10,
        _ => 20,
    }
}

/// チップ額を計算（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::calculate_tip;
/// assert_eq!(calculate_tip(100, 20), 20);
/// assert_eq!(calculate_tip(100, 10), 10);
/// assert_eq!(calculate_tip(50, 15), 7);  // 50 * 15 / 100 = 7.5 → 7
/// ```
pub fn calculate_tip(bill: i32, percentage: i32) -> i32 {
    bill * percentage / 100
}

// =============================================================================
// 2.4 参照透過性
// =============================================================================

/// 参照透過性を持つ関数
///
/// 式をその評価結果で置き換えても、プログラムの意味が変わらない
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::referential_transparency_example;
/// // word_score("Scala") は 3 ('a'除外で"Scl")
/// // word_score("Java") は 2 ('a'除外で"Jv")
/// // 合計は 5
/// let result1 = referential_transparency_example();
/// let result2 = 3 + 2;  // word_score の結果で直接置き換え
/// assert_eq!(result1, result2);
/// ```
pub fn referential_transparency_example() -> usize {
    let score1 = word_score("Scala");  // 3 (Scl)
    let score2 = word_score("Java");   // 2 (Jv)
    score1 + score2
}

// =============================================================================
// 2.5 Rust の所有権と純粋性
// =============================================================================

/// イミュータブルなデータ変換（所有権を取得して新しい値を返す）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::append_exclamation;
/// let original = String::from("Hello");
/// let result = append_exclamation(original);
/// assert_eq!(result, "Hello!");
/// // original は move されたので使用不可
/// ```
pub fn append_exclamation(s: String) -> String {
    format!("{}!", s)
}

/// 借用を使った純粋関数（元のデータを変更しない）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::count_vowels;
/// let text = "Hello World";
/// assert_eq!(count_vowels(text), 3);  // e, o, o
/// // text はまだ使用可能
/// ```
pub fn count_vowels(s: &str) -> usize {
    s.chars()
        .filter(|c| matches!(c.to_ascii_lowercase(), 'a' | 'e' | 'i' | 'o' | 'u'))
        .count()
}

/// Vec を変換する純粋関数（新しい Vec を返す）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::double_all;
/// let numbers = vec![1, 2, 3];
/// let doubled = double_all(&numbers);
/// assert_eq!(doubled, vec![2, 4, 6]);
/// // numbers はまだ使用可能
/// assert_eq!(numbers, vec![1, 2, 3]);
/// ```
pub fn double_all(numbers: &[i32]) -> Vec<i32> {
    numbers.iter().map(|x| x * 2).collect()
}

/// フィルタリング（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::filter_positive;
/// let numbers = vec![-2, -1, 0, 1, 2];
/// let positive = filter_positive(&numbers);
/// assert_eq!(positive, vec![1, 2]);
/// ```
pub fn filter_positive(numbers: &[i32]) -> Vec<i32> {
    numbers.iter().filter(|&&x| x > 0).copied().collect()
}

// =============================================================================
// 2.6 高度な純粋関数
// =============================================================================

/// 文字列のリストから最長の文字列を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::find_longest;
/// let words = vec!["apple", "banana", "cherry"];
/// assert_eq!(find_longest(&words), Some("cherry"));  // banana と cherry は同じ長さ(6)
/// let words2 = vec!["a", "bb", "ccc"];
/// assert_eq!(find_longest(&words2), Some("ccc"));
/// assert_eq!(find_longest(&vec![]), None);
/// ```
pub fn find_longest<'a>(words: &[&'a str]) -> Option<&'a str> {
    words.iter().max_by_key(|w| w.len()).copied()
}

/// 全ての要素が条件を満たすか確認
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::all_positive;
/// assert_eq!(all_positive(&[1, 2, 3]), true);
/// assert_eq!(all_positive(&[1, -2, 3]), false);
/// assert_eq!(all_positive(&[]), true);  // 空リストは true
/// ```
pub fn all_positive(numbers: &[i32]) -> bool {
    numbers.iter().all(|&x| x > 0)
}

/// いずれかの要素が条件を満たすか確認
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::any_negative;
/// assert_eq!(any_negative(&[1, -2, 3]), true);
/// assert_eq!(any_negative(&[1, 2, 3]), false);
/// assert_eq!(any_negative(&[]), false);  // 空リストは false
/// ```
pub fn any_negative(numbers: &[i32]) -> bool {
    numbers.iter().any(|&x| x < 0)
}

/// 合計を計算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::sum;
/// assert_eq!(sum(&[1, 2, 3, 4, 5]), 15);
/// assert_eq!(sum(&[]), 0);
/// ```
pub fn sum(numbers: &[i32]) -> i32 {
    numbers.iter().sum()
}

/// 平均を計算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::average;
/// assert_eq!(average(&[1, 2, 3, 4, 5]), Some(3.0));
/// assert_eq!(average(&[]), None);
/// ```
pub fn average(numbers: &[i32]) -> Option<f64> {
    if numbers.is_empty() {
        None
    } else {
        Some(sum(numbers) as f64 / numbers.len() as f64)
    }
}

// =============================================================================
// 2.7 不純な関数の例（参考）
// =============================================================================

/// 不純な関数の例: 現在時刻を返す
///
/// 注意: これは純粋ではない（呼び出すたびに異なる値を返す）
///
/// この関数は副作用の例として提供されています。
/// 実際のコードでは、時刻を引数として受け取る純粋関数を使用することを推奨します。
pub fn current_timestamp() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

/// 純粋なバージョン: タイムスタンプを引数として受け取る
///
/// # Examples
///
/// ```
/// use grokking_fp::ch02_pure_functions::format_timestamp;
/// assert_eq!(format_timestamp(0), "Timestamp: 0");
/// assert_eq!(format_timestamp(1234567890), "Timestamp: 1234567890");
/// ```
pub fn format_timestamp(timestamp: u64) -> String {
    format!("Timestamp: {}", timestamp)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_score() {
        assert_eq!(word_score("Scala"), 3);
        assert_eq!(word_score("function"), 8);
        assert_eq!(word_score("aaa"), 0);
        assert_eq!(word_score(""), 0);
        assert_eq!(word_score("Rust"), 4);
    }

    #[test]
    fn test_bonus_score() {
        assert_eq!(bonus_score("Scala"), 8);  // 3 + 5
        assert_eq!(bonus_score("Rust"), 4);   // 4 + 0
    }

    #[test]
    fn test_get_discount_percentage() {
        assert_eq!(get_discount_percentage(&["Apple", "Book"]), 5);
        assert_eq!(get_discount_percentage(&["Apple"]), 0);
        assert_eq!(get_discount_percentage(&[]), 0);
    }

    #[test]
    fn test_calculate_final_price() {
        assert_eq!(calculate_final_price(100, &["Apple", "Book"]), 95);
        assert_eq!(calculate_final_price(100, &["Apple"]), 100);
        assert_eq!(calculate_final_price(200, &["Book"]), 190);
    }

    #[test]
    fn test_get_tip_percentage() {
        assert_eq!(get_tip_percentage(&[]), 0);
        assert_eq!(get_tip_percentage(&["A"]), 10);
        assert_eq!(get_tip_percentage(&["A", "B", "C", "D", "E"]), 10);
        assert_eq!(get_tip_percentage(&["A", "B", "C", "D", "E", "F"]), 20);
    }

    #[test]
    fn test_count_vowels() {
        assert_eq!(count_vowels("Hello World"), 3);
        assert_eq!(count_vowels("AEIOU"), 5);
        assert_eq!(count_vowels("xyz"), 0);
    }

    #[test]
    fn test_double_all() {
        assert_eq!(double_all(&[1, 2, 3]), vec![2, 4, 6]);
        assert_eq!(double_all(&[]), Vec::<i32>::new());
    }

    #[test]
    fn test_filter_positive() {
        assert_eq!(filter_positive(&[-2, -1, 0, 1, 2]), vec![1, 2]);
        assert_eq!(filter_positive(&[-1, -2]), Vec::<i32>::new());
    }

    #[test]
    fn test_find_longest() {
        assert_eq!(find_longest(&["a", "bb", "ccc"]), Some("ccc"));
        assert_eq!(find_longest(&["apple", "banana", "cherry"]), Some("cherry"));
        assert_eq!(find_longest(&[]), None);
    }

    #[test]
    fn test_all_positive() {
        assert!(all_positive(&[1, 2, 3]));
        assert!(!all_positive(&[1, -2, 3]));
        assert!(all_positive(&[]));
    }

    #[test]
    fn test_any_negative() {
        assert!(any_negative(&[1, -2, 3]));
        assert!(!any_negative(&[1, 2, 3]));
        assert!(!any_negative(&[]));
    }

    #[test]
    fn test_sum() {
        assert_eq!(sum(&[1, 2, 3, 4, 5]), 15);
        assert_eq!(sum(&[]), 0);
    }

    #[test]
    fn test_average() {
        assert_eq!(average(&[1, 2, 3, 4, 5]), Some(3.0));
        assert_eq!(average(&[]), None);
    }

    #[test]
    fn test_referential_transparency() {
        // 参照透過性: 同じ呼び出しは常に同じ結果
        let result1 = word_score("Scala");
        let result2 = word_score("Scala");
        assert_eq!(result1, result2);
        assert_eq!(result1, 3);  // "Scl"

        // 式を結果で置き換えても同じ
        let computed = word_score("Scala") + word_score("Java");
        let direct = 3 + 2;  // "Scl" = 3, "Jv" = 2
        assert_eq!(computed, direct);
        assert_eq!(computed, 5);
    }
}
