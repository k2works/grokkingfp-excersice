//! 第1章: 関数型プログラミング入門
//!
//! 命令型プログラミングと関数型プログラミングの違いを学び、
//! Rust における関数定義の基本を理解します。

// =============================================================================
// 1.1 命令型 vs 関数型
// =============================================================================

/// 命令型スタイル: ステップバイステップで「どうやるか」を記述
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::calculate_score_imperative;
/// assert_eq!(calculate_score_imperative("hello"), 5);
/// ```
pub fn calculate_score_imperative(word: &str) -> usize {
    let mut score = 0;
    for _ in word.chars() {
        score += 1;
    }
    score
}

/// 関数型スタイル: 「何をするか」を宣言的に記述
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::word_score;
/// assert_eq!(word_score("hello"), 5);
/// assert_eq!(word_score("Rust"), 4);
/// assert_eq!(word_score(""), 0);
/// ```
pub fn word_score(word: &str) -> usize {
    word.len()
}

// =============================================================================
// 1.2 Rust の基本的な関数
// =============================================================================

/// 整数をインクリメント
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::increment;
/// assert_eq!(increment(0), 1);
/// assert_eq!(increment(5), 6);
/// assert_eq!(increment(-1), 0);
/// ```
pub fn increment(x: i32) -> i32 {
    x + 1
}

/// 文字列の最初の文字を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::get_first_character;
/// assert_eq!(get_first_character("hello"), Some('h'));
/// assert_eq!(get_first_character("Rust"), Some('R'));
/// assert_eq!(get_first_character(""), None);
/// ```
pub fn get_first_character(s: &str) -> Option<char> {
    s.chars().next()
}

/// 2つの整数を加算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::add;
/// assert_eq!(add(2, 3), 5);
/// assert_eq!(add(-1, 1), 0);
/// assert_eq!(add(0, 0), 0);
/// ```
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

/// 整数を2倍にする
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::double;
/// assert_eq!(double(5), 10);
/// assert_eq!(double(0), 0);
/// assert_eq!(double(-3), -6);
/// ```
pub fn double(x: i32) -> i32 {
    x * 2
}

// =============================================================================
// 1.3 文字列操作
// =============================================================================

/// 挨拶文を生成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::greet;
/// assert_eq!(greet("World"), "Hello, World!");
/// assert_eq!(greet("Rust"), "Hello, Rust!");
/// ```
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

/// 文字列を大文字に変換
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::to_uppercase;
/// assert_eq!(to_uppercase("hello"), "HELLO");
/// assert_eq!(to_uppercase("Rust"), "RUST");
/// ```
pub fn to_uppercase(s: &str) -> String {
    s.to_uppercase()
}

/// 文字列を逆順にする
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::reverse_string;
/// assert_eq!(reverse_string("hello"), "olleh");
/// assert_eq!(reverse_string("Rust"), "tsuR");
/// assert_eq!(reverse_string(""), "");
/// ```
pub fn reverse_string(s: &str) -> String {
    s.chars().rev().collect()
}

// =============================================================================
// 1.4 数値計算
// =============================================================================

/// 絶対値を計算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::absolute;
/// assert_eq!(absolute(5), 5);
/// assert_eq!(absolute(-5), 5);
/// assert_eq!(absolute(0), 0);
/// ```
pub fn absolute(x: i32) -> i32 {
    x.abs()
}

/// 最大値を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::max;
/// assert_eq!(max(3, 5), 5);
/// assert_eq!(max(10, 2), 10);
/// assert_eq!(max(-1, -5), -1);
/// ```
pub fn max(a: i32, b: i32) -> i32 {
    if a > b { a } else { b }
}

/// 最小値を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::min;
/// assert_eq!(min(3, 5), 3);
/// assert_eq!(min(10, 2), 2);
/// assert_eq!(min(-1, -5), -5);
/// ```
pub fn min(a: i32, b: i32) -> i32 {
    if a < b { a } else { b }
}

/// 値をクランプ（範囲内に制限）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::clamp;
/// assert_eq!(clamp(5, 0, 10), 5);
/// assert_eq!(clamp(-5, 0, 10), 0);
/// assert_eq!(clamp(15, 0, 10), 10);
/// ```
pub fn clamp(value: i32, min_val: i32, max_val: i32) -> i32 {
    if value < min_val {
        min_val
    } else if value > max_val {
        max_val
    } else {
        value
    }
}

// =============================================================================
// 1.5 述語関数（Predicate）
// =============================================================================

/// 偶数かどうかを判定
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::is_even;
/// assert_eq!(is_even(2), true);
/// assert_eq!(is_even(3), false);
/// assert_eq!(is_even(0), true);
/// assert_eq!(is_even(-4), true);
/// ```
pub fn is_even(n: i32) -> bool {
    n % 2 == 0
}

/// 正の数かどうかを判定
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::is_positive;
/// assert_eq!(is_positive(5), true);
/// assert_eq!(is_positive(-3), false);
/// assert_eq!(is_positive(0), false);
/// ```
pub fn is_positive(n: i32) -> bool {
    n > 0
}

/// 文字列が空かどうかを判定
///
/// # Examples
///
/// ```
/// use grokking_fp::ch01_intro::is_empty;
/// assert_eq!(is_empty(""), true);
/// assert_eq!(is_empty("hello"), false);
/// ```
pub fn is_empty(s: &str) -> bool {
    s.is_empty()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_score() {
        assert_eq!(word_score("hello"), 5);
        assert_eq!(word_score(""), 0);
        assert_eq!(word_score("Rust"), 4);
    }

    #[test]
    fn test_increment() {
        assert_eq!(increment(0), 1);
        assert_eq!(increment(-1), 0);
        assert_eq!(increment(i32::MAX - 1), i32::MAX);
    }

    #[test]
    fn test_get_first_character() {
        assert_eq!(get_first_character("hello"), Some('h'));
        assert_eq!(get_first_character(""), None);
    }

    #[test]
    fn test_greet() {
        assert_eq!(greet("World"), "Hello, World!");
    }

    #[test]
    fn test_is_even() {
        assert!(is_even(0));
        assert!(is_even(2));
        assert!(is_even(-4));
        assert!(!is_even(1));
        assert!(!is_even(-3));
    }

    #[test]
    fn test_clamp() {
        assert_eq!(clamp(5, 0, 10), 5);
        assert_eq!(clamp(-5, 0, 10), 0);
        assert_eq!(clamp(15, 0, 10), 10);
        assert_eq!(clamp(0, 0, 10), 0);
        assert_eq!(clamp(10, 0, 10), 10);
    }
}
