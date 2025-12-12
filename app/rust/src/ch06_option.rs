//! 第6章: Option 型による安全なエラーハンドリング
//!
//! Rust の `Option<T>` は「値があるかないか」を型安全に表現します。
//! null や例外に頼らず、コンパイル時にエラーを検出できます。

// =============================================================================
// 6.1 なぜ Option が必要か
// =============================================================================

/// Option を使わない場合の問題
/// - panic! が発生する可能性
/// - コンパイル時に検出できない
///
/// # Panics
///
/// 不正な入力でパニックする可能性あり
pub fn parse_show_unsafe(raw_show: &str) -> (&str, i32, i32) {
    let bracket_open = raw_show.find('(').unwrap();
    let bracket_close = raw_show.find(')').unwrap();
    let dash = raw_show.find('-').unwrap();

    let name = raw_show[..bracket_open].trim();
    let year_start: i32 = raw_show[bracket_open + 1..dash].parse().unwrap();
    let year_end: i32 = raw_show[dash + 1..bracket_close].parse().unwrap();

    (name, year_start, year_end)
}

// =============================================================================
// 6.2 Option の基本
// =============================================================================

/// TV番組を表す構造体
#[derive(Debug, Clone, PartialEq)]
pub struct TvShow {
    pub title: String,
    pub start: i32,
    pub end: i32,
}

impl TvShow {
    pub fn new(title: &str, start: i32, end: i32) -> Self {
        Self {
            title: title.to_string(),
            start,
            end,
        }
    }
}

// =============================================================================
// 6.3 TV番組のパース - Option を使う方法
// =============================================================================

/// 番組名を抽出
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::extract_name;
///
/// assert_eq!(extract_name("Breaking Bad (2008-2013)"), Some("Breaking Bad".to_string()));
/// assert_eq!(extract_name("(2008-2013)"), None);
/// assert_eq!(extract_name("No brackets"), None);
/// ```
pub fn extract_name(raw_show: &str) -> Option<String> {
    let bracket_open = raw_show.find('(')?;
    if bracket_open > 0 {
        Some(raw_show[..bracket_open].trim().to_string())
    } else {
        None
    }
}

/// 開始年を抽出
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::extract_year_start;
///
/// assert_eq!(extract_year_start("Breaking Bad (2008-2013)"), Some(2008));
/// assert_eq!(extract_year_start("Chernobyl (2019)"), None);
/// assert_eq!(extract_year_start("Invalid"), None);
/// ```
pub fn extract_year_start(raw_show: &str) -> Option<i32> {
    let bracket_open = raw_show.find('(')?;
    let dash = raw_show.find('-')?;

    if dash > bracket_open + 1 {
        let year_str = &raw_show[bracket_open + 1..dash];
        year_str.parse().ok()
    } else {
        None
    }
}

/// 終了年を抽出
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::extract_year_end;
///
/// assert_eq!(extract_year_end("Breaking Bad (2008-2013)"), Some(2013));
/// assert_eq!(extract_year_end("Chernobyl (2019)"), None);
/// assert_eq!(extract_year_end("Invalid"), None);
/// ```
pub fn extract_year_end(raw_show: &str) -> Option<i32> {
    let dash = raw_show.find('-')?;
    let bracket_close = raw_show.find(')')?;

    if bracket_close > dash + 1 {
        let year_str = &raw_show[dash + 1..bracket_close];
        year_str.parse().ok()
    } else {
        None
    }
}

/// 単年の番組から年を抽出（例: "Chernobyl (2019)"）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::extract_single_year;
///
/// assert_eq!(extract_single_year("Chernobyl (2019)"), Some(2019));
/// assert_eq!(extract_single_year("Breaking Bad (2008-2013)"), None);
/// assert_eq!(extract_single_year("Invalid"), None);
/// ```
pub fn extract_single_year(raw_show: &str) -> Option<i32> {
    // ダッシュがある場合は単年ではない
    if raw_show.contains('-') {
        return None;
    }

    let bracket_open = raw_show.find('(')?;
    let bracket_close = raw_show.find(')')?;

    if bracket_close > bracket_open + 1 {
        let year_str = &raw_show[bracket_open + 1..bracket_close];
        year_str.parse().ok()
    } else {
        None
    }
}

/// TV番組をパース（Option 版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::{TvShow, parse_show};
///
/// assert_eq!(
///     parse_show("Breaking Bad (2008-2013)"),
///     Some(TvShow::new("Breaking Bad", 2008, 2013))
/// );
/// assert_eq!(
///     parse_show("Chernobyl (2019)"),
///     Some(TvShow::new("Chernobyl", 2019, 2019))
/// );
/// assert_eq!(parse_show("Invalid"), None);
/// assert_eq!(parse_show("The Wire 2002-2008"), None);
/// ```
pub fn parse_show(raw_show: &str) -> Option<TvShow> {
    let name = extract_name(raw_show)?;
    let year_start = extract_year_start(raw_show).or_else(|| extract_single_year(raw_show))?;
    let year_end = extract_year_end(raw_show).or_else(|| extract_single_year(raw_show))?;

    Some(TvShow::new(&name, year_start, year_end))
}

// =============================================================================
// 6.4 orElse によるフォールバック
// =============================================================================

/// orElse のデモ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::or_else_demo;
///
/// or_else_demo();
/// ```
pub fn or_else_demo() {
    let seven: Option<i32> = Some(7);
    let eight: Option<i32> = Some(8);
    let none: Option<i32> = None;

    assert_eq!(seven.or(eight), Some(7));   // 最初が Some なのでそのまま
    assert_eq!(none.or(eight), Some(8));    // 最初が None なので代替を使用
    assert_eq!(seven.or(none), Some(7));
    assert_eq!(none.or(none), None);
}

// =============================================================================
// 6.5 Option の主要メソッド
// =============================================================================

/// map: 値があれば変換
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::option_map_demo;
///
/// option_map_demo();
/// ```
pub fn option_map_demo() {
    let year: Option<i32> = Some(996);
    let no_year: Option<i32> = None;

    assert_eq!(year.map(|y| y * 2), Some(1992));
    assert_eq!(no_year.map(|y| y * 2), None);
}

/// and_then (flatMap): 値があれば Option を返す関数を適用
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::option_and_then_demo;
///
/// option_and_then_demo();
/// ```
pub fn option_and_then_demo() {
    let year: Option<i32> = Some(996);
    let no_year: Option<i32> = None;

    assert_eq!(year.and_then(|y| Some(y * 2)), Some(1992));
    assert_eq!(no_year.and_then(|y| Some(y * 2)), None);
}

/// filter: 条件を満たさなければ None
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::option_filter_demo;
///
/// option_filter_demo();
/// ```
pub fn option_filter_demo() {
    let year: Option<i32> = Some(996);

    assert_eq!(year.filter(|&y| y < 2020), Some(996));
    assert_eq!(year.filter(|&y| y > 2020), None);
}

/// unwrap_or: None ならデフォルト値
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::option_unwrap_or_demo;
///
/// option_unwrap_or_demo();
/// ```
pub fn option_unwrap_or_demo() {
    let year: Option<i32> = Some(996);
    let no_year: Option<i32> = None;

    assert_eq!(year.unwrap_or(0), 996);
    assert_eq!(no_year.unwrap_or(0), 0);
}

// =============================================================================
// 6.6 エラーハンドリング戦略
// =============================================================================

/// Best-effort 戦略: パースできたものだけ返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::{TvShow, parse_shows_best_effort};
///
/// let raw_shows = vec![
///     "Breaking Bad (2008-2013)",
///     "The Wire 2002 2008",  // 無効な形式
///     "Mad Men (2007-2015)",
/// ];
///
/// let shows = parse_shows_best_effort(&raw_shows);
/// assert_eq!(shows.len(), 2);
/// assert_eq!(shows[0].title, "Breaking Bad");
/// assert_eq!(shows[1].title, "Mad Men");
/// ```
pub fn parse_shows_best_effort(raw_shows: &[&str]) -> Vec<TvShow> {
    raw_shows
        .iter()
        .filter_map(|raw| parse_show(raw))
        .collect()
}

/// All-or-nothing 戦略: 全部成功するか、全部失敗
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::{TvShow, parse_shows_all_or_nothing};
///
/// // 全部成功
/// let valid = vec!["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"];
/// let result = parse_shows_all_or_nothing(&valid);
/// assert!(result.is_some());
/// assert_eq!(result.unwrap().len(), 2);
///
/// // 一つでも失敗 → None
/// let invalid = vec!["Breaking Bad (2008-2013)", "Invalid"];
/// let result = parse_shows_all_or_nothing(&invalid);
/// assert!(result.is_none());
/// ```
pub fn parse_shows_all_or_nothing(raw_shows: &[&str]) -> Option<Vec<TvShow>> {
    raw_shows.iter().map(|raw| parse_show(raw)).collect()
}

// =============================================================================
// 6.7 安全な除算
// =============================================================================

/// 安全な除算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::safe_divide;
///
/// assert_eq!(safe_divide(10, 2), Some(5));
/// assert_eq!(safe_divide(10, 0), None);
/// assert_eq!(safe_divide(7, 2), Some(3));
/// ```
pub fn safe_divide(a: i32, b: i32) -> Option<i32> {
    if b == 0 {
        None
    } else {
        Some(a / b)
    }
}

/// 2つの数値文字列を足し算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::add_strings;
///
/// assert_eq!(add_strings("10", "20"), Some(30));
/// assert_eq!(add_strings("10", "abc"), None);
/// assert_eq!(add_strings("xyz", "20"), None);
/// ```
pub fn add_strings(a: &str, b: &str) -> Option<i32> {
    let x: i32 = a.parse().ok()?;
    let y: i32 = b.parse().ok()?;
    Some(x + y)
}

// =============================================================================
// 6.8 forall と exists 相当
// =============================================================================

/// Option に対する forall（Rust では map + unwrap_or で実現）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::option_forall;
///
/// let year: Option<i32> = Some(996);
/// let no_year: Option<i32> = None;
///
/// // forall: 「全て」または「存在しない」
/// assert!(option_forall(&year, |&y| y < 2020));
/// assert!(option_forall(&no_year, |&y| y < 2020)); // None は true
/// assert!(!option_forall(&year, |&y| y > 2020));
/// ```
pub fn option_forall<T, F>(opt: &Option<T>, predicate: F) -> bool
where
    F: Fn(&T) -> bool,
{
    opt.as_ref().map(predicate).unwrap_or(true)
}

/// Option に対する exists（Rust では map + unwrap_or で実現）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::option_exists;
///
/// let year: Option<i32> = Some(996);
/// let no_year: Option<i32> = None;
///
/// // exists: 「存在して条件を満たす」
/// assert!(option_exists(&year, |&y| y < 2020));
/// assert!(!option_exists(&no_year, |&y| y < 2020)); // None は false
/// ```
pub fn option_exists<T, F>(opt: &Option<T>, predicate: F) -> bool
where
    F: Fn(&T) -> bool,
{
    opt.as_ref().map(predicate).unwrap_or(false)
}

/// ユーザー構造体
#[derive(Debug, Clone, PartialEq)]
pub struct User {
    pub name: String,
    pub email: Option<String>,
    pub age: i32,
}

impl User {
    pub fn new(name: &str, email: Option<&str>, age: i32) -> Self {
        Self {
            name: name.to_string(),
            email: email.map(String::from),
            age,
        }
    }
}

/// メールアドレスが設定されていないか、特定ドメインのユーザーをフィルタ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::{User, filter_users_forall};
///
/// let users = vec![
///     User::new("Alice", Some("alice@example.com"), 25),
///     User::new("Bob", None, 30),
///     User::new("Charlie", Some("charlie@test.com"), 17),
/// ];
///
/// let result = filter_users_forall(&users, "@example.com");
/// assert_eq!(result.len(), 2); // Alice と Bob
/// ```
pub fn filter_users_forall<'a>(users: &'a [User], domain: &str) -> Vec<&'a User> {
    users
        .iter()
        .filter(|user| option_forall(&user.email, |e| e.ends_with(domain)))
        .collect()
}

/// メールアドレスが設定されていて、特定ドメインのユーザーをフィルタ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch06_option::{User, filter_users_exists};
///
/// let users = vec![
///     User::new("Alice", Some("alice@example.com"), 25),
///     User::new("Bob", None, 30),
///     User::new("Charlie", Some("charlie@test.com"), 17),
/// ];
///
/// let result = filter_users_exists(&users, "@test.com");
/// assert_eq!(result.len(), 1); // Charlie のみ
/// ```
pub fn filter_users_exists<'a>(users: &'a [User], domain: &str) -> Vec<&'a User> {
    users
        .iter()
        .filter(|user| option_exists(&user.email, |e| e.ends_with(domain)))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_name() {
        assert_eq!(
            extract_name("Breaking Bad (2008-2013)"),
            Some("Breaking Bad".to_string())
        );
        assert_eq!(extract_name("(2008-2013)"), None);
        assert_eq!(extract_name("No brackets"), None);
    }

    #[test]
    fn test_extract_year_start() {
        assert_eq!(extract_year_start("Breaking Bad (2008-2013)"), Some(2008));
        assert_eq!(extract_year_start("Chernobyl (2019)"), None);
    }

    #[test]
    fn test_extract_year_end() {
        assert_eq!(extract_year_end("Breaking Bad (2008-2013)"), Some(2013));
        assert_eq!(extract_year_end("Chernobyl (2019)"), None);
    }

    #[test]
    fn test_extract_single_year() {
        assert_eq!(extract_single_year("Chernobyl (2019)"), Some(2019));
        assert_eq!(extract_single_year("Breaking Bad (2008-2013)"), None);
    }

    #[test]
    fn test_parse_show() {
        assert_eq!(
            parse_show("Breaking Bad (2008-2013)"),
            Some(TvShow::new("Breaking Bad", 2008, 2013))
        );
        assert_eq!(
            parse_show("Chernobyl (2019)"),
            Some(TvShow::new("Chernobyl", 2019, 2019))
        );
        assert_eq!(parse_show("Invalid"), None);
        assert_eq!(parse_show("The Wire 2002-2008"), None);
    }

    #[test]
    fn test_parse_shows_best_effort() {
        let raw_shows = vec![
            "Breaking Bad (2008-2013)",
            "Invalid",
            "Mad Men (2007-2015)",
        ];
        let shows = parse_shows_best_effort(&raw_shows);
        assert_eq!(shows.len(), 2);
    }

    #[test]
    fn test_parse_shows_all_or_nothing() {
        let valid = vec!["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"];
        assert!(parse_shows_all_or_nothing(&valid).is_some());

        let invalid = vec!["Breaking Bad (2008-2013)", "Invalid"];
        assert!(parse_shows_all_or_nothing(&invalid).is_none());
    }

    #[test]
    fn test_safe_divide() {
        assert_eq!(safe_divide(10, 2), Some(5));
        assert_eq!(safe_divide(10, 0), None);
    }

    #[test]
    fn test_add_strings() {
        assert_eq!(add_strings("10", "20"), Some(30));
        assert_eq!(add_strings("10", "abc"), None);
    }

    #[test]
    fn test_option_forall_exists() {
        let some_value: Option<i32> = Some(5);
        let none_value: Option<i32> = None;

        assert!(option_forall(&some_value, |&x| x < 10));
        assert!(option_forall(&none_value, |&x| x < 10));

        assert!(option_exists(&some_value, |&x| x < 10));
        assert!(!option_exists(&none_value, |&x| x < 10));
    }

    #[test]
    fn test_filter_users() {
        let users = vec![
            User::new("Alice", Some("alice@example.com"), 25),
            User::new("Bob", None, 30),
            User::new("Charlie", Some("charlie@test.com"), 17),
        ];

        let forall_result = filter_users_forall(&users, "@example.com");
        assert_eq!(forall_result.len(), 2);

        let exists_result = filter_users_exists(&users, "@test.com");
        assert_eq!(exists_result.len(), 1);
    }
}
