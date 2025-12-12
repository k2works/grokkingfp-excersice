//! 第8章: 非同期処理と副作用の管理
//!
//! Rust では `async/await` と `Future` を使って副作用を管理します。
//! Scala の IO モナドに相当する概念を学びます。

use std::future::Future;
use std::pin::Pin;

// =============================================================================
// 8.1 副作用の問題
// =============================================================================

/// 不純な関数（副作用あり）- サイコロを振る
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::cast_the_die_impure;
///
/// let result = cast_the_die_impure();
/// assert!(result >= 1 && result <= 6);
/// ```
pub fn cast_the_die_impure() -> i32 {
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .subsec_nanos();
    (nanos % 6) as i32 + 1
}

// =============================================================================
// 8.2 async/await による副作用の遅延実行
// =============================================================================

/// 非同期でサイコロを振る（副作用を遅延）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::cast_the_die;
///
/// // async 関数は呼び出しても実行されない
/// let future = cast_the_die();
/// // tokio runtime で実行
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let result = rt.block_on(future);
/// assert!(result >= 1 && result <= 6);
/// ```
pub async fn cast_the_die() -> i32 {
    cast_the_die_impure()
}

/// 2回サイコロを振って合計を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::cast_the_die_twice;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let result = rt.block_on(cast_the_die_twice());
/// assert!(result >= 2 && result <= 12);
/// ```
pub async fn cast_the_die_twice() -> i32 {
    let first = cast_the_die().await;
    let second = cast_the_die().await;
    first + second
}

// =============================================================================
// 8.3 純粋関数と非同期関数の分離
// =============================================================================

/// ミーティング時間を表す構造体
#[derive(Debug, Clone, PartialEq)]
pub struct MeetingTime {
    pub start_hour: i32,
    pub end_hour: i32,
}

impl MeetingTime {
    pub fn new(start_hour: i32, end_hour: i32) -> Self {
        Self {
            start_hour,
            end_hour,
        }
    }
}

/// 2つのミーティングが重なっているか判定（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::{MeetingTime, meetings_overlap};
///
/// let m1 = MeetingTime::new(9, 10);
/// let m2 = MeetingTime::new(10, 11);
/// assert!(!meetings_overlap(&m1, &m2)); // 隣接は重ならない
///
/// let m3 = MeetingTime::new(9, 11);
/// let m4 = MeetingTime::new(10, 12);
/// assert!(meetings_overlap(&m3, &m4)); // 重なっている
/// ```
pub fn meetings_overlap(m1: &MeetingTime, m2: &MeetingTime) -> bool {
    m1.start_hour < m2.end_hour && m2.start_hour < m1.end_hour
}

/// 可能なミーティング時間を計算（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::{MeetingTime, possible_meetings};
///
/// let existing = vec![
///     MeetingTime::new(9, 10),
///     MeetingTime::new(14, 15),
/// ];
///
/// let slots = possible_meetings(&existing, 8, 17, 1);
/// assert!(slots.contains(&MeetingTime::new(8, 9)));
/// assert!(slots.contains(&MeetingTime::new(10, 11)));
/// assert!(!slots.contains(&MeetingTime::new(9, 10))); // 既存ミーティングと重なる
/// ```
pub fn possible_meetings(
    existing_meetings: &[MeetingTime],
    start_hour: i32,
    end_hour: i32,
    length_hours: i32,
) -> Vec<MeetingTime> {
    (start_hour..=end_hour - length_hours)
        .map(|start| MeetingTime::new(start, start + length_hours))
        .filter(|slot| {
            existing_meetings
                .iter()
                .all(|meeting| !meetings_overlap(meeting, slot))
        })
        .collect()
}

// =============================================================================
// 8.4 カレンダー API のシミュレーション
// =============================================================================

/// カレンダーエントリを取得（非同期）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::calendar_entries;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let entries = rt.block_on(calendar_entries("Alice"));
/// // 結果は名前に基づいたダミーデータ
/// ```
pub async fn calendar_entries(name: &str) -> Vec<MeetingTime> {
    // 実際にはAPIコールをシミュレート
    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

    // ダミーデータを返す
    match name {
        "Alice" => vec![MeetingTime::new(9, 10), MeetingTime::new(14, 15)],
        "Bob" => vec![MeetingTime::new(10, 12)],
        _ => vec![],
    }
}

/// 複数人の予定を取得（非同期）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::scheduled_meetings;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let meetings = rt.block_on(scheduled_meetings("Alice", "Bob"));
/// assert!(!meetings.is_empty());
/// ```
pub async fn scheduled_meetings(person1: &str, person2: &str) -> Vec<MeetingTime> {
    let entries1 = calendar_entries(person1).await;
    let entries2 = calendar_entries(person2).await;

    let mut result = entries1;
    result.extend(entries2);
    result
}

// =============================================================================
// 8.5 Result を使ったエラーハンドリング
// =============================================================================

/// フォールバック付きのカレンダー取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::calendar_entries_with_fallback;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let entries = rt.block_on(calendar_entries_with_fallback("Alice"));
/// // エラーが発生しても空のリストを返す
/// ```
pub async fn calendar_entries_with_fallback(name: &str) -> Vec<MeetingTime> {
    calendar_entries_may_fail(name)
        .await
        .unwrap_or_else(|_| vec![])
}

/// 失敗する可能性のあるカレンダー取得
pub async fn calendar_entries_may_fail(name: &str) -> Result<Vec<MeetingTime>, String> {
    if name == "Error" {
        Err("API call failed".to_string())
    } else {
        Ok(calendar_entries(name).await)
    }
}

// =============================================================================
// 8.6 リトライ機能
// =============================================================================

/// 指定回数リトライする
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::{retry, calendar_entries_may_fail};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
///
/// // 成功するケース
/// let result = rt.block_on(retry(|| calendar_entries_may_fail("Alice"), 3));
/// assert!(result.is_ok());
///
/// // 失敗するケース
/// let result = rt.block_on(retry(|| calendar_entries_may_fail("Error"), 3));
/// assert!(result.is_err());
/// ```
pub async fn retry<T, E, F, Fut>(action: F, max_retries: usize) -> Result<T, E>
where
    F: Fn() -> Fut,
    Fut: Future<Output = Result<T, E>>,
{
    let mut last_error = None;

    for _ in 0..max_retries {
        match action().await {
            Ok(result) => return Ok(result),
            Err(e) => last_error = Some(e),
        }
    }

    Err(last_error.unwrap())
}

/// デフォルト値付きリトライ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::{retry_with_default, calendar_entries_may_fail, MeetingTime};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
///
/// // 失敗しても空のリストを返す
/// let result = rt.block_on(retry_with_default(
///     || calendar_entries_may_fail("Error"),
///     3,
///     vec![]
/// ));
/// assert_eq!(result, vec![]);
/// ```
pub async fn retry_with_default<T, E, F, Fut>(action: F, max_retries: usize, default: T) -> T
where
    F: Fn() -> Fut,
    Fut: Future<Output = Result<T, E>>,
{
    retry(action, max_retries).await.unwrap_or(default)
}

// =============================================================================
// 8.7 複数の非同期処理の合成
// =============================================================================

/// 複数人の予定を並行取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::scheduled_meetings_for_all;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let meetings = rt.block_on(scheduled_meetings_for_all(&["Alice", "Bob"]));
/// assert!(!meetings.is_empty());
/// ```
pub async fn scheduled_meetings_for_all(attendees: &[&str]) -> Vec<MeetingTime> {
    let futures: Vec<_> = attendees
        .iter()
        .map(|name| calendar_entries(name))
        .collect();

    let results = futures::future::join_all(futures).await;
    results.into_iter().flatten().collect()
}

/// 複数の非同期処理を順番に実行して結合
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::combine_async;
///
/// async fn get_a() -> i32 { 1 }
/// async fn get_b() -> i32 { 2 }
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let result = rt.block_on(combine_async(get_a(), get_b(), |a, b| a + b));
/// assert_eq!(result, 3);
/// ```
pub async fn combine_async<A, B, C, F>(
    future1: impl Future<Output = A>,
    future2: impl Future<Output = B>,
    combine: F,
) -> C
where
    F: FnOnce(A, B) -> C,
{
    let a = future1.await;
    let b = future2.await;
    combine(a, b)
}

// =============================================================================
// 8.8 printとreturn
// =============================================================================

/// メッセージを出力して返す（副作用）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::print_and_return;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// let result = rt.block_on(print_and_return("Hello"));
/// assert_eq!(result, "Hello");
/// ```
pub async fn print_and_return(message: &str) -> String {
    println!("{}", message);
    message.to_string()
}

// =============================================================================
// 8.9 Box<dyn Future> を使った動的ディスパッチ
// =============================================================================

/// 動的な Future を作成するヘルパー型
pub type BoxFuture<'a, T> = Pin<Box<dyn Future<Output = T> + Send + 'a>>;

/// 条件に応じて異なる非同期処理を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch08_io::conditional_async;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
///
/// let result = rt.block_on(conditional_async(true));
/// assert_eq!(result, "Success");
///
/// let result = rt.block_on(conditional_async(false));
/// assert_eq!(result, "Failure");
/// ```
pub fn conditional_async(condition: bool) -> BoxFuture<'static, &'static str> {
    if condition {
        Box::pin(async { "Success" })
    } else {
        Box::pin(async { "Failure" })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cast_the_die_impure() {
        for _ in 0..100 {
            let result = cast_the_die_impure();
            assert!(result >= 1 && result <= 6);
        }
    }

    #[tokio::test]
    async fn test_cast_the_die() {
        let result = cast_the_die().await;
        assert!(result >= 1 && result <= 6);
    }

    #[tokio::test]
    async fn test_cast_the_die_twice() {
        let result = cast_the_die_twice().await;
        assert!(result >= 2 && result <= 12);
    }

    #[test]
    fn test_meetings_overlap() {
        let m1 = MeetingTime::new(9, 10);
        let m2 = MeetingTime::new(10, 11);
        assert!(!meetings_overlap(&m1, &m2));

        let m3 = MeetingTime::new(9, 11);
        let m4 = MeetingTime::new(10, 12);
        assert!(meetings_overlap(&m3, &m4));

        let m5 = MeetingTime::new(9, 10);
        let m6 = MeetingTime::new(11, 12);
        assert!(!meetings_overlap(&m5, &m6));
    }

    #[test]
    fn test_possible_meetings() {
        let existing = vec![MeetingTime::new(9, 10), MeetingTime::new(14, 15)];
        let slots = possible_meetings(&existing, 8, 17, 1);

        assert!(slots.contains(&MeetingTime::new(8, 9)));
        assert!(slots.contains(&MeetingTime::new(10, 11)));
        assert!(slots.contains(&MeetingTime::new(15, 16)));
        assert!(!slots.contains(&MeetingTime::new(9, 10)));
        assert!(!slots.contains(&MeetingTime::new(14, 15)));
    }

    #[tokio::test]
    async fn test_calendar_entries() {
        let entries = calendar_entries("Alice").await;
        assert!(!entries.is_empty());

        let entries = calendar_entries("Unknown").await;
        assert!(entries.is_empty());
    }

    #[tokio::test]
    async fn test_scheduled_meetings() {
        let meetings = scheduled_meetings("Alice", "Bob").await;
        assert!(!meetings.is_empty());
    }

    #[tokio::test]
    async fn test_calendar_entries_with_fallback() {
        let entries = calendar_entries_with_fallback("Alice").await;
        assert!(!entries.is_empty());

        let entries = calendar_entries_with_fallback("Error").await;
        assert!(entries.is_empty());
    }

    #[tokio::test]
    async fn test_retry() {
        let result = retry(|| calendar_entries_may_fail("Alice"), 3).await;
        assert!(result.is_ok());

        let result = retry(|| calendar_entries_may_fail("Error"), 3).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_retry_with_default() {
        let result = retry_with_default(|| calendar_entries_may_fail("Error"), 3, vec![]).await;
        assert!(result.is_empty());
    }

    #[tokio::test]
    async fn test_scheduled_meetings_for_all() {
        let meetings = scheduled_meetings_for_all(&["Alice", "Bob"]).await;
        assert!(!meetings.is_empty());
    }

    #[tokio::test]
    async fn test_combine_async() {
        async fn get_a() -> i32 {
            1
        }
        async fn get_b() -> i32 {
            2
        }

        let result = combine_async(get_a(), get_b(), |a, b| a + b).await;
        assert_eq!(result, 3);
    }

    #[tokio::test]
    async fn test_print_and_return() {
        let result = print_and_return("Test").await;
        assert_eq!(result, "Test");
    }

    #[tokio::test]
    async fn test_conditional_async() {
        let result = conditional_async(true).await;
        assert_eq!(result, "Success");

        let result = conditional_async(false).await;
        assert_eq!(result, "Failure");
    }
}
