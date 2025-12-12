//! 第9章: ストリーム処理
//!
//! Rust では `tokio-stream` と `futures` を使ってストリーム処理を実装します。
//! Scala の fs2 Stream に相当する概念を学びます。

use futures::stream::{self, Stream, StreamExt};
use std::pin::Pin;

// =============================================================================
// 9.1 ストリームの基本
// =============================================================================

/// イテレータからストリームを作成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::numbers_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 5);
///     let collected: Vec<i32> = stream.collect().await;
///     assert_eq!(collected, vec![1, 2, 3, 4, 5]);
/// });
/// ```
pub fn numbers_stream(start: i32, end: i32) -> impl Stream<Item = i32> {
    stream::iter(start..=end)
}

/// ストリームに map を適用
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, map_stream};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 3);
///     let doubled = map_stream(stream, |x| x * 2);
///     let collected: Vec<i32> = doubled.collect().await;
///     assert_eq!(collected, vec![2, 4, 6]);
/// });
/// ```
pub fn map_stream<S, F, T, U>(stream: S, f: F) -> impl Stream<Item = U>
where
    S: Stream<Item = T>,
    F: FnMut(T) -> U,
{
    stream.map(f)
}

/// ストリームに filter を適用
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, filter_stream};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 5);
///     let evens = filter_stream(stream, |x| x % 2 == 0);
///     let collected: Vec<i32> = evens.collect().await;
///     assert_eq!(collected, vec![2, 4]);
/// });
/// ```
pub fn filter_stream<S, F, T>(stream: S, mut f: F) -> impl Stream<Item = T>
where
    S: Stream<Item = T>,
    F: FnMut(&T) -> bool,
{
    stream.filter(move |x| {
        let result = f(x);
        async move { result }
    })
}

// =============================================================================
// 9.2 ストリームの合成
// =============================================================================

/// 2つのストリームを結合
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, concat_streams};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let s1 = numbers_stream(1, 3);
///     let s2 = numbers_stream(4, 6);
///     let combined = concat_streams(s1, s2);
///     let collected: Vec<i32> = combined.collect().await;
///     assert_eq!(collected, vec![1, 2, 3, 4, 5, 6]);
/// });
/// ```
pub fn concat_streams<S1, S2, T>(stream1: S1, stream2: S2) -> impl Stream<Item = T>
where
    S1: Stream<Item = T>,
    S2: Stream<Item = T>,
{
    stream1.chain(stream2)
}

/// ストリームを take で切り取り
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, take_stream};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 100);
///     let first_five = take_stream(stream, 5);
///     let collected: Vec<i32> = first_five.collect().await;
///     assert_eq!(collected, vec![1, 2, 3, 4, 5]);
/// });
/// ```
pub fn take_stream<S, T>(stream: S, n: usize) -> impl Stream<Item = T>
where
    S: Stream<Item = T>,
{
    stream.take(n)
}

/// ストリームを skip で読み飛ばし
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, skip_stream};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 5);
///     let skipped = skip_stream(stream, 2);
///     let collected: Vec<i32> = skipped.collect().await;
///     assert_eq!(collected, vec![3, 4, 5]);
/// });
/// ```
pub fn skip_stream<S, T>(stream: S, n: usize) -> impl Stream<Item = T>
where
    S: Stream<Item = T>,
{
    stream.skip(n)
}

// =============================================================================
// 9.3 無限ストリーム
// =============================================================================

/// 無限ストリームを生成（イテレータ版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::infinite_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = infinite_stream(0, |x| x + 1);
///     let first_five: Vec<i32> = stream.take(5).collect().await;
///     assert_eq!(first_five, vec![0, 1, 2, 3, 4]);
/// });
/// ```
pub fn infinite_stream<T, F>(initial: T, f: F) -> impl Stream<Item = T>
where
    T: Clone,
    F: FnMut(&T) -> T,
{
    stream::unfold((initial, f), |(state, mut f)| async move {
        let next = f(&state);
        Some((state, (next, f)))
    })
}

/// フィボナッチ数列のストリーム
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::fibonacci_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = fibonacci_stream();
///     let first_ten: Vec<u64> = stream.take(10).collect().await;
///     assert_eq!(first_ten, vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
/// });
/// ```
pub fn fibonacci_stream() -> impl Stream<Item = u64> {
    stream::unfold((0u64, 1u64), |(a, b)| async move { Some((a, (b, a + b))) })
}

/// 繰り返しストリーム
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::repeat_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = repeat_stream("hello");
///     let first_three: Vec<&str> = stream.take(3).collect().await;
///     assert_eq!(first_three, vec!["hello", "hello", "hello"]);
/// });
/// ```
pub fn repeat_stream<T: Clone>(value: T) -> impl Stream<Item = T> {
    stream::repeat(value)
}

// =============================================================================
// 9.4 ストリームの畳み込み
// =============================================================================

/// ストリームを fold で畳み込み
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, fold_stream};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 5);
///     let sum = fold_stream(stream, 0, |acc, x| acc + x).await;
///     assert_eq!(sum, 15);
/// });
/// ```
pub async fn fold_stream<S, T, U, F>(stream: S, initial: U, mut f: F) -> U
where
    S: Stream<Item = T>,
    F: FnMut(U, T) -> U,
{
    stream
        .fold(initial, |acc, x| {
            let result = f(acc, x);
            async move { result }
        })
        .await
}

/// ストリームの合計
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, sum_stream};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 5);
///     let sum = sum_stream(stream).await;
///     assert_eq!(sum, 15);
/// });
/// ```
pub async fn sum_stream<S>(stream: S) -> i32
where
    S: Stream<Item = i32>,
{
    stream.fold(0, |acc, x| async move { acc + x }).await
}

// =============================================================================
// 9.5 スライディングウィンドウ
// =============================================================================

/// スライディングウィンドウを適用（チャンク版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, chunks_stream};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = numbers_stream(1, 6);
///     let chunks = chunks_stream(stream, 2);
///     let collected: Vec<Vec<i32>> = chunks.collect().await;
///     assert_eq!(collected, vec![vec![1, 2], vec![3, 4], vec![5, 6]]);
/// });
/// ```
pub fn chunks_stream<S, T>(stream: S, size: usize) -> impl Stream<Item = Vec<T>>
where
    S: Stream<Item = T>,
{
    stream.chunks(size)
}

/// 移動平均を計算するストリーム
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::moving_average;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let values = vec![1.0, 2.0, 3.0, 4.0, 5.0];
///     let avg_stream = moving_average(futures::stream::iter(values), 3);
///     let averages: Vec<f64> = avg_stream.collect().await;
///     // [1.0], [1.0, 2.0], [1.0, 2.0, 3.0], [2.0, 3.0, 4.0], [3.0, 4.0, 5.0]
///     // -> [1.0, 1.5, 2.0, 3.0, 4.0]
///     assert_eq!(averages.len(), 5);
/// });
/// ```
pub fn moving_average<S>(stream: S, window_size: usize) -> impl Stream<Item = f64>
where
    S: Stream<Item = f64> + Send + 'static,
{
    stream::unfold(
        (stream.boxed(), Vec::new(), window_size),
        |(mut stream, mut window, size)| async move {
            match stream.next().await {
                Some(value) => {
                    window.push(value);
                    if window.len() > size {
                        window.remove(0);
                    }
                    let avg = window.iter().sum::<f64>() / window.len() as f64;
                    Some((avg, (stream, window, size)))
                }
                None => None,
            }
        },
    )
}

// =============================================================================
// 9.6 zip でストリームを結合
// =============================================================================

/// 2つのストリームを zip
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{numbers_stream, zip_streams};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let s1 = numbers_stream(1, 3);
///     let s2 = numbers_stream(10, 12);
///     let zipped = zip_streams(s1, s2);
///     let collected: Vec<(i32, i32)> = zipped.collect().await;
///     assert_eq!(collected, vec![(1, 10), (2, 11), (3, 12)]);
/// });
/// ```
pub fn zip_streams<S1, S2, T1, T2>(stream1: S1, stream2: S2) -> impl Stream<Item = (T1, T2)>
where
    S1: Stream<Item = T1>,
    S2: Stream<Item = T2>,
{
    stream1.zip(stream2)
}

/// インデックス付きストリーム
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::enumerate_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let items = futures::stream::iter(vec!["a", "b", "c"]);
///     let indexed = enumerate_stream(items);
///     let collected: Vec<(usize, &str)> = indexed.collect().await;
///     assert_eq!(collected, vec![(0, "a"), (1, "b"), (2, "c")]);
/// });
/// ```
pub fn enumerate_stream<S, T>(stream: S) -> impl Stream<Item = (usize, T)>
where
    S: Stream<Item = T>,
{
    stream.enumerate()
}

// =============================================================================
// 9.7 flatMap でストリームを平坦化
// =============================================================================

/// ストリームの各要素を展開して平坦化
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::flat_map_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = futures::stream::iter(vec![1, 2, 3]);
///     let expanded = flat_map_stream(stream, |x| Box::pin(futures::stream::iter(vec![x, x * 10])));
///     let collected: Vec<i32> = expanded.collect().await;
///     assert_eq!(collected, vec![1, 10, 2, 20, 3, 30]);
/// });
/// ```
pub fn flat_map_stream<S, F, U, T>(stream: S, f: F) -> impl Stream<Item = T>
where
    S: Stream<Item = U>,
    F: FnMut(U) -> Pin<Box<dyn Stream<Item = T> + Send>>,
{
    stream.flat_map(f)
}

/// ネストされたストリームを平坦化
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::flatten_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let nested = futures::stream::iter(vec![
///         futures::stream::iter(vec![1, 2]),
///         futures::stream::iter(vec![3, 4]),
///     ]);
///     let flat = flatten_stream(nested);
///     let collected: Vec<i32> = flat.collect().await;
///     assert_eq!(collected, vec![1, 2, 3, 4]);
/// });
/// ```
pub fn flatten_stream<S, Inner, T>(stream: S) -> impl Stream<Item = T>
where
    S: Stream<Item = Inner>,
    Inner: Stream<Item = T>,
{
    stream.flatten()
}

// =============================================================================
// 9.8 非同期ストリーム処理
// =============================================================================

/// 各要素に非同期処理を適用
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::async_map_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = futures::stream::iter(vec![1, 2, 3]);
///     let doubled = async_map_stream(stream, |x| async move { x * 2 });
///     let collected: Vec<i32> = doubled.collect().await;
///     assert_eq!(collected, vec![2, 4, 6]);
/// });
/// ```
pub fn async_map_stream<S, F, Fut, T, U>(stream: S, f: F) -> impl Stream<Item = U>
where
    S: Stream<Item = T>,
    F: FnMut(T) -> Fut,
    Fut: std::future::Future<Output = U>,
{
    stream.then(f)
}

/// バッファ付き非同期処理（並行実行）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::buffered_async_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = futures::stream::iter(vec![1, 2, 3, 4, 5]);
///     let processed = buffered_async_stream(stream, |x| async move { x * 2 }, 2);
///     let collected: Vec<i32> = processed.collect().await;
///     assert_eq!(collected, vec![2, 4, 6, 8, 10]);
/// });
/// ```
pub fn buffered_async_stream<S, F, Fut, T, U>(
    stream: S,
    f: F,
    buffer_size: usize,
) -> impl Stream<Item = U>
where
    S: Stream<Item = T>,
    F: FnMut(T) -> Fut,
    Fut: std::future::Future<Output = U>,
{
    stream.map(f).buffered(buffer_size)
}

// =============================================================================
// 9.9 エラーハンドリング
// =============================================================================

/// Result を含むストリームから成功値のみを抽出
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::filter_ok_stream;
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = futures::stream::iter(vec![
///         Ok(1),
///         Err("error"),
///         Ok(2),
///         Ok(3),
///     ]);
///     let oks = filter_ok_stream(stream);
///     let collected: Vec<i32> = oks.collect().await;
///     assert_eq!(collected, vec![1, 2, 3]);
/// });
/// ```
pub fn filter_ok_stream<S, T, E>(stream: S) -> impl Stream<Item = T>
where
    S: Stream<Item = Result<T, E>>,
{
    stream.filter_map(|r| async move { r.ok() })
}

/// ストリーム処理中のエラーを収集
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::collect_results_stream;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let stream = futures::stream::iter(vec![Ok(1), Ok(2), Ok(3)]);
///     let result: Result<Vec<i32>, &str> = collect_results_stream(stream).await;
///     assert_eq!(result, Ok(vec![1, 2, 3]));
///
///     let stream_with_error = futures::stream::iter(vec![Ok(1), Err("error"), Ok(3)]);
///     let result: Result<Vec<i32>, &str> = collect_results_stream(stream_with_error).await;
///     assert_eq!(result, Err("error"));
/// });
/// ```
pub async fn collect_results_stream<S, T, E>(stream: S) -> Result<Vec<T>, E>
where
    S: Stream<Item = Result<T, E>>,
{
    stream.collect::<Vec<_>>().await.into_iter().collect()
}

// =============================================================================
// 9.10 イベント処理シミュレーション
// =============================================================================

/// イベントの種類
#[derive(Debug, Clone, PartialEq)]
pub enum Event {
    Click { x: i32, y: i32 },
    KeyPress { key: char },
    Scroll { delta: i32 },
}

/// イベントストリームをフィルタリング
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{Event, filter_clicks};
/// use futures::StreamExt;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let events = vec![
///         Event::Click { x: 10, y: 20 },
///         Event::KeyPress { key: 'a' },
///         Event::Click { x: 30, y: 40 },
///     ];
///     let stream = futures::stream::iter(events);
///     let clicks = filter_clicks(stream);
///     let collected: Vec<Event> = clicks.collect().await;
///     assert_eq!(collected.len(), 2);
/// });
/// ```
pub fn filter_clicks<S>(stream: S) -> impl Stream<Item = Event>
where
    S: Stream<Item = Event>,
{
    stream.filter(|e| {
        let is_click = matches!(e, Event::Click { .. });
        async move { is_click }
    })
}

/// イベントを集計
///
/// # Examples
///
/// ```
/// use grokking_fp::ch09_stream::{Event, count_events};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let events = vec![
///         Event::Click { x: 10, y: 20 },
///         Event::KeyPress { key: 'a' },
///         Event::Click { x: 30, y: 40 },
///         Event::KeyPress { key: 'b' },
///         Event::Scroll { delta: 10 },
///     ];
///     let stream = futures::stream::iter(events);
///     let (clicks, keypresses, scrolls) = count_events(stream).await;
///     assert_eq!(clicks, 2);
///     assert_eq!(keypresses, 2);
///     assert_eq!(scrolls, 1);
/// });
/// ```
pub async fn count_events<S>(stream: S) -> (usize, usize, usize)
where
    S: Stream<Item = Event>,
{
    stream
        .fold((0, 0, 0), |(clicks, keys, scrolls), event| async move {
            match event {
                Event::Click { .. } => (clicks + 1, keys, scrolls),
                Event::KeyPress { .. } => (clicks, keys + 1, scrolls),
                Event::Scroll { .. } => (clicks, keys, scrolls + 1),
            }
        })
        .await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_numbers_stream() {
        let stream = numbers_stream(1, 5);
        let collected: Vec<i32> = stream.collect().await;
        assert_eq!(collected, vec![1, 2, 3, 4, 5]);
    }

    #[tokio::test]
    async fn test_map_stream() {
        let stream = numbers_stream(1, 3);
        let doubled = map_stream(stream, |x| x * 2);
        let collected: Vec<i32> = doubled.collect().await;
        assert_eq!(collected, vec![2, 4, 6]);
    }

    #[tokio::test]
    async fn test_filter_stream() {
        let stream = numbers_stream(1, 5);
        let evens = filter_stream(stream, |x| x % 2 == 0);
        let collected: Vec<i32> = evens.collect().await;
        assert_eq!(collected, vec![2, 4]);
    }

    #[tokio::test]
    async fn test_concat_streams() {
        let s1 = numbers_stream(1, 3);
        let s2 = numbers_stream(4, 6);
        let combined = concat_streams(s1, s2);
        let collected: Vec<i32> = combined.collect().await;
        assert_eq!(collected, vec![1, 2, 3, 4, 5, 6]);
    }

    #[tokio::test]
    async fn test_take_stream() {
        let stream = numbers_stream(1, 100);
        let first_five = take_stream(stream, 5);
        let collected: Vec<i32> = first_five.collect().await;
        assert_eq!(collected, vec![1, 2, 3, 4, 5]);
    }

    #[tokio::test]
    async fn test_skip_stream() {
        let stream = numbers_stream(1, 5);
        let skipped = skip_stream(stream, 2);
        let collected: Vec<i32> = skipped.collect().await;
        assert_eq!(collected, vec![3, 4, 5]);
    }

    #[tokio::test]
    async fn test_infinite_stream() {
        let stream = infinite_stream(0, |x| x + 1);
        let first_five: Vec<i32> = stream.take(5).collect().await;
        assert_eq!(first_five, vec![0, 1, 2, 3, 4]);
    }

    #[tokio::test]
    async fn test_fibonacci_stream() {
        let stream = fibonacci_stream();
        let first_ten: Vec<u64> = stream.take(10).collect().await;
        assert_eq!(first_ten, vec![0, 1, 1, 2, 3, 5, 8, 13, 21, 34]);
    }

    #[tokio::test]
    async fn test_repeat_stream() {
        let stream = repeat_stream("hello");
        let first_three: Vec<&str> = stream.take(3).collect().await;
        assert_eq!(first_three, vec!["hello", "hello", "hello"]);
    }

    #[tokio::test]
    async fn test_fold_stream() {
        let stream = numbers_stream(1, 5);
        let sum = fold_stream(stream, 0, |acc, x| acc + x).await;
        assert_eq!(sum, 15);
    }

    #[tokio::test]
    async fn test_sum_stream() {
        let stream = numbers_stream(1, 5);
        let sum = sum_stream(stream).await;
        assert_eq!(sum, 15);
    }

    #[tokio::test]
    async fn test_chunks_stream() {
        let stream = numbers_stream(1, 6);
        let chunks = chunks_stream(stream, 2);
        let collected: Vec<Vec<i32>> = chunks.collect().await;
        assert_eq!(collected, vec![vec![1, 2], vec![3, 4], vec![5, 6]]);
    }

    #[tokio::test]
    async fn test_zip_streams() {
        let s1 = numbers_stream(1, 3);
        let s2 = numbers_stream(10, 12);
        let zipped = zip_streams(s1, s2);
        let collected: Vec<(i32, i32)> = zipped.collect().await;
        assert_eq!(collected, vec![(1, 10), (2, 11), (3, 12)]);
    }

    #[tokio::test]
    async fn test_enumerate_stream() {
        let items = stream::iter(vec!["a", "b", "c"]);
        let indexed = enumerate_stream(items);
        let collected: Vec<(usize, &str)> = indexed.collect().await;
        assert_eq!(collected, vec![(0, "a"), (1, "b"), (2, "c")]);
    }

    #[tokio::test]
    async fn test_flatten_stream() {
        let nested = stream::iter(vec![stream::iter(vec![1, 2]), stream::iter(vec![3, 4])]);
        let flat = flatten_stream(nested);
        let collected: Vec<i32> = flat.collect().await;
        assert_eq!(collected, vec![1, 2, 3, 4]);
    }

    #[tokio::test]
    async fn test_async_map_stream() {
        let stream = stream::iter(vec![1, 2, 3]);
        let doubled = async_map_stream(stream, |x| async move { x * 2 });
        let collected: Vec<i32> = doubled.collect().await;
        assert_eq!(collected, vec![2, 4, 6]);
    }

    #[tokio::test]
    async fn test_filter_ok_stream() {
        let stream = stream::iter(vec![Ok(1), Err("error"), Ok(2), Ok(3)]);
        let oks = filter_ok_stream(stream);
        let collected: Vec<i32> = oks.collect().await;
        assert_eq!(collected, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn test_collect_results_stream_success() {
        let stream = stream::iter(vec![Ok(1), Ok(2), Ok(3)]);
        let result: Result<Vec<i32>, &str> = collect_results_stream(stream).await;
        assert_eq!(result, Ok(vec![1, 2, 3]));
    }

    #[tokio::test]
    async fn test_collect_results_stream_error() {
        let stream = stream::iter(vec![Ok(1), Err("error"), Ok(3)]);
        let result: Result<Vec<i32>, &str> = collect_results_stream(stream).await;
        assert_eq!(result, Err("error"));
    }

    #[tokio::test]
    async fn test_filter_clicks() {
        let events = vec![
            Event::Click { x: 10, y: 20 },
            Event::KeyPress { key: 'a' },
            Event::Click { x: 30, y: 40 },
        ];
        let stream = stream::iter(events);
        let clicks = filter_clicks(stream);
        let collected: Vec<Event> = clicks.collect().await;
        assert_eq!(collected.len(), 2);
    }

    #[tokio::test]
    async fn test_count_events() {
        let events = vec![
            Event::Click { x: 10, y: 20 },
            Event::KeyPress { key: 'a' },
            Event::Click { x: 30, y: 40 },
            Event::KeyPress { key: 'b' },
            Event::Scroll { delta: 10 },
        ];
        let stream = stream::iter(events);
        let (clicks, keypresses, scrolls) = count_events(stream).await;
        assert_eq!(clicks, 2);
        assert_eq!(keypresses, 2);
        assert_eq!(scrolls, 1);
    }

    #[tokio::test]
    async fn test_moving_average() {
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let avg_stream = moving_average(stream::iter(values), 3);
        let averages: Vec<f64> = avg_stream.collect().await;
        assert_eq!(averages.len(), 5);
        assert!((averages[0] - 1.0).abs() < 0.001);
        assert!((averages[1] - 1.5).abs() < 0.001);
        assert!((averages[2] - 2.0).abs() < 0.001);
        assert!((averages[3] - 3.0).abs() < 0.001);
        assert!((averages[4] - 4.0).abs() < 0.001);
    }
}
