//! 第10章: 並行・並列処理
//!
//! Rust では tokio タスク、Arc、Mutex/RwLock を使って並行処理を実装します。
//! Scala の Ref と Fiber に相当する概念を学びます。

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};
use tokio::task::JoinHandle;

// =============================================================================
// 10.1 共有状態の基本 - Arc と Mutex
// =============================================================================

/// アトミックカウンター
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::AtomicCounter;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let counter = AtomicCounter::new(0);
///     counter.increment().await;
///     counter.increment().await;
///     counter.increment().await;
///     assert_eq!(counter.get().await, 3);
/// });
/// ```
#[derive(Clone)]
pub struct AtomicCounter {
    value: Arc<Mutex<i32>>,
}

impl AtomicCounter {
    pub fn new(initial: i32) -> Self {
        Self {
            value: Arc::new(Mutex::new(initial)),
        }
    }

    pub async fn get(&self) -> i32 {
        *self.value.lock().await
    }

    pub async fn set(&self, value: i32) {
        *self.value.lock().await = value;
    }

    pub async fn increment(&self) {
        let mut guard = self.value.lock().await;
        *guard += 1;
    }

    pub async fn update<F>(&self, f: F)
    where
        F: FnOnce(i32) -> i32,
    {
        let mut guard = self.value.lock().await;
        *guard = f(*guard);
    }
}

// =============================================================================
// 10.2 RwLock - 読み書きロック
// =============================================================================

/// 読み書きロック付きの値
///
/// 複数の読み取りを同時に許可しつつ、書き込みは排他的に行う
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::SharedValue;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let shared = SharedValue::new(42);
///     assert_eq!(shared.read().await, 42);
///     shared.write(100).await;
///     assert_eq!(shared.read().await, 100);
/// });
/// ```
#[derive(Clone)]
pub struct SharedValue<T> {
    value: Arc<RwLock<T>>,
}

impl<T: Clone> SharedValue<T> {
    pub fn new(initial: T) -> Self {
        Self {
            value: Arc::new(RwLock::new(initial)),
        }
    }

    pub async fn read(&self) -> T {
        self.value.read().await.clone()
    }

    pub async fn write(&self, value: T) {
        *self.value.write().await = value;
    }

    pub async fn update<F>(&self, f: F)
    where
        F: FnOnce(&T) -> T,
    {
        let mut guard = self.value.write().await;
        *guard = f(&*guard);
    }
}

// =============================================================================
// 10.3 チェックインのリアルタイム集計
// =============================================================================

/// 都市を表す構造体
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct City {
    pub name: String,
}

impl City {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

/// 都市の統計情報
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CityStats {
    pub city: City,
    pub check_ins: i32,
}

impl CityStats {
    pub fn new(city: City, check_ins: i32) -> Self {
        Self { city, check_ins }
    }
}

/// トップN都市を計算（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::{City, CityStats, top_cities};
/// use std::collections::HashMap;
///
/// let mut city_check_ins = HashMap::new();
/// city_check_ins.insert(City::new("Sydney"), 100);
/// city_check_ins.insert(City::new("Dublin"), 50);
/// city_check_ins.insert(City::new("Tokyo"), 200);
/// city_check_ins.insert(City::new("Lima"), 75);
///
/// let top = top_cities(&city_check_ins, 3);
/// assert_eq!(top[0].city.name, "Tokyo");
/// assert_eq!(top[1].city.name, "Sydney");
/// assert_eq!(top[2].city.name, "Lima");
/// ```
pub fn top_cities(city_check_ins: &HashMap<City, i32>, n: usize) -> Vec<CityStats> {
    let mut stats: Vec<CityStats> = city_check_ins
        .iter()
        .map(|(city, &check_ins)| CityStats::new(city.clone(), check_ins))
        .collect();

    stats.sort_by(|a, b| b.check_ins.cmp(&a.check_ins));
    stats.truncate(n);
    stats
}

// =============================================================================
// 10.4 共有状態を使ったチェックイン処理
// =============================================================================

/// チェックインストア
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::{City, CheckInStore};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let store = CheckInStore::new();
///     store.store_check_in(City::new("Tokyo")).await;
///     store.store_check_in(City::new("Tokyo")).await;
///     store.store_check_in(City::new("Sydney")).await;
///
///     let check_ins = store.get_all().await;
///     assert_eq!(check_ins.get(&City::new("Tokyo")), Some(&2));
///     assert_eq!(check_ins.get(&City::new("Sydney")), Some(&1));
/// });
/// ```
#[derive(Clone)]
pub struct CheckInStore {
    check_ins: Arc<RwLock<HashMap<City, i32>>>,
}

impl CheckInStore {
    pub fn new() -> Self {
        Self {
            check_ins: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn store_check_in(&self, city: City) {
        let mut guard = self.check_ins.write().await;
        *guard.entry(city).or_insert(0) += 1;
    }

    pub async fn get_all(&self) -> HashMap<City, i32> {
        self.check_ins.read().await.clone()
    }

    pub async fn get_top(&self, n: usize) -> Vec<CityStats> {
        let check_ins = self.check_ins.read().await;
        top_cities(&check_ins, n)
    }
}

impl Default for CheckInStore {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// 10.5 並列実行 - join_all と spawn
// =============================================================================

/// 複数のタスクを並列実行
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::run_parallel;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let results = run_parallel(vec![1, 2, 3], |x| async move { x * 2 }).await;
///     assert_eq!(results, vec![2, 4, 6]);
/// });
/// ```
pub async fn run_parallel<T, U, F, Fut>(items: Vec<T>, f: F) -> Vec<U>
where
    T: Send + 'static,
    U: Send + 'static,
    F: Fn(T) -> Fut + Send + Sync + 'static,
    Fut: std::future::Future<Output = U> + Send,
{
    let f = Arc::new(f);
    let handles: Vec<JoinHandle<U>> = items
        .into_iter()
        .map(|item| {
            let f = Arc::clone(&f);
            tokio::spawn(async move { f(item).await })
        })
        .collect();

    let mut results = Vec::with_capacity(handles.len());
    for handle in handles {
        results.push(handle.await.unwrap());
    }
    results
}

/// 並列でサイコロを振る
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::cast_dice_parallel;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let results = cast_dice_parallel(3).await;
///     assert_eq!(results.len(), 3);
///     for r in &results {
///         assert!(*r >= 1 && *r <= 6);
///     }
/// });
/// ```
pub async fn cast_dice_parallel(count: usize) -> Vec<i32> {
    use std::time::{SystemTime, UNIX_EPOCH};

    let handles: Vec<JoinHandle<i32>> = (0..count)
        .map(|i| {
            tokio::spawn(async move {
                // 各タスクで異なるシードを使用
                let nanos = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .subsec_nanos()
                    .wrapping_add(i as u32 * 1000);
                (nanos % 6) as i32 + 1
            })
        })
        .collect();

    let mut results = Vec::with_capacity(handles.len());
    for handle in handles {
        results.push(handle.await.unwrap());
    }
    results
}

// =============================================================================
// 10.6 タスクのキャンセル - JoinHandle と AbortHandle
// =============================================================================

/// バックグラウンドタスクのハンドル
pub struct BackgroundTask<T> {
    handle: JoinHandle<T>,
}

impl<T> BackgroundTask<T> {
    /// タスクをキャンセル
    pub fn cancel(self) {
        self.handle.abort();
    }

    /// タスクの完了を待機
    pub async fn join(self) -> Result<T, tokio::task::JoinError> {
        self.handle.await
    }
}

/// バックグラウンドでカウントアップするタスクを開始
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::start_counter_task;
/// use std::time::Duration;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let (counter, task) = start_counter_task(Duration::from_millis(10));
///
///     // しばらく待機
///     tokio::time::sleep(Duration::from_millis(50)).await;
///
///     // カウンターの値を確認（0より大きいはず）
///     let value = counter.get().await;
///     assert!(value > 0);
///
///     // タスクをキャンセル
///     task.cancel();
/// });
/// ```
pub fn start_counter_task(
    interval: std::time::Duration,
) -> (AtomicCounter, BackgroundTask<()>) {
    let counter = AtomicCounter::new(0);
    let counter_clone = counter.clone();

    let handle = tokio::spawn(async move {
        loop {
            tokio::time::sleep(interval).await;
            counter_clone.increment().await;
        }
    });

    (counter, BackgroundTask { handle })
}

// =============================================================================
// 10.7 チェックイン処理システム
// =============================================================================

/// チェックイン処理の結果を保持する構造体
pub struct ProcessingCheckIns {
    store: CheckInStore,
    ranking: SharedValue<Vec<CityStats>>,
    tasks: Vec<JoinHandle<()>>,
}

impl ProcessingCheckIns {
    /// 現在のランキングを取得
    pub async fn current_ranking(&self) -> Vec<CityStats> {
        self.ranking.read().await
    }

    /// 全てのチェックインを取得
    pub async fn all_check_ins(&self) -> HashMap<City, i32> {
        self.store.get_all().await
    }

    /// 処理を停止
    pub fn stop(self) {
        for task in self.tasks {
            task.abort();
        }
    }
}

/// チェックイン処理を開始
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::{City, process_check_ins};
/// use std::time::Duration;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let cities = vec![
///         City::new("Tokyo"),
///         City::new("Sydney"),
///         City::new("Tokyo"),
///         City::new("Dublin"),
///         City::new("Tokyo"),
///     ];
///
///     let processing = process_check_ins(cities, Duration::from_millis(10)).await;
///
///     // しばらく待機して処理させる
///     tokio::time::sleep(Duration::from_millis(50)).await;
///
///     let ranking = processing.current_ranking().await;
///     // Tokyo が最も多いはず
///     if !ranking.is_empty() {
///         assert_eq!(ranking[0].city.name, "Tokyo");
///     }
///
///     processing.stop();
/// });
/// ```
pub async fn process_check_ins(
    cities: Vec<City>,
    ranking_update_interval: std::time::Duration,
) -> ProcessingCheckIns {
    let store = CheckInStore::new();
    let ranking = SharedValue::new(Vec::new());

    // チェックイン処理タスク
    let store_clone = store.clone();
    let check_in_task = tokio::spawn(async move {
        for city in cities {
            store_clone.store_check_in(city).await;
        }
    });

    // ランキング更新タスク
    let store_clone = store.clone();
    let ranking_clone = ranking.clone();
    let ranking_task = tokio::spawn(async move {
        loop {
            tokio::time::sleep(ranking_update_interval).await;
            let top = store_clone.get_top(3).await;
            ranking_clone.write(top).await;
        }
    });

    ProcessingCheckIns {
        store,
        ranking,
        tasks: vec![check_in_task, ranking_task],
    }
}

// =============================================================================
// 10.8 並行カウント
// =============================================================================

/// 並行してカウントを行う
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::count_parallel;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let items = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
///     let even_count = count_parallel(&items, |x| x % 2 == 0).await;
///     assert_eq!(even_count, 5);
/// });
/// ```
pub async fn count_parallel<T, F>(items: &[T], predicate: F) -> usize
where
    T: Clone + Send + Sync + 'static,
    F: Fn(&T) -> bool + Send + Sync + 'static,
{
    let counter = Arc::new(Mutex::new(0usize));
    let predicate = Arc::new(predicate);

    let handles: Vec<JoinHandle<()>> = items
        .iter()
        .cloned()
        .map(|item| {
            let counter = Arc::clone(&counter);
            let predicate = Arc::clone(&predicate);
            tokio::spawn(async move {
                if predicate(&item) {
                    let mut guard = counter.lock().await;
                    *guard += 1;
                }
            })
        })
        .collect();

    for handle in handles {
        handle.await.unwrap();
    }

    let result = *counter.lock().await;
    result
}

// =============================================================================
// 10.9 並行マップ更新
// =============================================================================

/// 更新操作
#[derive(Debug, Clone)]
pub struct Update {
    pub key: String,
    pub value: i32,
}

impl Update {
    pub fn new(key: &str, value: i32) -> Self {
        Self {
            key: key.to_string(),
            value,
        }
    }
}

/// 並行してマップを更新
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::{Update, apply_updates_parallel};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let updates = vec![
///         Update::new("a", 1),
///         Update::new("b", 2),
///         Update::new("c", 3),
///     ];
///     let result = apply_updates_parallel(updates).await;
///     assert_eq!(result.get("a"), Some(&1));
///     assert_eq!(result.get("b"), Some(&2));
///     assert_eq!(result.get("c"), Some(&3));
/// });
/// ```
pub async fn apply_updates_parallel(updates: Vec<Update>) -> HashMap<String, i32> {
    let map = Arc::new(RwLock::new(HashMap::new()));

    let handles: Vec<JoinHandle<()>> = updates
        .into_iter()
        .map(|update| {
            let map = Arc::clone(&map);
            tokio::spawn(async move {
                let mut guard = map.write().await;
                guard.insert(update.key, update.value);
            })
        })
        .collect();

    for handle in handles {
        handle.await.unwrap();
    }

    Arc::try_unwrap(map).unwrap().into_inner()
}

// =============================================================================
// 10.10 タイムアウト付き収集
// =============================================================================

/// 指定時間内にデータを収集
///
/// # Examples
///
/// ```
/// use grokking_fp::ch10_concurrency::collect_for_duration;
/// use std::time::Duration;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let results = collect_for_duration(
///         Duration::from_millis(100),
///         Duration::from_millis(20),
///         || 42
///     ).await;
///     // 100ms / 20ms = 約5個のデータが収集されるはず
///     assert!(results.len() >= 3 && results.len() <= 7);
/// });
/// ```
pub async fn collect_for_duration<T, F>(
    duration: std::time::Duration,
    interval: std::time::Duration,
    producer: F,
) -> Vec<T>
where
    T: Send + Clone + 'static,
    F: Fn() -> T + Send + Sync + 'static,
{
    let collected = Arc::new(Mutex::new(Vec::new()));
    let producer = Arc::new(producer);

    let collected_clone = Arc::clone(&collected);
    let handle = tokio::spawn(async move {
        loop {
            tokio::time::sleep(interval).await;
            let value = producer();
            collected_clone.lock().await.push(value);
        }
    });

    tokio::time::sleep(duration).await;
    handle.abort();

    // 少し待機してタスクが確実に終了するのを待つ
    tokio::time::sleep(std::time::Duration::from_millis(10)).await;

    match Arc::try_unwrap(collected) {
        Ok(mutex) => mutex.into_inner(),
        Err(arc) => arc.lock().await.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_atomic_counter() {
        let counter = AtomicCounter::new(0);
        counter.increment().await;
        counter.increment().await;
        counter.increment().await;
        assert_eq!(counter.get().await, 3);
    }

    #[tokio::test]
    async fn test_atomic_counter_update() {
        let counter = AtomicCounter::new(10);
        counter.update(|x| x * 2).await;
        assert_eq!(counter.get().await, 20);
    }

    #[tokio::test]
    async fn test_shared_value() {
        let shared = SharedValue::new(42);
        assert_eq!(shared.read().await, 42);
        shared.write(100).await;
        assert_eq!(shared.read().await, 100);
    }

    #[tokio::test]
    async fn test_shared_value_update() {
        let shared = SharedValue::new(vec![1, 2, 3]);
        shared.update(|v| {
            let mut new_v = v.clone();
            new_v.push(4);
            new_v
        }).await;
        assert_eq!(shared.read().await, vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_top_cities() {
        let mut city_check_ins = HashMap::new();
        city_check_ins.insert(City::new("Sydney"), 100);
        city_check_ins.insert(City::new("Dublin"), 50);
        city_check_ins.insert(City::new("Tokyo"), 200);
        city_check_ins.insert(City::new("Lima"), 75);

        let top = top_cities(&city_check_ins, 3);
        assert_eq!(top.len(), 3);
        assert_eq!(top[0].city.name, "Tokyo");
        assert_eq!(top[0].check_ins, 200);
        assert_eq!(top[1].city.name, "Sydney");
        assert_eq!(top[2].city.name, "Lima");
    }

    #[tokio::test]
    async fn test_check_in_store() {
        let store = CheckInStore::new();
        store.store_check_in(City::new("Tokyo")).await;
        store.store_check_in(City::new("Tokyo")).await;
        store.store_check_in(City::new("Sydney")).await;

        let check_ins = store.get_all().await;
        assert_eq!(check_ins.get(&City::new("Tokyo")), Some(&2));
        assert_eq!(check_ins.get(&City::new("Sydney")), Some(&1));
    }

    #[tokio::test]
    async fn test_check_in_store_top() {
        let store = CheckInStore::new();
        store.store_check_in(City::new("Tokyo")).await;
        store.store_check_in(City::new("Tokyo")).await;
        store.store_check_in(City::new("Tokyo")).await;
        store.store_check_in(City::new("Sydney")).await;
        store.store_check_in(City::new("Sydney")).await;
        store.store_check_in(City::new("Dublin")).await;

        let top = store.get_top(2).await;
        assert_eq!(top.len(), 2);
        assert_eq!(top[0].city.name, "Tokyo");
        assert_eq!(top[1].city.name, "Sydney");
    }

    #[tokio::test]
    async fn test_run_parallel() {
        let results = run_parallel(vec![1, 2, 3], |x| async move { x * 2 }).await;
        assert_eq!(results, vec![2, 4, 6]);
    }

    #[tokio::test]
    async fn test_cast_dice_parallel() {
        let results = cast_dice_parallel(5).await;
        assert_eq!(results.len(), 5);
        for r in &results {
            assert!(*r >= 1 && *r <= 6);
        }
    }

    #[tokio::test]
    async fn test_start_counter_task() {
        let (counter, task) = start_counter_task(Duration::from_millis(10));

        tokio::time::sleep(Duration::from_millis(55)).await;

        let value = counter.get().await;
        assert!(value >= 3 && value <= 7);

        task.cancel();
    }

    #[tokio::test]
    async fn test_count_parallel() {
        let items = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let even_count = count_parallel(&items, |x| x % 2 == 0).await;
        assert_eq!(even_count, 5);
    }

    #[tokio::test]
    async fn test_apply_updates_parallel() {
        let updates = vec![
            Update::new("a", 1),
            Update::new("b", 2),
            Update::new("c", 3),
        ];
        let result = apply_updates_parallel(updates).await;
        assert_eq!(result.get("a"), Some(&1));
        assert_eq!(result.get("b"), Some(&2));
        assert_eq!(result.get("c"), Some(&3));
    }

    #[tokio::test]
    async fn test_collect_for_duration() {
        let results = collect_for_duration(
            Duration::from_millis(100),
            Duration::from_millis(20),
            || 42
        ).await;
        assert!(results.len() >= 3);
        for r in &results {
            assert_eq!(*r, 42);
        }
    }

    #[tokio::test]
    async fn test_process_check_ins() {
        let cities = vec![
            City::new("Tokyo"),
            City::new("Sydney"),
            City::new("Tokyo"),
            City::new("Dublin"),
            City::new("Tokyo"),
        ];

        let processing = process_check_ins(cities, Duration::from_millis(10)).await;

        tokio::time::sleep(Duration::from_millis(50)).await;

        let all = processing.all_check_ins().await;
        assert_eq!(all.get(&City::new("Tokyo")), Some(&3));

        processing.stop();
    }
}
