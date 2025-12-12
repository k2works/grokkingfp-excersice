//! 第11章: チャネルと並行パターン
//!
//! Rust では mpsc/broadcast/oneshot チャネルを使って並行処理間の通信を行います。
//! Go の goroutine と channel に似た概念を学びます。

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, oneshot, Mutex, RwLock};
use tokio::task::JoinHandle;

// =============================================================================
// 11.1 mpsc チャネル - 複数送信者・単一受信者
// =============================================================================

/// mpsc チャネルの基本的な使用例
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::basic_mpsc_example;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let result = basic_mpsc_example().await;
///     assert_eq!(result, vec![1, 2, 3]);
/// });
/// ```
pub async fn basic_mpsc_example() -> Vec<i32> {
    let (tx, mut rx) = mpsc::channel(10);

    // 送信タスク
    tokio::spawn(async move {
        for i in 1..=3 {
            tx.send(i).await.unwrap();
        }
    });

    // 受信
    let mut results = Vec::new();
    while let Some(value) = rx.recv().await {
        results.push(value);
    }

    results
}

/// 複数の送信者からデータを受信
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::multiple_senders;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let result = multiple_senders(3).await;
///     assert_eq!(result.len(), 3);
/// });
/// ```
pub async fn multiple_senders(sender_count: usize) -> Vec<String> {
    let (tx, mut rx) = mpsc::channel(sender_count * 2);

    // 複数の送信タスクを起動
    for i in 0..sender_count {
        let tx = tx.clone();
        tokio::spawn(async move {
            tx.send(format!("Message from sender {}", i)).await.unwrap();
        });
    }

    // 元の tx をドロップして、全送信者が完了したらチャネルが閉じるようにする
    drop(tx);

    // 受信
    let mut results = Vec::new();
    while let Some(value) = rx.recv().await {
        results.push(value);
    }

    results
}

// =============================================================================
// 11.2 oneshot チャネル - 一回限りの通信
// =============================================================================

/// oneshot チャネルで結果を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::compute_with_oneshot;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let result = compute_with_oneshot(5).await;
///     assert_eq!(result, 25);
/// });
/// ```
pub async fn compute_with_oneshot(input: i32) -> i32 {
    let (tx, rx) = oneshot::channel();

    tokio::spawn(async move {
        let result = input * input;
        tx.send(result).unwrap();
    });

    rx.await.unwrap()
}

/// 複数の計算を並列実行して結果を収集
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::parallel_compute;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let results = parallel_compute(vec![1, 2, 3, 4, 5]).await;
///     assert_eq!(results, vec![1, 4, 9, 16, 25]);
/// });
/// ```
pub async fn parallel_compute(inputs: Vec<i32>) -> Vec<i32> {
    let mut receivers = Vec::with_capacity(inputs.len());

    for input in inputs {
        let (tx, rx) = oneshot::channel();
        tokio::spawn(async move {
            let result = input * input;
            let _ = tx.send(result);
        });
        receivers.push(rx);
    }

    let mut results = Vec::with_capacity(receivers.len());
    for rx in receivers {
        results.push(rx.await.unwrap());
    }
    results
}

// =============================================================================
// 11.3 broadcast チャネル - 複数受信者
// =============================================================================

/// broadcast チャネルで複数の受信者にメッセージを配信
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::broadcast_example;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let results = broadcast_example(3).await;
///     // 各受信者が同じメッセージを受け取る
///     for messages in &results {
///         assert_eq!(messages, &vec!["Hello".to_string(), "World".to_string()]);
///     }
/// });
/// ```
pub async fn broadcast_example(receiver_count: usize) -> Vec<Vec<String>> {
    let (tx, _) = broadcast::channel(16);

    // 受信タスクを起動
    let mut handles = Vec::new();
    for _ in 0..receiver_count {
        let mut rx = tx.subscribe();
        handles.push(tokio::spawn(async move {
            let mut received = Vec::new();
            while let Ok(msg) = rx.recv().await {
                received.push(msg);
            }
            received
        }));
    }

    // メッセージを送信
    tx.send("Hello".to_string()).unwrap();
    tx.send("World".to_string()).unwrap();
    drop(tx); // 送信者をドロップしてチャネルを閉じる

    // 結果を収集
    let mut results = Vec::new();
    for handle in handles {
        results.push(handle.await.unwrap());
    }
    results
}

// =============================================================================
// 11.4 ワーカープールパターン
// =============================================================================

/// ワーカープールで並列処理
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::worker_pool;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let items = vec![1, 2, 3, 4, 5, 6, 7, 8];
///     let results = worker_pool(items, 3, |x| x * 2).await;
///     let mut sorted = results.clone();
///     sorted.sort();
///     assert_eq!(sorted, vec![2, 4, 6, 8, 10, 12, 14, 16]);
/// });
/// ```
pub async fn worker_pool<T, U, F>(items: Vec<T>, worker_count: usize, f: F) -> Vec<U>
where
    T: Send + 'static,
    U: Send + 'static,
    F: Fn(T) -> U + Send + Sync + Clone + 'static,
{
    let (work_tx, work_rx) = async_channel::bounded(items.len());
    let (result_tx, mut result_rx) = mpsc::channel(items.len());

    let work_rx = Arc::new(work_rx);

    // ワーカーを起動
    let mut handles = Vec::new();
    for _ in 0..worker_count {
        let work_rx = Arc::clone(&work_rx);
        let result_tx = result_tx.clone();
        let f = f.clone();

        handles.push(tokio::spawn(async move {
            while let Ok(item) = work_rx.recv().await {
                let result = f(item);
                let _ = result_tx.send(result).await;
            }
        }));
    }

    // 作業をキューに追加
    let item_count = items.len();
    for item in items {
        work_tx.send(item).await.unwrap();
    }
    work_tx.close();

    // 元の result_tx をドロップ
    drop(result_tx);

    // 結果を収集
    let mut results = Vec::with_capacity(item_count);
    while let Some(result) = result_rx.recv().await {
        results.push(result);
    }

    // ワーカーの終了を待機
    for handle in handles {
        let _ = handle.await;
    }

    results
}

// =============================================================================
// 11.5 パイプラインパターン
// =============================================================================

/// パイプラインステージ
pub struct Pipeline<T> {
    receiver: mpsc::Receiver<T>,
}

impl<T: Send + 'static> Pipeline<T> {
    /// ソースからパイプラインを作成
    pub fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T> + Send + 'static,
        I::IntoIter: Send,
    {
        let (tx, rx) = mpsc::channel(100);

        tokio::spawn(async move {
            for item in iter {
                if tx.send(item).await.is_err() {
                    break;
                }
            }
        });

        Pipeline { receiver: rx }
    }

    /// map ステージを追加
    pub fn map<U, F>(self, f: F) -> Pipeline<U>
    where
        U: Send + 'static,
        F: Fn(T) -> U + Send + 'static,
    {
        let (tx, rx) = mpsc::channel(100);
        let mut receiver = self.receiver;

        tokio::spawn(async move {
            while let Some(item) = receiver.recv().await {
                if tx.send(f(item)).await.is_err() {
                    break;
                }
            }
        });

        Pipeline { receiver: rx }
    }

    /// filter ステージを追加
    pub fn filter<F>(self, f: F) -> Pipeline<T>
    where
        F: Fn(&T) -> bool + Send + 'static,
    {
        let (tx, rx) = mpsc::channel(100);
        let mut receiver = self.receiver;

        tokio::spawn(async move {
            while let Some(item) = receiver.recv().await {
                if f(&item) {
                    if tx.send(item).await.is_err() {
                        break;
                    }
                }
            }
        });

        Pipeline { receiver: rx }
    }

    /// 結果を収集
    pub async fn collect(mut self) -> Vec<T> {
        let mut results = Vec::new();
        while let Some(item) = self.receiver.recv().await {
            results.push(item);
        }
        results
    }
}

/// パイプラインの使用例
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::Pipeline;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let results = Pipeline::from_iter(1..=10)
///         .map(|x| x * 2)
///         .filter(|x| *x > 10)
///         .collect()
///         .await;
///
///     assert_eq!(results, vec![12, 14, 16, 18, 20]);
/// });
/// ```
pub fn pipeline_example() {}

// =============================================================================
// 11.6 アクターパターン
// =============================================================================

/// アクターへのメッセージ
#[derive(Debug)]
pub enum CounterMessage {
    Increment,
    Decrement,
    Get(oneshot::Sender<i32>),
    Stop,
}

/// カウンターアクター
pub struct CounterActor {
    sender: mpsc::Sender<CounterMessage>,
}

impl CounterActor {
    /// 新しいアクターを起動
    ///
    /// # Examples
    ///
    /// ```
    /// use grokking_fp::ch11_channels::CounterActor;
    ///
    /// let rt = tokio::runtime::Runtime::new().unwrap();
    /// rt.block_on(async {
    ///     let actor = CounterActor::new(0);
    ///
    ///     actor.increment().await;
    ///     actor.increment().await;
    ///     actor.increment().await;
    ///     actor.decrement().await;
    ///
    ///     assert_eq!(actor.get().await, 2);
    ///
    ///     actor.stop().await;
    /// });
    /// ```
    pub fn new(initial: i32) -> Self {
        let (tx, mut rx) = mpsc::channel::<CounterMessage>(32);

        tokio::spawn(async move {
            let mut value = initial;

            while let Some(msg) = rx.recv().await {
                match msg {
                    CounterMessage::Increment => value += 1,
                    CounterMessage::Decrement => value -= 1,
                    CounterMessage::Get(reply) => {
                        let _ = reply.send(value);
                    }
                    CounterMessage::Stop => break,
                }
            }
        });

        CounterActor { sender: tx }
    }

    pub async fn increment(&self) {
        let _ = self.sender.send(CounterMessage::Increment).await;
    }

    pub async fn decrement(&self) {
        let _ = self.sender.send(CounterMessage::Decrement).await;
    }

    pub async fn get(&self) -> i32 {
        let (tx, rx) = oneshot::channel();
        let _ = self.sender.send(CounterMessage::Get(tx)).await;
        rx.await.unwrap_or(0)
    }

    pub async fn stop(&self) {
        let _ = self.sender.send(CounterMessage::Stop).await;
    }
}

// =============================================================================
// 11.7 バンク口座アクター
// =============================================================================

/// 銀行口座へのメッセージ
#[derive(Debug)]
pub enum BankMessage {
    Deposit(i32),
    Withdraw(i32, oneshot::Sender<Result<(), String>>),
    GetBalance(oneshot::Sender<i32>),
    Stop,
}

/// 銀行口座アクター
pub struct BankAccount {
    sender: mpsc::Sender<BankMessage>,
}

impl BankAccount {
    /// 新しい口座を作成
    ///
    /// # Examples
    ///
    /// ```
    /// use grokking_fp::ch11_channels::BankAccount;
    ///
    /// let rt = tokio::runtime::Runtime::new().unwrap();
    /// rt.block_on(async {
    ///     let account = BankAccount::new(100);
    ///
    ///     account.deposit(50).await;
    ///     assert_eq!(account.get_balance().await, 150);
    ///
    ///     assert!(account.withdraw(100).await.is_ok());
    ///     assert_eq!(account.get_balance().await, 50);
    ///
    ///     // 残高不足
    ///     assert!(account.withdraw(100).await.is_err());
    ///
    ///     account.stop().await;
    /// });
    /// ```
    pub fn new(initial_balance: i32) -> Self {
        let (tx, mut rx) = mpsc::channel::<BankMessage>(32);

        tokio::spawn(async move {
            let mut balance = initial_balance;

            while let Some(msg) = rx.recv().await {
                match msg {
                    BankMessage::Deposit(amount) => {
                        balance += amount;
                    }
                    BankMessage::Withdraw(amount, reply) => {
                        if balance >= amount {
                            balance -= amount;
                            let _ = reply.send(Ok(()));
                        } else {
                            let _ = reply.send(Err("Insufficient funds".to_string()));
                        }
                    }
                    BankMessage::GetBalance(reply) => {
                        let _ = reply.send(balance);
                    }
                    BankMessage::Stop => break,
                }
            }
        });

        BankAccount { sender: tx }
    }

    pub async fn deposit(&self, amount: i32) {
        let _ = self.sender.send(BankMessage::Deposit(amount)).await;
    }

    pub async fn withdraw(&self, amount: i32) -> Result<(), String> {
        let (tx, rx) = oneshot::channel();
        let _ = self.sender.send(BankMessage::Withdraw(amount, tx)).await;
        rx.await.unwrap_or(Err("Actor not responding".to_string()))
    }

    pub async fn get_balance(&self) -> i32 {
        let (tx, rx) = oneshot::channel();
        let _ = self.sender.send(BankMessage::GetBalance(tx)).await;
        rx.await.unwrap_or(0)
    }

    pub async fn stop(&self) {
        let _ = self.sender.send(BankMessage::Stop).await;
    }
}

// =============================================================================
// 11.8 イベントバス
// =============================================================================

/// イベントの種類
#[derive(Debug, Clone)]
pub enum Event {
    UserLoggedIn { user_id: String },
    UserLoggedOut { user_id: String },
    MessageSent { from: String, to: String, content: String },
}

/// イベントバス
pub struct EventBus {
    sender: broadcast::Sender<Event>,
}

impl EventBus {
    /// 新しいイベントバスを作成
    ///
    /// # Examples
    ///
    /// ```
    /// use grokking_fp::ch11_channels::{EventBus, Event};
    ///
    /// let rt = tokio::runtime::Runtime::new().unwrap();
    /// rt.block_on(async {
    ///     let bus = EventBus::new();
    ///
    ///     // サブスクライバーを作成
    ///     let mut rx = bus.subscribe();
    ///
    ///     // イベントを発行
    ///     bus.publish(Event::UserLoggedIn { user_id: "user1".to_string() });
    ///
    ///     // イベントを受信
    ///     if let Ok(Event::UserLoggedIn { user_id }) = rx.recv().await {
    ///         assert_eq!(user_id, "user1");
    ///     }
    /// });
    /// ```
    pub fn new() -> Self {
        let (sender, _) = broadcast::channel(100);
        EventBus { sender }
    }

    pub fn subscribe(&self) -> broadcast::Receiver<Event> {
        self.sender.subscribe()
    }

    pub fn publish(&self, event: Event) {
        let _ = self.sender.send(event);
    }
}

impl Default for EventBus {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// 11.9 セマフォによる同時実行制限
// =============================================================================

use tokio::sync::Semaphore;

/// セマフォで同時実行数を制限
///
/// # Examples
///
/// ```
/// use grokking_fp::ch11_channels::rate_limited_process;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let items = vec![1, 2, 3, 4, 5, 6, 7, 8];
///     let results = rate_limited_process(items, 2, |x| x * 2).await;
///     let mut sorted = results.clone();
///     sorted.sort();
///     assert_eq!(sorted, vec![2, 4, 6, 8, 10, 12, 14, 16]);
/// });
/// ```
pub async fn rate_limited_process<T, U, F>(
    items: Vec<T>,
    max_concurrent: usize,
    f: F,
) -> Vec<U>
where
    T: Send + 'static,
    U: Send + Clone + 'static,
    F: Fn(T) -> U + Send + Sync + Clone + 'static,
{
    let semaphore = Arc::new(Semaphore::new(max_concurrent));
    let results = Arc::new(Mutex::new(Vec::with_capacity(items.len())));

    let mut handles = Vec::new();

    for item in items {
        let semaphore = Arc::clone(&semaphore);
        let results = Arc::clone(&results);
        let f = f.clone();

        handles.push(tokio::spawn(async move {
            let _permit = semaphore.acquire().await.unwrap();
            let result = f(item);
            results.lock().await.push(result);
        }));
    }

    for handle in handles {
        let _ = handle.await;
    }

    match Arc::try_unwrap(results) {
        Ok(mutex) => mutex.into_inner(),
        Err(arc) => arc.lock().await.clone(),
    }
}

// =============================================================================
// 11.10 チャネルを使ったチェックイン集計
// =============================================================================

use super::ch10_concurrency::{City, CityStats};

/// チャネルベースのチェックイン処理
pub struct ChannelCheckInProcessor {
    check_in_tx: mpsc::Sender<City>,
    ranking_rx: Arc<RwLock<Vec<CityStats>>>,
    tasks: Vec<JoinHandle<()>>,
}

impl ChannelCheckInProcessor {
    /// 新しいプロセッサを作成
    ///
    /// # Examples
    ///
    /// ```
    /// use grokking_fp::ch11_channels::ChannelCheckInProcessor;
    /// use grokking_fp::ch10_concurrency::City;
    /// use std::time::Duration;
    ///
    /// let rt = tokio::runtime::Runtime::new().unwrap();
    /// rt.block_on(async {
    ///     let processor = ChannelCheckInProcessor::new(Duration::from_millis(10));
    ///
    ///     processor.check_in(City::new("Tokyo")).await;
    ///     processor.check_in(City::new("Tokyo")).await;
    ///     processor.check_in(City::new("Sydney")).await;
    ///
    ///     tokio::time::sleep(Duration::from_millis(50)).await;
    ///
    ///     let ranking = processor.get_ranking().await;
    ///     if !ranking.is_empty() {
    ///         assert_eq!(ranking[0].city.name, "Tokyo");
    ///     }
    ///
    ///     processor.stop();
    /// });
    /// ```
    pub fn new(ranking_update_interval: std::time::Duration) -> Self {
        let (check_in_tx, mut check_in_rx) = mpsc::channel::<City>(1000);
        let check_ins = Arc::new(RwLock::new(HashMap::<City, i32>::new()));
        let ranking_rx = Arc::new(RwLock::new(Vec::<CityStats>::new()));

        // チェックイン受信タスク
        let check_ins_clone = Arc::clone(&check_ins);
        let check_in_task = tokio::spawn(async move {
            while let Some(city) = check_in_rx.recv().await {
                let mut guard = check_ins_clone.write().await;
                *guard.entry(city).or_insert(0) += 1;
            }
        });

        // ランキング更新タスク
        let check_ins_clone = Arc::clone(&check_ins);
        let ranking_clone = Arc::clone(&ranking_rx);
        let ranking_task = tokio::spawn(async move {
            loop {
                tokio::time::sleep(ranking_update_interval).await;

                let check_ins = check_ins_clone.read().await;
                let mut stats: Vec<CityStats> = check_ins
                    .iter()
                    .map(|(city, &count)| CityStats::new(city.clone(), count))
                    .collect();
                stats.sort_by(|a, b| b.check_ins.cmp(&a.check_ins));
                stats.truncate(3);

                *ranking_clone.write().await = stats;
            }
        });

        ChannelCheckInProcessor {
            check_in_tx,
            ranking_rx,
            tasks: vec![check_in_task, ranking_task],
        }
    }

    pub async fn check_in(&self, city: City) {
        let _ = self.check_in_tx.send(city).await;
    }

    pub async fn get_ranking(&self) -> Vec<CityStats> {
        self.ranking_rx.read().await.clone()
    }

    pub fn stop(self) {
        for task in self.tasks {
            task.abort();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_basic_mpsc() {
        let result = basic_mpsc_example().await;
        assert_eq!(result, vec![1, 2, 3]);
    }

    #[tokio::test]
    async fn test_multiple_senders() {
        let result = multiple_senders(5).await;
        assert_eq!(result.len(), 5);
    }

    #[tokio::test]
    async fn test_compute_with_oneshot() {
        let result = compute_with_oneshot(7).await;
        assert_eq!(result, 49);
    }

    #[tokio::test]
    async fn test_parallel_compute() {
        let results = parallel_compute(vec![1, 2, 3, 4, 5]).await;
        assert_eq!(results, vec![1, 4, 9, 16, 25]);
    }

    #[tokio::test]
    async fn test_broadcast() {
        let results = broadcast_example(3).await;
        assert_eq!(results.len(), 3);
        for messages in &results {
            assert_eq!(messages, &vec!["Hello".to_string(), "World".to_string()]);
        }
    }

    #[tokio::test]
    async fn test_worker_pool() {
        let items = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let results = worker_pool(items, 3, |x| x * 2).await;
        let mut sorted = results;
        sorted.sort();
        assert_eq!(sorted, vec![2, 4, 6, 8, 10, 12, 14, 16]);
    }

    #[tokio::test]
    async fn test_pipeline() {
        let results = Pipeline::from_iter(1..=10)
            .map(|x| x * 2)
            .filter(|x| *x > 10)
            .collect()
            .await;

        assert_eq!(results, vec![12, 14, 16, 18, 20]);
    }

    #[tokio::test]
    async fn test_counter_actor() {
        let actor = CounterActor::new(0);

        actor.increment().await;
        actor.increment().await;
        actor.increment().await;
        actor.decrement().await;

        assert_eq!(actor.get().await, 2);

        actor.stop().await;
    }

    #[tokio::test]
    async fn test_bank_account() {
        let account = BankAccount::new(100);

        account.deposit(50).await;
        assert_eq!(account.get_balance().await, 150);

        assert!(account.withdraw(100).await.is_ok());
        assert_eq!(account.get_balance().await, 50);

        assert!(account.withdraw(100).await.is_err());

        account.stop().await;
    }

    #[tokio::test]
    async fn test_event_bus() {
        let bus = EventBus::new();
        let mut rx = bus.subscribe();

        bus.publish(Event::UserLoggedIn {
            user_id: "user1".to_string(),
        });

        if let Ok(Event::UserLoggedIn { user_id }) = rx.recv().await {
            assert_eq!(user_id, "user1");
        } else {
            panic!("Expected UserLoggedIn event");
        }
    }

    #[tokio::test]
    async fn test_rate_limited_process() {
        let items = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let results = rate_limited_process(items, 2, |x| x * 2).await;
        let mut sorted = results;
        sorted.sort();
        assert_eq!(sorted, vec![2, 4, 6, 8, 10, 12, 14, 16]);
    }

    #[tokio::test]
    async fn test_channel_check_in_processor() {
        let processor = ChannelCheckInProcessor::new(Duration::from_millis(10));

        processor.check_in(City::new("Tokyo")).await;
        processor.check_in(City::new("Tokyo")).await;
        processor.check_in(City::new("Sydney")).await;

        tokio::time::sleep(Duration::from_millis(50)).await;

        let ranking = processor.get_ranking().await;
        if !ranking.is_empty() {
            assert_eq!(ranking[0].city.name, "Tokyo");
        }

        processor.stop();
    }
}
