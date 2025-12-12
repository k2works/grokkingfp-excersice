(ns ch11.core-async
  "第11章: core.async と CSP

   Clojure における CSP（Communicating Sequential Processes）を学びます。
   - チャネル（channel）による通信
   - go ブロック（軽量スレッド）
   - パイプライン処理
   - バックプレッシャー"
  (:require [clojure.core.async :as async
             :refer [chan go go-loop <! >! <!! >!! close!
                     timeout alts! alts!! put! take!
                     pipe pipeline buffer
                     sliding-buffer dropping-buffer]]))

;; =============================================================================
;; チャネルの基本
;; =============================================================================

;; チャネルは goroutine 間の通信に使用
;; Scala の cats-effect の Queue や fs2 の Stream に相当

(defn create-channel
  "基本的なチャネルを作成"
  []
  (chan))

(defn create-buffered-channel
  "バッファ付きチャネルを作成"
  [size]
  (chan size))

(defn create-sliding-channel
  "スライディングバッファ付きチャネル（古い値を破棄）"
  [size]
  (chan (sliding-buffer size)))

(defn create-dropping-channel
  "ドロッピングバッファ付きチャネル（新しい値を破棄）"
  [size]
  (chan (dropping-buffer size)))

;; =============================================================================
;; go ブロック（軽量スレッド）
;; =============================================================================

;; go ブロックは Scala の Fiber に相当
;; OS スレッドをブロックせずに並行処理を実行

(defn simple-producer
  "シンプルなプロデューサー"
  [ch values]
  (go
    (doseq [v values]
      (>! ch v))
    (close! ch)))

(defn simple-consumer
  "シンプルなコンシューマー"
  [ch]
  (go-loop [results []]
    (if-let [v (<! ch)]
      (recur (conj results v))
      results)))

;; =============================================================================
;; 非同期操作
;; =============================================================================

(defn async-operation
  "非同期操作をシミュレート"
  [value delay-ms]
  (let [result-ch (chan)]
    (go
      (<! (timeout delay-ms))
      (>! result-ch {:value value :processed true})
      (close! result-ch))
    result-ch))

(defn parallel-async-operations
  "複数の非同期操作を並列実行"
  [values delay-ms]
  (let [result-ch (chan (count values))]
    (doseq [v values]
      (go
        (<! (timeout delay-ms))
        (>! result-ch {:value v :processed true})))
    result-ch))

;; =============================================================================
;; チェックイン処理（core.async 版）
;; =============================================================================

(defrecord City [name])
(defrecord CityStats [city check-ins])

(defn city [name]
  (->City name))

(defn city-stats [city check-ins]
  (->CityStats city check-ins))

(defn top-cities
  "チェックイン数でトップN都市を計算"
  [city-check-ins n]
  (->> city-check-ins
       (map (fn [[city check-ins]] (city-stats city check-ins)))
       (sort-by :check-ins >)
       (take n)))

(defn check-in-processor
  "チェックイン処理を開始"
  [check-in-ch]
  (let [state (atom {})
        ranking-ch (chan (sliding-buffer 1))]
    ;; チェックインを処理
    (go-loop []
      (when-let [city (<! check-in-ch)]
        (swap! state update city (fnil inc 0))
        (>! ranking-ch (top-cities @state 3))
        (recur)))
    ranking-ch))

;; =============================================================================
;; パイプラインパターン
;; =============================================================================

(defn map-pipeline
  "マッピングパイプライン"
  [f in-ch]
  (let [out-ch (chan)]
    (go-loop []
      (if-let [v (<! in-ch)]
        (do
          (>! out-ch (f v))
          (recur))
        (close! out-ch)))
    out-ch))

(defn filter-pipeline
  "フィルタリングパイプライン"
  [pred in-ch]
  (let [out-ch (chan)]
    (go-loop []
      (if-let [v (<! in-ch)]
        (do
          (when (pred v)
            (>! out-ch v))
          (recur))
        (close! out-ch)))
    out-ch))

(defn reduce-pipeline
  "リダクションパイプライン"
  [f init in-ch]
  (let [result-ch (chan)]
    (go-loop [acc init]
      (if-let [v (<! in-ch)]
        (recur (f acc v))
        (do
          (>! result-ch acc)
          (close! result-ch))))
    result-ch))

;; =============================================================================
;; Fan-out / Fan-in パターン
;; =============================================================================

(defn fan-out
  "1つのチャネルから複数のチャネルに分配"
  [in-ch out-chs]
  (go-loop []
    (when-let [v (<! in-ch)]
      (doseq [out-ch out-chs]
        (>! out-ch v))
      (recur))))

(defn fan-in
  "複数のチャネルから1つのチャネルに集約"
  [in-chs]
  (let [out-ch (chan)]
    (doseq [in-ch in-chs]
      (go-loop []
        (when-let [v (<! in-ch)]
          (>! out-ch v)
          (recur))))
    out-ch))

(defn merge-channels
  "複数のチャネルをマージ"
  [chs]
  (let [out-ch (chan)]
    (go-loop [open-chs (set chs)]
      (if (empty? open-chs)
        (close! out-ch)
        (let [[v ch] (alts! (vec open-chs))]
          (if (nil? v)
            (recur (disj open-chs ch))
            (do
              (>! out-ch v)
              (recur open-chs))))))
    out-ch))

;; =============================================================================
;; タイムアウトとセレクト
;; =============================================================================

(defn with-timeout
  "タイムアウト付きで値を取得"
  [ch timeout-ms default]
  (let [timeout-ch (timeout timeout-ms)]
    (go
      (let [[v ch] (alts! [ch timeout-ch])]
        (if (= ch timeout-ch)
          default
          v)))))

(defn first-response
  "最初に応答したチャネルの値を取得"
  [chs]
  (go
    (let [[v _] (alts! chs)]
      v)))

;; =============================================================================
;; ワーカープール
;; =============================================================================

(defn worker
  "ワーカーを作成"
  [id work-ch result-ch]
  (go-loop []
    (when-let [work (<! work-ch)]
      (let [result (work)]
        (>! result-ch {:worker id :result result}))
      (recur))))

(defn create-worker-pool
  "ワーカープールを作成"
  [n work-ch result-ch]
  (dotimes [i n]
    (worker i work-ch result-ch)))

(defn submit-work
  "ワークを送信"
  [work-ch work]
  (put! work-ch work))

;; =============================================================================
;; 並行ビルトイン pipeline
;; =============================================================================

(defn parallel-transform
  "並列変換（core.async の pipeline を使用）"
  [n xf in-ch]
  (let [out-ch (chan n)]
    (pipeline n out-ch xf in-ch)
    out-ch))

;; =============================================================================
;; pub/sub パターン
;; =============================================================================

(defn create-pub-sub
  "pub/sub システムを作成"
  []
  (let [pub-ch (chan)
        subscribers (atom {})]
    {:publish (fn [topic message]
                (put! pub-ch {:topic topic :message message}))
     :subscribe (fn [topic]
                  (let [sub-ch (chan)]
                    (swap! subscribers update topic (fnil conj []) sub-ch)
                    sub-ch))
     :start (fn []
              (go-loop []
                (when-let [{:keys [topic message]} (<! pub-ch)]
                  (doseq [sub-ch (get @subscribers topic [])]
                    (>! sub-ch message))
                  (recur))))
     :close (fn []
              (close! pub-ch)
              (doseq [topic-subs (vals @subscribers)
                      sub-ch topic-subs]
                (close! sub-ch)))}))

;; =============================================================================
;; レートリミッター
;; =============================================================================

(defn create-rate-limiter
  "レートリミッターを作成"
  [rate-per-second]
  (let [interval-ms (/ 1000 rate-per-second)
        token-ch (chan 1)]
    ;; トークンを定期的に供給
    (go-loop []
      (>! token-ch :token)
      (<! (timeout interval-ms))
      (recur))
    {:acquire (fn []
                (<!! token-ch))
     :try-acquire (fn [timeout-ms]
                    (let [[v _] (alts!! [token-ch (timeout timeout-ms)])]
                      (some? v)))}))

;; =============================================================================
;; セマフォ
;; =============================================================================

(defn create-semaphore
  "セマフォを作成"
  [permits]
  (let [sem-ch (chan permits)]
    ;; 初期許可を追加
    (dotimes [_ permits]
      (put! sem-ch :permit))
    {:acquire (fn []
                (<!! sem-ch))
     :release (fn []
                (put! sem-ch :permit))
     :with-permit (fn [f]
                    (<!! sem-ch)
                    (try
                      (f)
                      (finally
                        (put! sem-ch :permit))))}))

;; =============================================================================
;; イベントソーシング
;; =============================================================================

(defrecord Event [type timestamp data])

(defn create-event
  "イベントを作成"
  [type data]
  (->Event type (System/currentTimeMillis) data))

(defn event-sourced-entity
  "イベントソースエンティティを作成"
  [initial-state reducer]
  (let [state (atom initial-state)
        event-log (atom [])
        event-ch (chan 100)]
    ;; イベント処理ループ
    (go-loop []
      (when-let [event (<! event-ch)]
        (swap! event-log conj event)
        (swap! state reducer event)
        (recur)))
    {:emit (fn [event]
             (put! event-ch event))
     :get-state (fn [] @state)
     :get-events (fn [] @event-log)
     :close (fn [] (close! event-ch))}))

;; =============================================================================
;; 実践例：並行ダウンローダー
;; =============================================================================

(defn simulate-download
  "ダウンロードをシミュレート"
  [url]
  (let [result-ch (chan)]
    (go
      (<! (timeout (+ 100 (rand-int 200))))
      (>! result-ch {:url url
                     :size (+ 1000 (rand-int 9000))
                     :status :completed})
      (close! result-ch))
    result-ch))

(defn concurrent-downloader
  "並行ダウンローダー"
  [urls max-concurrent]
  (let [result-ch (chan (count urls))
        semaphore (chan max-concurrent)]
    ;; セマフォを初期化
    (dotimes [_ max-concurrent]
      (put! semaphore :permit))

    ;; ダウンロードを開始
    (doseq [url urls]
      (go
        (<! semaphore)  ; 許可を取得
        (let [download-ch (simulate-download url)
              result (<! download-ch)]
          (>! result-ch result)
          (>! semaphore :permit))))  ; 許可を返却

    result-ch))

;; =============================================================================
;; 実践例：メッセージブローカー
;; =============================================================================

(defn create-message-broker
  "メッセージブローカーを作成"
  []
  (let [queues (atom {})
        running (atom true)]
    {:create-queue (fn [name buffer-size]
                     (swap! queues assoc name (chan buffer-size))
                     name)
     :send (fn [queue-name message]
             (when-let [q (get @queues queue-name)]
               (put! q message)))
     :receive (fn [queue-name]
                (when-let [q (get @queues queue-name)]
                  (<!! q)))
     :receive-async (fn [queue-name]
                      (when-let [q (get @queues queue-name)]
                        (go (<! q))))
     :close (fn []
              (reset! running false)
              (doseq [q (vals @queues)]
                (close! q)))}))

;; =============================================================================
;; 便利な同期ヘルパー
;; =============================================================================

(defn channel->seq
  "チャネルをシーケンスに変換（ブロッキング）"
  [ch]
  (lazy-seq
   (when-let [v (<!! ch)]
     (cons v (channel->seq ch)))))

(defn seq->channel
  "シーケンスをチャネルに変換"
  [coll]
  (let [ch (chan)]
    (go
      (doseq [v coll]
        (>! ch v))
      (close! ch))
    ch))

(defn collect-all
  "チャネルからすべての値を収集（ブロッキング）"
  [ch]
  (loop [results []]
    (if-let [v (<!! ch)]
      (recur (conj results v))
      results)))

(defn collect-n
  "チャネルから n 個の値を収集（ブロッキング）"
  [ch n]
  (loop [results []
         remaining n]
    (if (zero? remaining)
      results
      (if-let [v (<!! ch)]
        (recur (conj results v) (dec remaining))
        results))))

