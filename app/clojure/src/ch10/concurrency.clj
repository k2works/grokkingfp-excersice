(ns ch10.concurrency
  "第10章: 並行・並列処理

   Clojure における並行処理の基本を学びます。
   - atom/ref/agent による共有状態管理
   - pmap による並列処理
   - future による非同期処理
   - チェックイン処理のリアルタイム集計")

;; =============================================================================
;; 並行処理の課題
;; =============================================================================

;; 従来の並行処理の問題:
;; - デッドロック
;; - 競合状態（Race Condition）
;; - 共有状態の管理の複雑さ
;; - スレッドのオーバーヘッド

;; Clojure の解決策:
;; - イミュータブルデータ
;; - atom/ref/agent（アトミック参照）
;; - STM（Software Transactional Memory）
;; - 宣言的な並行処理

;; =============================================================================
;; チェックインのリアルタイム集計
;; =============================================================================

(defrecord City [name])
(defrecord CityStats [city check-ins])

(defn city [name]
  (->City name))

(defn city-stats [city check-ins]
  (->CityStats city check-ins))

;; サンプルチェックインデータ
(def sample-cities
  [(city "Sydney")
   (city "Dublin")
   (city "Cape Town")
   (city "Lima")
   (city "Singapore")])

(defn generate-check-ins
  "チェックインのシーケンスを生成"
  [n]
  (repeatedly n #(rand-nth sample-cities)))

;; =============================================================================
;; トップ都市の計算（純粋関数）
;; =============================================================================

(defn top-cities
  "チェックイン数でトップN都市を計算（純粋関数）"
  [city-check-ins n]
  (->> city-check-ins
       (map (fn [[city check-ins]] (city-stats city check-ins)))
       (sort-by :check-ins >)
       (take n)))

;; =============================================================================
;; 逐次処理版
;; =============================================================================

(defn process-check-ins-sequential
  "チェックインを逐次処理"
  [check-ins]
  (reduce
   (fn [city-check-ins city]
     (update city-check-ins city (fnil inc 0)))
   {}
   check-ins))

;; =============================================================================
;; atom を使った並行処理版
;; =============================================================================

(defn store-check-in!
  "チェックインを atom に保存"
  [stored-check-ins city]
  (swap! stored-check-ins update city (fnil inc 0)))

(defn process-check-ins-concurrent
  "チェックインを並行処理"
  [check-ins]
  (let [stored-check-ins (atom {})]
    (dorun (pmap #(store-check-in! stored-check-ins %) check-ins))
    @stored-check-ins))

;; =============================================================================
;; ランキングの継続的な更新
;; =============================================================================

(defn update-ranking!
  "ランキングを継続的に更新"
  [stored-check-ins stored-ranking n]
  (reset! stored-ranking (top-cities @stored-check-ins n)))

(defn start-ranking-updater!
  "ランキング更新を開始（バックグラウンド）"
  [stored-check-ins stored-ranking n interval-ms]
  (future
    (while (not (Thread/interrupted))
      (update-ranking! stored-check-ins stored-ranking n)
      (Thread/sleep interval-ms))))

;; =============================================================================
;; pmap による並列処理
;; =============================================================================

(defn parallel-map
  "並列マップ処理"
  [f coll]
  (doall (pmap f coll)))

;; 順次実行 vs 並列実行の比較
(defn slow-operation
  "遅い操作をシミュレート"
  [n]
  (Thread/sleep 100)
  (* n n))

(defn sequential-process
  "順次処理"
  [coll]
  (mapv slow-operation coll))

(defn parallel-process
  "並列処理"
  [coll]
  (vec (pmap slow-operation coll)))

;; =============================================================================
;; サイコロを並行して振る
;; =============================================================================

(defn cast-the-die
  "サイコロを振る"
  []
  (inc (rand-int 6)))

(defn cast-dice-concurrent
  "複数のサイコロを並行して振る"
  [n]
  (vec (pmap (fn [_] (cast-the-die)) (range n))))

(defn cast-dice-and-sum
  "複数のサイコロを並行して振り、合計を返す"
  [n]
  (reduce + (cast-dice-concurrent n)))

;; atom を使ったサイコロ結果の保存
(defn cast-dice-with-storage!
  "サイコロを振って結果を atom に保存"
  [n]
  (let [stored-casts (atom [])]
    (dorun
     (pmap
      (fn [_]
        (let [result (cast-the-die)]
          (swap! stored-casts conj result)))
      (range n)))
    @stored-casts))

;; =============================================================================
;; チェックイン処理の完全版
;; =============================================================================

(defrecord ProcessingCheckIns [current-ranking stop])

(defn start-check-in-processing!
  "チェックイン処理を開始"
  [check-ins-seq]
  (let [stored-check-ins (atom {})
        stored-ranking (atom [])
        running (atom true)

        ;; チェックイン処理（バックグラウンド）
        check-in-processor
        (future
          (doseq [city check-ins-seq
                  :while @running]
            (store-check-in! stored-check-ins city)))

        ;; ランキング更新（バックグラウンド）
        ranking-updater
        (future
          (while @running
            (update-ranking! stored-check-ins stored-ranking 3)
            (Thread/sleep 100)))]

    (->ProcessingCheckIns
     (fn [] @stored-ranking)  ; current-ranking
     (fn []                    ; stop
       (reset! running false)
       (future-cancel check-in-processor)
       (future-cancel ranking-updater)))))

;; =============================================================================
;; agent による非同期状態更新
;; =============================================================================

(def check-in-agent (agent {}))

(defn agent-store-check-in!
  "agent を使ってチェックインを保存"
  [city]
  (send check-in-agent update city (fnil inc 0)))

(defn agent-get-check-ins
  "agent からチェックイン数を取得"
  []
  @check-in-agent)

(defn agent-reset-check-ins!
  "agent をリセット"
  []
  (send check-in-agent (constantly {})))

;; エラーハンドリング付き agent
(defn agent-with-error-handler
  "エラーハンドリング付き agent を作成"
  [initial-value]
  (agent initial-value
         :error-handler (fn [ag ex]
                          (println "Agent error:" (.getMessage ex)))))

;; =============================================================================
;; ref と STM（Software Transactional Memory）
;; =============================================================================

(def check-in-ref (ref {}))
(def ranking-ref (ref []))

(defn stm-store-check-in!
  "STM を使ってチェックインを保存"
  [city]
  (dosync
   (alter check-in-ref update city (fnil inc 0))))

(defn stm-update-ranking!
  "STM を使ってランキングを更新"
  [n]
  (dosync
   (ref-set ranking-ref (top-cities @check-in-ref n))))

(defn stm-transfer-check-ins!
  "2つの都市間でチェックイン数を移動（トランザクション）"
  [from-city to-city amount]
  (dosync
   (let [from-count (get @check-in-ref from-city 0)]
     (when (>= from-count amount)
       (alter check-in-ref update from-city - amount)
       (alter check-in-ref update to-city (fnil + 0) amount)
       true))))

;; =============================================================================
;; future による非同期処理
;; =============================================================================

(defn async-api-call
  "非同期 API 呼び出しをシミュレート"
  [name delay-ms]
  (future
    (Thread/sleep delay-ms)
    {:name name :result (str "Result for " name)}))

(defn parallel-api-calls
  "複数の API を並列呼び出し"
  [names]
  (let [futures (mapv #(async-api-call % 100) names)]
    (mapv deref futures)))

;; タイムアウト付き
(defn parallel-api-calls-with-timeout
  "タイムアウト付き並列 API 呼び出し"
  [names timeout-ms]
  (let [futures (mapv #(async-api-call % 100) names)]
    (mapv #(deref % timeout-ms {:error "timeout"}) futures)))

;; =============================================================================
;; 並行カウント
;; =============================================================================

(defn count-evens
  "並行処理で偶数をカウント"
  [numbers]
  (let [counter (atom 0)]
    (dorun
     (pmap
      (fn [n]
        (when (even? n)
          (swap! counter inc)))
      numbers))
    @counter))

(defn parallel-filter-count
  "並列フィルタリングとカウント"
  [pred coll]
  (let [counter (atom 0)]
    (dorun
     (pmap
      (fn [x]
        (when (pred x)
          (swap! counter inc)))
      coll))
    @counter))

;; =============================================================================
;; タイムアウト付き収集
;; =============================================================================

(defn collect-for
  "指定時間だけ値を収集"
  [duration-ms producer-fn interval-ms]
  (let [collected (atom [])
        running (atom true)]
    ;; プロデューサーを起動
    (future
      (while @running
        (swap! collected conj (producer-fn))
        (Thread/sleep interval-ms)))
    ;; 指定時間待機
    (Thread/sleep duration-ms)
    ;; 停止
    (reset! running false)
    @collected))

;; =============================================================================
;; 並行マップ更新
;; =============================================================================

(defrecord Update [key value])

(defn apply-updates
  "複数の更新を並行して適用"
  [updates]
  (let [map-ref (atom {})]
    (dorun
     (pmap
      (fn [update]
        (swap! map-ref assoc (:key update) (:value update)))
      updates))
    @map-ref))

;; =============================================================================
;; Worker Pool パターン
;; =============================================================================

(defn create-worker-pool
  "ワーカープールを作成"
  [n]
  (let [executor (java.util.concurrent.Executors/newFixedThreadPool n)]
    {:executor executor
     :submit (fn [task]
               (.submit executor ^Callable task))
     :shutdown (fn []
                 (.shutdown executor))}))

(defn process-with-pool
  "ワーカープールで処理"
  [pool tasks]
  (let [futures (mapv (:submit pool) tasks)]
    (mapv #(.get %) futures)))

;; =============================================================================
;; 実践例：リアルタイムイベント処理
;; =============================================================================

(defrecord Event [type timestamp data])

(defn create-event
  "イベントを作成"
  [type data]
  (->Event type (System/currentTimeMillis) data))

(defn event-processor
  "イベントプロセッサを作成"
  []
  (let [events (atom [])
        handlers (atom {})
        running (atom true)]
    {:add-handler (fn [event-type handler]
                    (swap! handlers assoc event-type handler))
     :emit (fn [event]
             (swap! events conj event)
             (when-let [handler (get @handlers (:type event))]
               (handler event)))
     :get-events (fn [] @events)
     :stop (fn [] (reset! running false))}))

;; =============================================================================
;; 実践例：並行キャッシュ
;; =============================================================================

(defn create-cache
  "並行キャッシュを作成"
  []
  (let [cache (atom {})
        hits (atom 0)
        misses (atom 0)]
    {:get (fn [key compute-fn]
            (if-let [value (get @cache key)]
              (do (swap! hits inc) value)
              (let [value (compute-fn)]
                (swap! misses inc)
                (swap! cache assoc key value)
                value)))
     :invalidate (fn [key]
                   (swap! cache dissoc key))
     :clear (fn []
              (reset! cache {})
              (reset! hits 0)
              (reset! misses 0))
     :stats (fn []
              {:size (count @cache)
               :hits @hits
               :misses @misses})}))

