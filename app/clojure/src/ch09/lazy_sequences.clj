(ns ch09.lazy-sequences
  "第9章: 遅延シーケンスとストリーム処理

   Clojure の遅延シーケンスとストリーム処理を学びます。
   - 遅延シーケンスの基本
   - 無限シーケンス
   - トランスデューサー
   - 為替レートのトレンド検出")

;; =============================================================================
;; 遅延シーケンスの基本
;; =============================================================================

;; Clojure のシーケンスは遅延評価される
;; 必要な時に要素が生成される

(defn lazy-range
  "遅延範囲シーケンス"
  [start end]
  (lazy-seq
   (when (< start end)
     (cons start (lazy-range (inc start) end)))))

;; range は遅延シーケンス
(def numbers (range 1 11))
;; まだ評価されていない

;; take で必要な分だけ取得
(take 3 numbers)  ; => (1 2 3)

;; =============================================================================
;; 無限シーケンス
;; =============================================================================

;; repeat: 無限に同じ値を繰り返す
(def infinite-ones (repeat 1))
(take 5 infinite-ones)  ; => (1 1 1 1 1)

;; iterate: 関数を繰り返し適用
(def powers-of-2 (iterate #(* 2 %) 1))
(take 10 powers-of-2)  ; => (1 2 4 8 16 32 64 128 256 512)

;; cycle: シーケンスを無限に繰り返す
(def cycling-123 (cycle [1 2 3]))
(take 8 cycling-123)  ; => (1 2 3 1 2 3 1 2)

;; repeatedly: 関数を無限に呼び出す
(defn random-die-roll []
  (inc (rand-int 6)))

(def infinite-die-rolls (repeatedly random-die-roll))
;; (take 5 infinite-die-rolls) ; => (3 1 5 2 4)

;; =============================================================================
;; 遅延シーケンスの操作
;; =============================================================================

(defn process-stream
  "ストリームを処理"
  [stream]
  (->> stream
       (filter odd?)
       (map #(* % %))
       (take 5)))

(process-stream (range))
; => (1 9 25 49 81)

;; take-while: 条件を満たす間取得
(defn take-while-positive
  "正の値の間取得"
  [coll]
  (take-while pos? coll))

(take-while-positive [1 2 3 -1 4 5])
; => (1 2 3)

;; drop-while: 条件を満たす間スキップ
(defn skip-small-values
  "小さい値をスキップ"
  [threshold coll]
  (drop-while #(< % threshold) coll))

(skip-small-values 5 [1 2 3 4 5 6 7])
; => (5 6 7)

;; =============================================================================
;; partition と partition-all
;; =============================================================================

;; partition: 指定サイズで分割（端数は捨てる）
(partition 3 (range 10))
; => ((0 1 2) (3 4 5) (6 7 8))

;; partition-all: 端数も含める
(partition-all 3 (range 10))
; => ((0 1 2) (3 4 5) (6 7 8) (9))

;; partition with step: スライディングウィンドウ
(partition 3 1 (range 5))
; => ((0 1 2) (1 2 3) (2 3 4))

;; =============================================================================
;; スライディングウィンドウ
;; =============================================================================

(defn sliding-window
  "スライディングウィンドウを生成"
  [n coll]
  (partition n 1 coll))

(sliding-window 3 [1 2 3 4 5])
; => ((1 2 3) (2 3 4) (3 4 5))

;; =============================================================================
;; トレンド検出
;; =============================================================================

(defn trending?
  "上昇トレンドかどうか"
  [rates]
  (and (> (count rates) 1)
       (every? (fn [[prev curr]] (< prev curr))
               (partition 2 1 rates))))

(trending? [0.81 0.82 0.83])  ; => true
(trending? [0.81 0.84 0.83])  ; => false

(defn declining?
  "下降トレンドかどうか"
  [rates]
  (and (> (count rates) 1)
       (every? (fn [[prev curr]] (> prev curr))
               (partition 2 1 rates))))

(defn stable?
  "安定しているかどうか（変動が少ない）"
  [rates tolerance]
  (and (> (count rates) 1)
       (every? (fn [[prev curr]] (<= (Math/abs (- prev curr)) tolerance))
               (partition 2 1 rates))))

;; =============================================================================
;; 為替レートのストリーム
;; =============================================================================

(def exchange-rates
  "シミュレートされた為替レート"
  (atom {:USD {:EUR 0.85 :GBP 0.73 :JPY 110.0}
         :EUR {:USD 1.18 :GBP 0.86 :JPY 129.5}
         :GBP {:USD 1.37 :EUR 1.16 :JPY 150.2}}))

(defn get-rate
  "為替レートを取得"
  [from to]
  (get-in @exchange-rates [from to]))

(defn simulate-rate-change
  "レート変動をシミュレート"
  [rate]
  (let [change (- (rand) 0.5)
        factor (+ 1 (* change 0.01))]
    (* rate factor)))

(defn rate-stream
  "為替レートのストリームを生成"
  [from to]
  (let [initial-rate (get-rate from to)]
    (iterate simulate-rate-change initial-rate)))

;; =============================================================================
;; トレンドを検出して通知
;; =============================================================================

(defn find-trend
  "トレンドを検出"
  [window-size stream]
  (->> stream
       (sliding-window window-size)
       (map (fn [window]
              {:rates (vec window)
               :trending? (trending? window)
               :declining? (declining? window)}))))

(defn first-uptrend
  "最初の上昇トレンドを見つける"
  [window-size stream]
  (->> (find-trend window-size stream)
       (filter :trending?)
       first))

;; =============================================================================
;; reductions（スキャン操作）
;; =============================================================================

;; reductions は reduce の中間結果をすべて返す
(def running-sum (reductions + (range 1 6)))
; => (1 3 6 10 15)

(defn moving-average
  "移動平均を計算"
  [window-size coll]
  (->> coll
       (sliding-window window-size)
       (map #(/ (reduce + %) (count %)))))

(moving-average 3 [1 2 3 4 5 6 7 8 9 10])
; => (2 3 4 5 6 7 8 9)

;; =============================================================================
;; トランスデューサー
;; =============================================================================

;; トランスデューサーは変換を合成する効率的な方法

(def xform
  "変換を合成"
  (comp
   (filter odd?)
   (map #(* % %))
   (take 5)))

(defn process-with-transducer
  "トランスデューサーで処理"
  [coll]
  (into [] xform coll))

(process-with-transducer (range 20))
; => [1 9 25 49 81]

;; sequence でも使用可能
(defn lazy-process-with-transducer
  "トランスデューサーで遅延処理"
  [coll]
  (sequence xform coll))

;; =============================================================================
;; 実践例：ログストリーム処理
;; =============================================================================

(def log-levels #{:debug :info :warn :error})

(defn generate-log-entry
  "ログエントリを生成"
  []
  {:timestamp (System/currentTimeMillis)
   :level (rand-nth (vec log-levels))
   :message (str "Log message " (rand-int 1000))})

(def log-stream (repeatedly generate-log-entry))

(defn filter-errors
  "エラーログのみ抽出"
  [logs]
  (filter #(= :error (:level %)) logs))

(defn count-by-level
  "レベルごとにカウント"
  [logs]
  (frequencies (map :level logs)))

(defn recent-errors
  "最近のエラーを取得"
  [n logs]
  (->> logs
       filter-errors
       (take n)))

;; =============================================================================
;; 実践例：センサーデータストリーム
;; =============================================================================

(defn generate-sensor-reading
  "センサー読み取り値を生成"
  [base-value variance]
  (+ base-value (* (- (rand) 0.5) variance)))

(defn sensor-stream
  "センサーデータストリームを生成"
  [base-value variance]
  (repeatedly #(generate-sensor-reading base-value variance)))

(defn detect-anomalies
  "異常値を検出"
  [threshold stream]
  (filter #(> (Math/abs (- % 50)) threshold) stream))

(defn alert-on-anomaly
  "異常時にアラート"
  [threshold n stream]
  (->> stream
       (detect-anomalies threshold)
       (take n)
       (map (fn [value]
              {:type :anomaly
               :value value
               :timestamp (System/currentTimeMillis)}))))

;; =============================================================================
;; バッチ処理
;; =============================================================================

(defn process-in-batches
  "バッチで処理"
  [batch-size process-fn coll]
  (->> coll
       (partition-all batch-size)
       (map process-fn)))

(defn batch-sum
  "バッチごとに合計"
  [batch-size coll]
  (process-in-batches batch-size #(reduce + %) coll))

(batch-sum 3 [1 2 3 4 5 6 7 8 9 10])
; => (6 15 24 10)

;; =============================================================================
;; パイプライン処理
;; =============================================================================

(defn pipeline
  "処理パイプラインを構築"
  [& fns]
  (fn [coll]
    (reduce (fn [acc f] (f acc)) coll fns)))

(def my-pipeline
  (pipeline
   (partial filter odd?)
   (partial map #(* % 2))
   (partial take 5)))

(my-pipeline (range 20))
; => (2 6 10 14 18)

;; =============================================================================
;; eduction（効率的な変換）
;; =============================================================================

(defn efficient-process
  "eduction を使った効率的な処理"
  [coll]
  (eduction
   (filter odd?)
   (map #(* % %))
   (take 5)
   coll))

;; eduction は遅延評価で、中間コレクションを作らない
;; reduce, into, run! などと組み合わせて使用
(into [] (efficient-process (range 20)))
; => [1 9 25 49 81]
