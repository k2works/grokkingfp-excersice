(ns ch08.state-management
  "第8章: 状態管理と副作用

   Clojure における状態管理と副作用の扱いを学びます。
   - atom による状態管理
   - delay と force による遅延評価
   - future による非同期処理
   - 副作用の分離")

;; =============================================================================
;; 副作用の問題
;; =============================================================================

;; 不純な関数（副作用あり）
(defn cast-the-die-impure!
  "サイコロを振る（不純）"
  []
  (inc (rand-int 6)))

;; 呼び出すたびに異なる値が返る
;; (cast-the-die-impure!) ; => 3
;; (cast-the-die-impure!) ; => 5
;; (cast-the-die-impure!) ; => 1

;; =============================================================================
;; delay による遅延評価
;; =============================================================================

;; delay は副作用を遅延させる（Scala の IO.delay に相当）
(defn cast-the-die
  "サイコロを振る（遅延）"
  []
  (delay (cast-the-die-impure!)))

;; delay を作成しただけでは実行されない
;; (def die-cast (cast-the-die))
;; die-cast ; => #<Delay@...>

;; force で実行
;; (force die-cast) ; => 4

;; 注意: delay は一度だけ評価され、結果がキャッシュされる
;; 毎回実行するには新しい delay を作成する必要がある

;; =============================================================================
;; fn による副作用のラッピング
;; =============================================================================

;; 毎回実行するには関数でラップ
(defn make-die-caster
  "サイコロを振る関数を返す"
  []
  (fn [] (cast-the-die-impure!)))

(defn cast-die-twice
  "サイコロを2回振って合計を返す"
  []
  (let [caster (make-die-caster)]
    (+ (caster) (caster))))

;; =============================================================================
;; atom による状態管理
;; =============================================================================

;; atom は変更可能な参照（Scala の Ref に相当）
(def counter (atom 0))

(defn increment-counter!
  "カウンターを増加"
  []
  (swap! counter inc))

(defn get-counter
  "カウンターの値を取得"
  []
  @counter)

(defn reset-counter!
  "カウンターをリセット"
  []
  (reset! counter 0))

;; swap! は関数を適用して新しい値を設定
;; reset! は新しい値を直接設定

;; =============================================================================
;; atom の高度な操作
;; =============================================================================

(defn swap-with-old!
  "古い値を返しながら swap"
  [atom f]
  (let [old-val @atom]
    (swap! atom f)
    old-val))

;; compare-and-set! は期待値と一致する場合のみ更新
(defn compare-and-swap!
  "比較して一致すれば更新"
  [atom expected new-val]
  (compare-and-set! atom expected new-val))

;; =============================================================================
;; 複数の atom を使った状態管理
;; =============================================================================

(def users (atom {}))
(def sessions (atom {}))

(defn add-user!
  "ユーザーを追加"
  [id name email]
  (swap! users assoc id {:name name :email email}))

(defn get-user
  "ユーザーを取得"
  [id]
  (get @users id))

(defn create-session!
  "セッションを作成"
  [user-id]
  (let [session-id (str (java.util.UUID/randomUUID))]
    (swap! sessions assoc session-id {:user-id user-id
                                       :created-at (System/currentTimeMillis)})
    session-id))

(defn get-session
  "セッションを取得"
  [session-id]
  (get @sessions session-id))

;; =============================================================================
;; サイコロゲームの例
;; =============================================================================

(def game-state (atom {:rolls [] :total 0}))

(defn roll-die!
  "サイコロを振って状態を更新"
  []
  (let [roll (cast-the-die-impure!)]
    (swap! game-state
           (fn [state]
             (-> state
                 (update :rolls conj roll)
                 (update :total + roll))))
    roll))

(defn get-game-state
  "ゲーム状態を取得"
  []
  @game-state)

(defn reset-game!
  "ゲームをリセット"
  []
  (reset! game-state {:rolls [] :total 0}))

(defn play-rounds!
  "n 回サイコロを振る"
  [n]
  (reset-game!)
  (dotimes [_ n]
    (roll-die!))
  (get-game-state))

;; =============================================================================
;; future による非同期処理
;; =============================================================================

(defn async-api-call
  "非同期 API 呼び出しをシミュレート"
  [name delay-ms]
  (future
    (Thread/sleep delay-ms)
    {:name name :result (str "Result for " name)}))

(defn combine-results
  "複数の future の結果を結合"
  [futures]
  (mapv deref futures))

(defn parallel-api-calls
  "並列で API を呼び出す"
  [names]
  (let [futures (mapv #(async-api-call % 100) names)]
    (combine-results futures)))

;; =============================================================================
;; promise による同期
;; =============================================================================

(defn create-promise
  "promise を作成"
  []
  (promise))

(defn deliver-value!
  "promise に値を設定"
  [p value]
  (deliver p value))

(defn get-promise-value
  "promise の値を取得（ブロック）"
  [p]
  @p)

(defn get-promise-value-with-timeout
  "promise の値を取得（タイムアウト付き）"
  [p timeout-ms default]
  (deref p timeout-ms default))

;; =============================================================================
;; ミーティングスケジューリングの例
;; =============================================================================

(defn meeting-time
  "ミーティング時間を作成"
  [start-hour end-hour]
  {:start-hour start-hour :end-hour end-hour})

(defn meetings-overlap?
  "2つのミーティングが重複するか"
  [m1 m2]
  (not (or (>= (:start-hour m1) (:end-hour m2))
           (>= (:start-hour m2) (:end-hour m1)))))

;; シミュレートされたカレンダー API
(def calendar-db
  (atom {"Alice" [(meeting-time 9 10) (meeting-time 14 15)]
         "Bob" [(meeting-time 10 11) (meeting-time 15 16)]
         "Carol" [(meeting-time 9 11) (meeting-time 13 14)]}))

(defn calendar-entries
  "カレンダーエントリを取得（副作用をシミュレート）"
  [name]
  (Thread/sleep 10)  ; API 遅延をシミュレート
  (get @calendar-db name []))

(defn scheduled-meetings
  "2人の予定を取得"
  [person1 person2]
  (concat (calendar-entries person1)
          (calendar-entries person2)))

(defn possible-meetings
  "空き時間を計算（純粋関数）"
  [existing-meetings start-hour end-hour length-hours]
  (let [slots (for [start (range start-hour (- end-hour length-hours -1))]
                (meeting-time start (+ start length-hours)))]
    (filter (fn [slot]
              (not-any? #(meetings-overlap? % slot) existing-meetings))
            slots)))

(defn find-meeting-slots
  "2人の空き時間を見つける"
  [person1 person2 start-hour end-hour length-hours]
  (let [meetings (scheduled-meetings person1 person2)]
    (possible-meetings meetings start-hour end-hour length-hours)))

;; =============================================================================
;; リトライ戦略
;; =============================================================================

(defn retry
  "アクションを n 回リトライ"
  [action max-retries]
  (loop [attempts 0]
    (let [result (try
                   {:ok (action)}
                   (catch Exception e
                     {:error (.getMessage e)}))]
      (if (or (:ok result) (>= attempts max-retries))
        result
        (recur (inc attempts))))))

(defn retry-with-delay
  "遅延付きでリトライ"
  [action max-retries delay-ms]
  (loop [attempts 0]
    (let [result (try
                   {:ok (action)}
                   (catch Exception e
                     {:error (.getMessage e)}))]
      (if (or (:ok result) (>= attempts max-retries))
        result
        (do
          (Thread/sleep delay-ms)
          (recur (inc attempts)))))))

(defn retry-with-default
  "リトライして失敗したらデフォルト値"
  [action max-retries default]
  (let [result (retry action max-retries)]
    (if (:ok result)
      (:ok result)
      default)))

;; =============================================================================
;; 副作用の分離パターン
;; =============================================================================

;; 純粋なコアロジック
(defn calculate-discount
  "割引を計算（純粋関数）"
  [price discount-rate]
  (* price (- 1 discount-rate)))

(defn calculate-total
  "合計を計算（純粋関数）"
  [items]
  (reduce + (map :price items)))

;; 副作用を持つシェル
(def order-db (atom {}))

(defn save-order!
  "注文を保存（副作用）"
  [order-id order]
  (swap! order-db assoc order-id order)
  order-id)

(defn get-order
  "注文を取得（副作用）"
  [order-id]
  (get @order-db order-id))

(defn process-order!
  "注文を処理（副作用を分離）"
  [order-id items discount-rate]
  (let [subtotal (calculate-total items)          ; 純粋
        total (calculate-discount subtotal discount-rate)]  ; 純粋
    (save-order! order-id {:items items          ; 副作用
                           :subtotal subtotal
                           :discount-rate discount-rate
                           :total total})))

;; =============================================================================
;; do-sync パターン
;; =============================================================================

(defmacro do-sync
  "副作用を順序付けて実行"
  [& forms]
  `(do ~@forms))

(defn example-do-sync
  "do-sync の使用例"
  []
  (do-sync
    (println "Step 1: Starting...")
    (Thread/sleep 100)
    (println "Step 2: Processing...")
    (Thread/sleep 100)
    (println "Step 3: Done!")
    :completed))
