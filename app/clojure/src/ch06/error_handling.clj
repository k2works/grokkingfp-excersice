(ns ch06.error-handling
  "第6章: エラーハンドリング

   Clojure における安全なエラーハンドリングを学びます。
   - nil の扱い
   - some-> と some->> マクロ
   - 結果型パターン
   - TV番組パースの例")

;; =============================================================================
;; nil の基本
;; =============================================================================

;; Clojure では nil が「値がない」ことを表す
;; Scala の Option に相当する機能は、nil チェックと some-> マクロで実現

(defn some-value?
  "nil でないかチェック"
  [x]
  (some? x))

(defn nil-value?
  "nil かチェック"
  [x]
  (nil? x))

;; =============================================================================
;; 安全な除算
;; =============================================================================

(defn safe-divide
  "ゼロ除算を防ぐ安全な除算"
  [a b]
  (when-not (zero? b)
    (/ a b)))

(defn safe-sqrt
  "負の数のルートを防ぐ安全な平方根"
  [n]
  (when (>= n 0)
    (Math/sqrt n)))

;; =============================================================================
;; some-> と some->> マクロ
;; =============================================================================

;; some-> は途中で nil になったら処理を中断する
;; Scala の Option.flatMap のチェーンに相当

(defn safe-process-number
  "some-> を使った安全な数値処理"
  [n]
  (some-> n
          (when (number? n) n)
          (* 2)
          (+ 10)))

(defn get-nested-value
  "ネストしたマップから安全に値を取得"
  [data]
  (some-> data
          :user
          :profile
          :name))

;; some->> は最後の引数位置に挿入
(defn safe-process-list
  "some->> を使った安全なリスト処理"
  [coll]
  (some->> coll
           (filter pos?)
           seq
           (reduce +)))

;; =============================================================================
;; TV番組パースの例
;; =============================================================================

;; TV番組のデータ構造
(defn tv-show
  "TV番組を作成"
  [title start end]
  {:title title :start start :end end})

;; 文字列パース用ヘルパー
(defn parse-int
  "文字列を整数にパース（失敗時は nil）"
  [s]
  (try
    (Integer/parseInt (clojure.string/trim s))
    (catch Exception _ nil)))

(defn index-of
  "文字列内の文字のインデックスを返す（見つからない場合は nil）"
  [s ch]
  (let [idx (.indexOf s (str ch))]
    (when (>= idx 0) idx)))

;; =============================================================================
;; 名前抽出
;; =============================================================================

(defn extract-name
  "TV番組文字列から名前を抽出
   例: 'Breaking Bad (2008-2013)' -> 'Breaking Bad'"
  [raw-show]
  (when-let [bracket-open (index-of raw-show \()]
    (when (pos? bracket-open)
      (clojure.string/trim (subs raw-show 0 bracket-open)))))

;; =============================================================================
;; 開始年抽出
;; =============================================================================

(defn extract-year-start
  "TV番組文字列から開始年を抽出
   例: 'Breaking Bad (2008-2013)' -> 2008"
  [raw-show]
  (let [bracket-open (index-of raw-show \()
        dash (index-of raw-show \-)]
    (when (and bracket-open dash (> dash (inc bracket-open)))
      (parse-int (subs raw-show (inc bracket-open) dash)))))

;; =============================================================================
;; 終了年抽出
;; =============================================================================

(defn extract-year-end
  "TV番組文字列から終了年を抽出
   例: 'Breaking Bad (2008-2013)' -> 2013"
  [raw-show]
  (let [dash (index-of raw-show \-)
        bracket-close (index-of raw-show \))]
    (when (and dash bracket-close (> bracket-close (inc dash)))
      (parse-int (subs raw-show (inc dash) bracket-close)))))

;; =============================================================================
;; 単年抽出
;; =============================================================================

(defn extract-single-year
  "単年の番組から年を抽出
   例: 'Chernobyl (2019)' -> 2019"
  [raw-show]
  (let [dash (index-of raw-show \-)
        bracket-open (index-of raw-show \()
        bracket-close (index-of raw-show \))]
    (when (and (nil? dash) bracket-open bracket-close (> bracket-close (inc bracket-open)))
      (parse-int (subs raw-show (inc bracket-open) bracket-close)))))

;; =============================================================================
;; TV番組パース
;; =============================================================================

(defn parse-show
  "TV番組文字列をパース"
  [raw-show]
  (when-let [name (extract-name raw-show)]
    (when-let [year-start (or (extract-year-start raw-show)
                              (extract-single-year raw-show))]
      (when-let [year-end (or (extract-year-end raw-show)
                              (extract-single-year raw-show))]
        (tv-show name year-start year-end)))))

;; =============================================================================
;; エラーハンドリング戦略
;; =============================================================================

;; Best-effort 戦略: パースできたものだけ返す
(defn parse-shows-best-effort
  "パースできたものだけ返す"
  [raw-shows]
  (keep parse-show raw-shows))

;; All-or-nothing 戦略: 全部成功するか、全部失敗
(defn parse-shows-all-or-nothing
  "全部成功するか nil"
  [raw-shows]
  (let [parsed (map parse-show raw-shows)]
    (when (every? some? parsed)
      (vec parsed))))

;; =============================================================================
;; or によるフォールバック
;; =============================================================================

(defn get-with-default
  "値を取得、なければデフォルト値"
  [value default]
  (or value default))

(defn find-first-valid
  "最初の有効な値を見つける"
  [& values]
  (first (filter some? values)))

;; =============================================================================
;; if-let と when-let
;; =============================================================================

(defn greet-user
  "ユーザーに挨拶（名前がない場合はゲスト）"
  [user]
  (if-let [name (:name user)]
    (str "Hello, " name "!")
    "Hello, Guest!"))

(defn process-order
  "注文を処理（有効な場合のみ）"
  [order]
  (when-let [items (:items order)]
    (when (seq items)
      {:order-id (:id order)
       :total (reduce + (map :price items))})))

;; =============================================================================
;; fnil - nil に対するデフォルト値
;; =============================================================================

(def safe-inc
  "nil を 0 として扱う inc"
  (fnil inc 0))

(def safe-conj
  "nil を空ベクターとして扱う conj"
  (fnil conj []))

(defn count-occurrences
  "出現回数をカウント"
  [coll]
  (reduce (fn [acc item]
            (update acc item (fnil inc 0)))
          {}
          coll))

;; =============================================================================
;; 実践例：ユーザープロファイル
;; =============================================================================

(def users
  [{:id 1 :name "Alice" :email "alice@example.com" :age 30}
   {:id 2 :name "Bob" :email nil :age 25}
   {:id 3 :name "Carol" :email "carol@test.com" :age nil}])

(defn get-user-email
  "ユーザーのメールを取得（なければデフォルト）"
  [user]
  (or (:email user) "no-email@example.com"))

(defn get-adult-users
  "成人ユーザーを取得（年齢が設定されているもののみ）"
  [users]
  (filter (fn [user]
            (when-let [age (:age user)]
              (>= age 18)))
          users))

(defn user-summary
  "ユーザーサマリを作成"
  [user]
  (let [name (or (:name user) "Unknown")
        email (or (:email user) "N/A")
        age (or (:age user) "N/A")]
    (str name " (" email ") - Age: " age)))

;; =============================================================================
;; 実践例：設定の読み込み
;; =============================================================================

(def default-config
  {:host "localhost"
   :port 8080
   :timeout 30000
   :max-connections 100})

(defn load-config
  "設定を読み込み、デフォルト値でマージ"
  [user-config]
  (merge default-config user-config))

(defn get-config-value
  "設定値を取得（ネストした値も対応）"
  [config & keys]
  (get-in config keys))

(defn validate-config
  "設定を検証"
  [config]
  (and (some? (:host config))
       (some? (:port config))
       (pos? (:port config))
       (pos? (:timeout config))))

;; =============================================================================
;; not-empty と seq
;; =============================================================================

(defn process-items
  "アイテムを処理（空でない場合のみ）"
  [items]
  (when-let [non-empty (not-empty items)]
    {:count (count non-empty)
     :first (first non-empty)
     :last (last non-empty)}))

(defn safe-first
  "安全な first（空コレクションも対応）"
  [coll]
  (first (seq coll)))

(defn safe-rest
  "安全な rest（空コレクションも対応）"
  [coll]
  (rest (seq coll)))
