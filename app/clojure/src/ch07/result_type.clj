(ns ch07.result-type
  "第7章: Result 型パターンと ADT

   Clojure における Either 相当のパターンを学びます。
   - Result 型（:ok/:error マップ）
   - 代数的データ型（ADT）
   - パターンマッチング
   - 音楽アーティスト検索の例")

;; =============================================================================
;; Result 型パターン
;; =============================================================================

;; Clojure では Either の代わりに :ok/:error のマップを使うのが一般的

(defn ok
  "成功結果を作成"
  [value]
  {:status :ok :value value})

(defn error
  "エラー結果を作成"
  [message]
  {:status :error :error message})

(defn ok?
  "成功かどうかをチェック"
  [result]
  (= (:status result) :ok))

(defn error?
  "エラーかどうかをチェック"
  [result]
  (= (:status result) :error))

;; =============================================================================
;; Result の操作
;; =============================================================================

(defn result-map
  "Result の値を変換（成功時のみ）"
  [f result]
  (if (ok? result)
    (ok (f (:value result)))
    result))

(defn result-flatmap
  "Result を返す関数を適用"
  [f result]
  (if (ok? result)
    (f (:value result))
    result))

(defn result-or-else
  "エラー時に代替を使用"
  [result alternative]
  (if (ok? result)
    result
    alternative))

(defn result-get-or-else
  "値を取得、エラー時はデフォルト値"
  [result default]
  (if (ok? result)
    (:value result)
    default))

(defn result-to-option
  "Result を nil/値 に変換"
  [result]
  (when (ok? result)
    (:value result)))

;; =============================================================================
;; Result チェーン用マクロ
;; =============================================================================

(defmacro result->
  "Result をチェーンする（成功時のみ続行）"
  [expr & forms]
  (let [g (gensym)
        steps (for [form forms]
                `(if (error? ~g)
                   ~g
                   (let [~g ~(if (seq? form)
                               `(~(first form) (:value ~g) ~@(rest form))
                               `(~form (:value ~g)))]
                     ~g)))]
    `(let [~g ~expr
           ~@(interleave (repeat g) steps)]
       ~g)))

;; =============================================================================
;; TV番組パース（Result 版）
;; =============================================================================

(defn tv-show
  "TV番組を作成"
  [title start end]
  {:title title :start start :end end})

(defn parse-int-result
  "文字列を整数にパース（Result 版）"
  [s]
  (try
    (ok (Integer/parseInt (clojure.string/trim s)))
    (catch Exception _
      (error (str "Cannot parse '" s "' as integer")))))

(defn index-of
  "文字列内の文字のインデックスを返す"
  [s ch]
  (let [idx (.indexOf s (str ch))]
    (when (>= idx 0) idx)))

(defn extract-name-result
  "TV番組文字列から名前を抽出（Result 版）"
  [raw-show]
  (if-let [bracket-open (index-of raw-show \()]
    (if (pos? bracket-open)
      (ok (clojure.string/trim (subs raw-show 0 bracket-open)))
      (error (str "Can't extract name from " raw-show)))
    (error (str "Can't extract name from " raw-show))))

(defn extract-year-start-result
  "TV番組文字列から開始年を抽出（Result 版）"
  [raw-show]
  (let [bracket-open (index-of raw-show \()
        dash (index-of raw-show \-)]
    (if (and bracket-open dash (> dash (inc bracket-open)))
      (parse-int-result (subs raw-show (inc bracket-open) dash))
      (error (str "Can't extract start year from " raw-show)))))

(defn extract-year-end-result
  "TV番組文字列から終了年を抽出（Result 版）"
  [raw-show]
  (let [dash (index-of raw-show \-)
        bracket-close (index-of raw-show \))]
    (if (and dash bracket-close (> bracket-close (inc dash)))
      (parse-int-result (subs raw-show (inc dash) bracket-close))
      (error (str "Can't extract end year from " raw-show)))))

(defn extract-single-year-result
  "単年の番組から年を抽出（Result 版）"
  [raw-show]
  (let [dash (index-of raw-show \-)
        bracket-open (index-of raw-show \()
        bracket-close (index-of raw-show \))]
    (if (and (nil? dash) bracket-open bracket-close (> bracket-close (inc bracket-open)))
      (parse-int-result (subs raw-show (inc bracket-open) bracket-close))
      (error (str "Can't extract single year from " raw-show)))))

(defn parse-show-result
  "TV番組文字列をパース（Result 版）"
  [raw-show]
  (let [name-result (extract-name-result raw-show)
        year-start-result (result-or-else (extract-year-start-result raw-show)
                                          (extract-single-year-result raw-show))
        year-end-result (result-or-else (extract-year-end-result raw-show)
                                        (extract-single-year-result raw-show))]
    (if (and (ok? name-result) (ok? year-start-result) (ok? year-end-result))
      (ok (tv-show (:value name-result)
                   (:value year-start-result)
                   (:value year-end-result)))
      (cond
        (error? name-result) name-result
        (error? year-start-result) year-start-result
        :else year-end-result))))

;; =============================================================================
;; 代数的データ型（ADT）
;; =============================================================================

;; Clojure では defrecord + プロトコル、またはマップ + type タグで ADT を表現

;; 音楽ジャンル（列挙型）
(def music-genres #{:heavy-metal :pop :hard-rock :jazz :electronic})

;; 活動期間（直和型）
(defn still-active
  "現在も活動中"
  [since]
  {:type :still-active :since since})

(defn active-between
  "特定期間のみ活動"
  [start end]
  {:type :active-between :start start :end end})

;; アーティスト（直積型）
(defn artist
  "アーティストを作成"
  [name genre origin years-active]
  {:name name
   :genre genre
   :origin origin
   :years-active years-active})

;; =============================================================================
;; パターンマッチング
;; =============================================================================

(defn was-artist-active?
  "指定期間にアーティストが活動していたか"
  [artist year-start year-end]
  (let [years (:years-active artist)]
    (case (:type years)
      :still-active (<= (:since years) year-end)
      :active-between (and (<= (:start years) year-end)
                           (>= (:end years) year-start)))))

(defn active-length
  "活動年数を計算"
  [artist current-year]
  (let [years (:years-active artist)]
    (case (:type years)
      :still-active (- current-year (:since years))
      :active-between (- (:end years) (:start years)))))

;; =============================================================================
;; 検索条件のモデリング
;; =============================================================================

(defn search-by-genre
  "ジャンルで検索"
  [genres]
  {:type :by-genre :genres (set genres)})

(defn search-by-origin
  "出身地で検索"
  [locations]
  {:type :by-origin :locations (set locations)})

(defn search-by-active-years
  "活動年で検索"
  [start end]
  {:type :by-active-years :start start :end end})

(defn matches-condition?
  "アーティストが条件にマッチするか"
  [artist condition]
  (case (:type condition)
    :by-genre (contains? (:genres condition) (:genre artist))
    :by-origin (contains? (:locations condition) (:origin artist))
    :by-active-years (was-artist-active? artist (:start condition) (:end condition))))

(defn search-artists
  "条件に合うアーティストを検索"
  [artists conditions]
  (filter (fn [artist]
            (every? #(matches-condition? artist %) conditions))
          artists))

;; =============================================================================
;; サンプルデータ
;; =============================================================================

(def sample-artists
  [(artist "Metallica" :heavy-metal "U.S." (still-active 1981))
   (artist "Led Zeppelin" :hard-rock "England" (active-between 1968 1980))
   (artist "Pink Floyd" :progressive-rock "England" (active-between 1965 1995))
   (artist "Bee Gees" :pop "Australia" (active-between 1958 2003))
   (artist "Daft Punk" :electronic "France" (active-between 1993 2021))])

;; =============================================================================
;; every? / some? / not-any? / not-every?
;; =============================================================================

(defn all-active?
  "すべてのアーティストが現在活動中か"
  [artists]
  (every? #(= :still-active (get-in % [:years-active :type])) artists))

(defn any-from-origin?
  "いずれかのアーティストが指定出身地か"
  [artists origin]
  (some #(= (:origin %) origin) artists))

;; =============================================================================
;; 実践例：バリデーション
;; =============================================================================

(defn validate-not-empty
  "空でないことを検証"
  [field-name value]
  (if (and value (not (clojure.string/blank? value)))
    (ok value)
    (error (str field-name " cannot be empty"))))

(defn validate-min-length
  "最小長を検証"
  [field-name min-len value]
  (if (>= (count value) min-len)
    (ok value)
    (error (str field-name " must be at least " min-len " characters"))))

(defn validate-email
  "メールアドレス形式を検証"
  [email]
  (if (re-matches #".+@.+\..+" email)
    (ok email)
    (error "Invalid email format")))

(defn validate-age
  "年齢を検証"
  [age]
  (cond
    (not (integer? age)) (error "Age must be an integer")
    (neg? age) (error "Age cannot be negative")
    (> age 150) (error "Age seems unrealistic")
    :else (ok age)))

(defn validate-user
  "ユーザー情報を検証"
  [user]
  (let [name-result (validate-not-empty "Name" (:name user))
        email-result (validate-email (or (:email user) ""))
        age-result (validate-age (:age user))]
    (if (and (ok? name-result) (ok? email-result) (ok? age-result))
      (ok user)
      (error {:errors (remove nil?
                               [(when (error? name-result) (:error name-result))
                                (when (error? email-result) (:error email-result))
                                (when (error? age-result) (:error age-result))])}))))

;; =============================================================================
;; 複数エラーの収集
;; =============================================================================

(defn collect-errors
  "すべてのエラーを収集"
  [results]
  (let [errors (keep #(when (error? %) (:error %)) results)]
    (if (empty? errors)
      (ok (map :value results))
      (error {:errors errors}))))

(defn validate-all
  "すべてのバリデーションを実行してエラーを収集"
  [validations]
  (collect-errors validations))

;; =============================================================================
;; ex-info による例外処理
;; =============================================================================

(defn divide-or-throw
  "除算（ゼロ除算時は例外）"
  [a b]
  (if (zero? b)
    (throw (ex-info "Division by zero" {:a a :b b}))
    (/ a b)))

(defn safe-divide-with-info
  "安全な除算（Result 版、詳細情報付き）"
  [a b]
  (try
    (ok (divide-or-throw a b))
    (catch Exception e
      (error {:message (ex-message e)
              :data (ex-data e)}))))

(defn try-result
  "例外をキャッチして Result に変換"
  [f]
  (try
    (ok (f))
    (catch Exception e
      (error {:message (ex-message e)
              :data (ex-data e)}))))
