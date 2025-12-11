(ns ch02.pure-functions
  "第2章: 純粋関数

   純粋関数の概念と利点を学びます。
   - 純粋関数の定義
   - 参照透過性
   - 副作用の排除
   - テストの容易さ")

;; =============================================================================
;; 純粋関数とは
;; =============================================================================

;; 純粋関数の条件:
;; 1. 同じ入力に対して常に同じ出力を返す
;; 2. 副作用がない（外部状態を変更しない）

;; 純粋関数の例
(defn add
  "純粋関数: 2つの数を加算"
  [a b]
  (+ a b))

(defn multiply
  "純粋関数: 2つの数を乗算"
  [a b]
  (* a b))

(defn square
  "純粋関数: 数を2乗"
  [n]
  (* n n))

;; =============================================================================
;; 参照透過性
;; =============================================================================

;; 参照透過性: 式をその結果で置き換えても動作が変わらない

(defn referential-transparency-demo
  "参照透過性のデモ"
  []
  (let [x (+ 1 2)
        y (* x 3)
        z (+ x y)]
    ;; 以下の2つは同じ結果
    {:using-names z
     :using-values (+ 3 9)})) ; x=3, y=9 を直接使用

;; =============================================================================
;; 不純な関数（副作用あり）
;; =============================================================================

;; 不純な関数の例（避けるべき）

;; グローバル状態を参照
(def ^:dynamic *tax-rate* 0.1)

(defn impure-calculate-tax
  "不純な関数: グローバル状態に依存"
  [amount]
  (* amount *tax-rate*))

;; 純粋なバージョン
(defn pure-calculate-tax
  "純粋関数: 税率を引数として受け取る"
  [amount tax-rate]
  (* amount tax-rate))

;; 現在時刻に依存（不純）
(defn impure-greeting
  "不純な関数: 現在時刻に依存"
  [name]
  (let [hour (.getHour (java.time.LocalTime/now))]
    (str (cond
           (< hour 12) "Good morning"
           (< hour 18) "Good afternoon"
           :else "Good evening")
         ", " name "!")))

;; 純粋なバージョン
(defn pure-greeting
  "純粋関数: 時間を引数として受け取る"
  [name hour]
  (str (cond
         (< hour 12) "Good morning"
         (< hour 18) "Good afternoon"
         :else "Good evening")
       ", " name "!"))

;; =============================================================================
;; 副作用の分離
;; =============================================================================

;; 不純な部分を分離するパターン

(defn get-current-hour
  "副作用: 現在時刻を取得"
  []
  (.getHour (java.time.LocalTime/now)))

(defn create-greeting-with-time
  "純粋なコアロジック + 副作用を持つラッパー"
  [name]
  (let [hour (get-current-hour)]
    (pure-greeting name hour)))

;; =============================================================================
;; 高階関数と純粋性
;; =============================================================================

(defn transform-all
  "純粋な高階関数: 全要素を変換"
  [f coll]
  (map f coll))

(defn filter-by
  "純粋な高階関数: 条件で絞り込み"
  [pred coll]
  (filter pred coll))

(defn reduce-with
  "純粋な高階関数: 集約"
  [f init coll]
  (reduce f init coll))

;; =============================================================================
;; 実践例: 給与計算システム
;; =============================================================================

(def tax-rates
  {:income 0.20
   :social 0.10
   :health 0.05})

(defn calculate-gross-salary
  "純粋関数: 総支給額を計算"
  [base-salary overtime-hours hourly-rate]
  (+ base-salary (* overtime-hours hourly-rate)))

(defn calculate-deductions
  "純粋関数: 控除額を計算"
  [gross-salary rates]
  (let [income-tax (* gross-salary (:income rates))
        social (* gross-salary (:social rates))
        health (* gross-salary (:health rates))]
    {:income-tax income-tax
     :social social
     :health health
     :total (+ income-tax social health)}))

(defn calculate-net-salary
  "純粋関数: 手取りを計算"
  [gross-salary deductions]
  (- gross-salary (:total deductions)))

(defn process-payroll
  "純粋関数: 給与計算全体"
  [employee rates]
  (let [gross (calculate-gross-salary
               (:base-salary employee)
               (:overtime-hours employee 0)
               (:hourly-rate employee 0))
        deductions (calculate-deductions gross rates)
        net (calculate-net-salary gross deductions)]
    {:employee-id (:id employee)
     :gross-salary gross
     :deductions deductions
     :net-salary net}))

;; =============================================================================
;; 実践例: 文字列処理
;; =============================================================================

(defn normalize-string
  "純粋関数: 文字列を正規化"
  [s]
  (-> s
      clojure.string/trim
      clojure.string/lower-case))

(defn count-words
  "純粋関数: 単語数をカウント"
  [s]
  (if (clojure.string/blank? s)
    0
    (count (clojure.string/split (clojure.string/trim s) #"\s+"))))

(defn extract-words
  "純粋関数: 単語を抽出"
  [s]
  (if (clojure.string/blank? s)
    []
    (clojure.string/split (normalize-string s) #"\s+")))

(defn word-frequencies
  "純粋関数: 単語頻度を計算"
  [s]
  (frequencies (extract-words s)))

;; =============================================================================
;; 実践例: データバリデーション
;; =============================================================================

(defn validate-not-empty
  "純粋関数: 空でないことを検証"
  [field-name value]
  (if (clojure.string/blank? value)
    {:valid false :error (str field-name " cannot be empty")}
    {:valid true :value value}))

(defn validate-min-length
  "純粋関数: 最小長を検証"
  [field-name min-len value]
  (if (< (count value) min-len)
    {:valid false :error (str field-name " must be at least " min-len " characters")}
    {:valid true :value value}))

(defn validate-email-format
  "純粋関数: メールアドレス形式を検証"
  [email]
  (if (re-matches #".+@.+\..+" email)
    {:valid true :value email}
    {:valid false :error "Invalid email format"}))

(defn validate-age
  "純粋関数: 年齢を検証"
  [age]
  (cond
    (not (integer? age)) {:valid false :error "Age must be an integer"}
    (neg? age) {:valid false :error "Age cannot be negative"}
    (> age 150) {:valid false :error "Age seems unrealistic"}
    :else {:valid true :value age}))

(defn validate-user
  "純粋関数: ユーザー情報を検証"
  [user]
  (let [name-result (validate-not-empty "Name" (:name user ""))
        email-result (validate-email-format (:email user ""))
        age-result (validate-age (:age user))]
    (if (and (:valid name-result)
             (:valid email-result)
             (:valid age-result))
      {:valid true :user user}
      {:valid false
       :errors (remove nil?
                       [(when-not (:valid name-result) (:error name-result))
                        (when-not (:valid email-result) (:error email-result))
                        (when-not (:valid age-result) (:error age-result))])})))

;; =============================================================================
;; 実践例: リスト処理
;; =============================================================================

(defn double-all
  "純粋関数: 全要素を2倍"
  [numbers]
  (map #(* 2 %) numbers))

(defn keep-even
  "純粋関数: 偶数のみ保持"
  [numbers]
  (filter even? numbers))

(defn sum-all
  "純粋関数: 合計"
  [numbers]
  (reduce + 0 numbers))

(defn average
  "純粋関数: 平均"
  [numbers]
  (if (empty? numbers)
    0
    (/ (sum-all numbers) (count numbers))))

(defn statistics
  "純粋関数: 統計情報を計算"
  [numbers]
  (if (empty? numbers)
    {:count 0 :sum 0 :average 0 :min nil :max nil}
    {:count (count numbers)
     :sum (sum-all numbers)
     :average (average numbers)
     :min (apply min numbers)
     :max (apply max numbers)}))

;; =============================================================================
;; コンポジション
;; =============================================================================

(defn process-numbers
  "関数合成のデモ"
  [numbers]
  (->> numbers
       (filter pos?)
       (map square)
       (reduce +)))

;; comp による関数合成
(def process-string
  "comp による関数合成"
  (comp clojure.string/upper-case
        clojure.string/trim
        clojure.string/reverse))

;; partial による部分適用
(def add-ten
  "部分適用: 10を加算"
  (partial + 10))

(def multiply-by-two
  "部分適用: 2倍"
  (partial * 2))
