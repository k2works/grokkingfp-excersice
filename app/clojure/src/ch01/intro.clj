(ns ch01.intro
  "第1章: Clojure イントロダクション

   関数型プログラミングの基本概念と Clojure の特徴を学びます。
   - 命令型 vs 関数型
   - S式（S-expression）
   - 基本構文（def, defn, let）
   - データ構造（リスト、ベクター、マップ、セット）")

;; =============================================================================
;; 命令型 vs 関数型
;; =============================================================================

;; 命令型スタイル（Clojure でも可能だが推奨されない）
(defn imperative-sum
  "命令型スタイルで合計を計算（非推奨）"
  [numbers]
  (let [result (atom 0)]
    (doseq [n numbers]
      (swap! result + n))
    @result))

;; 関数型スタイル（推奨）
(defn functional-sum
  "関数型スタイルで合計を計算（推奨）"
  [numbers]
  (reduce + 0 numbers))

;; =============================================================================
;; 基本構文
;; =============================================================================

;; 定数定義（def）
(def greeting "Hello, Clojure!")
(def pi 3.14159)
(def languages ["Scala" "Java" "Clojure" "Haskell"])

;; 関数定義（defn）
(defn greet
  "名前を受け取って挨拶を返す"
  [name]
  (str "Hello, " name "!"))

(defn add
  "2つの数を加算"
  [a b]
  (+ a b))

;; 複数アリティ関数
(defn greet-multi
  "複数のアリティを持つ挨拶関数"
  ([] (greet-multi "World"))
  ([name] (str "Hello, " name "!"))
  ([title name] (str "Hello, " title " " name "!")))

;; =============================================================================
;; ローカル束縛（let）
;; =============================================================================

(defn calculate-area
  "円の面積を計算"
  [radius]
  (let [pi 3.14159
        r-squared (* radius radius)]
    (* pi r-squared)))

(defn format-name
  "名前をフォーマット"
  [first-name last-name]
  (let [full-name (str first-name " " last-name)
        upper-name (clojure.string/upper-case full-name)]
    {:full full-name
     :upper upper-name
     :initials (str (first first-name) "." (first last-name) ".")}))

;; =============================================================================
;; データ構造
;; =============================================================================

;; リスト（連結リスト - 先頭への追加が高速）
(def my-list '(1 2 3 4 5))

;; ベクター（インデックスアクセスが高速）
(def my-vector [1 2 3 4 5])

;; マップ（キー・値ペア）
(def person {:name "Alice"
             :age 30
             :city "Tokyo"})

;; セット（重複なし）
(def unique-numbers #{1 2 3 4 5})

;; ネストしたデータ構造
(def company
  {:name "Tech Corp"
   :employees [{:name "Alice" :role "Developer"}
               {:name "Bob" :role "Designer"}
               {:name "Carol" :role "Manager"}]
   :locations #{"Tokyo" "Osaka" "Nagoya"}})

;; =============================================================================
;; データアクセス
;; =============================================================================

(defn get-person-info
  "人物情報を取得"
  [person]
  (let [name (:name person)
        age (:age person)]
    (str name " is " age " years old")))

(defn get-employee-names
  "従業員名のリストを取得"
  [company]
  (map :name (:employees company)))

;; キーワードを関数として使用
(defn keyword-as-function-demo
  "キーワードを関数として使用するデモ"
  []
  (let [person {:name "Alice" :age 30}]
    ;; 以下は同等
    [(:name person)      ; キーワードを関数として使用
     (get person :name)  ; get 関数を使用
     (person :name)]))   ; マップを関数として使用

;; =============================================================================
;; 条件分岐
;; =============================================================================

(defn check-age
  "年齢に基づいてメッセージを返す"
  [age]
  (cond
    (< age 0) "Invalid age"
    (< age 13) "Child"
    (< age 20) "Teenager"
    (< age 65) "Adult"
    :else "Senior"))

(defn describe-number
  "数値を説明"
  [n]
  (cond
    (zero? n) "zero"
    (pos? n) "positive"
    (neg? n) "negative"))

;; if-let と when-let
(defn greet-optional
  "オプショナルな名前で挨拶"
  [person]
  (if-let [name (:name person)]
    (str "Hello, " name "!")
    "Hello, stranger!"))

;; =============================================================================
;; ループと再帰
;; =============================================================================

;; loop/recur による末尾再帰
(defn factorial
  "階乗を計算（末尾再帰）"
  [n]
  (loop [i n
         acc 1]
    (if (<= i 1)
      acc
      (recur (dec i) (* acc i)))))

;; reduce による集計
(defn sum-of-squares
  "平方和を計算"
  [numbers]
  (reduce (fn [acc n] (+ acc (* n n))) 0 numbers))

;; =============================================================================
;; 実践例：ショッピングカート
;; =============================================================================

(def sample-cart
  [{:name "Apple" :price 100 :quantity 3}
   {:name "Banana" :price 80 :quantity 5}
   {:name "Orange" :price 120 :quantity 2}])

(defn item-total
  "アイテムの小計を計算"
  [item]
  (* (:price item) (:quantity item)))

(defn cart-total
  "カートの合計を計算"
  [cart]
  (reduce + (map item-total cart)))

(defn apply-discount
  "割引を適用"
  [total discount-rate]
  (let [discount (* total discount-rate)]
    (- total discount)))

(defn checkout
  "チェックアウト処理"
  [cart discount-rate]
  (let [subtotal (cart-total cart)
        total (apply-discount subtotal discount-rate)]
    {:subtotal subtotal
     :discount-rate discount-rate
     :discount (* subtotal discount-rate)
     :total total}))
