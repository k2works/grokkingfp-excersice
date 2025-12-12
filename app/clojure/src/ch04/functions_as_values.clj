(ns ch04.functions-as-values
  "第4章: 関数を値として扱う

   高階関数と関数合成を学びます。
   - 関数を引数として渡す
   - 関数を返す関数
   - map, filter, reduce
   - 部分適用とカリー化")

;; =============================================================================
;; 高階関数とは
;; =============================================================================

;; 高階関数は以下のいずれかを満たす：
;; 1. 関数を引数として受け取る
;; 2. 関数を戻り値として返す

;; Clojure では関数はファーストクラスの値

;; =============================================================================
;; map - 各要素を変換
;; =============================================================================

(defn len
  "文字列の長さを返す"
  [s]
  (count s))

(defn double-value
  "値を2倍にする"
  [n]
  (* 2 n))

(defn demonstrate-map
  "map のデモ"
  []
  {:lengths (map len ["scala" "rust" "ada"])
   :doubled (map double-value [5 1 2 4 0])
   ;; 無名関数を使う
   :squared (map #(* % %) [1 2 3 4 5])})

;; =============================================================================
;; filter - 条件に合う要素を抽出
;; =============================================================================

(defn odd-number?
  "奇数かどうか"
  [n]
  (odd? n))

(defn larger-than-4?
  "4より大きいか"
  [n]
  (> n 4))

(defn demonstrate-filter
  "filter のデモ"
  []
  {:odds (filter odd-number? [5 1 2 4 0])
   :large (filter larger-than-4? [5 1 2 4 0])
   ;; 無名関数を使う
   :even (filter even? [1 2 3 4 5 6])})

;; =============================================================================
;; reduce - 畳み込み
;; =============================================================================

(defn sum-all
  "合計を計算"
  [numbers]
  (reduce + 0 numbers))

(defn find-max
  "最大値を見つける"
  [numbers]
  (reduce (fn [max-val n]
            (if (> n max-val) n max-val))
          Integer/MIN_VALUE
          numbers))

(defn find-min
  "最小値を見つける"
  [numbers]
  (reduce (fn [min-val n]
            (if (< n min-val) n min-val))
          Integer/MAX_VALUE
          numbers))

(defn product
  "積を計算"
  [numbers]
  (reduce * 1 numbers))

(defn demonstrate-reduce
  "reduce のデモ"
  []
  {:sum (sum-all [5 1 2 4 100])
   :max (find-max [5 1 2 4 15])
   :min (find-min [5 1 2 4 15])
   :product (product [1 2 3 4 5])})

;; =============================================================================
;; sort-by - ソート基準を関数で指定
;; =============================================================================

(defn score
  "単語のスコアを計算（'a' を除いた文字数）"
  [word]
  (count (clojure.string/replace word "a" "")))

(defn sort-by-score
  "スコアでソート"
  [words]
  (sort-by score words))

(defn sort-by-length
  "長さでソート"
  [words]
  (sort-by count words))

;; =============================================================================
;; 関数を返す関数
;; =============================================================================

(defn larger-than
  "n より大きいか判定する関数を返す"
  [n]
  (fn [i] (> i n)))

(defn smaller-than
  "n より小さいか判定する関数を返す"
  [n]
  (fn [i] (< i n)))

(defn divisible-by
  "n で割り切れるか判定する関数を返す"
  [n]
  (fn [i] (zero? (mod i n))))

(defn between
  "min と max の間か判定する関数を返す"
  [min-val max-val]
  (fn [i] (and (>= i min-val) (<= i max-val))))

(defn demonstrate-function-returning
  "関数を返す関数のデモ"
  []
  {:larger-than-4 (filter (larger-than 4) [5 1 2 4 0])
   :larger-than-1 (filter (larger-than 1) [5 1 2 4 0])
   :divisible-by-2 (filter (divisible-by 2) [1 2 3 4 5 6])
   :between-2-and-5 (filter (between 2 5) [1 2 3 4 5 6])})

;; =============================================================================
;; 部分適用（partial）
;; =============================================================================

(def add-10 (partial + 10))
(def multiply-by-2 (partial * 2))
(def prepend-hello (partial str "Hello, "))

(defn demonstrate-partial
  "partial のデモ"
  []
  {:add-10 (add-10 5)
   :multiply-by-2 (multiply-by-2 5)
   :greeting (prepend-hello "World")
   :mapped (map (partial + 100) [1 2 3])})

;; =============================================================================
;; 関数合成（comp）
;; =============================================================================

;; comp は右から左に関数を適用
(def process-number
  "数を2倍して10を足す"
  (comp (partial + 10) (partial * 2)))

(def normalize-name
  "名前を正規化"
  (comp clojure.string/capitalize
        clojure.string/trim
        clojure.string/lower-case))

(defn demonstrate-comp
  "comp のデモ"
  []
  {:processed (process-number 5)  ; (* 2 5) = 10, (+ 10 10) = 20
   :normalized (normalize-name "  JOHN DOE  ")})

;; =============================================================================
;; スレッディングマクロ
;; =============================================================================

;; -> (thread-first): 前の結果を最初の引数として挿入
;; ->> (thread-last): 前の結果を最後の引数として挿入

(defn process-with-threading
  "スレッディングマクロのデモ"
  [numbers]
  (->> numbers
       (filter odd?)
       (map #(* % %))
       (reduce +)))

(defn string-processing
  "文字列処理のデモ"
  [s]
  (-> s
      clojure.string/trim
      clojure.string/lower-case
      (clojure.string/replace #"\s+" "-")))

;; =============================================================================
;; 実践例：ワードスコアリング
;; =============================================================================

(defn bonus
  "ボーナスを計算（'c' を含むと +5）"
  [word]
  (if (clojure.string/includes? word "c") 5 0))

(defn penalty
  "ペナルティを計算（'s' を含むと -7）"
  [word]
  (if (clojure.string/includes? word "s") 7 0))

(defn word-score
  "単語の総合スコアを計算"
  [word]
  (+ (score word) (bonus word) (- (penalty word))))

(defn ranked-words
  "スコア順にランキング"
  [word-scorer words]
  (reverse (sort-by word-scorer words)))

(defn demonstrate-word-scoring
  "ワードスコアリングのデモ"
  []
  (let [words ["ada" "haskell" "scala" "java" "rust"]]
    {:by-score (ranked-words score words)
     :with-bonus (ranked-words #(+ (score %) (bonus %)) words)
     :with-all (ranked-words word-score words)}))

;; =============================================================================
;; 実践例：プログラミング言語
;; =============================================================================

(def languages
  [{:name "Java" :year 1995 :paradigm :oop}
   {:name "Scala" :year 2004 :paradigm :multi}
   {:name "Clojure" :year 2007 :paradigm :functional}
   {:name "Haskell" :year 1990 :paradigm :functional}
   {:name "Rust" :year 2010 :paradigm :multi}])

(defn languages-after
  "指定年より後に作られた言語"
  [year]
  (filter #(> (:year %) year) languages))

(defn language-names
  "言語名のリストを取得"
  [langs]
  (map :name langs))

(defn functional-languages
  "関数型言語のみを取得"
  []
  (filter #(= (:paradigm %) :functional) languages))

(defn average-year
  "平均作成年を計算"
  [langs]
  (if (empty? langs)
    0
    (/ (reduce + (map :year langs)) (count langs))))

;; =============================================================================
;; juxt - 複数の関数を同時に適用
;; =============================================================================

(def min-max-sum
  "最小、最大、合計を同時に計算"
  (juxt #(apply min %) #(apply max %) #(reduce + %)))

(defn demonstrate-juxt
  "juxt のデモ"
  []
  {:stats (min-max-sum [3 1 4 1 5 9 2 6])
   :person-info ((juxt :name :age) {:name "Alice" :age 30 :city "Tokyo"})})

;; =============================================================================
;; every? / some / not-any? / not-every?
;; =============================================================================

(defn demonstrate-predicates
  "述語関数のデモ"
  []
  {:all-positive? (every? pos? [1 2 3 4 5])
   :any-negative? (some neg? [1 2 -3 4 5])
   :no-zeros? (not-any? zero? [1 2 3 4 5])
   :not-all-even? (not-every? even? [2 4 5 6])})

;; =============================================================================
;; group-by
;; =============================================================================

(defn group-by-paradigm
  "パラダイムでグループ化"
  []
  (group-by :paradigm languages))

(defn group-numbers-by-parity
  "偶奇でグループ化"
  [numbers]
  (group-by #(if (even? %) :even :odd) numbers))
