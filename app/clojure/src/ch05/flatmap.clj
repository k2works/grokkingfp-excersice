(ns ch05.flatmap
  "第5章: mapcat とネスト構造

   Clojure における flatMap（mapcat）とシーケンス内包表記を学びます。
   - flatten と mapcat
   - ネストした変換
   - for 内包表記
   - ガード式")

;; =============================================================================
;; flatten - ネストしたコレクションを平坦化
;; =============================================================================

;; flatten は完全に平坦化する（すべてのネストを解除）
(defn demonstrate-flatten
  "flatten のデモ"
  []
  {:basic (flatten [[1 2] [3 4] [5]])
   :nested (flatten [[[1 2] [3]] [[4 5]]])
   :mixed (flatten [[1 [2 3]] [4]])})

;; apply concat は1レベルだけ平坦化
(defn flatten-one-level
  "1レベルだけ平坦化"
  [coll]
  (apply concat coll))

;; =============================================================================
;; mapcat = map + concat（Scala の flatMap に相当）
;; =============================================================================

;; mapcat は各要素に関数を適用し、結果を連結する

(defn demonstrate-mapcat
  "mapcat のデモ"
  []
  (let [books [{:title "FP in Scala" :authors ["Chiusano" "Bjarnason"]}
               {:title "The Hobbit" :authors ["Tolkien"]}]]
    ;; map だけだとネストする
    {:with-map (map :authors books)
     ;; mapcat で平坦化
     :with-mapcat (mapcat :authors books)}))

;; =============================================================================
;; mapcat によるサイズ変化
;; =============================================================================

;; 要素数が増える
(defn duplicate-each
  "各要素を複製"
  [coll]
  (mapcat (fn [x] [x x]) coll))

;; 要素数が同じ
(defn wrap-each
  "各要素をラップ"
  [coll]
  (mapcat (fn [x] [(* x 2)]) coll))

;; 要素数が減る（フィルタリング）
(defn keep-even
  "偶数のみを保持（mapcat 版）"
  [coll]
  (mapcat (fn [x] (if (even? x) [x] [])) coll))

(defn demonstrate-size-change
  "mapcat によるサイズ変化のデモ"
  []
  {:increased (duplicate-each [1 2 3])      ; [1 1 2 2 3 3]
   :same (wrap-each [1 2 3])                 ; [2 4 6]
   :decreased (keep-even [1 2 3 4 5])})      ; [2 4]

;; =============================================================================
;; ネストした mapcat
;; =============================================================================

(def books
  [{:title "FP in Scala" :authors ["Chiusano" "Bjarnason"]}
   {:title "The Hobbit" :authors ["Tolkien"]}
   {:title "Clean Code" :authors ["Martin"]}])

(defn book-adaptations
  "著者による映画化作品"
  [author]
  (case author
    "Tolkien" [{:title "An Unexpected Journey"}
               {:title "The Desolation of Smaug"}]
    []))

(defn get-recommendations-nested
  "ネストした mapcat でレコメンデーションを生成"
  []
  (mapcat
   (fn [book]
     (mapcat
      (fn [author]
        (map
         (fn [movie]
           (str "You may like " (:title movie)
                ", because you liked " author "'s " (:title book)))
         (book-adaptations author)))
      (:authors book)))
   books))

;; =============================================================================
;; for 内包表記
;; =============================================================================

;; Clojure の for は Scala の for 内包表記に相当
;; 遅延シーケンスを生成する

(defn get-recommendations-for
  "for 内包表記でレコメンデーションを生成"
  []
  (for [book books
        author (:authors book)
        movie (book-adaptations author)]
    (str "You may like " (:title movie)
         ", because you liked " author "'s " (:title book))))

;; for は :when（ガード式）と :let（ローカル束縛）をサポート

(defn even-products
  "偶数の積のみを生成"
  []
  (for [x (range 1 4)
        y (range 1 4)
        :let [product (* x y)]
        :when (even? product)]
    {:x x :y y :product product}))

;; =============================================================================
;; 円内の点の判定
;; =============================================================================

(defn inside?
  "点が円の内部にあるか判定"
  [point radius]
  (<= (+ (* (:x point) (:x point))
         (* (:y point) (:y point)))
      (* radius radius)))

(defn points-inside-circles
  "円内にある点を見つける"
  []
  (let [points [{:x 5 :y 2} {:x 1 :y 1}]
        radiuses [2 1]]
    (for [r radiuses
          point points
          :when (inside? point r)]
      {:point point :radius r})))

;; すべての組み合わせを生成
(defn all-point-radius-combinations
  "すべての点と半径の組み合わせ"
  []
  (let [points [{:x 5 :y 2} {:x 1 :y 1}]
        radiuses [2 1]]
    (for [r radiuses
          point points]
      {:point point
       :radius r
       :inside? (inside? point r)})))

;; =============================================================================
;; for 内包表記の型
;; =============================================================================

;; for は常に遅延シーケンスを返す
(defn demonstrate-for-types
  "for の戻り値型のデモ"
  []
  {:from-vector (for [x [1 2] y [3 4]] (* x y))
   :from-set (for [x #{1 2} y #{3 4}] (* x y))
   :from-list (for [x '(1 2) y '(3 4)] (* x y))})

;; into でコレクション型を変換
(defn for-with-into
  "into で型を変換"
  []
  {:as-vector (into [] (for [x [1 2] y [3 4]] (* x y)))
   :as-set (into #{} (for [x [1 2] y [3 4]] (* x y)))})

;; =============================================================================
;; 実践例：ECサイトの商品検索
;; =============================================================================

(def products
  [{:id 1 :name "Laptop" :category :electronics :price 1000
    :tags ["computer" "portable" "work"]}
   {:id 2 :name "Phone" :category :electronics :price 800
    :tags ["mobile" "portable" "communication"]}
   {:id 3 :name "Desk" :category :furniture :price 300
    :tags ["office" "work"]}
   {:id 4 :name "Chair" :category :furniture :price 200
    :tags ["office" "comfort"]}])

(def discounts
  {:electronics 0.1
   :furniture 0.2})

(defn search-products
  "条件に合う商品を検索"
  [min-price max-price categories]
  (for [product products
        :when (and (>= (:price product) min-price)
                   (<= (:price product) max-price)
                   (contains? (set categories) (:category product)))]
    product))

(defn products-with-discounts
  "割引を適用した商品リスト"
  []
  (for [product products
        :let [discount-rate (get discounts (:category product) 0)
              discounted-price (* (:price product) (- 1 discount-rate))]]
    (assoc product :discounted-price discounted-price)))

(defn product-tag-pairs
  "商品とタグのペアを生成"
  []
  (for [product products
        tag (:tags product)]
    {:product-name (:name product)
     :tag tag}))

(defn find-products-by-tag
  "タグで商品を検索"
  [search-tag]
  (for [product products
        tag (:tags product)
        :when (= tag search-tag)]
    product))

;; =============================================================================
;; 実践例：組み合わせ生成
;; =============================================================================

(defn combinations
  "n 個から k 個を選ぶ組み合わせ"
  [coll k]
  (if (zero? k)
    [[]]
    (if (empty? coll)
      []
      (let [[x & xs] coll]
        (concat
         (map #(cons x %) (combinations xs (dec k)))
         (combinations xs k))))))

(defn permutations
  "順列を生成"
  [coll]
  (if (empty? coll)
    [[]]
    (for [x coll
          rest-perm (permutations (remove #{x} coll))]
      (cons x rest-perm))))

(defn cartesian-product
  "デカルト積を生成"
  [& colls]
  (reduce
   (fn [acc coll]
     (for [a acc
           b coll]
       (conj a b)))
   [[]]
   colls))

;; =============================================================================
;; 実践例：ファイルシステムの走査
;; =============================================================================

(def file-system
  {:name "root"
   :type :directory
   :children [{:name "docs"
               :type :directory
               :children [{:name "readme.md" :type :file :size 100}
                          {:name "guide.md" :type :file :size 200}]}
              {:name "src"
               :type :directory
               :children [{:name "core.clj" :type :file :size 500}
                          {:name "utils"
                           :type :directory
                           :children [{:name "helpers.clj" :type :file :size 150}]}]}
              {:name "project.clj" :type :file :size 50}]})

(defn all-files
  "すべてのファイルを再帰的に取得"
  [node]
  (if (= (:type node) :file)
    [node]
    (mapcat all-files (:children node []))))

(defn files-with-path
  "パス付きですべてのファイルを取得"
  [node path]
  (let [current-path (str path "/" (:name node))]
    (if (= (:type node) :file)
      [{:path current-path :size (:size node)}]
      (mapcat #(files-with-path % current-path) (:children node [])))))

(defn total-size
  "総サイズを計算"
  [node]
  (reduce + (map :size (all-files node))))
