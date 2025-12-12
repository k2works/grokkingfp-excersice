(ns ch03.immutable-values
  "第3章: イミュータブルなデータ操作

   Clojure のイミュータブルデータ構造と永続データ構造を学びます。
   - 永続データ構造
   - ベクターとリストの操作
   - 文字列操作
   - 構造共有")

;; =============================================================================
;; イミュータブルとは
;; =============================================================================

;; Clojure のデータ構造はデフォルトでイミュータブル
;; 「変更」する代わりに、新しいデータを「作成」する

(def original-list [1 2 3])

;; conj で要素を追加しても元のリストは変わらない
(def new-list (conj original-list 4))

;; original-list は [1 2 3] のまま
;; new-list は [1 2 3 4]

;; =============================================================================
;; ベクターの基本操作
;; =============================================================================

;; 要素の追加（末尾）
(defn appended
  "ベクターの末尾に要素を追加"
  [coll element]
  (conj coll element))

;; 複数要素の追加
(defn appended-all
  "ベクターに複数の要素を追加"
  [coll elements]
  (into coll elements))

;; スライス操作
(defn slice
  "ベクターの一部を取り出す（start から end-1 まで）"
  [coll start end]
  (subvec (vec coll) start (min end (count coll))))

;; 最初の n 要素
(defn first-n
  "最初の n 要素を取得"
  [coll n]
  (vec (take n coll)))

;; 最後の n 要素
(defn last-n
  "最後の n 要素を取得"
  [coll n]
  (vec (take-last n coll)))

;; =============================================================================
;; リスト変換パターン
;; =============================================================================

;; 最初の2要素を末尾に移動
(defn move-first-two-to-end
  "最初の2要素を末尾に移動"
  [coll]
  (let [first-two (first-n coll 2)
        rest-items (vec (drop 2 coll))]
    (appended-all rest-items first-two)))

;; 最後の要素の前に挿入
(defn insert-before-last
  "最後の要素の前に要素を挿入"
  [coll element]
  (let [without-last (vec (butlast coll))
        last-elem (last coll)]
    (conj (conj without-last element) last-elem)))

;; 指定位置に挿入
(defn insert-at
  "指定位置に要素を挿入"
  [coll index element]
  (let [before (vec (take index coll))
        after (vec (drop index coll))]
    (into (conj before element) after)))

;; 中央に挿入
(defn insert-at-middle
  "中央に要素を挿入"
  [coll element]
  (let [middle (quot (count coll) 2)]
    (insert-at coll middle element)))

;; =============================================================================
;; 旅程の再計画
;; =============================================================================

(defn replan
  "旅程に新しい都市を挿入"
  [plan new-city before-city]
  (let [index (.indexOf (vec plan) before-city)
        cities-before (vec (take index plan))
        cities-after (vec (drop index plan))]
    (into (conj cities-before new-city) cities-after)))

;; 使用例
(def plan-a ["Paris" "Berlin" "Kraków"])
;; (replan plan-a "Vienna" "Kraków")
;; => ["Paris" "Berlin" "Vienna" "Kraków"]

;; =============================================================================
;; 文字列操作
;; =============================================================================

;; 文字列もイミュータブル

(defn abbreviate
  "名前を省略形にする"
  [name]
  (let [initial (subs name 0 1)
        separator-idx (clojure.string/index-of name " ")]
    (if separator-idx
      (str initial ". " (subs name (inc separator-idx)))
      name)))

(defn first-word
  "最初の単語を取得"
  [s]
  (first (clojure.string/split s #"\s+")))

(defn last-word
  "最後の単語を取得"
  [s]
  (last (clojure.string/split s #"\s+")))

;; =============================================================================
;; 永続データ構造と構造共有
;; =============================================================================

;; Clojure の永続データ構造は構造共有を使用
;; 新しいバージョンを作成しても、変わっていない部分は共有される

(defn demonstrate-structural-sharing
  "構造共有のデモ"
  []
  (let [original [1 2 3 4 5]
        modified (assoc original 2 :replaced)]
    ;; original と modified は同じメモリの一部を共有している
    {:original original
     :modified modified
     :same-head? (identical? (first original) (first modified))}))

;; =============================================================================
;; assoc / update / dissoc
;; =============================================================================

(defn update-person-age
  "人物の年齢を更新"
  [person new-age]
  (assoc person :age new-age))

(defn increment-age
  "年齢を1増やす"
  [person]
  (update person :age inc))

(defn remove-field
  "フィールドを削除"
  [person field]
  (dissoc person field))

;; ネストした更新
(defn update-nested
  "ネストした値を更新"
  [data]
  (update-in data [:user :profile :name] clojure.string/upper-case))

;; =============================================================================
;; 実践例：買い物リスト
;; =============================================================================

(def empty-cart [])

(defn add-item
  "アイテムをカートに追加"
  [cart item]
  (conj cart item))

(defn remove-item
  "アイテムをカートから削除（最初の一致のみ）"
  [cart item-name]
  (let [idx (.indexOf (mapv :name cart) item-name)]
    (if (>= idx 0)
      (into (subvec cart 0 idx) (subvec cart (inc idx)))
      cart)))

(defn update-quantity
  "アイテムの数量を更新"
  [cart item-name new-quantity]
  (mapv (fn [item]
          (if (= (:name item) item-name)
            (assoc item :quantity new-quantity)
            item))
        cart))

(defn cart-operations-demo
  "カート操作のデモ"
  []
  (let [cart1 (add-item empty-cart {:name "Apple" :price 100 :quantity 2})
        cart2 (add-item cart1 {:name "Banana" :price 80 :quantity 3})
        cart3 (update-quantity cart2 "Apple" 5)
        cart4 (remove-item cart3 "Banana")]
    {:step1 cart1
     :step2 cart2
     :step3 cart3
     :step4 cart4}))

;; =============================================================================
;; 実践例：ゲームの状態管理
;; =============================================================================

(def initial-game-state
  {:player {:name "Hero"
            :hp 100
            :mp 50
            :position {:x 0 :y 0}}
   :enemies [{:name "Goblin" :hp 30}
             {:name "Orc" :hp 50}]
   :items []})

(defn move-player
  "プレイヤーを移動"
  [state dx dy]
  (-> state
      (update-in [:player :position :x] + dx)
      (update-in [:player :position :y] + dy)))

(defn damage-player
  "プレイヤーにダメージ"
  [state damage]
  (update-in state [:player :hp] - damage))

(defn heal-player
  "プレイヤーを回復"
  [state amount]
  (update-in state [:player :hp] + amount))

(defn add-item-to-inventory
  "アイテムをインベントリに追加"
  [state item]
  (update state :items conj item))

(defn remove-enemy
  "敵を削除"
  [state enemy-name]
  (update state :enemies
          (fn [enemies]
            (vec (remove #(= (:name %) enemy-name) enemies)))))

;; ゲーム状態の履歴を保持（イミュータブルなので容易）
(defn apply-actions
  "一連のアクションを適用して履歴を返す"
  [initial-state actions]
  (reductions
   (fn [state action]
     (case (:type action)
       :move (move-player state (:dx action) (:dy action))
       :damage (damage-player state (:amount action))
       :heal (heal-player state (:amount action))
       :add-item (add-item-to-inventory state (:item action))
       :remove-enemy (remove-enemy state (:enemy action))
       state))
   initial-state
   actions))
