(ns ch12.travel-guide
  "第12章: 実践的なアプリケーション構築

   これまで学んだ関数型プログラミングの概念を統合し、
   実践的なアプリケーションを構築します。
   - ドメインモデルの定義
   - データアクセス層の抽象化
   - リソース管理
   - キャッシュの実装
   - テスト戦略")

;; =============================================================================
;; ドメインモデルの定義
;; =============================================================================

(defrecord Location [id name population])
(defrecord Attraction [name description location])
(defrecord MusicArtist [name genre origin])
(defrecord Movie [name year director])
(defrecord Hotel [name rating location])
(defrecord SearchReport [attractions-searched errors])
(defrecord TravelGuide [attraction subjects search-report])

;; コンストラクタ関数
(defn location
  "ロケーションを作成"
  [id name population]
  (->Location id name population))

(defn attraction
  "アトラクションを作成"
  ([name location]
   (->Attraction name nil location))
  ([name description location]
   (->Attraction name description location)))

(defn music-artist
  "ミュージックアーティストを作成"
  [name genre origin]
  (->MusicArtist name genre origin))

(defn movie
  "映画を作成"
  [name year director]
  (->Movie name year director))

(defn hotel
  "ホテルを作成"
  [name rating location]
  (->Hotel name rating location))

(defn search-report
  "検索レポートを作成"
  [attractions-searched errors]
  (->SearchReport attractions-searched errors))

(defn travel-guide
  "旅行ガイドを作成"
  [attraction subjects search-report]
  (->TravelGuide attraction subjects search-report))

;; =============================================================================
;; データアクセスプロトコル
;; =============================================================================

(defprotocol DataAccess
  "データアクセス層の抽象化"
  (find-attractions [this name ordering limit]
    "アトラクションを検索")
  (find-artists-from-location [this location-id limit]
    "ロケーションからアーティストを検索")
  (find-movies-about-location [this location-id limit]
    "ロケーションに関する映画を検索")
  (find-hotels-near-location [this location-id limit]
    "ロケーション近くのホテルを検索"))

;; ソート順序
(def by-name :by-name)
(def by-population :by-location-population)

;; =============================================================================
;; テスト用スタブ実装
;; =============================================================================

(def test-location
  (location "Q123" "Test City" 100000))

(def test-attraction
  (attraction "Test Attraction" "A test attraction" test-location))

(def test-artist
  (music-artist "Test Artist" :rock "Test City"))

(def test-movie
  (movie "Test Movie" 2023 "Test Director"))

(def test-hotel
  (hotel "Test Hotel" 4.5 test-location))

(defrecord TestDataAccess [attractions artists movies hotels]
  DataAccess
  (find-attractions [_ name ordering limit]
    {:ok (take limit (filter #(clojure.string/includes?
                               (clojure.string/lower-case (:name %))
                               (clojure.string/lower-case name))
                             attractions))})

  (find-artists-from-location [_ location-id limit]
    {:ok (take limit (filter #(= location-id (get-in % [:origin])) artists))})

  (find-movies-about-location [_ location-id limit]
    {:ok (take limit movies)})

  (find-hotels-near-location [_ location-id limit]
    {:ok (take limit hotels)}))

(defn create-test-data-access
  "テスト用データアクセスを作成"
  []
  (->TestDataAccess
   [test-attraction
    (attraction "Sydney Opera House" "Famous opera house"
                (location "Q456" "Sydney" 5000000))
    (attraction "Eiffel Tower" "Famous tower"
                (location "Q789" "Paris" 2000000))]
   [test-artist
    (music-artist "AC/DC" :rock "Sydney")
    (music-artist "Edith Piaf" :chanson "Paris")]
   [test-movie
    (movie "Finding Nemo" 2003 "Andrew Stanton")]
   [test-hotel
    (hotel "Grand Hotel" 5.0 test-location)]))

;; =============================================================================
;; エラー処理付きデータアクセス
;; =============================================================================

(defrecord FailingDataAccess []
  DataAccess
  (find-attractions [_ name ordering limit]
    {:ok [(attraction "Fallback Attraction" nil test-location)]})

  (find-artists-from-location [_ location-id limit]
    {:error "Network error: Failed to fetch artists"})

  (find-movies-about-location [_ location-id limit]
    {:error "Timeout: Failed to fetch movies"})

  (find-hotels-near-location [_ location-id limit]
    {:error "Service unavailable"}))

(defn create-failing-data-access
  "失敗するデータアクセスを作成"
  []
  (->FailingDataAccess))

;; =============================================================================
;; Result ユーティリティ
;; =============================================================================

(defn ok? [result]
  (contains? result :ok))

(defn error? [result]
  (contains? result :error))

(defn get-value [result default]
  (if (ok? result)
    (:ok result)
    default))

(defn get-error [result]
  (:error result))

;; =============================================================================
;; TravelGuide アプリケーション
;; =============================================================================

(defn build-travel-guide
  "旅行ガイドを構築"
  [data-access attraction-name]
  (let [attractions-result (find-attractions data-access attraction-name by-population 3)]
    (if (error? attractions-result)
      {:error (get-error attractions-result)}
      (let [attractions (get-value attractions-result [])
            _ (when (empty? attractions)
                (throw (ex-info "No attractions found" {:name attraction-name})))]
        (map (fn [attr]
               (let [location-id (get-in attr [:location :id])
                     artists-result (find-artists-from-location data-access location-id 2)
                     movies-result (find-movies-about-location data-access location-id 2)

                     ;; エラーを収集
                     errors (cond-> []
                              (error? artists-result) (conj (get-error artists-result))
                              (error? movies-result) (conj (get-error movies-result)))

                     ;; 結果を取得（エラー時は空リスト）
                     artists (get-value artists-result [])
                     movies (get-value movies-result [])

                     ;; 件名を構築
                     subjects (concat
                               (map :name artists)
                               (map :name movies))]
                 (travel-guide
                  attr
                  (vec subjects)
                  (search-report (count attractions) errors))))
             attractions)))))

(defn get-travel-guide
  "旅行ガイドを取得（最初の結果のみ）"
  [data-access attraction-name]
  (try
    (let [guides (build-travel-guide data-access attraction-name)]
      (if (map? guides)  ; error map
        guides
        {:ok (first guides)}))
    (catch Exception e
      {:error (.getMessage e)})))

;; =============================================================================
;; キャッシュ付きデータアクセス
;; =============================================================================

(defn create-cached-data-access
  "キャッシュ付きデータアクセスを作成"
  [underlying-data-access]
  (let [attractions-cache (atom {})
        artists-cache (atom {})
        movies-cache (atom {})
        hotels-cache (atom {})]
    (reify DataAccess
      (find-attractions [_ name ordering limit]
        (let [cache-key (str name "-" ordering "-" limit)]
          (if-let [cached (get @attractions-cache cache-key)]
            {:ok cached}
            (let [result (find-attractions underlying-data-access name ordering limit)]
              (when (ok? result)
                (swap! attractions-cache assoc cache-key (:ok result)))
              result))))

      (find-artists-from-location [_ location-id limit]
        (let [cache-key (str location-id "-" limit)]
          (if-let [cached (get @artists-cache cache-key)]
            {:ok cached}
            (let [result (find-artists-from-location underlying-data-access location-id limit)]
              (when (ok? result)
                (swap! artists-cache assoc cache-key (:ok result)))
              result))))

      (find-movies-about-location [_ location-id limit]
        (let [cache-key (str location-id "-" limit)]
          (if-let [cached (get @movies-cache cache-key)]
            {:ok cached}
            (let [result (find-movies-about-location underlying-data-access location-id limit)]
              (when (ok? result)
                (swap! movies-cache assoc cache-key (:ok result)))
              result))))

      (find-hotels-near-location [_ location-id limit]
        (let [cache-key (str location-id "-" limit)]
          (if-let [cached (get @hotels-cache cache-key)]
            {:ok cached}
            (let [result (find-hotels-near-location underlying-data-access location-id limit)]
              (when (ok? result)
                (swap! hotels-cache assoc cache-key (:ok result)))
              result)))))))

;; =============================================================================
;; リソース管理
;; =============================================================================

(defn with-resource
  "リソースを安全に使用（try-with-resources パターン）"
  [acquire release use-fn]
  (let [resource (acquire)]
    (try
      (use-fn resource)
      (finally
        (release resource)))))

(defn make-resource
  "リソースを作成"
  [acquire release]
  {:acquire acquire
   :release release})

(defn use-resource
  "リソースを使用"
  [resource use-fn]
  (with-resource (:acquire resource) (:release resource) use-fn))

;; ファイルリソースの例
(defn file-resource
  "ファイルリソースを作成"
  [path]
  (make-resource
   #(clojure.java.io/reader path)
   #(.close %)))

(defn read-lines
  "ファイルから行を読み取り"
  [path]
  (use-resource
   (file-resource path)
   (fn [reader]
     (vec (line-seq reader)))))

;; =============================================================================
;; バリデーション
;; =============================================================================

(defn validate-attraction-name
  "アトラクション名をバリデート"
  [name]
  (cond
    (nil? name)
    {:error "Attraction name cannot be nil"}

    (clojure.string/blank? name)
    {:error "Attraction name cannot be empty"}

    (> (count name) 100)
    {:error "Attraction name too long"}

    :else
    {:ok name}))

(defn validate-limit
  "リミットをバリデート"
  [limit]
  (cond
    (nil? limit)
    {:error "Limit cannot be nil"}

    (not (integer? limit))
    {:error "Limit must be an integer"}

    (<= limit 0)
    {:error "Limit must be positive"}

    (> limit 100)
    {:error "Limit cannot exceed 100"}

    :else
    {:ok limit}))

(defn validate-search-params
  "検索パラメータをバリデート"
  [name limit]
  (let [name-result (validate-attraction-name name)
        limit-result (validate-limit limit)]
    (cond
      (error? name-result)
      name-result

      (error? limit-result)
      limit-result

      :else
      {:ok {:name (:ok name-result)
            :limit (:ok limit-result)}})))

;; =============================================================================
;; 純粋関数（テスト容易）
;; =============================================================================

(defn filter-popular-locations
  "人口の多いロケーションをフィルタ"
  [locations min-population]
  (filter #(>= (:population %) min-population) locations))

(defn sort-attractions-by-population
  "アトラクションを人口順にソート"
  [attractions]
  (sort-by #(get-in % [:location :population]) > attractions))

(defn calculate-average-rating
  "ホテルの平均評価を計算"
  [hotels]
  (if (empty? hotels)
    0.0
    (/ (reduce + (map :rating hotels))
       (count hotels))))

(defn group-artists-by-genre
  "アーティストをジャンルごとにグループ化"
  [artists]
  (group-by :genre artists))

(defn extract-subjects
  "サブジェクト（アーティスト名 + 映画名）を抽出"
  [artists movies]
  (concat (map :name artists) (map :name movies)))

(defn merge-search-reports
  "検索レポートをマージ"
  [reports]
  (search-report
   (reduce + (map :attractions-searched reports))
   (vec (mapcat :errors reports))))

;; =============================================================================
;; プロパティベーステスト用ジェネレータ
;; =============================================================================

(defn random-string
  "ランダムな文字列を生成"
  [length]
  (apply str (repeatedly length #(char (+ 97 (rand-int 26))))))

(defn random-location
  "ランダムなロケーションを生成"
  []
  (location
   (str "Q" (rand-int 10000))
   (random-string 10)
   (rand-int 10000000)))

(defn random-attraction
  "ランダムなアトラクションを生成"
  []
  (attraction
   (random-string 15)
   (when (< (rand) 0.5) (random-string 50))
   (random-location)))

(defn random-attractions
  "複数のランダムなアトラクションを生成"
  [n]
  (repeatedly n random-attraction))

;; =============================================================================
;; テストヘルパー
;; =============================================================================

(defn with-test-data-access
  "テストデータアクセスでコードを実行"
  [f]
  (f (create-test-data-access)))

(defn assert-ok
  "結果が成功であることをアサート"
  [result]
  (assert (ok? result) (str "Expected ok, got: " result)))

(defn assert-error
  "結果がエラーであることをアサート"
  [result]
  (assert (error? result) (str "Expected error, got: " result)))

;; =============================================================================
;; アプリケーションエントリポイント
;; =============================================================================

(defn run-travel-guide-app
  "旅行ガイドアプリケーションを実行"
  [attraction-name]
  (let [data-access (create-cached-data-access (create-test-data-access))
        validation-result (validate-search-params attraction-name 3)]
    (if (error? validation-result)
      (println "Validation error:" (get-error validation-result))
      (let [result (get-travel-guide data-access attraction-name)]
        (if (ok? result)
          (let [guide (:ok result)]
            (println "=== Travel Guide ===")
            (println "Attraction:" (get-in guide [:attraction :name]))
            (println "Location:" (get-in guide [:attraction :location :name]))
            (println "Subjects:" (:subjects guide))
            (println "Errors:" (get-in guide [:search-report :errors])))
          (println "Error:" (get-error result)))))))

