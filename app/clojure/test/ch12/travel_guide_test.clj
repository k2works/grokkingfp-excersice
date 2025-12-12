(ns ch12.travel-guide-test
  (:require [clojure.test :refer [deftest testing is are]]
            [ch12.travel-guide :as tg]))

;; =============================================================================
;; ドメインモデルのテスト
;; =============================================================================

(deftest location-tests
  (testing "location creation"
    (let [loc (tg/location "Q123" "Tokyo" 14000000)]
      (is (= "Q123" (:id loc)))
      (is (= "Tokyo" (:name loc)))
      (is (= 14000000 (:population loc))))))

(deftest attraction-tests
  (testing "attraction with description"
    (let [loc (tg/location "Q456" "Paris" 2000000)
          attr (tg/attraction "Eiffel Tower" "Famous tower" loc)]
      (is (= "Eiffel Tower" (:name attr)))
      (is (= "Famous tower" (:description attr)))
      (is (= loc (:location attr)))))

  (testing "attraction without description"
    (let [loc (tg/location "Q789" "London" 9000000)
          attr (tg/attraction "Big Ben" loc)]
      (is (= "Big Ben" (:name attr)))
      (is (nil? (:description attr))))))

(deftest music-artist-tests
  (testing "music-artist creation"
    (let [artist (tg/music-artist "Beatles" :rock "Liverpool")]
      (is (= "Beatles" (:name artist)))
      (is (= :rock (:genre artist)))
      (is (= "Liverpool" (:origin artist))))))

(deftest movie-tests
  (testing "movie creation"
    (let [m (tg/movie "Inception" 2010 "Christopher Nolan")]
      (is (= "Inception" (:name m)))
      (is (= 2010 (:year m)))
      (is (= "Christopher Nolan" (:director m))))))

(deftest hotel-tests
  (testing "hotel creation"
    (let [loc (tg/location "Q111" "New York" 8000000)
          h (tg/hotel "Plaza Hotel" 5.0 loc)]
      (is (= "Plaza Hotel" (:name h)))
      (is (= 5.0 (:rating h)))
      (is (= loc (:location h))))))

(deftest search-report-tests
  (testing "search-report creation"
    (let [report (tg/search-report 3 ["error1" "error2"])]
      (is (= 3 (:attractions-searched report)))
      (is (= ["error1" "error2"] (:errors report))))))

(deftest travel-guide-tests
  (testing "travel-guide creation"
    (let [loc (tg/location "Q222" "Rome" 2800000)
          attr (tg/attraction "Colosseum" "Ancient arena" loc)
          report (tg/search-report 1 [])
          guide (tg/travel-guide attr ["Artist1" "Movie1"] report)]
      (is (= attr (:attraction guide)))
      (is (= ["Artist1" "Movie1"] (:subjects guide)))
      (is (= report (:search-report guide))))))

;; =============================================================================
;; Result ユーティリティのテスト
;; =============================================================================

(deftest ok?-tests
  (testing "ok? returns true for ok result"
    (is (true? (tg/ok? {:ok 42})))
    (is (false? (tg/ok? {:error "oops"})))))

(deftest error?-tests
  (testing "error? returns true for error result"
    (is (true? (tg/error? {:error "oops"})))
    (is (false? (tg/error? {:ok 42})))))

(deftest get-value-tests
  (testing "get-value returns value or default"
    (is (= 42 (tg/get-value {:ok 42} 0)))
    (is (= 0 (tg/get-value {:error "oops"} 0)))))

(deftest get-error-tests
  (testing "get-error returns error message"
    (is (= "oops" (tg/get-error {:error "oops"})))))

;; =============================================================================
;; バリデーションのテスト
;; =============================================================================

(deftest validate-attraction-name-tests
  (testing "valid names"
    (is (tg/ok? (tg/validate-attraction-name "Tokyo Tower")))
    (is (tg/ok? (tg/validate-attraction-name "A"))))

  (testing "invalid names"
    (is (tg/error? (tg/validate-attraction-name nil)))
    (is (tg/error? (tg/validate-attraction-name "")))
    (is (tg/error? (tg/validate-attraction-name "   ")))
    (is (tg/error? (tg/validate-attraction-name (apply str (repeat 101 "a")))))))

(deftest validate-limit-tests
  (testing "valid limits"
    (is (tg/ok? (tg/validate-limit 1)))
    (is (tg/ok? (tg/validate-limit 50)))
    (is (tg/ok? (tg/validate-limit 100))))

  (testing "invalid limits"
    (is (tg/error? (tg/validate-limit nil)))
    (is (tg/error? (tg/validate-limit "10")))
    (is (tg/error? (tg/validate-limit 0)))
    (is (tg/error? (tg/validate-limit -1)))
    (is (tg/error? (tg/validate-limit 101)))))

(deftest validate-search-params-tests
  (testing "valid params"
    (let [result (tg/validate-search-params "Tokyo" 10)]
      (is (tg/ok? result))
      (is (= "Tokyo" (get-in result [:ok :name])))
      (is (= 10 (get-in result [:ok :limit])))))

  (testing "invalid name"
    (is (tg/error? (tg/validate-search-params "" 10))))

  (testing "invalid limit"
    (is (tg/error? (tg/validate-search-params "Tokyo" -1)))))

;; =============================================================================
;; TestDataAccess のテスト
;; =============================================================================

(deftest test-data-access-tests
  (let [data-access (tg/create-test-data-access)]

    (testing "find-attractions returns matching attractions"
      (let [result (tg/find-attractions data-access "Test" tg/by-name 10)]
        (is (tg/ok? result))
        (is (= 1 (count (:ok result))))
        (is (= "Test Attraction" (:name (first (:ok result)))))))

    (testing "find-attractions with limit"
      (let [result (tg/find-attractions data-access "" tg/by-name 2)]
        (is (tg/ok? result))
        (is (<= (count (:ok result)) 2))))

    (testing "find-artists-from-location"
      (let [result (tg/find-artists-from-location data-access "Test City" 10)]
        (is (tg/ok? result))))

    (testing "find-movies-about-location"
      (let [result (tg/find-movies-about-location data-access "Q123" 10)]
        (is (tg/ok? result))))

    (testing "find-hotels-near-location"
      (let [result (tg/find-hotels-near-location data-access "Q123" 10)]
        (is (tg/ok? result))))))

;; =============================================================================
;; FailingDataAccess のテスト
;; =============================================================================

(deftest failing-data-access-tests
  (let [data-access (tg/create-failing-data-access)]

    (testing "find-attractions returns fallback"
      (let [result (tg/find-attractions data-access "Test" tg/by-name 10)]
        (is (tg/ok? result))))

    (testing "find-artists-from-location returns error"
      (let [result (tg/find-artists-from-location data-access "Q123" 10)]
        (is (tg/error? result))))

    (testing "find-movies-about-location returns error"
      (let [result (tg/find-movies-about-location data-access "Q123" 10)]
        (is (tg/error? result))))

    (testing "find-hotels-near-location returns error"
      (let [result (tg/find-hotels-near-location data-access "Q123" 10)]
        (is (tg/error? result))))))

;; =============================================================================
;; TravelGuide アプリケーションのテスト
;; =============================================================================

(deftest build-travel-guide-tests
  (let [data-access (tg/create-test-data-access)]

    (testing "build-travel-guide returns guides"
      (let [guides (tg/build-travel-guide data-access "Test")]
        (is (seq guides))
        (let [guide (first guides)]
          (is (= "Test Attraction" (get-in guide [:attraction :name])))
          (is (vector? (:subjects guide)))
          (is (empty? (get-in guide [:search-report :errors]))))))))

(deftest get-travel-guide-tests
  (let [data-access (tg/create-test-data-access)]

    (testing "get-travel-guide returns first guide"
      (let [result (tg/get-travel-guide data-access "Test")]
        (is (tg/ok? result))
        (let [guide (:ok result)]
          (is (= "Test Attraction" (get-in guide [:attraction :name]))))))

    (testing "get-travel-guide with no matches"
      (let [result (tg/get-travel-guide data-access "NonExistent12345")]
        ;; 空の場合はエラーになる
        (is (or (tg/error? result)
                (nil? (:ok result))))))))

(deftest travel-guide-with-errors-tests
  (let [data-access (tg/create-failing-data-access)]

    (testing "travel guide collects errors from failing services"
      (let [result (tg/get-travel-guide data-access "Test")]
        (is (tg/ok? result))
        (let [guide (:ok result)
              errors (get-in guide [:search-report :errors])]
          ;; エラーが2つ（artists と movies）
          (is (= 2 (count errors))))))))

;; =============================================================================
;; キャッシュ付きデータアクセスのテスト
;; =============================================================================

(deftest cached-data-access-tests
  (let [call-count (atom 0)
        underlying (reify tg/DataAccess
                     (find-attractions [_ name ordering limit]
                       (swap! call-count inc)
                       {:ok [(tg/attraction "Cached Attraction" nil tg/test-location)]})
                     (find-artists-from-location [_ location-id limit]
                       {:ok []})
                     (find-movies-about-location [_ location-id limit]
                       {:ok []})
                     (find-hotels-near-location [_ location-id limit]
                       {:ok []}))
        cached (tg/create-cached-data-access underlying)]

    (testing "first call hits underlying"
      (reset! call-count 0)
      (tg/find-attractions cached "Test" tg/by-name 10)
      (is (= 1 @call-count)))

    (testing "second call uses cache"
      (tg/find-attractions cached "Test" tg/by-name 10)
      (is (= 1 @call-count)))  ; カウントは増えない

    (testing "different params hit underlying"
      (tg/find-attractions cached "Other" tg/by-name 10)
      (is (= 2 @call-count)))))

;; =============================================================================
;; 純粋関数のテスト
;; =============================================================================

(deftest filter-popular-locations-tests
  (testing "filters by minimum population"
    (let [locations [(tg/location "Q1" "Small" 1000)
                     (tg/location "Q2" "Medium" 100000)
                     (tg/location "Q3" "Large" 1000000)]
          result (tg/filter-popular-locations locations 50000)]
      (is (= 2 (count result)))
      (is (every? #(>= (:population %) 50000) result))))

  (testing "returns empty for high threshold"
    (let [locations [(tg/location "Q1" "Small" 1000)]
          result (tg/filter-popular-locations locations 1000000)]
      (is (empty? result)))))

(deftest sort-attractions-by-population-tests
  (testing "sorts by population descending"
    (let [small-loc (tg/location "Q1" "Small" 1000)
          large-loc (tg/location "Q2" "Large" 1000000)
          attractions [(tg/attraction "Small Attr" small-loc)
                       (tg/attraction "Large Attr" large-loc)]
          sorted (tg/sort-attractions-by-population attractions)]
      (is (= "Large Attr" (:name (first sorted))))
      (is (= "Small Attr" (:name (second sorted)))))))

(deftest calculate-average-rating-tests
  (testing "calculates average"
    (let [loc (tg/location "Q1" "City" 100000)
          hotels [(tg/hotel "H1" 4.0 loc)
                  (tg/hotel "H2" 5.0 loc)
                  (tg/hotel "H3" 3.0 loc)]]
      (is (= 4.0 (tg/calculate-average-rating hotels)))))

  (testing "returns 0 for empty list"
    (is (= 0.0 (tg/calculate-average-rating [])))))

(deftest group-artists-by-genre-tests
  (testing "groups by genre"
    (let [artists [(tg/music-artist "A1" :rock "City1")
                   (tg/music-artist "A2" :jazz "City2")
                   (tg/music-artist "A3" :rock "City3")]
          grouped (tg/group-artists-by-genre artists)]
      (is (= 2 (count (:rock grouped))))
      (is (= 1 (count (:jazz grouped)))))))

(deftest extract-subjects-tests
  (testing "extracts names from artists and movies"
    (let [artists [(tg/music-artist "Artist1" :rock "City")
                   (tg/music-artist "Artist2" :jazz "City")]
          movies [(tg/movie "Movie1" 2020 "Dir1")
                  (tg/movie "Movie2" 2021 "Dir2")]
          subjects (tg/extract-subjects artists movies)]
      (is (= 4 (count subjects)))
      (is (= ["Artist1" "Artist2" "Movie1" "Movie2"] (vec subjects))))))

(deftest merge-search-reports-tests
  (testing "merges multiple reports"
    (let [r1 (tg/search-report 3 ["e1"])
          r2 (tg/search-report 2 ["e2" "e3"])
          merged (tg/merge-search-reports [r1 r2])]
      (is (= 5 (:attractions-searched merged)))
      (is (= ["e1" "e2" "e3"] (:errors merged))))))

;; =============================================================================
;; プロパティベーステスト（簡易版）
;; =============================================================================

(deftest property-filter-popular-locations-tests
  (testing "result size is at most input size"
    (dotimes [_ 100]
      (let [n (inc (rand-int 20))
            locations (repeatedly n tg/random-location)
            min-pop (rand-int 10000000)
            result (tg/filter-popular-locations locations min-pop)]
        (is (<= (count result) n)))))

  (testing "all results meet minimum population"
    (dotimes [_ 100]
      (let [n (inc (rand-int 20))
            locations (repeatedly n tg/random-location)
            min-pop (rand-int 10000000)
            result (tg/filter-popular-locations locations min-pop)]
        (is (every? #(>= (:population %) min-pop) result))))))

(deftest property-sort-attractions-tests
  (testing "sorted list is same size as input"
    (dotimes [_ 50]
      (let [n (inc (rand-int 10))
            attractions (tg/random-attractions n)
            sorted (tg/sort-attractions-by-population attractions)]
        (is (= n (count sorted))))))

  (testing "sorted list is in descending order"
    (dotimes [_ 50]
      (let [n (inc (rand-int 10))
            attractions (tg/random-attractions n)
            sorted (tg/sort-attractions-by-population attractions)
            populations (map #(get-in % [:location :population]) sorted)]
        (is (= populations (reverse (sort populations))))))))

;; =============================================================================
;; リソース管理のテスト
;; =============================================================================

(deftest with-resource-tests
  (testing "resource is released after use"
    (let [released (atom false)]
      (tg/with-resource
        (fn [] {:data "resource"})
        (fn [_] (reset! released true))
        (fn [r] (:data r)))
      (is @released)))

  (testing "resource is released on exception"
    (let [released (atom false)]
      (try
        (tg/with-resource
          (fn [] {:data "resource"})
          (fn [_] (reset! released true))
          (fn [_] (throw (Exception. "error"))))
        (catch Exception _))
      (is @released))))

;; =============================================================================
;; 統合テスト
;; =============================================================================

(deftest integration-travel-guide-tests
  (testing "end-to-end travel guide workflow"
    (let [data-access (tg/create-cached-data-access (tg/create-test-data-access))
          ;; バリデーション
          validation (tg/validate-search-params "Test" 3)]
      (is (tg/ok? validation))

      ;; ガイド取得
      (let [result (tg/get-travel-guide data-access "Test")]
        (is (tg/ok? result))
        (let [guide (:ok result)]
          (is (some? (:attraction guide)))
          (is (vector? (:subjects guide)))
          (is (some? (:search-report guide)))))))

  (testing "workflow with validation failure"
    (let [validation (tg/validate-search-params "" 3)]
      (is (tg/error? validation))
      (is (clojure.string/includes? (tg/get-error validation) "empty"))))

  (testing "workflow with partial failure"
    (let [data-access (tg/create-failing-data-access)
          result (tg/get-travel-guide data-access "Test")]
      (is (tg/ok? result))
      (let [guide (:ok result)]
        (is (= 2 (count (get-in guide [:search-report :errors]))))))))

