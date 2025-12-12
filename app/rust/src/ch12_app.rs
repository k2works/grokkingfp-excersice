//! 第12章: 実践的なアプリケーション構築とテスト
//!
//! trait による抽象化、依存性注入、proptest によるプロパティベーステストを学びます。
//! TravelGuide アプリケーションを例に、実践的な FP アプリケーションの構築方法を習得します。

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

// =============================================================================
// 12.1 ドメインモデルの定義
// =============================================================================

/// 位置 ID（Newtype パターン）
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocationId(pub String);

impl LocationId {
    pub fn new(id: &str) -> Self {
        Self(id.to_string())
    }
}

/// ロケーション
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{LocationId, Location};
///
/// let location = Location::new(LocationId::new("Q90"), "Paris", 2_161_000);
/// assert_eq!(location.name, "Paris");
/// assert_eq!(location.population, 2_161_000);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    pub id: LocationId,
    pub name: String,
    pub population: i32,
}

impl Location {
    pub fn new(id: LocationId, name: &str, population: i32) -> Self {
        Self {
            id,
            name: name.to_string(),
            population,
        }
    }
}

/// アトラクション（観光地）
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attraction {
    pub name: String,
    pub description: Option<String>,
    pub location: Location,
}

impl Attraction {
    pub fn new(name: &str, description: Option<&str>, location: Location) -> Self {
        Self {
            name: name.to_string(),
            description: description.map(|s| s.to_string()),
            location,
        }
    }
}

/// 音楽アーティスト
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MusicArtist {
    pub name: String,
}

impl MusicArtist {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

/// 映画
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Movie {
    pub name: String,
}

impl Movie {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

/// 検索レポート
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SearchReport {
    pub attractions_searched: i32,
    pub errors: Vec<String>,
}

impl SearchReport {
    pub fn new(attractions_searched: i32, errors: Vec<String>) -> Self {
        Self {
            attractions_searched,
            errors,
        }
    }

    pub fn empty() -> Self {
        Self {
            attractions_searched: 0,
            errors: vec![],
        }
    }
}

/// 旅行ガイド
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TravelGuide {
    pub attraction: Attraction,
    pub subjects: Vec<String>,
    pub search_report: SearchReport,
}

impl TravelGuide {
    pub fn new(attraction: Attraction, subjects: Vec<String>, search_report: SearchReport) -> Self {
        Self {
            attraction,
            subjects,
            search_report,
        }
    }
}

/// アトラクションの並び順
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AttractionOrdering {
    ByName,
    ByLocationPopulation,
}

// =============================================================================
// 12.2 DataAccess トレイト - データアクセス層の抽象化
// =============================================================================

/// データアクセス層のトレイト
///
/// 外部データソースへのアクセスを抽象化します。
/// テスト時にはスタブ実装に差し替えることができます。
#[async_trait::async_trait]
pub trait DataAccess: Send + Sync {
    /// アトラクションを検索
    async fn find_attractions(
        &self,
        name: &str,
        ordering: AttractionOrdering,
        limit: usize,
    ) -> Vec<Attraction>;

    /// ロケーションからアーティストを検索
    async fn find_artists_from_location(
        &self,
        location_id: &LocationId,
        limit: usize,
    ) -> Result<Vec<MusicArtist>, String>;

    /// ロケーションに関する映画を検索
    async fn find_movies_about_location(
        &self,
        location_id: &LocationId,
        limit: usize,
    ) -> Result<Vec<Movie>, String>;
}

// =============================================================================
// 12.3 テスト用スタブ実装
// =============================================================================

/// テスト用のスタブ DataAccess 実装
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{StubDataAccess, DataAccess, AttractionOrdering};
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let data_access = StubDataAccess::default();
///     let attractions = data_access.find_attractions("Paris", AttractionOrdering::ByName, 10).await;
///     assert!(!attractions.is_empty());
/// });
/// ```
pub struct StubDataAccess {
    attractions: Vec<Attraction>,
    artists: HashMap<LocationId, Vec<MusicArtist>>,
    movies: HashMap<LocationId, Vec<Movie>>,
    artists_error: Option<String>,
    movies_error: Option<String>,
}

impl StubDataAccess {
    pub fn new() -> Self {
        Self {
            attractions: vec![],
            artists: HashMap::new(),
            movies: HashMap::new(),
            artists_error: None,
            movies_error: None,
        }
    }

    pub fn with_attractions(mut self, attractions: Vec<Attraction>) -> Self {
        self.attractions = attractions;
        self
    }

    pub fn with_artists(mut self, location_id: LocationId, artists: Vec<MusicArtist>) -> Self {
        self.artists.insert(location_id, artists);
        self
    }

    pub fn with_movies(mut self, location_id: LocationId, movies: Vec<Movie>) -> Self {
        self.movies.insert(location_id, movies);
        self
    }

    pub fn with_artists_error(mut self, error: &str) -> Self {
        self.artists_error = Some(error.to_string());
        self
    }

    pub fn with_movies_error(mut self, error: &str) -> Self {
        self.movies_error = Some(error.to_string());
        self
    }
}

impl Default for StubDataAccess {
    fn default() -> Self {
        let paris = Location::new(LocationId::new("Q90"), "Paris", 2_161_000);
        let tokyo = Location::new(LocationId::new("Q1490"), "Tokyo", 13_960_000);

        Self::new()
            .with_attractions(vec![
                Attraction::new("Eiffel Tower", Some("Iconic iron lattice tower"), paris.clone()),
                Attraction::new("Tokyo Tower", Some("Communications and observation tower"), tokyo.clone()),
            ])
            .with_artists(paris.id.clone(), vec![
                MusicArtist::new("Édith Piaf"),
                MusicArtist::new("Daft Punk"),
            ])
            .with_artists(tokyo.id.clone(), vec![
                MusicArtist::new("Hikaru Utada"),
                MusicArtist::new("YMO"),
            ])
            .with_movies(paris.id, vec![
                Movie::new("Amélie"),
                Movie::new("Midnight in Paris"),
            ])
            .with_movies(tokyo.id, vec![
                Movie::new("Lost in Translation"),
                Movie::new("Tokyo Story"),
            ])
    }
}

#[async_trait::async_trait]
impl DataAccess for StubDataAccess {
    async fn find_attractions(
        &self,
        name: &str,
        ordering: AttractionOrdering,
        limit: usize,
    ) -> Vec<Attraction> {
        let name_lower = name.to_lowercase();
        let mut results: Vec<_> = self
            .attractions
            .iter()
            .filter(|a| a.name.to_lowercase().contains(&name_lower) ||
                       a.location.name.to_lowercase().contains(&name_lower))
            .cloned()
            .collect();

        match ordering {
            AttractionOrdering::ByName => results.sort_by(|a, b| a.name.cmp(&b.name)),
            AttractionOrdering::ByLocationPopulation => {
                results.sort_by(|a, b| b.location.population.cmp(&a.location.population))
            }
        }

        results.truncate(limit);
        results
    }

    async fn find_artists_from_location(
        &self,
        location_id: &LocationId,
        limit: usize,
    ) -> Result<Vec<MusicArtist>, String> {
        if let Some(error) = &self.artists_error {
            return Err(error.clone());
        }

        let mut artists = self
            .artists
            .get(location_id)
            .cloned()
            .unwrap_or_default();
        artists.truncate(limit);
        Ok(artists)
    }

    async fn find_movies_about_location(
        &self,
        location_id: &LocationId,
        limit: usize,
    ) -> Result<Vec<Movie>, String> {
        if let Some(error) = &self.movies_error {
            return Err(error.clone());
        }

        let mut movies = self
            .movies
            .get(location_id)
            .cloned()
            .unwrap_or_default();
        movies.truncate(limit);
        Ok(movies)
    }
}

// =============================================================================
// 12.4 キャッシュ付き DataAccess
// =============================================================================

/// キャッシュ付き DataAccess
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{CachedDataAccess, StubDataAccess, DataAccess, AttractionOrdering};
/// use std::sync::Arc;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let inner = Arc::new(StubDataAccess::default());
///     let cached = CachedDataAccess::new(inner);
///
///     // 最初の呼び出し
///     let result1 = cached.find_attractions("Paris", AttractionOrdering::ByName, 10).await;
///     // キャッシュからの呼び出し
///     let result2 = cached.find_attractions("Paris", AttractionOrdering::ByName, 10).await;
///
///     assert_eq!(result1, result2);
/// });
/// ```
pub struct CachedDataAccess<D: DataAccess> {
    inner: Arc<D>,
    attractions_cache: Arc<RwLock<HashMap<String, Vec<Attraction>>>>,
}

impl<D: DataAccess> CachedDataAccess<D> {
    pub fn new(inner: Arc<D>) -> Self {
        Self {
            inner,
            attractions_cache: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    fn cache_key(name: &str, ordering: AttractionOrdering, limit: usize) -> String {
        format!("{}-{:?}-{}", name, ordering, limit)
    }
}

#[async_trait::async_trait]
impl<D: DataAccess + 'static> DataAccess for CachedDataAccess<D> {
    async fn find_attractions(
        &self,
        name: &str,
        ordering: AttractionOrdering,
        limit: usize,
    ) -> Vec<Attraction> {
        let key = Self::cache_key(name, ordering, limit);

        // キャッシュを確認
        {
            let cache = self.attractions_cache.read().await;
            if let Some(cached) = cache.get(&key) {
                return cached.clone();
            }
        }

        // キャッシュにない場合は取得
        let result = self.inner.find_attractions(name, ordering, limit).await;

        // キャッシュに保存
        {
            let mut cache = self.attractions_cache.write().await;
            cache.insert(key, result.clone());
        }

        result
    }

    async fn find_artists_from_location(
        &self,
        location_id: &LocationId,
        limit: usize,
    ) -> Result<Vec<MusicArtist>, String> {
        // アーティストはキャッシュしない（簡略化のため）
        self.inner.find_artists_from_location(location_id, limit).await
    }

    async fn find_movies_about_location(
        &self,
        location_id: &LocationId,
        limit: usize,
    ) -> Result<Vec<Movie>, String> {
        // 映画はキャッシュしない（簡略化のため）
        self.inner.find_movies_about_location(location_id, limit).await
    }
}

// =============================================================================
// 12.5 TravelGuide アプリケーション
// =============================================================================

/// 旅行ガイドを生成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{travel_guide, StubDataAccess};
/// use std::sync::Arc;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let data_access = Arc::new(StubDataAccess::default());
///     let guide = travel_guide(data_access.as_ref(), "Paris").await;
///
///     assert!(guide.is_some());
///     let guide = guide.unwrap();
///     assert_eq!(guide.attraction.name, "Eiffel Tower");
///     assert!(!guide.subjects.is_empty());
/// });
/// ```
pub async fn travel_guide(
    data: &dyn DataAccess,
    attraction_name: &str,
) -> Option<TravelGuide> {
    let attractions = data
        .find_attractions(attraction_name, AttractionOrdering::ByLocationPopulation, 1)
        .await;

    let attraction = attractions.into_iter().next()?;

    let artists_result = data
        .find_artists_from_location(&attraction.location.id, 2)
        .await;
    let movies_result = data
        .find_movies_about_location(&attraction.location.id, 2)
        .await;

    let mut errors = Vec::new();
    if let Err(e) = &artists_result {
        errors.push(e.clone());
    }
    if let Err(e) = &movies_result {
        errors.push(e.clone());
    }

    let artists = artists_result.unwrap_or_default();
    let movies = movies_result.unwrap_or_default();

    let subjects: Vec<String> = artists
        .into_iter()
        .map(|a| a.name)
        .chain(movies.into_iter().map(|m| m.name))
        .collect();

    Some(TravelGuide::new(
        attraction,
        subjects,
        SearchReport::new(1, errors),
    ))
}

/// 複数のアトラクションから旅行ガイドを生成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{travel_guides, StubDataAccess};
/// use std::sync::Arc;
///
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let data_access = Arc::new(StubDataAccess::default());
///     let guides = travel_guides(data_access.as_ref(), "Tower", 5).await;
///
///     assert!(!guides.is_empty());
/// });
/// ```
pub async fn travel_guides(
    data: &dyn DataAccess,
    attraction_name: &str,
    limit: usize,
) -> Vec<TravelGuide> {
    let attractions = data
        .find_attractions(attraction_name, AttractionOrdering::ByLocationPopulation, limit)
        .await;

    let mut guides = Vec::new();

    for attraction in attractions {
        let artists_result = data
            .find_artists_from_location(&attraction.location.id, 2)
            .await;
        let movies_result = data
            .find_movies_about_location(&attraction.location.id, 2)
            .await;

        let mut errors = Vec::new();
        if let Err(e) = &artists_result {
            errors.push(e.clone());
        }
        if let Err(e) = &movies_result {
            errors.push(e.clone());
        }

        let artists = artists_result.unwrap_or_default();
        let movies = movies_result.unwrap_or_default();

        let subjects: Vec<String> = artists
            .into_iter()
            .map(|a| a.name)
            .chain(movies.into_iter().map(|m| m.name))
            .collect();

        guides.push(TravelGuide::new(
            attraction,
            subjects,
            SearchReport::new(1, errors),
        ));
    }

    guides
}

// =============================================================================
// 12.6 純粋関数のユーティリティ
// =============================================================================

/// 人気のあるロケーションをフィルタリング（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{Location, LocationId, filter_popular_locations};
///
/// let locations = vec![
///     Location::new(LocationId::new("1"), "Tokyo", 13_960_000),
///     Location::new(LocationId::new("2"), "Small Town", 1_000),
///     Location::new(LocationId::new("3"), "Paris", 2_161_000),
/// ];
///
/// let popular = filter_popular_locations(&locations, 1_000_000);
/// assert_eq!(popular.len(), 2);
/// assert_eq!(popular[0].name, "Tokyo");
/// assert_eq!(popular[1].name, "Paris");
/// ```
pub fn filter_popular_locations(locations: &[Location], min_population: i32) -> Vec<Location> {
    locations
        .iter()
        .filter(|l| l.population >= min_population)
        .cloned()
        .collect()
}

/// アトラクションを人口でソート（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{Attraction, Location, LocationId, sort_by_population};
///
/// let tokyo = Location::new(LocationId::new("1"), "Tokyo", 13_960_000);
/// let paris = Location::new(LocationId::new("2"), "Paris", 2_161_000);
///
/// let attractions = vec![
///     Attraction::new("Eiffel Tower", None, paris),
///     Attraction::new("Tokyo Tower", None, tokyo),
/// ];
///
/// let sorted = sort_by_population(&attractions);
/// assert_eq!(sorted[0].name, "Tokyo Tower");
/// assert_eq!(sorted[1].name, "Eiffel Tower");
/// ```
pub fn sort_by_population(attractions: &[Attraction]) -> Vec<Attraction> {
    let mut sorted: Vec<_> = attractions.to_vec();
    sorted.sort_by(|a, b| b.location.population.cmp(&a.location.population));
    sorted
}

/// サブジェクト名を結合（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::combine_subjects;
///
/// let artists = vec!["Artist 1".to_string(), "Artist 2".to_string()];
/// let movies = vec!["Movie 1".to_string()];
///
/// let combined = combine_subjects(&artists, &movies);
/// assert_eq!(combined, vec!["Artist 1", "Artist 2", "Movie 1"]);
/// ```
pub fn combine_subjects(artists: &[String], movies: &[String]) -> Vec<String> {
    artists.iter().chain(movies.iter()).cloned().collect()
}

/// 検索レポートを集約（純粋関数）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{SearchReport, aggregate_reports};
///
/// let reports = vec![
///     SearchReport::new(1, vec!["Error 1".to_string()]),
///     SearchReport::new(2, vec![]),
///     SearchReport::new(1, vec!["Error 2".to_string()]),
/// ];
///
/// let aggregated = aggregate_reports(&reports);
/// assert_eq!(aggregated.attractions_searched, 4);
/// assert_eq!(aggregated.errors.len(), 2);
/// ```
pub fn aggregate_reports(reports: &[SearchReport]) -> SearchReport {
    let attractions_searched = reports.iter().map(|r| r.attractions_searched).sum();
    let errors: Vec<String> = reports
        .iter()
        .flat_map(|r| r.errors.iter().cloned())
        .collect();

    SearchReport::new(attractions_searched, errors)
}

// =============================================================================
// 12.7 リソース管理（Resource パターン）
// =============================================================================

/// リソース管理のトレイト
pub trait Resource {
    type Item;

    fn use_resource<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&Self::Item) -> R;
}

/// ファイルリソース
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{FileResource, Resource};
/// use std::io::Write;
///
/// // テスト用の一時ファイルを作成
/// let temp_dir = std::env::temp_dir();
/// let temp_path = temp_dir.join("test_file.txt");
/// std::fs::write(&temp_path, "Hello, World!").unwrap();
///
/// let resource = FileResource::new(temp_path.to_str().unwrap());
/// let content = resource.use_resource(|result| {
///     result.as_ref().map(|s| s.clone()).unwrap()
/// });
///
/// assert_eq!(content, "Hello, World!");
///
/// // クリーンアップ
/// std::fs::remove_file(temp_path).ok();
/// ```
pub struct FileResource {
    path: String,
}

impl FileResource {
    pub fn new(path: &str) -> Self {
        Self {
            path: path.to_string(),
        }
    }
}

impl Resource for FileResource {
    type Item = Result<String, std::io::Error>;

    fn use_resource<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&Self::Item) -> R,
    {
        let content = std::fs::read_to_string(&self.path);
        f(&content)
    }
}

// =============================================================================
// 12.8 バリデーション
// =============================================================================

/// バリデーション結果
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Validation<T> {
    Valid(T),
    Invalid(Vec<String>),
}

impl<T> Validation<T> {
    pub fn valid(value: T) -> Self {
        Validation::Valid(value)
    }

    pub fn invalid(errors: Vec<String>) -> Self {
        Validation::Invalid(errors)
    }

    pub fn is_valid(&self) -> bool {
        matches!(self, Validation::Valid(_))
    }

    pub fn map<U, F>(self, f: F) -> Validation<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Validation::Valid(v) => Validation::Valid(f(v)),
            Validation::Invalid(e) => Validation::Invalid(e),
        }
    }
}

/// ロケーションのバリデーション
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{validate_location, Validation};
///
/// let valid = validate_location("Tokyo", 13_960_000);
/// assert!(valid.is_valid());
///
/// let invalid = validate_location("", -100);
/// assert!(!invalid.is_valid());
/// if let Validation::Invalid(errors) = invalid {
///     assert_eq!(errors.len(), 2);
/// }
/// ```
pub fn validate_location(name: &str, population: i32) -> Validation<(String, i32)> {
    let mut errors = Vec::new();

    if name.is_empty() {
        errors.push("Name cannot be empty".to_string());
    }

    if population < 0 {
        errors.push("Population cannot be negative".to_string());
    }

    if errors.is_empty() {
        Validation::valid((name.to_string(), population))
    } else {
        Validation::invalid(errors)
    }
}

/// アトラクションのバリデーション
///
/// # Examples
///
/// ```
/// use grokking_fp::ch12_app::{validate_attraction, Validation};
///
/// let valid = validate_attraction("Eiffel Tower", Some("A tower"));
/// assert!(valid.is_valid());
///
/// let invalid = validate_attraction("", None);
/// assert!(!invalid.is_valid());
/// ```
pub fn validate_attraction(name: &str, description: Option<&str>) -> Validation<(String, Option<String>)> {
    let mut errors = Vec::new();

    if name.is_empty() {
        errors.push("Attraction name cannot be empty".to_string());
    }

    if errors.is_empty() {
        Validation::valid((name.to_string(), description.map(|s| s.to_string())))
    } else {
        Validation::invalid(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_location_creation() {
        let location = Location::new(LocationId::new("Q90"), "Paris", 2_161_000);
        assert_eq!(location.name, "Paris");
        assert_eq!(location.population, 2_161_000);
    }

    #[test]
    fn test_attraction_creation() {
        let location = Location::new(LocationId::new("Q90"), "Paris", 2_161_000);
        let attraction = Attraction::new("Eiffel Tower", Some("A tower"), location);
        assert_eq!(attraction.name, "Eiffel Tower");
        assert!(attraction.description.is_some());
    }

    #[test]
    fn test_search_report() {
        let report = SearchReport::new(5, vec!["Error 1".to_string()]);
        assert_eq!(report.attractions_searched, 5);
        assert_eq!(report.errors.len(), 1);
    }

    #[tokio::test]
    async fn test_stub_data_access_find_attractions() {
        let data_access = StubDataAccess::default();
        let attractions = data_access
            .find_attractions("Paris", AttractionOrdering::ByName, 10)
            .await;

        assert!(!attractions.is_empty());
        assert!(attractions.iter().any(|a| a.location.name == "Paris"));
    }

    #[tokio::test]
    async fn test_stub_data_access_find_artists() {
        let data_access = StubDataAccess::default();
        let artists = data_access
            .find_artists_from_location(&LocationId::new("Q90"), 10)
            .await;

        assert!(artists.is_ok());
        assert!(!artists.unwrap().is_empty());
    }

    #[tokio::test]
    async fn test_stub_data_access_find_movies() {
        let data_access = StubDataAccess::default();
        let movies = data_access
            .find_movies_about_location(&LocationId::new("Q90"), 10)
            .await;

        assert!(movies.is_ok());
        assert!(!movies.unwrap().is_empty());
    }

    #[tokio::test]
    async fn test_stub_data_access_with_error() {
        let data_access = StubDataAccess::new()
            .with_artists_error("Network error");

        let result = data_access
            .find_artists_from_location(&LocationId::new("Q90"), 10)
            .await;

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Network error");
    }

    #[tokio::test]
    async fn test_cached_data_access() {
        let inner = Arc::new(StubDataAccess::default());
        let cached = CachedDataAccess::new(inner);

        let result1 = cached
            .find_attractions("Paris", AttractionOrdering::ByName, 10)
            .await;
        let result2 = cached
            .find_attractions("Paris", AttractionOrdering::ByName, 10)
            .await;

        assert_eq!(result1, result2);
    }

    #[tokio::test]
    async fn test_travel_guide() {
        let data_access = Arc::new(StubDataAccess::default());
        let guide = travel_guide(data_access.as_ref(), "Paris").await;

        assert!(guide.is_some());
        let guide = guide.unwrap();
        assert!(!guide.subjects.is_empty());
        assert!(guide.search_report.errors.is_empty());
    }

    #[tokio::test]
    async fn test_travel_guide_with_errors() {
        let data_access = StubDataAccess::default()
            .with_artists_error("API error");
        let guide = travel_guide(&data_access, "Paris").await;

        assert!(guide.is_some());
        let guide = guide.unwrap();
        assert_eq!(guide.search_report.errors.len(), 1);
    }

    #[tokio::test]
    async fn test_travel_guides() {
        let data_access = Arc::new(StubDataAccess::default());
        let guides = travel_guides(data_access.as_ref(), "Tower", 5).await;

        assert!(!guides.is_empty());
    }

    #[test]
    fn test_filter_popular_locations() {
        let locations = vec![
            Location::new(LocationId::new("1"), "Tokyo", 13_960_000),
            Location::new(LocationId::new("2"), "Small Town", 1_000),
            Location::new(LocationId::new("3"), "Paris", 2_161_000),
        ];

        let popular = filter_popular_locations(&locations, 1_000_000);
        assert_eq!(popular.len(), 2);
    }

    #[test]
    fn test_sort_by_population() {
        let tokyo = Location::new(LocationId::new("1"), "Tokyo", 13_960_000);
        let paris = Location::new(LocationId::new("2"), "Paris", 2_161_000);

        let attractions = vec![
            Attraction::new("Eiffel Tower", None, paris),
            Attraction::new("Tokyo Tower", None, tokyo),
        ];

        let sorted = sort_by_population(&attractions);
        assert_eq!(sorted[0].name, "Tokyo Tower");
    }

    #[test]
    fn test_combine_subjects() {
        let artists = vec!["Artist 1".to_string()];
        let movies = vec!["Movie 1".to_string()];

        let combined = combine_subjects(&artists, &movies);
        assert_eq!(combined.len(), 2);
    }

    #[test]
    fn test_aggregate_reports() {
        let reports = vec![
            SearchReport::new(1, vec!["Error 1".to_string()]),
            SearchReport::new(2, vec![]),
        ];

        let aggregated = aggregate_reports(&reports);
        assert_eq!(aggregated.attractions_searched, 3);
        assert_eq!(aggregated.errors.len(), 1);
    }

    #[test]
    fn test_validate_location_valid() {
        let result = validate_location("Tokyo", 13_960_000);
        assert!(result.is_valid());
    }

    #[test]
    fn test_validate_location_invalid() {
        let result = validate_location("", -100);
        assert!(!result.is_valid());
        if let Validation::Invalid(errors) = result {
            assert_eq!(errors.len(), 2);
        }
    }

    #[test]
    fn test_validate_attraction_valid() {
        let result = validate_attraction("Eiffel Tower", Some("A tower"));
        assert!(result.is_valid());
    }

    #[test]
    fn test_validate_attraction_invalid() {
        let result = validate_attraction("", None);
        assert!(!result.is_valid());
    }

    #[test]
    fn test_validation_map() {
        let valid: Validation<i32> = Validation::valid(42);
        let mapped = valid.map(|x| x * 2);
        assert!(matches!(mapped, Validation::Valid(84)));

        let invalid: Validation<i32> = Validation::invalid(vec!["error".to_string()]);
        let mapped = invalid.map(|x| x * 2);
        assert!(matches!(mapped, Validation::Invalid(_)));
    }
}

// =============================================================================
// 12.9 proptest によるプロパティベーステスト
// =============================================================================

#[cfg(test)]
mod property_tests {
    use super::*;
    use proptest::prelude::*;

    // ロケーションのジェネレータ
    fn location_strategy() -> impl Strategy<Value = Location> {
        (
            "[a-zA-Z0-9]+",  // id
            "[a-zA-Z ]+",    // name
            0..100_000_000i32, // population
        )
            .prop_map(|(id, name, population)| {
                Location::new(LocationId::new(&id), &name, population)
            })
    }

    // アトラクションのジェネレータ
    fn attraction_strategy() -> impl Strategy<Value = Attraction> {
        (
            "[a-zA-Z ]+",           // name
            proptest::option::of("[a-zA-Z ]+"), // description
            location_strategy(),    // location
        )
            .prop_map(|(name, description, location)| {
                Attraction::new(&name, description.as_deref(), location)
            })
    }

    proptest! {
        #[test]
        fn filter_popular_locations_result_size(
            locations in proptest::collection::vec(location_strategy(), 0..100),
            min_pop in 0..100_000_000i32
        ) {
            let result = filter_popular_locations(&locations, min_pop);
            prop_assert!(result.len() <= locations.len());
        }

        #[test]
        fn filter_popular_locations_all_meet_criteria(
            locations in proptest::collection::vec(location_strategy(), 0..100),
            min_pop in 0..100_000_000i32
        ) {
            let result = filter_popular_locations(&locations, min_pop);
            prop_assert!(result.iter().all(|l| l.population >= min_pop));
        }

        #[test]
        fn sort_by_population_preserves_elements(
            attractions in proptest::collection::vec(attraction_strategy(), 0..50)
        ) {
            let sorted = sort_by_population(&attractions);
            prop_assert_eq!(sorted.len(), attractions.len());
        }

        #[test]
        fn sort_by_population_is_sorted(
            attractions in proptest::collection::vec(attraction_strategy(), 0..50)
        ) {
            let sorted = sort_by_population(&attractions);
            for i in 1..sorted.len() {
                prop_assert!(sorted[i - 1].location.population >= sorted[i].location.population);
            }
        }

        #[test]
        fn combine_subjects_length(
            artists in proptest::collection::vec("[a-zA-Z ]+", 0..10),
            movies in proptest::collection::vec("[a-zA-Z ]+", 0..10)
        ) {
            let combined = combine_subjects(&artists, &movies);
            prop_assert_eq!(combined.len(), artists.len() + movies.len());
        }

        #[test]
        fn aggregate_reports_sum(
            reports in proptest::collection::vec(
                (0..100i32, proptest::collection::vec("[a-zA-Z]+", 0..5))
                    .prop_map(|(count, errors)| SearchReport::new(count, errors)),
                0..10
            )
        ) {
            let aggregated = aggregate_reports(&reports);
            let expected_count: i32 = reports.iter().map(|r| r.attractions_searched).sum();
            prop_assert_eq!(aggregated.attractions_searched, expected_count);
        }

        #[test]
        fn validate_location_empty_name_is_invalid(
            population in 0..100_000_000i32
        ) {
            let result = validate_location("", population);
            prop_assert!(!result.is_valid());
        }

        #[test]
        fn validate_location_negative_population_is_invalid(
            name in "[a-zA-Z]+",
            population in -100_000_000i32..-1
        ) {
            let result = validate_location(&name, population);
            prop_assert!(!result.is_valid());
        }

        #[test]
        fn validate_location_valid_inputs(
            name in "[a-zA-Z]+",
            population in 0..100_000_000i32
        ) {
            let result = validate_location(&name, population);
            prop_assert!(result.is_valid());
        }
    }
}
