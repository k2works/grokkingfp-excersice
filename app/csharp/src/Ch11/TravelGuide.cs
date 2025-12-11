using LanguageExt;
using static LanguageExt.Prelude;
using Ch10;

namespace Ch11;

/// <summary>
/// 第11章: 実践的なアプリケーション構築
/// TravelGuide アプリケーションの実装
/// </summary>
public static class TravelGuideApp
{
    // ============================================
    // ドメインモデル
    // ============================================

    /// <summary>
    /// ロケーション ID（値オブジェクト）
    /// </summary>
    public readonly record struct LocationId(string Value)
    {
        public static LocationId Create(string id) => new(id);
        public override string ToString() => Value;
    }

    /// <summary>
    /// ロケーション
    /// </summary>
    public record Location(LocationId Id, string Name, int Population);

    /// <summary>
    /// アトラクション（観光地）
    /// </summary>
    public record Attraction(string Name, Option<string> Description, Location Location);

    /// <summary>
    /// ミュージックアーティスト
    /// </summary>
    public record MusicArtist(string Name, Option<string> Genre);

    /// <summary>
    /// 映画
    /// </summary>
    public record Movie(string Name, Option<int> BoxOffice);

    /// <summary>
    /// 旅行ガイド
    /// </summary>
    public record TravelGuide(Attraction Attraction, Seq<string> Subjects);

    /// <summary>
    /// アトラクションのソート順
    /// </summary>
    public enum AttractionOrdering
    {
        ByName,
        ByLocationPopulation
    }

    // ============================================
    // データアクセス層の抽象化
    // ============================================

    /// <summary>
    /// データアクセスインターフェース
    /// </summary>
    public interface IDataAccess
    {
        Task<Seq<Attraction>> FindAttractions(string name, AttractionOrdering ordering, int limit);
        Task<Seq<MusicArtist>> FindArtistsFromLocation(LocationId locationId, int limit);
        Task<Seq<Movie>> FindMoviesAboutLocation(LocationId locationId, int limit);
    }

    // ============================================
    // リソース管理
    // ============================================

    /// <summary>
    /// リソースを安全に管理するための型
    /// </summary>
    public class Resource<T>
    {
        private readonly Func<Task<T>> _acquire;
        private readonly Func<T, Task> _release;

        private Resource(Func<Task<T>> acquire, Func<T, Task> release)
        {
            _acquire = acquire;
            _release = release;
        }

        /// <summary>
        /// リソースを作成
        /// </summary>
        public static Resource<T> Make(Func<Task<T>> acquire, Func<T, Task> release)
            => new(acquire, release);

        /// <summary>
        /// リソースを使用して操作を実行
        /// </summary>
        public async Task<TResult> UseAsync<TResult>(Func<T, Task<TResult>> f)
        {
            var resource = await _acquire();
            try
            {
                return await f(resource);
            }
            finally
            {
                await _release(resource);
            }
        }

        /// <summary>
        /// リソースを使用して操作を実行（同期版）
        /// </summary>
        public async Task<TResult> Use<TResult>(Func<T, TResult> f)
        {
            var resource = await _acquire();
            try
            {
                return f(resource);
            }
            finally
            {
                await _release(resource);
            }
        }
    }

    /// <summary>
    /// Resource 用の静的ヘルパー
    /// </summary>
    public static class Resource
    {
        public static Resource<T> Make<T>(Func<Task<T>> acquire, Func<T, Task> release)
            => Resource<T>.Make(acquire, release);
    }

    // ============================================
    // キャッシュの実装
    // ============================================

    /// <summary>
    /// キャッシュ付きデータアクセスを作成
    /// </summary>
    public static IDataAccess CachedDataAccess(IDataAccess dataAccess)
    {
        var attractionCache = ConcurrentProcessing.Ref<Map<string, Seq<Attraction>>>.Of(Map<string, Seq<Attraction>>());
        var artistCache = ConcurrentProcessing.Ref<Map<string, Seq<MusicArtist>>>.Of(Map<string, Seq<MusicArtist>>());
        var movieCache = ConcurrentProcessing.Ref<Map<string, Seq<Movie>>>.Of(Map<string, Seq<Movie>>());

        return new CachedDataAccessImpl(dataAccess, attractionCache, artistCache, movieCache);
    }

    private class CachedDataAccessImpl : IDataAccess
    {
        private readonly IDataAccess _dataAccess;
        private readonly ConcurrentProcessing.Ref<Map<string, Seq<Attraction>>> _attractionCache;
        private readonly ConcurrentProcessing.Ref<Map<string, Seq<MusicArtist>>> _artistCache;
        private readonly ConcurrentProcessing.Ref<Map<string, Seq<Movie>>> _movieCache;

        public CachedDataAccessImpl(
            IDataAccess dataAccess,
            ConcurrentProcessing.Ref<Map<string, Seq<Attraction>>> attractionCache,
            ConcurrentProcessing.Ref<Map<string, Seq<MusicArtist>>> artistCache,
            ConcurrentProcessing.Ref<Map<string, Seq<Movie>>> movieCache)
        {
            _dataAccess = dataAccess;
            _attractionCache = attractionCache;
            _artistCache = artistCache;
            _movieCache = movieCache;
        }

        public async Task<Seq<Attraction>> FindAttractions(string name, AttractionOrdering ordering, int limit)
        {
            var key = $"{name}-{ordering}-{limit}";
            var cached = _attractionCache.Get().Find(key);
            if (cached.IsSome)
            {
                return cached.IfNone(Seq<Attraction>());
            }

            var attractions = await _dataAccess.FindAttractions(name, ordering, limit);
            _attractionCache.Update(cache => cache.Add(key, attractions));
            return attractions;
        }

        public async Task<Seq<MusicArtist>> FindArtistsFromLocation(LocationId locationId, int limit)
        {
            var key = $"{locationId.Value}-{limit}";
            var cached = _artistCache.Get().Find(key);
            if (cached.IsSome)
            {
                return cached.IfNone(Seq<MusicArtist>());
            }

            var artists = await _dataAccess.FindArtistsFromLocation(locationId, limit);
            _artistCache.Update(cache => cache.Add(key, artists));
            return artists;
        }

        public async Task<Seq<Movie>> FindMoviesAboutLocation(LocationId locationId, int limit)
        {
            var key = $"{locationId.Value}-{limit}";
            var cached = _movieCache.Get().Find(key);
            if (cached.IsSome)
            {
                return cached.IfNone(Seq<Movie>());
            }

            var movies = await _dataAccess.FindMoviesAboutLocation(locationId, limit);
            _movieCache.Update(cache => cache.Add(key, movies));
            return movies;
        }
    }

    // ============================================
    // アプリケーションロジック
    // ============================================

    /// <summary>
    /// 旅行ガイドを生成
    /// </summary>
    public static async Task<Option<TravelGuide>> GetTravelGuide(IDataAccess data, string attractionName)
    {
        var attractions = await data.FindAttractions(attractionName, AttractionOrdering.ByLocationPopulation, 1);
        if (attractions.IsEmpty)
        {
            return None;
        }

        var attraction = attractions.First();
        var artists = await data.FindArtistsFromLocation(attraction.Location.Id, 2);
        var movies = await data.FindMoviesAboutLocation(attraction.Location.Id, 2);

        var subjects = artists.Map(a => a.Name).Concat(movies.Map(m => m.Name));

        return Some(new TravelGuide(attraction, subjects));
    }

    /// <summary>
    /// 複数のアトラクションから旅行ガイドを生成
    /// </summary>
    public static async Task<Seq<TravelGuide>> GetTravelGuides(IDataAccess data, string attractionName, int limit)
    {
        var attractions = await data.FindAttractions(attractionName, AttractionOrdering.ByLocationPopulation, limit);

        var guideTasks = attractions.Map(async attraction =>
        {
            var artists = await data.FindArtistsFromLocation(attraction.Location.Id, 2);
            var movies = await data.FindMoviesAboutLocation(attraction.Location.Id, 2);
            var subjects = artists.Map(a => a.Name).Concat(movies.Map(m => m.Name));
            return new TravelGuide(attraction, subjects);
        });

        var guides = await Task.WhenAll(guideTasks);
        return toSeq(guides);
    }

    // ============================================
    // テスト用のスタブ実装
    // ============================================

    /// <summary>
    /// テスト用のデータアクセス実装
    /// </summary>
    public static IDataAccess TestDataAccess(
        Seq<Attraction> testAttractions,
        Seq<MusicArtist> testArtists,
        Seq<Movie> testMovies)
    {
        return new TestDataAccessImpl(testAttractions, testArtists, testMovies);
    }

    private class TestDataAccessImpl : IDataAccess
    {
        private readonly Seq<Attraction> _attractions;
        private readonly Seq<MusicArtist> _artists;
        private readonly Seq<Movie> _movies;

        public TestDataAccessImpl(Seq<Attraction> attractions, Seq<MusicArtist> artists, Seq<Movie> movies)
        {
            _attractions = attractions;
            _artists = artists;
            _movies = movies;
        }

        public Task<Seq<Attraction>> FindAttractions(string name, AttractionOrdering ordering, int limit)
        {
            var filtered = _attractions
                .Filter(a => a.Name.Contains(name))
                .Take(limit);
            return Task.FromResult(filtered);
        }

        public Task<Seq<MusicArtist>> FindArtistsFromLocation(LocationId locationId, int limit)
        {
            return Task.FromResult(_artists.Take(limit));
        }

        public Task<Seq<Movie>> FindMoviesAboutLocation(LocationId locationId, int limit)
        {
            return Task.FromResult(_movies.Take(limit));
        }
    }

    // ============================================
    // サンプルデータ
    // ============================================

    public static Location SampleLocation => new(
        LocationId.Create("Q123"),
        "Tokyo",
        14000000
    );

    public static Attraction SampleAttraction => new(
        "Tokyo Tower",
        Some("Famous landmark in Tokyo"),
        SampleLocation
    );

    public static MusicArtist SampleArtist => new(
        "Hikaru Utada",
        Some("Pop")
    );

    public static Movie SampleMovie => new(
        "Lost in Translation",
        Some(44000000)
    );
}
