package ch11;

import ch08.IO;
import ch10.Ref;
import io.vavr.collection.HashMap;
import io.vavr.collection.List;
import io.vavr.collection.Map;

import static ch11.TravelGuide.*;

/**
 * 第11章: キャッシュ付き DataAccess
 *
 * Ref を使用してスレッドセーフなキャッシュを実装します。
 */
public final class CachedDataAccess implements DataAccess {

    private final DataAccess underlying;
    private final Ref<Map<String, Object>> cache;

    private CachedDataAccess(DataAccess underlying, Ref<Map<String, Object>> cache) {
        this.underlying = underlying;
        this.cache = cache;
    }

    /**
     * キャッシュ付き DataAccess を作成
     */
    public static IO<DataAccess> create(DataAccess underlying) {
        return Ref.<Map<String, Object>>of(HashMap.empty())
                .map(cache -> new CachedDataAccess(underlying, cache));
    }

    /**
     * キャッシュ付き DataAccess を直接作成（テスト用）
     */
    public static DataAccess createUnsafe(DataAccess underlying) {
        return new CachedDataAccess(underlying, Ref.unsafe(HashMap.empty()));
    }

    @Override
    public IO<List<Attraction>> findAttractions(
            String name,
            AttractionOrdering ordering,
            int limit) {

        String key = "attractions:" + name + ":" + ordering + ":" + limit;
        return getCached(key, () -> underlying.findAttractions(name, ordering, limit));
    }

    @Override
    public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
        String key = "artists:" + locationId.value() + ":" + limit;
        return getCached(key, () -> underlying.findArtistsFromLocation(locationId, limit));
    }

    @Override
    public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
        String key = "movies:" + locationId.value() + ":" + limit;
        return getCached(key, () -> underlying.findMoviesAboutLocation(locationId, limit));
    }

    @SuppressWarnings("unchecked")
    private <A> IO<A> getCached(String key, java.util.function.Supplier<IO<A>> fetch) {
        return cache.get().flatMap(c ->
                c.get(key)
                        .map(cached -> IO.pure((A) cached))
                        .getOrElse(() ->
                                fetch.get().flatMap(result ->
                                        cache.update(m -> m.put(key, result))
                                                .andThen(IO.pure(result))
                                )
                        )
        );
    }

    /**
     * キャッシュをクリア
     */
    public IO<Void> clearCache() {
        return cache.set(HashMap.empty());
    }

    /**
     * キャッシュのサイズを取得
     */
    public IO<Integer> cacheSize() {
        return cache.get().map(Map::size);
    }
}
