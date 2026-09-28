package ch11

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.update

/**
 * 第11章: キャッシュ付き DataAccess
 *
 * MutableStateFlow にイミュータブルな Map を持たせ、update でアトミックに差し替えます。
 * Scala 版の Ref[IO, Map[K, V]] に相当します。
 */

/** キー K と値 V を型安全に保持する小さなキャッシュ */
class Cache<K, V> {
    private val entries = MutableStateFlow<Map<K, V>>(emptyMap())

    /** キャッシュにあればそれを返し、なければ fetch の結果を保存してから返す */
    suspend fun getOrFetch(key: K, fetch: suspend () -> V): V =
        entries.value[key] ?: fetch().also { value -> entries.update { it + (key to value) } }

    val size: Int get() = entries.value.size

    fun clear() {
        entries.value = emptyMap()
    }
}

class CachedDataAccess private constructor(private val underlying: DataAccess) : DataAccess {

    private data class AttractionsQuery(val name: String, val ordering: AttractionOrdering, val limit: Int)
    private data class LocationQuery(val locationId: LocationId, val limit: Int)

    private val attractions = Cache<AttractionsQuery, List<Attraction>>()
    private val artists = Cache<LocationQuery, List<Artist>>()
    private val movies = Cache<LocationQuery, List<Movie>>()

    override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int): List<Attraction> =
        attractions.getOrFetch(AttractionsQuery(name, ordering, limit)) {
            underlying.findAttractions(name, ordering, limit)
        }

    override suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int): List<Artist> =
        artists.getOrFetch(LocationQuery(locationId, limit)) {
            underlying.findArtistsFromLocation(locationId, limit)
        }

    override suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int): List<Movie> =
        movies.getOrFetch(LocationQuery(locationId, limit)) {
            underlying.findMoviesAboutLocation(locationId, limit)
        }

    /** キャッシュされているエントリの総数 */
    fun cacheSize(): Int = attractions.size + artists.size + movies.size

    /** すべてのキャッシュを空にする */
    fun clearCache() {
        attractions.clear()
        artists.clear()
        movies.clear()
    }

    companion object {
        fun create(underlying: DataAccess): CachedDataAccess = CachedDataAccess(underlying)
    }
}
