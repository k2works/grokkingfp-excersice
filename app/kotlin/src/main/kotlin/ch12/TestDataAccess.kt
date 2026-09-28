package ch12

import ch11.Artist
import ch11.Attraction
import ch11.AttractionOrdering
import ch11.DataAccess
import ch11.LocationId
import ch11.Movie

/**
 * 第12章: テスト用 DataAccess スタブ
 *
 * 各メソッドの振る舞いを suspend ラムダで受け取ります。
 * 名前付き引数とデフォルト引数があるので、Builder パターンは不要です。
 */
class TestDataAccess(
    private val attractions: suspend (name: String) -> List<Attraction> = { emptyList() },
    private val artists: suspend (locationId: LocationId) -> List<Artist> = { emptyList() },
    private val movies: suspend (locationId: LocationId) -> List<Movie> = { emptyList() },
) : DataAccess {

    override suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int): List<Attraction> =
        attractions(name).take(limit)

    override suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int): List<Artist> =
        artists(locationId).take(limit)

    override suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int): List<Movie> =
        movies(locationId).take(limit)

    companion object {
        /** 固定のデータを返すスタブを作る */
        fun of(
            attractions: List<Attraction> = emptyList(),
            artists: List<Artist> = emptyList(),
            movies: List<Movie> = emptyList(),
        ): TestDataAccess = TestDataAccess({ attractions }, { artists }, { movies })
    }
}

/** 外部データソースの失敗をシミュレートする */
fun failWith(message: String): Nothing = throw RuntimeException(message)
