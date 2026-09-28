package ch11

/**
 * 第11章: DataAccess インターフェース
 *
 * 外部データソースへのアクセスを抽象化します。
 * すべての操作は suspend 関数であり、「副作用を伴う処理」であることが型に現れます。
 */
interface DataAccess {
    /** アトラクションを検索する */
    suspend fun findAttractions(name: String, ordering: AttractionOrdering, limit: Int): List<Attraction>

    /** 指定されたロケーション出身のアーティストを検索する */
    suspend fun findArtistsFromLocation(locationId: LocationId, limit: Int): List<Artist>

    /** 指定されたロケーションを舞台とする映画を検索する */
    suspend fun findMoviesAboutLocation(locationId: LocationId, limit: Int): List<Movie>
}
