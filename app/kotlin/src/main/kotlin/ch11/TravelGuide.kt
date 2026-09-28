package ch11

import arrow.core.Either
import arrow.core.left
import arrow.core.raise.either
import arrow.core.right
import arrow.core.separateEither
import arrow.fx.coroutines.parMap
import arrow.fx.coroutines.parZip

/**
 * 第11章: TravelGuide アプリケーション
 *
 * ドメインモデル（data class / value class / sealed interface）と、
 * ガイドのスコア計算や組み立てを行う関数を定義します。
 */

// ============================================
// ドメインモデル
// ============================================

/** ロケーション ID（実行時には String として扱われる value class） */
@JvmInline
value class LocationId(val value: String)

/** ロケーション（場所） */
data class Location(val id: LocationId, val name: String, val population: Int)

/** アトラクション（観光地）。説明は存在しないこともあるので nullable 型 */
data class Attraction(val name: String, val description: String?, val location: Location)

/** ポップカルチャーの題材 */
sealed interface PopCultureSubject {
    val name: String
}

/** アーティスト */
data class Artist(override val name: String, val followers: Int) : PopCultureSubject

/** 映画 */
data class Movie(override val name: String, val boxOffice: Int) : PopCultureSubject

/** 旅行ガイド */
data class Guide(val attraction: Attraction, val subjects: List<PopCultureSubject>)

/** 検索レポート（良いガイドが見つからなかった理由を報告する） */
data class SearchReport(val badGuides: List<Guide>, val problems: List<String>)

/** アトラクションの並び順 */
enum class AttractionOrdering { ByName, ByLocationPopulation }

// ============================================
// ビジネスロジック（純粋関数）
// ============================================

const val GOOD_GUIDE_MIN_SCORE = 55

/**
 * ガイドのスコアを計算する
 *
 * - 30 点: 説明がある場合
 * - 10 点/件: アーティストまたは映画（最大 40 点）
 * - 1 点/100,000 フォロワー: 全アーティスト合計（最大 15 点）
 * - 1 点/10,000,000 ドル: 全映画の興行収入合計（最大 15 点）
 */
fun guideScore(guide: Guide): Int {
    val descriptionScore = if (guide.attraction.description != null) 30 else 0
    val quantityScore = minOf(40, guide.subjects.size * 10)

    // Long で合計してオーバーフローを防ぐ
    val totalFollowers = guide.subjects.filterIsInstance<Artist>().sumOf { it.followers.toLong() }
    val totalBoxOffice = guide.subjects.filterIsInstance<Movie>().sumOf { it.boxOffice.toLong() }

    val followersScore = minOf(15L, totalFollowers / 100_000).toInt()
    val boxOfficeScore = minOf(15L, totalBoxOffice / 10_000_000).toInt()

    return descriptionScore + quantityScore + followersScore + boxOfficeScore
}

/** 最高スコアのガイドを選ぶ（空なら null） */
fun findBestGuide(guides: List<Guide>): Guide? = guides.maxByOrNull(::guideScore)

/** 最高スコアのガイドが閾値を超えていれば Right、そうでなければ SearchReport を Left で返す */
fun findGoodGuide(guides: List<Guide>, problems: List<String> = emptyList()): Either<SearchReport, Guide> =
    findBestGuide(guides)
        ?.takeIf { guideScore(it) > GOOD_GUIDE_MIN_SCORE }
        ?.right()
        ?: SearchReport(guides, problems).left()

/** 成功したガイドと失敗の理由を分けてから findGoodGuide に渡す */
fun findGoodGuide(results: List<Either<Throwable, Guide>>): Either<SearchReport, Guide> {
    val (errors, guides) = results.separateEither()
    return findGoodGuide(guides, errors.map { it.message ?: it.toString() })
}

// ============================================
// アプリケーションの組み立て（suspend 関数）
// ============================================

/** バージョン 1: 最初のアトラクションだけでガイドを作る */
suspend fun travelGuideV1(dataAccess: DataAccess, attractionName: String): Guide? {
    val attraction = dataAccess
        .findAttractions(attractionName, AttractionOrdering.ByLocationPopulation, 1)
        .firstOrNull() ?: return null
    val artists = dataAccess.findArtistsFromLocation(attraction.location.id, 2)
    val movies = dataAccess.findMoviesAboutLocation(attraction.location.id, 2)
    return Guide(attraction, artists + movies)
}

/** バージョン 2: 3 件の候補からガイドを作り、最高スコアのものを選ぶ */
suspend fun travelGuideV2(dataAccess: DataAccess, attractionName: String): Guide? {
    val attractions = dataAccess.findAttractions(attractionName, AttractionOrdering.ByLocationPopulation, 3)
    val guides = attractions.map { attraction ->
        val artists = dataAccess.findArtistsFromLocation(attraction.location.id, 2)
        val movies = dataAccess.findMoviesAboutLocation(attraction.location.id, 2)
        Guide(attraction, artists + movies)
    }
    return findBestGuide(guides)
}

/** アーティストと映画を並列に取得してガイドを作る */
suspend fun guideForAttraction(dataAccess: DataAccess, attraction: Attraction): Guide =
    parZip(
        { dataAccess.findArtistsFromLocation(attraction.location.id, 2) },
        { dataAccess.findMoviesAboutLocation(attraction.location.id, 2) },
    ) { artists, movies -> Guide(attraction, artists + movies) }

/**
 * バージョン 3: 並列実行とエラーハンドリング
 *
 * - アトラクションの取得に失敗したら、その理由を SearchReport で返す
 * - 各アトラクションのガイド作成は並列に行い、個別の失敗は problems に集める
 */
suspend fun travelGuideV3(dataAccess: DataAccess, attractionName: String): Either<SearchReport, Guide> =
    either {
        val attractions = Either
            .catch { dataAccess.findAttractions(attractionName, AttractionOrdering.ByLocationPopulation, 3) }
            .mapLeft { SearchReport(emptyList(), listOf(it.message ?: it.toString())) }
            .bind()
        val results = attractions.parMap { Either.catch { guideForAttraction(dataAccess, it) } }
        findGoodGuide(results).bind()
    }
