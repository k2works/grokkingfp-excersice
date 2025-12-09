package ch11;

import ch08.IO;
import io.vavr.collection.List;

import static ch11.TravelGuide.*;

/**
 * 第11章: DataAccess インターフェース
 *
 * 外部データソースへのアクセスを抽象化するインターフェースです。
 * すべての操作は IO でラップされ、副作用は遅延されます。
 */
public interface DataAccess {

    /**
     * アトラクションを検索
     *
     * @param name     検索名
     * @param ordering 並び順
     * @param limit    最大件数
     * @return アトラクションのリスト（IO でラップ）
     */
    IO<List<Attraction>> findAttractions(
            String name,
            AttractionOrdering ordering,
            int limit
    );

    /**
     * 指定されたロケーション出身のアーティストを検索
     *
     * @param locationId ロケーション ID
     * @param limit      最大件数
     * @return アーティストのリスト（IO でラップ）
     */
    IO<List<Artist>> findArtistsFromLocation(
            LocationId locationId,
            int limit
    );

    /**
     * 指定されたロケーションを舞台とする映画を検索
     *
     * @param locationId ロケーション ID
     * @param limit      最大件数
     * @return 映画のリスト（IO でラップ）
     */
    IO<List<Movie>> findMoviesAboutLocation(
            LocationId locationId,
            int limit
    );
}
