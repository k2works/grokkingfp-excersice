package ch11

import arrow.core.Either
import arrow.fx.coroutines.Resource
import arrow.fx.coroutines.resource
import arrow.fx.coroutines.use

/**
 * 第11章: Arrow Resource によるリソース管理
 *
 * Resource<A> は `suspend ResourceScope.() -> A` の型エイリアスです。
 * install(acquire, release) で取得と解放の組を登録すると、
 * use / resourceScope を抜けるときに（成功・失敗・キャンセルを問わず）必ず解放されます。
 */

/** 外部データソースへの接続（本物の SPARQL 接続の代わり） */
class Connection(val address: String) : AutoCloseable {
    @Volatile
    var isOpen: Boolean = true
        private set

    /** 開いている接続でのみクエリを実行できる */
    suspend fun <A> query(run: suspend () -> A): A {
        check(isOpen) { "connection to $address is closed" }
        return run()
    }

    override fun close() {
        isOpen = false
    }
}

/** 接続を取得し、使い終わったら閉じる Resource */
fun connectionResource(address: String): Resource<Connection> = resource {
    install({ Connection(address) }) { connection, _ -> connection.close() }
}

/**
 * 接続とキャッシュを組み合わせた DataAccess の Resource
 *
 * makeDataAccess で「接続から DataAccess を作る方法」を差し替えられるので、
 * 本番では SPARQL 実装、テストではスタブを渡せます。
 */
fun dataAccessResource(address: String, makeDataAccess: (Connection) -> DataAccess): Resource<DataAccess> =
    resource {
        val connection = connectionResource(address).bind()
        CachedDataAccess.create(makeDataAccess(connection))
    }

/** リソースの取得からガイドの検索、解放までを 1 つの suspend 関数にまとめる */
suspend fun runTravelGuide(
    address: String,
    makeDataAccess: (Connection) -> DataAccess,
    attractionName: String,
): Either<SearchReport, Guide> =
    dataAccessResource(address, makeDataAccess).use { dataAccess ->
        travelGuideV3(dataAccess, attractionName)
    }
