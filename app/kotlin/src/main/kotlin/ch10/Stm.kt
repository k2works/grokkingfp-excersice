package ch10

import arrow.fx.coroutines.parMap
import arrow.fx.stm.STM
import arrow.fx.stm.TVar
import arrow.fx.stm.atomically
import arrow.fx.stm.check
import arrow.fx.stm.stm
import kotlinx.coroutines.Dispatchers
import kotlin.coroutines.CoroutineContext

/**
 * 第10章: STM（Software Transactional Memory）
 *
 * 複数の共有状態をまとめてアトミックに更新する例です。
 */

// ============================================
// チェックインとランキングの同時更新
// ============================================

/** チェックイン数とランキングのスナップショット */
data class CheckInSnapshot(
    val checkIns: Map<City, Int>,
    val ranking: List<CityStats>,
)

/** チェックイン数とランキングを別々の TVar で保持するストア */
class CheckInStore private constructor(
    val checkIns: TVar<Map<City, Int>>,
    val ranking: TVar<List<CityStats>>,
    val topN: Int,
) {
    /** 2 つの TVar を同じトランザクションで読み、一貫したスナップショットを返す */
    suspend fun snapshot(): CheckInSnapshot = atomically {
        CheckInSnapshot(checkIns.read(), ranking.read())
    }

    companion object {
        suspend fun create(topN: Int): CheckInStore =
            CheckInStore(TVar.new(emptyMap()), TVar.new(emptyList()), topN)
    }
}

/** チェックインを 1 件反映し、ランキングも同じトランザクションで更新する */
fun STM.recordCheckIn(store: CheckInStore, city: City) {
    val updated = updateCheckIns(store.checkIns.read(), city)
    store.checkIns.write(updated)
    store.ranking.write(topCities(updated, store.topN))
}

suspend fun storeCheckInStm(store: CheckInStore, city: City) =
    atomically { recordCheckIn(store, city) }

/** すべてのチェックインを並列に保存し、最終スナップショットを返す */
suspend fun processCheckInsStm(
    checkIns: List<City>,
    topN: Int,
    context: CoroutineContext = Dispatchers.Default,
): CheckInSnapshot {
    val store = CheckInStore.create(topN)
    checkIns.parMap(context) { city -> storeCheckInStm(store, city) }
    return store.snapshot()
}

// ============================================
// 口座間の送金（retry と orElse）
// ============================================

/** 残高が足りるまで待ってから送金する STM 操作 */
fun STM.transferStm(from: TVar<Int>, to: TVar<Int>, amount: Int) {
    val balance = from.read()
    check(balance >= amount) // false なら retry: from が変わるまで待機して再実行
    from.write(balance - amount)
    to.modify { it + amount }
}

/** 残高不足なら入金されるまで待機する送金 */
suspend fun transfer(from: TVar<Int>, to: TVar<Int>, amount: Int) =
    atomically { transferStm(from, to, amount) }

/** 残高不足なら待たずに false を返す送金 */
suspend fun tryTransfer(from: TVar<Int>, to: TVar<Int>, amount: Int): Boolean =
    atomically {
        stm { transferStm(from, to, amount); true } orElse { false }
    }

suspend fun deposit(account: TVar<Int>, amount: Int) =
    atomically { account.modify { it + amount } }

/** 口座番号で指定する送金 */
data class Transfer(val from: Int, val to: Int, val amount: Int)

/** 送金を並列に実行する */
suspend fun runTransfersConcurrently(
    accounts: List<TVar<Int>>,
    transfers: List<Transfer>,
    context: CoroutineContext = Dispatchers.Default,
) {
    transfers.parMap(context) { t -> transfer(accounts[t.from], accounts[t.to], t.amount) }
}
