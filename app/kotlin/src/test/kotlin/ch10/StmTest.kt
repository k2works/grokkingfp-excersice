@file:OptIn(ExperimentalCoroutinesApi::class)

package ch10

import arrow.fx.stm.TVar
import arrow.fx.stm.atomically
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.async
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest

class StmTest : FunSpec({

    val sydney = City("Sydney")
    val dublin = City("Dublin")

    context("チェックインとランキングの同時更新") {
        test("storeCheckInStm はチェックイン数とランキングを 1 つのトランザクションで更新する") {
            val store = CheckInStore.create(topN = 3)
            storeCheckInStm(store, sydney)
            storeCheckInStm(store, sydney)
            storeCheckInStm(store, dublin)

            store.snapshot() shouldBe CheckInSnapshot(
                checkIns = mapOf(sydney to 2, dublin to 1),
                ranking = listOf(CityStats(sydney, 2), CityStats(dublin, 1)),
            )
        }

        test("並列に保存しても件数が失われず、ランキングは常にチェックイン数と一致する") {
            val checkIns = sampleCheckIns(200)
            val snapshot = processCheckInsStm(checkIns, topN = 3, context = Dispatchers.Default)

            snapshot.checkIns.values.sum() shouldBe 1000
            snapshot.ranking shouldBe topCities(snapshot.checkIns, 3)
        }
    }

    context("口座間の送金") {
        test("transfer は残高を移動し、合計金額は変わらない") {
            val from = TVar.new(100)
            val to = TVar.new(0)

            transfer(from, to, 30)

            atomically { from.read() to to.read() } shouldBe (70 to 30)
        }

        test("残高不足の transfer は入金されるまで待機する（retry）") {
            runTest {
                val from = TVar.new(10)
                val to = TVar.new(0)

                val pending = async { transfer(from, to, 50) }
                runCurrent()
                pending.isCompleted shouldBe false

                deposit(from, 40)
                pending.await()

                atomically { from.read() to to.read() } shouldBe (0 to 50)
            }
        }

        test("tryTransfer は残高不足なら待たずに false を返す（orElse）") {
            val from = TVar.new(10)
            val to = TVar.new(0)

            tryTransfer(from, to, 50) shouldBe false
            atomically { from.read() to to.read() } shouldBe (10 to 0)
        }

        test("tryTransfer は残高が足りれば送金して true を返す") {
            val from = TVar.new(100)
            val to = TVar.new(0)

            tryTransfer(from, to, 50) shouldBe true
            atomically { from.read() to to.read() } shouldBe (50 to 50)
        }

        test("並行して送金を繰り返しても口座の合計金額は保存される") {
            val accounts = List(3) { TVar.new(1000) }
            val transfers = List(300) { i -> Transfer(from = i % 3, to = (i + 1) % 3, amount = 7) }

            runTransfersConcurrently(accounts, transfers, Dispatchers.Default)

            atomically { accounts.sumOf { it.read() } } shouldBe 3000
        }
    }
})
