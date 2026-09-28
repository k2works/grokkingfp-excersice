package ch07

import ch07.Payment.BankTransfer
import ch07.Payment.Cash
import ch07.Payment.CreditCard
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.doubles.plusOrMinus
import io.kotest.matchers.shouldBe

class PaymentMethodTest : FunSpec({

    val creditCard = CreditCard("1234-5678-9012-3456", "12/25")
    val bankTransfer = BankTransfer("9876543210")

    test("describePayment は支払い方法を説明する") {
        describePayment(creditCard) shouldBe "Credit card ending in 3456"
        describePayment(bankTransfer) shouldBe "Bank transfer to account 9876543210"
        describePayment(Cash) shouldBe "Cash payment"
    }

    test("describePayment は短いカード番号でもそのまま扱える") {
        describePayment(CreditCard("12", "01/30")) shouldBe "Credit card ending in 12"
    }

    test("calculateFee は支払い方法ごとの手数料を返す") {
        calculateFee(creditCard, 10000.0) shouldBe (300.0 plusOrMinus 0.001)
        calculateFee(bankTransfer, 10000.0) shouldBe 500.0
        calculateFee(Cash, 10000.0) shouldBe 0.0
    }

    test("isOnlinePaymentAvailable は現金以外で true を返す") {
        isOnlinePaymentAvailable(creditCard) shouldBe true
        isOnlinePaymentAvailable(bankTransfer) shouldBe true
        isOnlinePaymentAvailable(Cash) shouldBe false
    }
})
