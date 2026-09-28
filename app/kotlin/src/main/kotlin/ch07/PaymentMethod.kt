package ch07

import ch07.Payment.BankTransfer
import ch07.Payment.Cash
import ch07.Payment.CreditCard

/**
 * 第7章: 支払い方法を表す ADT
 */

private const val CREDIT_CARD_FEE_RATE = 0.03
private const val BANK_TRANSFER_FEE = 500.0
private const val CARD_DIGITS_SHOWN = 4

sealed interface Payment {
    data class CreditCard(val number: String, val expiry: String) : Payment
    data class BankTransfer(val accountNumber: String) : Payment
    data object Cash : Payment
}

fun describePayment(method: Payment): String =
    when (method) {
        is CreditCard -> "Credit card ending in ${method.number.takeLast(CARD_DIGITS_SHOWN)}"
        is BankTransfer -> "Bank transfer to account ${method.accountNumber}"
        Cash -> "Cash payment"
    }

fun calculateFee(method: Payment, amount: Double): Double =
    when (method) {
        is CreditCard -> amount * CREDIT_CARD_FEE_RATE
        is BankTransfer -> BANK_TRANSFER_FEE
        Cash -> 0.0
    }

fun isOnlinePaymentAvailable(method: Payment): Boolean =
    when (method) {
        is CreditCard, is BankTransfer -> true
        Cash -> false
    }
