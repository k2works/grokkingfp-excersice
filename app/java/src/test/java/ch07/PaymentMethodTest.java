package ch07;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static ch07.PaymentMethod.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;

/**
 * 第7章: 支払い方法のテスト
 */
@DisplayName("第7章: 支払い方法")
class PaymentMethodTest {

    private final Payment creditCard = new CreditCard("1234-5678-9012-3456", "12/25");
    private final Payment bankTransfer = new BankTransfer("9876543210");
    private final Payment cash = new Cash();

    @Nested
    @DisplayName("describePayment")
    class DescribePaymentTest {

        @Test
        @DisplayName("クレジットカードの説明")
        void describeCreditCard() {
            String desc = describePayment(creditCard);
            assertThat(desc).contains("Credit card").contains("3456");
        }

        @Test
        @DisplayName("銀行振込の説明")
        void describeBankTransfer() {
            String desc = describePayment(bankTransfer);
            assertThat(desc).contains("Bank transfer").contains("9876543210");
        }

        @Test
        @DisplayName("現金の説明")
        void describeCash() {
            String desc = describePayment(cash);
            assertThat(desc).isEqualTo("Cash payment");
        }
    }

    @Nested
    @DisplayName("calculateFee")
    class CalculateFeeTest {

        @Test
        @DisplayName("クレジットカードは 3% 手数料")
        void creditCardFee() {
            assertThat(calculateFee(creditCard, 10000.0)).isCloseTo(300.0, within(0.01));
        }

        @Test
        @DisplayName("銀行振込は固定 500 円")
        void bankTransferFee() {
            assertThat(calculateFee(bankTransfer, 10000.0)).isEqualTo(500.0);
            assertThat(calculateFee(bankTransfer, 100.0)).isEqualTo(500.0);
        }

        @Test
        @DisplayName("現金は手数料なし")
        void cashFee() {
            assertThat(calculateFee(cash, 10000.0)).isEqualTo(0.0);
        }
    }

    @Nested
    @DisplayName("isOnlinePaymentAvailable")
    class IsOnlinePaymentAvailableTest {

        @Test
        @DisplayName("クレジットカードはオンライン対応")
        void creditCardOnline() {
            assertThat(isOnlinePaymentAvailable(creditCard)).isTrue();
        }

        @Test
        @DisplayName("銀行振込はオンライン対応")
        void bankTransferOnline() {
            assertThat(isOnlinePaymentAvailable(bankTransfer)).isTrue();
        }

        @Test
        @DisplayName("現金はオンライン非対応")
        void cashNotOnline() {
            assertThat(isOnlinePaymentAvailable(cash)).isFalse();
        }
    }
}
