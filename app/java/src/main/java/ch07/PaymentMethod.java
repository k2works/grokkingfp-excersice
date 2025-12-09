package ch07;

/**
 * 第7章: 支払い方法を表す直和型（演習問題用）
 *
 * sealed interface によるパターンマッチングの例
 */
public class PaymentMethod {

    /**
     * 支払い方法を表す直和型
     */
    public sealed interface Payment permits CreditCard, BankTransfer, Cash {}

    /**
     * クレジットカード
     */
    public record CreditCard(String number, String expiry) implements Payment {}

    /**
     * 銀行振込
     */
    public record BankTransfer(String accountNumber) implements Payment {}

    /**
     * 現金
     */
    public record Cash() implements Payment {}

    /**
     * 支払い方法の説明を生成
     */
    public static String describePayment(Payment method) {
        return switch (method) {
            case CreditCard(String number, String expiry) ->
                    "Credit card ending in " + number.substring(Math.max(0, number.length() - 4));
            case BankTransfer(String account) ->
                    "Bank transfer to account " + account;
            case Cash() ->
                    "Cash payment";
        };
    }

    /**
     * 支払い手数料を計算
     */
    public static double calculateFee(Payment method, double amount) {
        return switch (method) {
            case CreditCard cc -> amount * 0.03;     // 3% 手数料
            case BankTransfer bt -> 500.0;          // 固定 500円
            case Cash c -> 0.0;                      // 手数料なし
        };
    }

    /**
     * オンライン支払いが可能か
     */
    public static boolean isOnlinePaymentAvailable(Payment method) {
        return switch (method) {
            case CreditCard cc -> true;
            case BankTransfer bt -> true;
            case Cash c -> false;
        };
    }

    public static void main(String[] args) {
        System.out.println("=== 支払い方法 ===\n");

        Payment creditCard = new CreditCard("1234-5678-9012-3456", "12/25");
        Payment bankTransfer = new BankTransfer("9876543210");
        Payment cash = new Cash();

        // 説明
        System.out.println("--- 説明 ---");
        System.out.println(describePayment(creditCard));
        System.out.println(describePayment(bankTransfer));
        System.out.println(describePayment(cash));

        // 手数料
        double amount = 10000.0;
        System.out.println("\n--- 手数料（" + amount + "円の場合）---");
        System.out.println("クレジットカード: " + calculateFee(creditCard, amount) + "円");
        System.out.println("銀行振込: " + calculateFee(bankTransfer, amount) + "円");
        System.out.println("現金: " + calculateFee(cash, amount) + "円");

        // オンライン支払い
        System.out.println("\n--- オンライン支払い可能 ---");
        System.out.println("クレジットカード: " + isOnlinePaymentAvailable(creditCard));
        System.out.println("銀行振込: " + isOnlinePaymentAvailable(bankTransfer));
        System.out.println("現金: " + isOnlinePaymentAvailable(cash));
    }
}
