package ch07;

import io.vavr.collection.List;
import io.vavr.control.Either;
import io.vavr.control.Option;

/**
 * 第7章: Either の基本操作
 *
 * Either の主要メソッドを学びます。
 */
public class EitherBasics {

    // ============================================
    // Either の作成
    // ============================================

    /**
     * Right: 成功（慣例的に「正しい」= right）
     */
    public static Either<String, Integer> rightValue() {
        return Either.right(42);
    }

    /**
     * Left: 失敗（エラー情報を保持）
     */
    public static Either<String, Integer> leftValue() {
        return Either.left("Error occurred");
    }

    // ============================================
    // バリデーション
    // ============================================

    /**
     * 年齢のバリデーション
     */
    public static Either<String, Integer> validateAge(int age) {
        if (age < 0) {
            return Either.left("Age cannot be negative");
        } else if (age > 150) {
            return Either.left("Age cannot be greater than 150");
        }
        return Either.right(age);
    }

    /**
     * メールアドレスのバリデーション
     */
    public static Either<String, String> validateEmail(String email) {
        if (email == null || email.isBlank()) {
            return Either.left("Email cannot be empty");
        } else if (!email.contains("@")) {
            return Either.left("Email must contain @");
        }
        return Either.right(email);
    }

    /**
     * ユーザー名のバリデーション
     */
    public static Either<String, String> validateUsername(String username) {
        if (username == null || username.isBlank()) {
            return Either.left("Username cannot be empty");
        } else if (username.length() < 3) {
            return Either.left("Username must be at least 3 characters");
        } else if (username.length() > 20) {
            return Either.left("Username must be at most 20 characters");
        }
        return Either.right(username);
    }

    // ============================================
    // Either の主要メソッド
    // ============================================

    /**
     * map: Right の値を変換
     */
    public static Either<String, Integer> mapExample(Either<String, Integer> either) {
        return either.map(x -> x * 2);
    }

    /**
     * flatMap: Right なら Either を返す関数を適用
     */
    public static Either<String, Integer> flatMapExample(Either<String, Integer> either) {
        return either.flatMap(x -> {
            if (x == 0) {
                return Either.left("Cannot divide by zero");
            }
            return Either.right(100 / x);
        });
    }

    /**
     * orElse: Left なら代替を使用
     */
    public static Either<String, Integer> orElseExample(
            Either<String, Integer> either,
            Either<String, Integer> alternative) {
        return either.orElse(alternative);
    }

    /**
     * getOrElse: Left ならデフォルト値
     */
    public static int getOrElseExample(Either<String, Integer> either, int defaultValue) {
        return either.getOrElse(defaultValue);
    }

    /**
     * toOption: Option に変換
     */
    public static Option<Integer> toOptionExample(Either<String, Integer> either) {
        return either.toOption();
    }

    /**
     * toList: List に変換
     */
    public static List<Integer> toListExample(Either<String, Integer> either) {
        return either.toList();
    }

    /**
     * isRight / isLeft
     */
    public static boolean isRightExample(Either<String, Integer> either) {
        return either.isRight();
    }

    /**
     * fold: 両方のケースを処理
     */
    public static String foldExample(Either<String, Integer> either) {
        return either.fold(
                error -> "Error: " + error,
                value -> "Success: " + value
        );
    }

    // ============================================
    // Option から Either への変換
    // ============================================

    /**
     * Option を Either に変換
     */
    public static Either<String, Integer> fromOption(Option<Integer> opt, String errorMsg) {
        return opt.toEither(errorMsg);
    }

    // ============================================
    // 複合バリデーション
    // ============================================

    /**
     * ユーザー登録のバリデーション（全フィールド）
     */
    public record User(String username, String email, int age) {}

    public static Either<String, User> validateUser(String username, String email, int age) {
        return validateUsername(username).flatMap(validUsername ->
                validateEmail(email).flatMap(validEmail ->
                        validateAge(age).map(validAge ->
                                new User(validUsername, validEmail, validAge)
                        )
                )
        );
    }

    public static void main(String[] args) {
        System.out.println("=== Either の基本操作 ===\n");

        Either<String, Integer> right = Either.right(996);
        Either<String, Integer> left = Either.left("no value");

        // map
        System.out.println("--- map ---");
        System.out.println("Right(996).map(_ * 2) = " + mapExample(right));
        System.out.println("Left.map(_ * 2) = " + mapExample(left));

        // flatMap
        System.out.println("\n--- flatMap ---");
        System.out.println("Right(996).flatMap(100 / _) = " + flatMapExample(right));
        System.out.println("Right(0).flatMap(100 / _) = " + flatMapExample(Either.right(0)));
        System.out.println("Left.flatMap(100 / _) = " + flatMapExample(left));

        // orElse
        System.out.println("\n--- orElse ---");
        System.out.println("Right(996).orElse(Right(2020)) = " + orElseExample(right, Either.right(2020)));
        System.out.println("Left.orElse(Right(2020)) = " + orElseExample(left, Either.right(2020)));

        // fold
        System.out.println("\n--- fold ---");
        System.out.println("Right(996).fold(...) = " + foldExample(right));
        System.out.println("Left.fold(...) = " + foldExample(left));

        // バリデーション
        System.out.println("\n--- バリデーション ---");
        System.out.println("validateAge(25) = " + validateAge(25));
        System.out.println("validateAge(-5) = " + validateAge(-5));
        System.out.println("validateAge(200) = " + validateAge(200));

        // 複合バリデーション
        System.out.println("\n--- 複合バリデーション ---");
        System.out.println("validateUser(\"alice\", \"alice@example.com\", 25) = " +
                validateUser("alice", "alice@example.com", 25));
        System.out.println("validateUser(\"ab\", \"alice@example.com\", 25) = " +
                validateUser("ab", "alice@example.com", 25));
        System.out.println("validateUser(\"alice\", \"invalid\", 25) = " +
                validateUser("alice", "invalid", 25));
    }
}
