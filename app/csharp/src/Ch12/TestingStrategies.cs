using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch12;

/// <summary>
/// 第12章: テスト戦略
/// 関数型プログラミングにおけるテスト手法の実装
/// </summary>
public static class TestingStrategies
{
    // ============================================
    // 純粋関数のテスト
    // ============================================

    /// <summary>
    /// 純粋関数の例: 加算
    /// 同じ入力に対して常に同じ出力を返す
    /// </summary>
    public static int Add(int a, int b) => a + b;

    /// <summary>
    /// 純粋関数の例: 文字列の大文字変換
    /// </summary>
    public static string ToUpperCase(string s) => s.ToUpper();

    /// <summary>
    /// 純粋関数の例: リストのフィルタリング
    /// </summary>
    public static Seq<T> FilterPositive<T>(Seq<T> items, Func<T, bool> predicate) =>
        items.Filter(predicate);

    // ============================================
    // プロパティベーステストのためのヘルパー
    // ============================================

    /// <summary>
    /// 加算の可換性を検証するためのヘルパー
    /// a + b == b + a
    /// </summary>
    public static bool AdditionIsCommutative(int a, int b) =>
        Add(a, b) == Add(b, a);

    /// <summary>
    /// 加算の結合性を検証するためのヘルパー
    /// (a + b) + c == a + (b + c)
    /// </summary>
    public static bool AdditionIsAssociative(int a, int b, int c) =>
        Add(Add(a, b), c) == Add(a, Add(b, c));

    /// <summary>
    /// 加算の単位元を検証するためのヘルパー
    /// a + 0 == a
    /// </summary>
    public static bool AdditionHasIdentity(int a) =>
        Add(a, 0) == a;

    // ============================================
    // Option のテストヘルパー
    // ============================================

    /// <summary>
    /// 安全な除算（ゼロ除算を防ぐ）
    /// </summary>
    public static Option<int> SafeDivide(int numerator, int denominator) =>
        denominator == 0 ? None : Some(numerator / denominator);

    /// <summary>
    /// 安全な文字列から整数への変換
    /// </summary>
    public static Option<int> ParseInt(string s) =>
        int.TryParse(s, out var result) ? Some(result) : None;

    /// <summary>
    /// 安全なリストの先頭要素取得
    /// </summary>
    public static Option<T> SafeHead<T>(Seq<T> items) =>
        items.IsEmpty ? None : Some(items.First());

    // ============================================
    // Either のテストヘルパー
    // ============================================

    /// <summary>
    /// エラーの種類
    /// </summary>
    public enum ValidationError
    {
        Empty,
        TooShort,
        TooLong,
        InvalidFormat
    }

    /// <summary>
    /// 文字列の検証
    /// </summary>
    public static Either<ValidationError, string> ValidateString(string input, int minLength, int maxLength)
    {
        if (string.IsNullOrEmpty(input))
            return ValidationError.Empty;
        if (input.Length < minLength)
            return ValidationError.TooShort;
        if (input.Length > maxLength)
            return ValidationError.TooLong;
        return input;
    }

    /// <summary>
    /// メールアドレスの簡易検証
    /// </summary>
    public static Either<ValidationError, string> ValidateEmail(string email)
    {
        if (string.IsNullOrEmpty(email))
            return ValidationError.Empty;
        if (!email.Contains('@'))
            return ValidationError.InvalidFormat;
        return email;
    }

    // ============================================
    // テスト用スタブとモック
    // ============================================

    /// <summary>
    /// 外部サービスのインターフェース
    /// </summary>
    public interface IExternalService
    {
        Task<Either<string, string>> FetchData(string id);
    }

    /// <summary>
    /// テスト用の成功するスタブ
    /// </summary>
    public class SuccessStub : IExternalService
    {
        private readonly string _data;

        public SuccessStub(string data) => _data = data;

        public Task<Either<string, string>> FetchData(string id) =>
            Task.FromResult<Either<string, string>>(Right<string, string>(_data));
    }

    /// <summary>
    /// テスト用の失敗するスタブ
    /// </summary>
    public class FailureStub : IExternalService
    {
        private readonly string _error;

        public FailureStub(string error) => _error = error;

        public Task<Either<string, string>> FetchData(string id) =>
            Task.FromResult<Either<string, string>>(Left<string, string>(_error));
    }

    /// <summary>
    /// 呼び出し回数を記録するスパイ
    /// </summary>
    public class SpyService : IExternalService
    {
        private readonly IExternalService _inner;
        private int _callCount;
        private readonly List<string> _calledIds = new();

        public SpyService(IExternalService inner) => _inner = inner;

        public int CallCount => _callCount;
        public IReadOnlyList<string> CalledIds => _calledIds;

        public Task<Either<string, string>> FetchData(string id)
        {
            _callCount++;
            _calledIds.Add(id);
            return _inner.FetchData(id);
        }
    }

    // ============================================
    // 合成可能なバリデーション
    // ============================================

    /// <summary>
    /// バリデーション結果
    /// </summary>
    public record ValidationResult<T>
    {
        public bool IsValid { get; init; }
        public T? Value { get; init; }
        public Seq<string> Errors { get; init; } = Seq<string>();

        public static ValidationResult<T> Valid(T value) =>
            new() { IsValid = true, Value = value, Errors = Seq<string>() };

        public static ValidationResult<T> Invalid(params string[] errors) =>
            new() { IsValid = false, Value = default, Errors = toSeq(errors) };

        public static ValidationResult<T> Invalid(Seq<string> errors) =>
            new() { IsValid = false, Value = default, Errors = errors };
    }

    /// <summary>
    /// バリデーターの合成
    /// </summary>
    public static class Validator
    {
        public static ValidationResult<T> Validate<T>(
            T value,
            params Func<T, Option<string>>[] validators)
        {
            var errors = validators
                .Select(v => v(value))
                .Where(e => e.IsSome)
                .Select(e => e.IfNone(""))
                .ToList();

            return errors.Count == 0
                ? ValidationResult<T>.Valid(value)
                : ValidationResult<T>.Invalid(toSeq(errors));
        }

        public static Func<string, Option<string>> NotEmpty() =>
            s => string.IsNullOrEmpty(s) ? Some("Value cannot be empty") : None;

        public static Func<string, Option<string>> MinLength(int min) =>
            s => s.Length < min ? Some($"Value must be at least {min} characters") : None;

        public static Func<string, Option<string>> MaxLength(int max) =>
            s => s.Length > max ? Some($"Value must be at most {max} characters") : None;

        public static Func<string, Option<string>> ContainsAt() =>
            s => !s.Contains('@') ? Some("Value must contain @") : None;
    }

    // ============================================
    // ジェネレーターベースのテストデータ
    // ============================================

    /// <summary>
    /// テストデータ生成器
    /// </summary>
    public static class TestDataGenerator
    {
        private static readonly Random Random = new();

        public static int RandomInt(int min = int.MinValue, int max = int.MaxValue) =>
            Random.Next(min, max);

        public static string RandomString(int length)
        {
            const string chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
            return new string(Enumerable.Range(0, length)
                .Select(_ => chars[Random.Next(chars.Length)])
                .ToArray());
        }

        public static Seq<T> RandomSeq<T>(Func<T> generator, int count) =>
            toSeq(Enumerable.Range(0, count).Select(_ => generator()));

        public static Option<T> RandomOption<T>(Func<T> generator, double someChance = 0.5) =>
            Random.NextDouble() < someChance ? Some(generator()) : None;
    }
}
