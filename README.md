# Grokking Functional Programming 演習

## 概要

「Grokking Functional Programming」（Michał Płachta 著）の学習用リポジトリ。
Scala、Java、F#、C#、Haskell、Clojure、Elixir、Rust、Python、TypeScript、Ruby の11言語で関数型プログラミングの実装例と日本語解説を含む。

### 目的

- 関数型プログラミングの基本概念を理解する
- 各言語での実践的な FP パターンを習得する
- IO モナド、ストリーム処理、並行処理を学ぶ

### 前提

| ソフトウェア | バージョン | 備考                  |
| :----------- | :--------- | :-------------------- |
| JDK          | 21+        | Scala/Java/Clojure    |
| sbt          | 1.10+      | Scala ビルド          |
| Gradle       | 8.0+       | Java ビルド           |
| .NET         | 8.0+       | F#/C# ビルド          |
| GHC          | 9.x        | Haskell ビルド        |
| Clojure CLI  | 1.11+      | Clojure ビルド        |
| Elixir       | 1.15+      | Elixir ビルド         |
| Rust         | 1.70+      | Rust ビルド           |
| Python       | 3.11+      | Python 実行           |
| Node.js      | 18+        | TypeScript/MkDocs     |
| Ruby         | 3.x        | Ruby 実行             |

## 構成

- [構築](#構築)
- [配置](#配置)
- [運用](#運用)
- [開発](#開発)

## 詳細

### Quick Start

```bash
# Scala サンプルコードの実行
cd app/scala
sbt run

# Java サンプルコードの実行
cd app/java
./gradlew run

# Ruby サンプルコードの実行
cd app/ruby
bundle install
bundle exec rspec

# ドキュメントサーバーの起動
mkdocs serve
```

### 構築

```bash
claude mcp add github npx @modelcontextprotocol/server-github -e GITHUB_PERSONAL_ACCESS_TOKEN=xxxxxxxxxxxxxxx
claude mcp add --transport http byterover-mcp --scope user https://mcp.byterover.dev/v2/mcp
claude mcp add github npx -y @modelcontextprotocol/server-github -s project
```

**[⬆ back to top](#構成)**

### 配置

#### GitHub Pages セットアップ

1. **GitHub リポジトリの Settings を開く**
    - リポジトリページで `Settings` タブをクリック

2. **Pages 設定を開く**
    - 左サイドバーの `Pages` をクリック

3. **Source を設定**
    - `Source` で `Deploy from a branch` を選択
    - `Branch` で `gh-pages` を選択し、フォルダは `/ (root)` を選択
    - `Save` をクリック

4. **初回デプロイ**
    - main ブランチにプッシュすると GitHub Actions が自動実行
    - Actions タブでデプロイ状況を確認

**[⬆ back to top](#構成)**

### 運用

**[⬆ back to top](#構成)**

### 開発

#### プロジェクト構造

```
grokkingfp-excersice/
├── app/
│   ├── scala/                   # Scala サンプルコード
│   ├── java/                    # Java サンプルコード
│   ├── fsharp/                  # F# サンプルコード
│   ├── csharp/                  # C# サンプルコード
│   ├── haskell/                 # Haskell サンプルコード
│   ├── clojure/                 # Clojure サンプルコード
│   ├── elixir/                  # Elixir サンプルコード
│   ├── rust/                    # Rust サンプルコード
│   ├── python/                  # Python サンプルコード
│   ├── typescript/              # TypeScript サンプルコード
│   └── ruby/                    # Ruby サンプルコード
├── docs/article/
│   ├── scala/                   # Scala 日本語解説記事
│   ├── java/                    # Java 日本語解説記事
│   ├── fsharp/                  # F# 日本語解説記事
│   ├── csharp/                  # C# 日本語解説記事
│   ├── haskell/                 # Haskell 日本語解説記事
│   ├── clojure/                 # Clojure 日本語解説記事
│   ├── elixir/                  # Elixir 日本語解説記事
│   ├── rust/                    # Rust 日本語解説記事
│   ├── python/                  # Python 日本語解説記事
│   ├── typescript/              # TypeScript 日本語解説記事
│   └── ruby/                    # Ruby 日本語解説記事
└── mkdocs.yml                   # ドキュメント設定
```

#### 章構成

| Part | 内容                           |
| ---- | ------------------------------ |
| I    | 関数型プログラミングの基礎     |
| II   | 関数型スタイルのプログラミング |
| III  | エラーハンドリング             |
| IV   | IO と副作用の管理              |
| V    | 並行処理                       |
| VI   | 実践的なアプリケーション       |

#### 言語別の主要ライブラリ

| 言語 | FP ライブラリ | テストライブラリ |
|------|---------------|------------------|
| Scala | cats-effect, fs2 | ScalaCheck |
| Java | Vavr | JUnit 5 |
| F# | FSharpPlus | FsCheck |
| C# | LanguageExt | xUnit |
| Haskell | base | QuickCheck |
| Clojure | core.async | test.check |
| Elixir | OTP | StreamData |
| Rust | tokio | proptest |
| Python | returns | Hypothesis |
| TypeScript | fp-ts | fast-check |
| Ruby | dry-rb | RSpec |

#### 主要概念の言語別対応

| 概念 | Scala | Java | Ruby |
|------|-------|------|------|
| イミュータブルリスト | `List` | `io.vavr.collection.List` | `Array#freeze` |
| Option 型 | `Option[A]` | `io.vavr.control.Option<A>` | `nil` / カスタム |
| Either 型 | `Either[E, A]` | `io.vavr.control.Either<L, R>` | `{ success:, value/error: }` |
| IO モナド | `cats.effect.IO[A]` | 独自 `IO<A>` 実装 | 独自 `IO` クラス |
| アトミック参照 | `Ref[IO, A]` | 独自 `Ref<A>` 実装 | `Ref` クラス (Mutex) |
| リソース管理 | `Resource[IO, A]` | 独自 `Resource<A>` 実装 | `Resource` クラス (ensure) |
| ストリーム | `fs2.Stream[IO, A]` | `io.vavr.collection.Stream<A>` | `Enumerator` |
| ADT | `sealed trait` | `sealed interface` | `Struct` / `Class` |

**[⬆ back to top](#構成)**

## 参照

### 書籍
- [Grokking Functional Programming](https://www.manning.com/books/grokking-functional-programming) - 原著

### Scala
- [Scala 公式ドキュメント](https://docs.scala-lang.org/)
- [cats-effect](https://typelevel.org/cats-effect/)
- [fs2](https://fs2.io/)

### Java
- [Vavr](https://www.vavr.io/) - Java 用関数型ライブラリ

### F# / C#
- [F# 公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/fsharp/)
- [LanguageExt](https://github.com/louthy/language-ext)

### Haskell
- [Haskell 公式サイト](https://www.haskell.org/)
- [Learn You a Haskell](http://learnyouahaskell.com/)

### Clojure
- [Clojure 公式サイト](https://clojure.org/)
- [ClojureDocs](https://clojuredocs.org/)

### Elixir
- [Elixir 公式ドキュメント](https://elixir-lang.org/docs.html)
- [Elixir School](https://elixirschool.com/ja/)

### Rust
- [Rust 公式ドキュメント](https://doc.rust-lang.org/book/)
- [tokio](https://tokio.rs/)

### Python
- [Python 公式ドキュメント](https://docs.python.org/3/)
- [returns](https://returns.readthedocs.io/)

### TypeScript
- [TypeScript 公式ドキュメント](https://www.typescriptlang.org/docs/)
- [fp-ts](https://gcanti.github.io/fp-ts/)

### Ruby
- [Ruby 公式ドキュメント](https://docs.ruby-lang.org/ja/)
- [dry-rb](https://dry-rb.org/)
- [RSpec](https://rspec.info/)
