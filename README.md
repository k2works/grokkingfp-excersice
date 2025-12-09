# Grokking Functional Programming 演習

## 概要

「Grokking Functional Programming」（Michał Płachta 著）の学習用リポジトリ。
Scala 3 と Java 21 の両方で関数型プログラミングの実装例と日本語解説を含む。

### 目的

- 関数型プログラミングの基本概念を理解する
- Scala と Java での実践的な FP パターンを習得する
- IO モナド、ストリーム処理、並行処理を学ぶ

### 前提

| ソフトウェア | バージョン | 備考                  |
| :----------- | :--------- | :-------------------- |
| JDK          | 21+        | Virtual Thread 対応   |
| sbt          | 1.10+      | Scala ビルド          |
| Gradle       | 8.0+       | Java ビルド           |
| Node.js      | 18+        | MkDocs 用             |

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

# テストの実行
cd app/java
./gradlew test

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
│   │   ├── src/main/scala/      # Scala 実装
│   │   └── src/test/scala/      # テストコード
│   └── java/                    # Java サンプルコード
│       ├── src/main/java/       # Java 実装
│       └── src/test/java/       # テストコード
├── docs/article/
│   ├── scala/                   # Scala 日本語解説記事
│   │   ├── index.md
│   │   └── part-1〜6.md
│   └── java/                    # Java 日本語解説記事
│       ├── index.md
│       └── part-1〜6.md
└── mkdocs.yml                   # ドキュメント設定
```

#### 章構成

| Part | 内容                           | Scala                           | Java                              |
| ---- | ------------------------------ | ------------------------------- | --------------------------------- |
| I    | 関数型プログラミングの基礎     | 純粋関数、参照透過性            | 関数型インターフェース            |
| II   | 関数型スタイルのプログラミング | List, Option, flatMap           | Vavr List, Option, flatMap        |
| III  | エラーハンドリング             | Option、Either、sealed trait    | Option、Either、sealed interface  |
| IV   | IO と副作用の管理              | cats-effect IO、fs2 Stream      | 独自 IO 実装、Vavr Stream         |
| V    | 並行処理                       | Ref、Fiber、parSequence         | Ref、Virtual Thread、ParallelIO   |
| VI   | 実践的なアプリケーション       | Resource、ScalaCheck            | Resource、プロパティベーステスト  |

#### Scala と Java の比較

| 概念 | Scala (cats-effect) | Java + Vavr |
|------|---------------------|-------------|
| イミュータブルリスト | `List` | `io.vavr.collection.List` |
| Option 型 | `Option[A]` | `io.vavr.control.Option<A>` |
| Either 型 | `Either[E, A]` | `io.vavr.control.Either<L, R>` |
| IO モナド | `cats.effect.IO[A]` | 独自 `IO<A>` 実装 |
| アトミック参照 | `Ref[IO, A]` | 独自 `Ref<A>` 実装 |
| 軽量スレッド | `Fiber[IO, A]` | `Fiber<A>` (Virtual Thread) |
| リソース管理 | `Resource[IO, A]` | 独自 `Resource<A>` 実装 |
| ストリーム | `fs2.Stream[IO, A]` | `io.vavr.collection.Stream<A>` |
| ADT | `sealed trait` | `sealed interface` (Java 17+) |

**[⬆ back to top](#構成)**

## 参照

- [Grokking Functional Programming](https://www.manning.com/books/grokking-functional-programming) - 原著
- [Scala 公式ドキュメント](https://docs.scala-lang.org/)
- [cats-effect](https://typelevel.org/cats-effect/)
- [fs2](https://fs2.io/)
- [Vavr](https://www.vavr.io/) - Java 用関数型ライブラリ
