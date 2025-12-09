# Grokking Functional Programming 演習

## 概要

「Grokking Functional Programming」（Michał Płachta 著）の学習用リポジトリ。
Scala 3 と cats-effect/fs2 を使った関数型プログラミングの実装例と日本語解説を含む。

### 目的

- 関数型プログラミングの基本概念を理解する
- Scala での実践的な FP パターンを習得する
- IO モナド、ストリーム処理、並行処理を学ぶ

### 前提

| ソフトウェア | バージョン | 備考         |
| :----------- | :--------- | :----------- |
| JDK          | 11+        |              |
| sbt          | 1.10+      | Scala ビルド |
| Node.js      | 18+        | MkDocs 用    |

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
├── app/scala/                    # Scala サンプルコード
│   ├── src/main/java/           # Java 比較用コード
│   ├── src/main/scala/          # Scala 実装
│   └── src/test/scala/          # テストコード
├── docs/article/scala/          # 日本語解説記事
│   ├── index.md                 # 概要
│   └── part-1〜6.md             # 各章解説
└── mkdocs.yml                   # ドキュメント設定
```

#### 章構成

| Part | 内容                           | 主要トピック                    |
| ---- | ------------------------------ | ------------------------------- |
| I    | 関数型プログラミングの基礎     | 純粋関数、参照透過性            |
| II   | 関数型スタイルのプログラミング | イミュータブル、高階関数        |
| III  | エラーハンドリング             | Option、Either、ADT             |
| IV   | IO と副作用の管理              | IO モナド、cats-effect、fs2     |
| V    | 並行処理                       | Ref、Fiber、parSequence         |
| VI   | 実践的なアプリケーション       | Resource、テスト、ScalaCheck    |

**[⬆ back to top](#構成)**

## 参照

- [Grokking Functional Programming](https://www.manning.com/books/grokking-functional-programming) - 原著
- [Scala 公式ドキュメント](https://docs.scala-lang.org/)
- [cats-effect](https://typelevel.org/cats-effect/)
- [fs2](https://fs2.io/)
