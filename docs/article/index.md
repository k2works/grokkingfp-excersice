# Grokking Functional Programming

「Grokking Functional Programming」（Michał Płachta 著）の学習用リポジトリです。
Scala と Java の両方で関数型プログラミングの実装例と日本語解説を提供します。

## 言語別解説

### Scala 版

Scala 3 と cats-effect/fs2 を使った関数型プログラミングの実装例です。

- [Scala 解説](scala/index.md)

### Java 版

Java 21 と Vavr を使った関数型プログラミングの実装例です。

- [Java 解説](java/index.md)

## 章構成

| Part | 内容 | Scala | Java |
|------|------|-------|------|
| I | 関数型プログラミングの基礎 | [part-1](scala/part-1.md) | [part-1](java/part-1.md) |
| II | 関数型スタイルのプログラミング | [part-2](scala/part-2.md) | [part-2](java/part-2.md) |
| III | エラーハンドリング | [part-3](scala/part-3.md) | [part-3](java/part-3.md) |
| IV | IO と副作用の管理 | [part-4](scala/part-4.md) | [part-4](java/part-4.md) |
| V | 並行処理 | [part-5](scala/part-5.md) | [part-5](java/part-5.md) |
| VI | 実践的なアプリケーション | [part-6](scala/part-6.md) | [part-6](java/part-6.md) |

## 主要トピック

| Part | 主要トピック | Scala | Java |
|------|-------------|-------|------|
| I | 純粋関数、参照透過性 | 関数型基礎 | 関数型インターフェース |
| II | イミュータブル、高階関数、flatMap | List, Option | Vavr List, Option |
| III | Option、Either、ADT | sealed trait | sealed interface |
| IV | IO モナド、Stream | cats-effect, fs2 | 独自 IO, Vavr Stream |
| V | Ref、Fiber、並列処理 | cats-effect Ref/Fiber | 独自 Ref, Virtual Thread |
| VI | Resource、テスト | ScalaCheck | JUnit 5 プロパティテスト |

## 参照

- [Grokking Functional Programming](https://www.manning.com/books/grokking-functional-programming) - 原著
- [Scala 公式ドキュメント](https://docs.scala-lang.org/)
- [cats-effect](https://typelevel.org/cats-effect/)
- [fs2](https://fs2.io/)
- [Vavr](https://www.vavr.io/)
