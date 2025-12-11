# Grokking Functional Programming

「Grokking Functional Programming」（Michal Plachta 著）の学習用リポジトリです。
Scala、Java、F#、C#、Haskell の5言語で関数型プログラミングの実装例と日本語解説を提供します。

## 言語別解説

### Scala 版

Scala 3 と cats-effect/fs2 を使った関数型プログラミングの実装例です。

- [Scala 解説](scala/index.md)

### Java 版

Java 21 と Vavr を使った関数型プログラミングの実装例です。

- [Java 解説](java/index.md)

### F# 版

F# 8 と .NET を使った関数型プログラミングの実装例です。

- [F# 解説](fsharp/index.md)

### C# 版

C# 12 と LanguageExt を使った関数型プログラミングの実装例です。

- [C# 解説](csharp/index.md)

### Haskell 版

Haskell（GHC 9.x）を使った純粋関数型プログラミングの実装例です。

- [Haskell 解説](haskell/index.md)

## 章構成

| Part | 内容 | Scala | Java | F# | C# | Haskell |
|------|------|-------|------|-----|-----|---------|
| I | 関数型プログラミングの基礎 | [part-1](scala/part-1.md) | [part-1](java/part-1.md) | [part-1](fsharp/part-1.md) | [part-1](csharp/part-1.md) | [part-1](haskell/part-1.md) |
| II | 関数型スタイルのプログラミング | [part-2](scala/part-2.md) | [part-2](java/part-2.md) | [part-2](fsharp/part-2.md) | [part-2](csharp/part-2.md) | [part-2](haskell/part-2.md) |
| III | エラーハンドリング | [part-3](scala/part-3.md) | [part-3](java/part-3.md) | [part-3](fsharp/part-3.md) | [part-3](csharp/part-3.md) | [part-3](haskell/part-3.md) |
| IV | IO と副作用の管理 | [part-4](scala/part-4.md) | [part-4](java/part-4.md) | [part-4](fsharp/part-4.md) | [part-4](csharp/part-4.md) | [part-4](haskell/part-4.md) |
| V | 並行処理 | [part-5](scala/part-5.md) | [part-5](java/part-5.md) | [part-5](fsharp/part-5.md) | [part-5](csharp/part-5.md) | [part-5](haskell/part-5.md) |
| VI | 実践的なアプリケーション | [part-6](scala/part-6.md) | [part-6](java/part-6.md) | [part-6](fsharp/part-6.md) | [part-6](csharp/part-6.md) | [part-6](haskell/part-6.md) |

## 主要トピック

| Part | 主要トピック | Scala | Java | F# | C# | Haskell |
|------|-------------|-------|------|-----|-----|---------|
| I | 純粋関数、参照透過性 | 関数型基礎 | 関数型インターフェース | let バインディング | 式形式メソッド | 純粋関数、遅延評価 |
| II | イミュータブル、高階関数、flatMap | List, Option | Vavr List, Option | List, パイプライン | Seq, LINQ | リスト内包表記, fold |
| III | Option、Either、ADT | sealed trait | sealed interface | 判別共用体 | Option, Either | Maybe, Either, ADT |
| IV | IO モナド、Stream | cats-effect, fs2 | 独自 IO, Vavr Stream | Async, Seq | Task, IAsyncEnumerable | IO モナド, conduit |
| V | Ref、Fiber、並列処理 | cats-effect Ref/Fiber | 独自 Ref, Virtual Thread | Ref, MailboxProcessor | Ref, Task並列 | STM, TVar, async |
| VI | Resource、テスト | ScalaCheck | JUnit 5 プロパティテスト | use, FsCheck | Resource, Validator | bracket, QuickCheck |

## 参照

- [Grokking Functional Programming](https://www.manning.com/books/grokking-functional-programming) - 原著
- [Scala 公式ドキュメント](https://docs.scala-lang.org/)
- [cats-effect](https://typelevel.org/cats-effect/)
- [fs2](https://fs2.io/)
- [Vavr](https://www.vavr.io/)
- [F# 公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/fsharp/)
- [F# for Fun and Profit](https://fsharpforfunandprofit.com/)
- [LanguageExt](https://github.com/louthy/language-ext)
- [Haskell 公式サイト](https://www.haskell.org/)
- [Learn You a Haskell](http://learnyouahaskell.com/)
