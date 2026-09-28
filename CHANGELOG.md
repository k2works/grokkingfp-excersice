# Changelog

このプロジェクトの主な変更履歴です。形式は [Conventional Commits](https://www.conventionalcommits.org/) の分類に基づき、バージョンは [Semantic Versioning](https://semver.org/) に従います。

## [0.1.0] - 2026-09-28

### Features

- **kotlin**: add STM section to Part V (1b0f8e1)
- **kotlin**: add Kotlin Arrow Part IV implementation and documentation (86ffc2b)
- **kotlin**: add Kotlin Arrow Part II implementation and documentation (1e0ddc2)
- **kotlin**: add Kotlin Arrow Part VI implementation and documentation (5b409f9)
- **kotlin**: add Kotlin Arrow Part III implementation and documentation (41b9de9)
- **kotlin**: add Kotlin Arrow Part V implementation and documentation (60b832d)
- **kotlin**: add Kotlin Arrow Part I implementation and documentation (c31098c)
- add project guidelines for AI coding agents and update devcontainer configuration (cc784fa)
- **docker**: add multi-language support to devcontainer (52c5d92)
- **ruby**: add Ruby dry-rb Part VI implementation and documentation (3976cf2)
- **ruby**: add Ruby dry-rb Part V implementation and documentation (59eeb31)
- **ruby**: add Ruby dry-rb Part IV implementation and documentation (b03fcfc)
- **ruby**: add Ruby dry-rb Part III implementation and documentation (90613cc)
- **ruby**: add Ruby dry-rb Part II implementation and documentation (731b382)
- **ruby**: add Ruby dry-rb Part I implementation and documentation (1fc91e5)
- **typescript**: add TypeScript fp-ts Part VI implementation and documentation (d9c866f)
- **typescript**: add TypeScript fp-ts Part V implementation and documentation (1c0f7dd)
- **typescript**: add TypeScript fp-ts Part IV implementation and documentation (42f5d12)
- **typescript**: add TypeScript fp-ts Part III implementation and documentation (d9e8769)
- **typescript**: add TypeScript fp-ts Part II implementation and documentation (1c1cd83)
- **typescript**: add TypeScript fp-ts Part I implementation and documentation (67f9f96)
- **python**: add Python Part VI implementation and documentation (ea62f5f)
- **python**: add Python Part V implementation and documentation (8eb32ac)
- **python**: add Python Part IV implementation and documentation (2b8a318)
- **python**: add Python Part III implementation and documentation (9916037)
- **python**: add Python Part II implementation and documentation (80ef3a9)
- **python**: add Python Part I implementation and documentation (e33b870)
- **rust**: add Rust Part VI implementation and documentation (a009581)

### Bug Fixes

- **nix**: Node.js を 20 LTS に戻し TypeScript 開発環境を追加 (df32810)
- **haskell/clojure**: STM テストの修正・依存追加・バリア同期処理の改善・Clojure テスト runner オプション変更 (2ef0752)
- **haskell**: ScopedTypeVariables を追加して型エラーを修正 (d27c456)
- **haskell**: ビルドエラーを修正 (17278c3)
- **nix**: TypeScript 環境の Node.js を 20 LTS に変更 (42767d2)
- **clojure**: テストの期待値を実装に合わせて修正 (796d6a1)
- **ci**: Java gradlew の実行権限を付与 (14035cf)
- **ci**: Python テストで uv を使用するよう修正 (0266c9f)
- **docker**: remove unsupported pip --break-system-packages flag (76aa877)
- **docker**: use Ubuntu packages for Erlang/Elixir installation (7903ab6)
- **clojure**: correct PlantUML syntax in part-4 documentation (c6c6142)
- **python**: use dependency-groups for uv compatibility (fe0f96d)

### Documentation

- **article**: 統合記事の目次に総合スコアによるレーダーチャート比較を追加 (765f221)
- **article**: 第4-9章の統合比較記事を12言語化しレーダーチャートを追加 (8677405)
- **article**: 第2-3章の統合比較記事を12言語化しレーダーチャートを追加 (8f544b6)
- **article**: 第11-12章の統合比較記事を12言語化しレーダーチャートを追加 (9262ee2)
- **article**: 統合記事の目次と第1章を12言語化しレーダーチャートを追加 (9449433)
- **article**: 第10章の統合比較記事を12言語化しレーダーチャートを追加 (a30bf04)
- Mermaid を 11.17.2 に更新し統合比較の表記を 12 言語に変更 (6c1855d)
- **kotlin**: 執筆計画に進捗と採用バージョンを反映 (eb3278d)
- **kotlin**: add Kotlin index and update navigation (ab7b102)
- **kotlin**: Kotlin 版の執筆計画を追加 (a71f582)
- **article**: fix links and table formatting in Part VI comparison article (0558a18)
- **article**: 11言語統合比較記事のイテレーション1・2（第1-9章）と執筆計画を追加 (3e00582)
- **article**: 11言語統合比較記事のイテレーション3（第10-12章）を追加しナビゲーションを更新 (519565b)
- add GHCR operations documentation to README (93fbc16)
- **haskell**: fix markdown formatting in part-5 and part-6 (7f8ce6f)
- update README.md to include all 11 languages (745c126)
- add Ruby to documentation index and mkdocs navigation (5a158b1)
- **ruby**: add README with setup instructions (b5447b3)
- add Python to index and mkdocs navigation (118a619)
- **python**: add Python returns version index and navigation (7d3288d)

### Chores

- **kotlin**: add Kotlin development environment and CI job (dcacbec)
- リファレンスドキュメント更新・テンプレート追加・claude-code-booster 依存追加 (0fc4071)
- 依存関係とドキュメントを更新 (3846133)
- **ruby**: add local bundle configuration and .gitignore (9cecea2)
- add root workspace pyproject.toml (fc4fa77)
- **haskell**: add Stack build artifacts (98eafec)

### CI

- 全言語テスト用 GitHub Actions ワークフローを追加 (82242b1)
- add GitHub Container Registry workflow (2bc9ab7)
