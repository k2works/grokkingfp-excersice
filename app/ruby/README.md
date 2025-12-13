# Grokking Functional Programming - Ruby Implementation

Ruby で学ぶ関数型プログラミングの実装例です。

## 必要環境

- Ruby 3.2+
- Bundler

## インストール

```bash
# 依存関係をインストール（vendor/bundle にローカルインストール）
bundle install
```

## テスト実行

```bash
bundle exec rspec
```

## 使用ライブラリ

- [dry-monads](https://dry-rb.org/gems/dry-monads/) - Maybe/Result/Try モナド
- [dry-struct](https://dry-rb.org/gems/dry-struct/) - イミュータブルな構造体
- [dry-types](https://dry-rb.org/gems/dry-types/) - 型システム
- [dry-validation](https://dry-rb.org/gems/dry-validation/) - バリデーション
- [concurrent-ruby](https://github.com/ruby-concurrency/concurrent-ruby) - 並行処理
- [rspec](https://rspec.info/) - テストフレームワーク
