/**
 * 第7章: Either 型と複合的なエラー処理
 *
 * Option の限界を超えて、エラー情報を保持できる Either を学びます。
 * また、代数的データ型（ADT）とパターンマッチングについても学習します。
 */

import { pipe } from 'fp-ts/function'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as RA from 'fp-ts/ReadonlyArray'

// =============================================================================
// 7.1 Either の基本
// =============================================================================

/**
 * Either[E, A] は「E 型のエラーか、A 型の成功値か」を表す型です。
 * - Right(value): 成功（慣例的に「正しい」= right）
 * - Left(error): 失敗（エラー情報を保持）
 */

/**
 * 安全な除算（エラーメッセージ付き）。
 *
 * @example
 * safeDivideE(10, 2) // => right(5)
 * safeDivideE(10, 0) // => left("Division by zero")
 */
export const safeDivideE = (a: number, b: number): E.Either<string, number> =>
  b === 0 ? E.left('Division by zero') : E.right(Math.floor(a / b))

/**
 * 文字列を数値に安全に変換する（エラーメッセージ付き）。
 *
 * @example
 * parseNumberE("42") // => right(42)
 * parseNumberE("abc") // => left("Cannot parse 'abc' as number")
 */
export const parseNumberE = (s: string): E.Either<string, number> => {
  const n = parseInt(s, 10)
  return isNaN(n) ? E.left(`Cannot parse '${s}' as number`) : E.right(n)
}

// =============================================================================
// 7.2 TV番組のパース（Either版）
// =============================================================================

/** TV番組を表すイミュータブルなインターフェース */
export interface TvShow {
  readonly title: string
  readonly start: number
  readonly end: number
}

/** TvShow を作成する */
export const createTvShow = (
  title: string,
  start: number,
  end: number
): TvShow => ({ title, start, end })

/**
 * 名前を抽出する（Either版）。
 *
 * @example
 * extractNameE("Breaking Bad (2008-2013)") // => right("Breaking Bad")
 * extractNameE("(2008-2013)") // => left("Can't extract name from (2008-2013)")
 */
export const extractNameE = (rawShow: string): E.Either<string, string> => {
  const bracketOpen = rawShow.indexOf('(')
  if (bracketOpen > 0) {
    return E.right(rawShow.substring(0, bracketOpen).trim())
  }
  return E.left(`Can't extract name from ${rawShow}`)
}

/**
 * 開始年を抽出する（Either版）。
 *
 * @example
 * extractYearStartE("Breaking Bad (2008-2013)") // => right(2008)
 * extractYearStartE("(-)") // => left("Can't extract start year from (-)")
 */
export const extractYearStartE = (rawShow: string): E.Either<string, number> => {
  const bracketOpen = rawShow.indexOf('(')
  const dash = rawShow.indexOf('-')
  if (bracketOpen !== -1 && dash > bracketOpen + 1) {
    const yearStr = rawShow.substring(bracketOpen + 1, dash)
    return pipe(
      parseNumberE(yearStr),
      E.mapLeft(() => `Can't parse start year from ${rawShow}`)
    )
  }
  return E.left(`Can't extract start year from ${rawShow}`)
}

/**
 * 終了年を抽出する（Either版）。
 *
 * @example
 * extractYearEndE("Breaking Bad (2008-2013)") // => right(2013)
 * extractYearEndE("(-)") // => left("Can't extract end year from (-)")
 */
export const extractYearEndE = (rawShow: string): E.Either<string, number> => {
  const dash = rawShow.indexOf('-')
  const bracketClose = rawShow.indexOf(')')
  if (dash !== -1 && bracketClose > dash + 1) {
    const yearStr = rawShow.substring(dash + 1, bracketClose)
    return pipe(
      parseNumberE(yearStr),
      E.mapLeft(() => `Can't parse end year from ${rawShow}`)
    )
  }
  return E.left(`Can't extract end year from ${rawShow}`)
}

/**
 * 単年を抽出する（Either版）。
 *
 * @example
 * extractSingleYearE("Chernobyl (2019)") // => right(2019)
 * extractSingleYearE("Breaking Bad (2008-2013)") // => left("...")
 */
export const extractSingleYearE = (
  rawShow: string
): E.Either<string, number> => {
  const dash = rawShow.indexOf('-')
  const bracketOpen = rawShow.indexOf('(')
  const bracketClose = rawShow.indexOf(')')
  if (dash === -1 && bracketOpen !== -1 && bracketClose > bracketOpen + 1) {
    const yearStr = rawShow.substring(bracketOpen + 1, bracketClose)
    return pipe(
      parseNumberE(yearStr),
      E.mapLeft(() => `Can't parse single year from ${rawShow}`)
    )
  }
  return E.left(`Can't extract single year from ${rawShow}`)
}

/**
 * TV番組の文字列をパースする（Either版）。
 *
 * @example
 * parseShowE("Breaking Bad (2008-2013)")
 * // => right({ title: "Breaking Bad", start: 2008, end: 2013 })
 *
 * parseShowE("()")
 * // => left("Can't extract name from ()")
 */
export const parseShowE = (rawShow: string): E.Either<string, TvShow> =>
  pipe(
    E.Do,
    E.bind('title', () => extractNameE(rawShow)),
    E.bind('start', () =>
      pipe(
        extractYearStartE(rawShow),
        E.orElse(() => extractSingleYearE(rawShow))
      )
    ),
    E.bind('end', () =>
      pipe(
        extractYearEndE(rawShow),
        E.orElse(() => extractSingleYearE(rawShow))
      )
    ),
    E.map(({ title, start, end }) => createTvShow(title, start, end))
  )

// =============================================================================
// 7.3 Either の主要操作
// =============================================================================

/**
 * Either の値を変換する（map）。
 *
 * @example
 * mapEither(right(5), x => x * 2) // => right(10)
 * mapEither(left("error"), x => x * 2) // => left("error")
 */
export const mapEither = <E, A, B>(
  either: E.Either<E, A>,
  f: (a: A) => B
): E.Either<E, B> => pipe(either, E.map(f))

/**
 * Either を返す関数を適用する（flatMap/chain）。
 *
 * @example
 * flatMapEither(right(10), x => safeDivideE(x, 2)) // => right(5)
 * flatMapEither(right(10), x => safeDivideE(x, 0)) // => left("Division by zero")
 */
export const flatMapEither = <E, A, B>(
  either: E.Either<E, A>,
  f: (a: A) => E.Either<E, B>
): E.Either<E, B> => pipe(either, E.chain(f))

/**
 * Left の場合に代替を使用する（orElse）。
 *
 * @example
 * orElseEither(left("error"), () => right(10)) // => right(10)
 * orElseEither(right(5), () => right(10)) // => right(5)
 */
export const orElseEither = <E, A>(
  either: E.Either<E, A>,
  alternative: () => E.Either<E, A>
): E.Either<E, A> => pipe(either, E.orElse(alternative))

/**
 * Left の場合にデフォルト値を返す（getOrElse）。
 *
 * @example
 * getOrElseEither(right(5), () => 0) // => 5
 * getOrElseEither(left("error"), () => 0) // => 0
 */
export const getOrElseEither = <E, A>(
  either: E.Either<E, A>,
  defaultValue: () => A
): A => pipe(either, E.getOrElse(defaultValue))

// =============================================================================
// 7.4 Option から Either への変換
// =============================================================================

/**
 * Option を Either に変換する。
 * None の場合は Left(errorValue) になる。
 *
 * @example
 * optionToEither(some(5), "no value") // => right(5)
 * optionToEither(none, "no value") // => left("no value")
 */
export const optionToEither = <E, A>(
  opt: O.Option<A>,
  errorValue: E
): E.Either<E, A> => pipe(opt, E.fromOption(() => errorValue))

/**
 * Either を Option に変換する。
 * Left の場合は None になる。
 *
 * @example
 * eitherToOption(right(5)) // => some(5)
 * eitherToOption(left("error")) // => none
 */
export const eitherToOption = <E, A>(either: E.Either<E, A>): O.Option<A> =>
  pipe(
    either,
    E.match(
      () => O.none,
      (a) => O.some(a)
    )
  )

// =============================================================================
// 7.5 代数的データ型（ADT）- 直和型
// =============================================================================

/**
 * 音楽ジャンルを表す直和型（discriminated union）。
 */
export type MusicGenre = 'HeavyMetal' | 'Pop' | 'HardRock' | 'Grunge'

/**
 * 活動期間を表す直和型（discriminated union）。
 */
export type YearsActive =
  | { readonly _tag: 'StillActive'; readonly since: number }
  | { readonly _tag: 'ActiveBetween'; readonly start: number; readonly end: number }

/** StillActive を作成する */
export const stillActive = (since: number): YearsActive => ({
  _tag: 'StillActive',
  since,
})

/** ActiveBetween を作成する */
export const activeBetween = (start: number, end: number): YearsActive => ({
  _tag: 'ActiveBetween',
  start,
  end,
})

/**
 * 場所を表す型。
 */
export type Location = 'US' | 'England' | 'Australia'

/**
 * アーティストを表すイミュータブルなインターフェース（直積型）。
 */
export interface Artist {
  readonly name: string
  readonly genre: MusicGenre
  readonly origin: Location
  readonly yearsActive: YearsActive
}

/** Artist を作成する */
export const createArtist = (
  name: string,
  genre: MusicGenre,
  origin: Location,
  yearsActive: YearsActive
): Artist => ({ name, genre, origin, yearsActive })

// =============================================================================
// 7.6 パターンマッチング
// =============================================================================

/**
 * アーティストが指定期間に活動していたかを判定する。
 *
 * @example
 * wasArtistActive(metallica, 1990, 2000) // => true
 */
export const wasArtistActive = (
  artist: Artist,
  yearStart: number,
  yearEnd: number
): boolean => {
  const years = artist.yearsActive
  switch (years._tag) {
    case 'StillActive':
      return years.since <= yearEnd
    case 'ActiveBetween':
      return years.start <= yearEnd && years.end >= yearStart
  }
}

/**
 * アーティストの活動年数を計算する。
 *
 * @example
 * activeLength(metallica, 2024) // => 43 (1981年から活動中)
 * activeLength(ledZeppelin, 2024) // => 12 (1968-1980)
 */
export const activeLength = (artist: Artist, currentYear: number): number => {
  const years = artist.yearsActive
  switch (years._tag) {
    case 'StillActive':
      return currentYear - years.since
    case 'ActiveBetween':
      return years.end - years.start
  }
}

// =============================================================================
// 7.7 検索条件のモデリング
// =============================================================================

/**
 * 検索条件を表す直和型。
 */
export type SearchCondition =
  | { readonly _tag: 'SearchByGenre'; readonly genres: readonly MusicGenre[] }
  | { readonly _tag: 'SearchByOrigin'; readonly locations: readonly Location[] }
  | {
      readonly _tag: 'SearchByActiveYears'
      readonly start: number
      readonly end: number
    }

/** SearchByGenre を作成する */
export const searchByGenre = (
  genres: readonly MusicGenre[]
): SearchCondition => ({
  _tag: 'SearchByGenre',
  genres,
})

/** SearchByOrigin を作成する */
export const searchByOrigin = (
  locations: readonly Location[]
): SearchCondition => ({
  _tag: 'SearchByOrigin',
  locations,
})

/** SearchByActiveYears を作成する */
export const searchByActiveYears = (
  start: number,
  end: number
): SearchCondition => ({
  _tag: 'SearchByActiveYears',
  start,
  end,
})

/**
 * アーティストが検索条件を満たすかを判定する。
 */
const matchesCondition = (
  artist: Artist,
  condition: SearchCondition
): boolean => {
  switch (condition._tag) {
    case 'SearchByGenre':
      return condition.genres.includes(artist.genre)
    case 'SearchByOrigin':
      return condition.locations.includes(artist.origin)
    case 'SearchByActiveYears':
      return wasArtistActive(artist, condition.start, condition.end)
  }
}

/**
 * 全ての検索条件を満たすアーティストを検索する。
 *
 * @example
 * searchArtists(artists, [searchByGenre(["HeavyMetal"]), searchByOrigin(["US"])])
 */
export const searchArtists = (
  artists: readonly Artist[],
  conditions: readonly SearchCondition[]
): readonly Artist[] =>
  pipe(
    artists,
    RA.filter((artist) =>
      conditions.every((condition) => matchesCondition(artist, condition))
    )
  )

/**
 * いずれかの検索条件を満たすアーティストを検索する。
 */
export const searchArtistsAny = (
  artists: readonly Artist[],
  conditions: readonly SearchCondition[]
): readonly Artist[] =>
  pipe(
    artists,
    RA.filter((artist) =>
      conditions.some((condition) => matchesCondition(artist, condition))
    )
  )

// =============================================================================
// 7.8 Either を使ったバリデーション
// =============================================================================

/**
 * 年齢を検証する。
 *
 * @example
 * validateAge(25) // => right(25)
 * validateAge(-5) // => left("Age cannot be negative")
 * validateAge(200) // => left("Age cannot be greater than 150")
 */
export const validateAge = (age: number): E.Either<string, number> => {
  if (age < 0) return E.left('Age cannot be negative')
  if (age > 150) return E.left('Age cannot be greater than 150')
  return E.right(age)
}

/**
 * ユーザー名を検証する。
 *
 * @example
 * validateUsername("alice") // => right("alice")
 * validateUsername("") // => left("Username cannot be empty")
 * validateUsername("ab") // => left("Username must be at least 3 characters")
 */
export const validateUsername = (username: string): E.Either<string, string> => {
  if (username.length === 0) return E.left('Username cannot be empty')
  if (username.length < 3)
    return E.left('Username must be at least 3 characters')
  return E.right(username)
}

/**
 * メールアドレスを検証する。
 *
 * @example
 * validateEmail("alice@example.com") // => right("alice@example.com")
 * validateEmail("invalid") // => left("Invalid email format")
 */
export const validateEmail = (email: string): E.Either<string, string> => {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
  if (!emailRegex.test(email)) return E.left('Invalid email format')
  return E.right(email)
}

/** ユーザーを表すインターフェース */
export interface User {
  readonly username: string
  readonly email: string
  readonly age: number
}

/**
 * ユーザー入力を検証してユーザーを作成する。
 *
 * @example
 * validateUser("alice", "alice@example.com", 25)
 * // => right({ username: "alice", email: "alice@example.com", age: 25 })
 */
export const validateUser = (
  username: string,
  email: string,
  age: number
): E.Either<string, User> =>
  pipe(
    E.Do,
    E.bind('username', () => validateUsername(username)),
    E.bind('email', () => validateEmail(email)),
    E.bind('age', () => validateAge(age))
  )

// =============================================================================
// 7.9 Either と ReadonlyArray の組み合わせ
// =============================================================================

/**
 * 配列の全要素に Either を返す関数を適用し、
 * 全て Right の場合のみ結果を返す（traverse）。
 *
 * @example
 * traverseArrayE(["1", "2", "3"], parseNumberE) // => right([1, 2, 3])
 * traverseArrayE(["1", "abc", "3"], parseNumberE) // => left("Cannot parse 'abc' as number")
 */
export const traverseArrayE = <E, A, B>(
  arr: readonly A[],
  f: (a: A) => E.Either<E, B>
): E.Either<E, readonly B[]> => pipe(arr, RA.traverse(E.Applicative)(f))

/**
 * 配列の全要素に Either を返す関数を適用し、
 * Right の要素のみを収集する（Best-effort）。
 *
 * @example
 * collectRights(["1", "abc", "3"], parseNumberE) // => [1, 3]
 */
export const collectRights = <E, A, B>(
  arr: readonly A[],
  f: (a: A) => E.Either<E, B>
): readonly B[] =>
  pipe(
    arr,
    RA.map(f),
    RA.filter(E.isRight),
    RA.map((e) => (e as E.Right<B>).right)
  )

// =============================================================================
// 7.10 支払い方法の例（ADT）
// =============================================================================

/**
 * 支払い方法を表す直和型。
 */
export type PaymentMethod =
  | { readonly _tag: 'CreditCard'; readonly number: string; readonly expiry: string }
  | { readonly _tag: 'BankTransfer'; readonly accountNumber: string }
  | { readonly _tag: 'Cash' }

/** CreditCard を作成する */
export const creditCard = (number: string, expiry: string): PaymentMethod => ({
  _tag: 'CreditCard',
  number,
  expiry,
})

/** BankTransfer を作成する */
export const bankTransfer = (accountNumber: string): PaymentMethod => ({
  _tag: 'BankTransfer',
  accountNumber,
})

/** Cash を作成する */
export const cash = (): PaymentMethod => ({ _tag: 'Cash' })

/**
 * 支払い方法の説明を生成する。
 *
 * @example
 * describePayment(creditCard("1234", "12/25")) // => "Credit card ending in 1234"
 * describePayment(bankTransfer("9876")) // => "Bank transfer to account 9876"
 * describePayment(cash()) // => "Cash payment"
 */
export const describePayment = (method: PaymentMethod): string => {
  switch (method._tag) {
    case 'CreditCard':
      return `Credit card ending in ${method.number}`
    case 'BankTransfer':
      return `Bank transfer to account ${method.accountNumber}`
    case 'Cash':
      return 'Cash payment'
  }
}

// =============================================================================
// 7.11 サンプルデータ
// =============================================================================

/** サンプルアーティストデータ */
export const sampleArtists: readonly Artist[] = [
  createArtist('Metallica', 'HeavyMetal', 'US', stillActive(1981)),
  createArtist('Led Zeppelin', 'HardRock', 'England', activeBetween(1968, 1980)),
  createArtist('Bee Gees', 'Pop', 'Australia', activeBetween(1958, 2003)),
]
