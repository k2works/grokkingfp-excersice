//! 第7章: Result 型と複合的なエラー処理
//!
//! Rust の `Result<T, E>` は「成功値かエラー情報か」を型安全に表現します。
//! Scala の `Either[E, A]` に相当し、失敗の理由を伝えることができます。

// =============================================================================
// 7.1 Option の限界
// =============================================================================

// Option は「値があるかないか」しか表現できません。
// **なぜ失敗したのか**を伝えられません。
// Result<T, E> を使うことで、エラー情報を保持できます。

// =============================================================================
// 7.2 Result の基本
// =============================================================================

/// TV番組を表す構造体（Result版）
#[derive(Debug, Clone, PartialEq)]
pub struct TvShow {
    pub title: String,
    pub start: i32,
    pub end: i32,
}

impl TvShow {
    pub fn new(title: &str, start: i32, end: i32) -> Self {
        Self {
            title: title.to_string(),
            start,
            end,
        }
    }
}

/// 番組名を抽出（Result版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::extract_name;
///
/// assert_eq!(extract_name("The Wire (2002-2008)"), Ok("The Wire".to_string()));
/// assert!(extract_name("(2002-2008)").is_err());
/// ```
pub fn extract_name(raw_show: &str) -> Result<String, String> {
    let bracket_open = raw_show
        .find('(')
        .ok_or_else(|| format!("Can't find '(' in {}", raw_show))?;

    if bracket_open > 0 {
        Ok(raw_show[..bracket_open].trim().to_string())
    } else {
        Err(format!("Can't extract name from {}", raw_show))
    }
}

/// 開始年を抽出（Result版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::extract_year_start;
///
/// assert_eq!(extract_year_start("The Wire (2002-2008)"), Ok(2002));
/// assert!(extract_year_start("The Wire (-2008)").is_err());
/// assert!(extract_year_start("The Wire (oops-2008)").is_err());
/// ```
pub fn extract_year_start(raw_show: &str) -> Result<i32, String> {
    let bracket_open = raw_show
        .find('(')
        .ok_or_else(|| format!("Can't find '(' in {}", raw_show))?;
    let dash = raw_show
        .find('-')
        .ok_or_else(|| format!("Can't find '-' in {}", raw_show))?;

    if dash > bracket_open + 1 {
        let year_str = &raw_show[bracket_open + 1..dash];
        year_str
            .parse()
            .map_err(|_| format!("Can't parse '{}' as year", year_str))
    } else {
        Err(format!("Can't extract start year from {}", raw_show))
    }
}

/// 終了年を抽出（Result版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::extract_year_end;
///
/// assert_eq!(extract_year_end("The Wire (2002-2008)"), Ok(2008));
/// assert!(extract_year_end("The Wire (2002-)").is_err());
/// ```
pub fn extract_year_end(raw_show: &str) -> Result<i32, String> {
    let dash = raw_show
        .find('-')
        .ok_or_else(|| format!("Can't find '-' in {}", raw_show))?;
    let bracket_close = raw_show
        .find(')')
        .ok_or_else(|| format!("Can't find ')' in {}", raw_show))?;

    if bracket_close > dash + 1 {
        let year_str = &raw_show[dash + 1..bracket_close];
        year_str
            .parse()
            .map_err(|_| format!("Can't parse '{}' as year", year_str))
    } else {
        Err(format!("Can't extract end year from {}", raw_show))
    }
}

/// 単年の番組から年を抽出（Result版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::extract_single_year;
///
/// assert_eq!(extract_single_year("Chernobyl (2019)"), Ok(2019));
/// assert!(extract_single_year("Breaking Bad (2008-2013)").is_err());
/// ```
pub fn extract_single_year(raw_show: &str) -> Result<i32, String> {
    if raw_show.contains('-') {
        return Err(format!(
            "Can't extract single year from {} (contains dash)",
            raw_show
        ));
    }

    let bracket_open = raw_show
        .find('(')
        .ok_or_else(|| format!("Can't find '(' in {}", raw_show))?;
    let bracket_close = raw_show
        .find(')')
        .ok_or_else(|| format!("Can't find ')' in {}", raw_show))?;

    if bracket_close > bracket_open + 1 {
        let year_str = &raw_show[bracket_open + 1..bracket_close];
        year_str
            .parse()
            .map_err(|_| format!("Can't parse '{}' as year", year_str))
    } else {
        Err(format!("Can't extract single year from {}", raw_show))
    }
}

/// TV番組をパース（Result版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::{TvShow, parse_show};
///
/// assert_eq!(
///     parse_show("The Wire (2002-2008)"),
///     Ok(TvShow::new("The Wire", 2002, 2008))
/// );
/// assert_eq!(
///     parse_show("Chernobyl (2019)"),
///     Ok(TvShow::new("Chernobyl", 2019, 2019))
/// );
/// assert!(parse_show("Mad Men ()").is_err());
/// assert!(parse_show("(2002-2008)").is_err());
/// ```
pub fn parse_show(raw_show: &str) -> Result<TvShow, String> {
    let name = extract_name(raw_show)?;
    let year_start = extract_year_start(raw_show).or_else(|_| extract_single_year(raw_show))?;
    let year_end = extract_year_end(raw_show).or_else(|_| extract_single_year(raw_show))?;

    Ok(TvShow::new(&name, year_start, year_end))
}

// =============================================================================
// 7.3 Option から Result への変換
// =============================================================================

/// ok_or / ok_or_else のデモ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::option_to_result_demo;
///
/// option_to_result_demo();
/// ```
pub fn option_to_result_demo() {
    let year: Option<i32> = Some(996);
    let no_year: Option<i32> = None;

    assert_eq!(year.ok_or("no year given"), Ok(996));
    assert_eq!(no_year.ok_or("no year given"), Err("no year given"));
}

// =============================================================================
// 7.4 Result の主要メソッド
// =============================================================================

/// Result の map/and_then デモ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::result_methods_demo;
///
/// result_methods_demo();
/// ```
pub fn result_methods_demo() {
    let year: Result<i32, &str> = Ok(996);
    let no_year: Result<i32, &str> = Err("no year");

    // map: Ok の値を変換
    assert_eq!(year.map(|y| y * 2), Ok(1992));
    assert_eq!(no_year.map(|y| y * 2), Err("no year"));

    // and_then (flatMap相当): Ok なら Result を返す関数を適用
    assert_eq!(year.and_then(|y| Ok(y * 2)), Ok(1992));
    assert_eq!(no_year.and_then(|y| Ok(y * 2)), Err("no year"));

    // or_else: Err なら代替を使用
    assert_eq!(year.or_else(|_: &str| Ok::<i32, &str>(2020)), Ok(996));
    assert_eq!(no_year.or_else(|_: &str| Ok::<i32, &str>(2020)), Ok(2020));

    // ok: Option に変換
    assert_eq!(year.ok(), Some(996));
    assert_eq!(no_year.ok(), None);
}

// =============================================================================
// 7.5 年齢のバリデーション
// =============================================================================

/// 年齢を検証
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::validate_age;
///
/// assert_eq!(validate_age(25), Ok(25));
/// assert_eq!(validate_age(-5), Err("Age cannot be negative".to_string()));
/// assert_eq!(validate_age(200), Err("Age cannot be greater than 150".to_string()));
/// ```
pub fn validate_age(age: i32) -> Result<i32, String> {
    if age < 0 {
        Err("Age cannot be negative".to_string())
    } else if age > 150 {
        Err("Age cannot be greater than 150".to_string())
    } else {
        Ok(age)
    }
}

// =============================================================================
// 7.6 代数的データ型（ADT）- enum
// =============================================================================

/// 音楽ジャンル
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MusicGenre {
    HeavyMetal,
    Pop,
    HardRock,
    Jazz,
}

/// 活動期間
#[derive(Debug, Clone, PartialEq)]
pub enum YearsActive {
    /// 現在も活動中
    StillActive { since: i32 },
    /// 活動終了
    ActiveBetween { start: i32, end: i32 },
}

/// アーティスト
#[derive(Debug, Clone, PartialEq)]
pub struct Artist {
    pub name: String,
    pub genre: MusicGenre,
    pub origin: String,
    pub years_active: YearsActive,
}

impl Artist {
    pub fn new(name: &str, genre: MusicGenre, origin: &str, years_active: YearsActive) -> Self {
        Self {
            name: name.to_string(),
            genre,
            origin: origin.to_string(),
            years_active,
        }
    }
}

/// アーティストが指定期間に活動していたか判定
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::{Artist, MusicGenre, YearsActive, was_artist_active};
///
/// let metallica = Artist::new(
///     "Metallica",
///     MusicGenre::HeavyMetal,
///     "U.S.",
///     YearsActive::StillActive { since: 1981 }
/// );
///
/// let led_zeppelin = Artist::new(
///     "Led Zeppelin",
///     MusicGenre::HardRock,
///     "England",
///     YearsActive::ActiveBetween { start: 1968, end: 1980 }
/// );
///
/// assert!(was_artist_active(&metallica, 1990, 2000));
/// assert!(was_artist_active(&metallica, 2020, 2025));
/// assert!(was_artist_active(&led_zeppelin, 1970, 1975));
/// assert!(!was_artist_active(&led_zeppelin, 1990, 2000));
/// ```
pub fn was_artist_active(artist: &Artist, year_start: i32, year_end: i32) -> bool {
    match &artist.years_active {
        YearsActive::StillActive { since } => *since <= year_end,
        YearsActive::ActiveBetween { start, end } => *start <= year_end && *end >= year_start,
    }
}

/// アーティストの活動年数を計算
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::{Artist, MusicGenre, YearsActive, active_length};
///
/// let metallica = Artist::new(
///     "Metallica",
///     MusicGenre::HeavyMetal,
///     "U.S.",
///     YearsActive::StillActive { since: 1981 }
/// );
///
/// let led_zeppelin = Artist::new(
///     "Led Zeppelin",
///     MusicGenre::HardRock,
///     "England",
///     YearsActive::ActiveBetween { start: 1968, end: 1980 }
/// );
///
/// assert_eq!(active_length(&metallica, 2024), 43);
/// assert_eq!(active_length(&led_zeppelin, 2024), 12);
/// ```
pub fn active_length(artist: &Artist, current_year: i32) -> i32 {
    match &artist.years_active {
        YearsActive::StillActive { since } => current_year - since,
        YearsActive::ActiveBetween { start, end } => end - start,
    }
}

// =============================================================================
// 7.7 検索条件のモデリング
// =============================================================================

/// 検索条件
#[derive(Debug, Clone, PartialEq)]
pub enum SearchCondition {
    SearchByGenre(Vec<MusicGenre>),
    SearchByOrigin(Vec<String>),
    SearchByActiveYears { start: i32, end: i32 },
}

/// アーティストを検索
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::{
///     Artist, MusicGenre, YearsActive, SearchCondition, search_artists
/// };
///
/// let artists = vec![
///     Artist::new("Metallica", MusicGenre::HeavyMetal, "U.S.", YearsActive::StillActive { since: 1981 }),
///     Artist::new("Led Zeppelin", MusicGenre::HardRock, "England", YearsActive::ActiveBetween { start: 1968, end: 1980 }),
///     Artist::new("Bee Gees", MusicGenre::Pop, "England", YearsActive::ActiveBetween { start: 1958, end: 2003 }),
/// ];
///
/// // ジャンルで検索
/// let conditions = vec![SearchCondition::SearchByGenre(vec![MusicGenre::HeavyMetal])];
/// let result = search_artists(&artists, &conditions);
/// assert_eq!(result.len(), 1);
/// assert_eq!(result[0].name, "Metallica");
///
/// // 複数条件で検索
/// let conditions = vec![
///     SearchCondition::SearchByOrigin(vec!["England".to_string()]),
///     SearchCondition::SearchByActiveYears { start: 1960, end: 1970 },
/// ];
/// let result = search_artists(&artists, &conditions);
/// assert_eq!(result.len(), 2); // Led Zeppelin と Bee Gees
/// ```
pub fn search_artists<'a>(
    artists: &'a [Artist],
    conditions: &[SearchCondition],
) -> Vec<&'a Artist> {
    artists
        .iter()
        .filter(|artist| {
            conditions.iter().all(|condition| match condition {
                SearchCondition::SearchByGenre(genres) => genres.contains(&artist.genre),
                SearchCondition::SearchByOrigin(locations) => locations.contains(&artist.origin),
                SearchCondition::SearchByActiveYears { start, end } => {
                    was_artist_active(artist, *start, *end)
                }
            })
        })
        .collect()
}

// =============================================================================
// 7.8 支払い方法のモデリング
// =============================================================================

/// 支払い方法
#[derive(Debug, Clone, PartialEq)]
pub enum PaymentMethod {
    CreditCard { number: String, expiry: String },
    BankTransfer { account_number: String },
    Cash,
}

/// 支払い方法を説明
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::{PaymentMethod, describe_payment};
///
/// assert_eq!(
///     describe_payment(&PaymentMethod::CreditCard {
///         number: "1234".to_string(),
///         expiry: "12/25".to_string()
///     }),
///     "Credit card ending in 1234"
/// );
/// assert_eq!(
///     describe_payment(&PaymentMethod::BankTransfer {
///         account_number: "9876".to_string()
///     }),
///     "Bank transfer to account 9876"
/// );
/// assert_eq!(
///     describe_payment(&PaymentMethod::Cash),
///     "Cash payment"
/// );
/// ```
pub fn describe_payment(method: &PaymentMethod) -> String {
    match method {
        PaymentMethod::CreditCard { number, .. } => {
            format!("Credit card ending in {}", number)
        }
        PaymentMethod::BankTransfer { account_number } => {
            format!("Bank transfer to account {}", account_number)
        }
        PaymentMethod::Cash => "Cash payment".to_string(),
    }
}

// =============================================================================
// 7.9 カスタムエラー型
// =============================================================================

/// パースエラー
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    MissingName,
    MissingYear,
    InvalidYear(String),
    MissingBracket,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MissingName => write!(f, "Missing name"),
            ParseError::MissingYear => write!(f, "Missing year"),
            ParseError::InvalidYear(s) => write!(f, "Invalid year: {}", s),
            ParseError::MissingBracket => write!(f, "Missing bracket"),
        }
    }
}

/// TV番組をパース（カスタムエラー型版）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch07_result::{TvShow, ParseError, parse_show_typed};
///
/// assert_eq!(
///     parse_show_typed("Breaking Bad (2008-2013)"),
///     Ok(TvShow::new("Breaking Bad", 2008, 2013))
/// );
/// assert_eq!(
///     parse_show_typed("(2008-2013)"),
///     Err(ParseError::MissingName)
/// );
/// ```
pub fn parse_show_typed(raw_show: &str) -> Result<TvShow, ParseError> {
    let bracket_open = raw_show.find('(').ok_or(ParseError::MissingBracket)?;

    if bracket_open == 0 {
        return Err(ParseError::MissingName);
    }

    let name = raw_show[..bracket_open].trim().to_string();
    let bracket_close = raw_show.find(')').ok_or(ParseError::MissingBracket)?;

    let has_dash = raw_show.contains('-');

    if has_dash {
        let dash = raw_show.find('-').unwrap();
        let year_start_str = &raw_show[bracket_open + 1..dash];
        let year_end_str = &raw_show[dash + 1..bracket_close];

        let year_start: i32 = year_start_str
            .parse()
            .map_err(|_| ParseError::InvalidYear(year_start_str.to_string()))?;
        let year_end: i32 = year_end_str
            .parse()
            .map_err(|_| ParseError::InvalidYear(year_end_str.to_string()))?;

        Ok(TvShow::new(&name, year_start, year_end))
    } else {
        let year_str = &raw_show[bracket_open + 1..bracket_close];
        if year_str.is_empty() {
            return Err(ParseError::MissingYear);
        }
        let year: i32 = year_str
            .parse()
            .map_err(|_| ParseError::InvalidYear(year_str.to_string()))?;

        Ok(TvShow::new(&name, year, year))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_name() {
        assert_eq!(
            extract_name("The Wire (2002-2008)"),
            Ok("The Wire".to_string())
        );
        assert!(extract_name("(2002-2008)").is_err());
    }

    #[test]
    fn test_extract_year_start() {
        assert_eq!(extract_year_start("The Wire (2002-2008)"), Ok(2002));
        assert!(extract_year_start("The Wire (-2008)").is_err());
    }

    #[test]
    fn test_extract_year_end() {
        assert_eq!(extract_year_end("The Wire (2002-2008)"), Ok(2008));
        assert!(extract_year_end("The Wire (2002-)").is_err());
    }

    #[test]
    fn test_extract_single_year() {
        assert_eq!(extract_single_year("Chernobyl (2019)"), Ok(2019));
        assert!(extract_single_year("Breaking Bad (2008-2013)").is_err());
    }

    #[test]
    fn test_parse_show() {
        assert_eq!(
            parse_show("The Wire (2002-2008)"),
            Ok(TvShow::new("The Wire", 2002, 2008))
        );
        assert_eq!(
            parse_show("Chernobyl (2019)"),
            Ok(TvShow::new("Chernobyl", 2019, 2019))
        );
        assert!(parse_show("Invalid").is_err());
    }

    #[test]
    fn test_validate_age() {
        assert_eq!(validate_age(25), Ok(25));
        assert!(validate_age(-5).is_err());
        assert!(validate_age(200).is_err());
    }

    #[test]
    fn test_was_artist_active() {
        let metallica = Artist::new(
            "Metallica",
            MusicGenre::HeavyMetal,
            "U.S.",
            YearsActive::StillActive { since: 1981 },
        );
        assert!(was_artist_active(&metallica, 1990, 2000));

        let led_zeppelin = Artist::new(
            "Led Zeppelin",
            MusicGenre::HardRock,
            "England",
            YearsActive::ActiveBetween {
                start: 1968,
                end: 1980,
            },
        );
        assert!(was_artist_active(&led_zeppelin, 1970, 1975));
        assert!(!was_artist_active(&led_zeppelin, 1990, 2000));
    }

    #[test]
    fn test_active_length() {
        let metallica = Artist::new(
            "Metallica",
            MusicGenre::HeavyMetal,
            "U.S.",
            YearsActive::StillActive { since: 1981 },
        );
        assert_eq!(active_length(&metallica, 2024), 43);

        let led_zeppelin = Artist::new(
            "Led Zeppelin",
            MusicGenre::HardRock,
            "England",
            YearsActive::ActiveBetween {
                start: 1968,
                end: 1980,
            },
        );
        assert_eq!(active_length(&led_zeppelin, 2024), 12);
    }

    #[test]
    fn test_search_artists() {
        let artists = vec![
            Artist::new(
                "Metallica",
                MusicGenre::HeavyMetal,
                "U.S.",
                YearsActive::StillActive { since: 1981 },
            ),
            Artist::new(
                "Led Zeppelin",
                MusicGenre::HardRock,
                "England",
                YearsActive::ActiveBetween {
                    start: 1968,
                    end: 1980,
                },
            ),
        ];

        let conditions = vec![SearchCondition::SearchByGenre(vec![MusicGenre::HeavyMetal])];
        let result = search_artists(&artists, &conditions);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name, "Metallica");
    }

    #[test]
    fn test_describe_payment() {
        assert_eq!(
            describe_payment(&PaymentMethod::Cash),
            "Cash payment"
        );
        assert_eq!(
            describe_payment(&PaymentMethod::CreditCard {
                number: "1234".to_string(),
                expiry: "12/25".to_string()
            }),
            "Credit card ending in 1234"
        );
    }

    #[test]
    fn test_parse_show_typed() {
        assert_eq!(
            parse_show_typed("Breaking Bad (2008-2013)"),
            Ok(TvShow::new("Breaking Bad", 2008, 2013))
        );
        assert_eq!(parse_show_typed("(2008-2013)"), Err(ParseError::MissingName));
        assert_eq!(parse_show_typed("Test ()"), Err(ParseError::MissingYear));
    }
}
