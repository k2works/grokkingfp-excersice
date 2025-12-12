//! 第5章: flatMap とネスト構造
//!
//! イテレータの `flat_map` を使ったネスト構造の平坦化を学びます。
//! Rust では for 内包表記の代わりにイテレータチェーンを使います。

// =============================================================================
// 5.1 flatten と flat_map
// =============================================================================

/// 本を表す構造体
#[derive(Debug, Clone, PartialEq)]
pub struct Book {
    pub title: String,
    pub authors: Vec<String>,
}

impl Book {
    pub fn new(title: &str, authors: Vec<&str>) -> Self {
        Self {
            title: title.to_string(),
            authors: authors.into_iter().map(String::from).collect(),
        }
    }
}

/// flatten: ネストしたイテレータを平坦化
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::flatten_vec;
///
/// let nested = vec![vec![1, 2], vec![3], vec![4, 5, 6]];
/// let flat = flatten_vec(nested);
///
/// assert_eq!(flat, vec![1, 2, 3, 4, 5, 6]);
/// ```
pub fn flatten_vec<T>(nested: Vec<Vec<T>>) -> Vec<T> {
    nested.into_iter().flatten().collect()
}

/// 本のリストから全著者を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Book, get_all_authors};
///
/// let books = vec![
///     Book::new("FP in Scala", vec!["Chiusano", "Bjarnason"]),
///     Book::new("The Hobbit", vec!["Tolkien"]),
/// ];
///
/// let authors = get_all_authors(&books);
/// assert_eq!(authors, vec!["Chiusano", "Bjarnason", "Tolkien"]);
/// ```
pub fn get_all_authors(books: &[Book]) -> Vec<String> {
    books
        .iter()
        .flat_map(|book| book.authors.clone())
        .collect()
}

/// map だけだとネストする例
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Book, get_authors_nested};
///
/// let books = vec![
///     Book::new("FP in Scala", vec!["Chiusano", "Bjarnason"]),
///     Book::new("The Hobbit", vec!["Tolkien"]),
/// ];
///
/// let nested = get_authors_nested(&books);
/// assert_eq!(nested.len(), 2); // 2つのVec
/// assert_eq!(nested[0], vec!["Chiusano", "Bjarnason"]);
/// assert_eq!(nested[1], vec!["Tolkien"]);
/// ```
pub fn get_authors_nested(books: &[Book]) -> Vec<Vec<String>> {
    books.iter().map(|book| book.authors.clone()).collect()
}

// =============================================================================
// 5.2 flat_map によるリストサイズの変化
// =============================================================================

/// 要素数が増える flat_map
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::expand;
///
/// let result = expand(&[1, 2, 3]);
/// assert_eq!(result, vec![1, 11, 2, 12, 3, 13]);
/// ```
pub fn expand(list: &[i32]) -> Vec<i32> {
    list.iter().flat_map(|&i| vec![i, i + 10]).collect()
}

/// 要素数が同じ flat_map（map と同等）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::double_flat_map;
///
/// let result = double_flat_map(&[1, 2, 3]);
/// assert_eq!(result, vec![2, 4, 6]);
/// ```
pub fn double_flat_map(list: &[i32]) -> Vec<i32> {
    list.iter().flat_map(|&i| vec![i * 2]).collect()
}

/// 要素数が減る flat_map（filter の代替）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::filter_even_flatmap;
///
/// let result = filter_even_flatmap(&[1, 2, 3, 4, 5]);
/// assert_eq!(result, vec![2, 4]);
/// ```
pub fn filter_even_flatmap(list: &[i32]) -> Vec<i32> {
    list.iter()
        .flat_map(|&i| if i % 2 == 0 { vec![i] } else { vec![] })
        .collect()
}

// =============================================================================
// 5.3 ネストした flat_map
// =============================================================================

/// 映画を表す構造体
#[derive(Debug, Clone, PartialEq)]
pub struct Movie {
    pub title: String,
}

impl Movie {
    pub fn new(title: &str) -> Self {
        Self {
            title: title.to_string(),
        }
    }
}

/// 著者から映画化作品を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Movie, book_adaptations};
///
/// let tolkien_movies = book_adaptations("Tolkien");
/// assert_eq!(tolkien_movies.len(), 2);
///
/// let chiusano_movies = book_adaptations("Chiusano");
/// assert!(chiusano_movies.is_empty());
/// ```
pub fn book_adaptations(author: &str) -> Vec<Movie> {
    if author == "Tolkien" {
        vec![
            Movie::new("An Unexpected Journey"),
            Movie::new("The Desolation of Smaug"),
        ]
    } else {
        vec![]
    }
}

/// ネストした flat_map でレコメンデーションを生成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Book, get_recommendations};
///
/// let books = vec![
///     Book::new("FP in Scala", vec!["Chiusano", "Bjarnason"]),
///     Book::new("The Hobbit", vec!["Tolkien"]),
/// ];
///
/// let recommendations = get_recommendations(&books);
/// assert_eq!(recommendations.len(), 2);
/// assert!(recommendations[0].contains("An Unexpected Journey"));
/// assert!(recommendations[0].contains("Tolkien"));
/// assert!(recommendations[0].contains("The Hobbit"));
/// ```
pub fn get_recommendations(books: &[Book]) -> Vec<String> {
    books
        .iter()
        .flat_map(|book| {
            let book_title = book.title.clone();
            book.authors.iter().flat_map(move |author| {
                let author_name = author.clone();
                let book_title_inner = book_title.clone();
                book_adaptations(&author_name).into_iter().map(move |movie| {
                    format!(
                        "You may like {}, because you liked {}'s {}",
                        movie.title, author_name, book_title_inner
                    )
                })
            })
        })
        .collect()
}

// =============================================================================
// 5.4 イテレータチェーン（Rust の for 内包表記相当）
// =============================================================================

/// 全組み合わせを生成
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::all_combinations;
///
/// let result = all_combinations(&[1, 2], &[10, 20]);
/// assert_eq!(result, vec![11, 21, 12, 22]);
/// ```
pub fn all_combinations(xs: &[i32], ys: &[i32]) -> Vec<i32> {
    xs.iter()
        .flat_map(|&x| ys.iter().map(move |&y| x + y))
        .collect()
}

/// 3つのリストの組み合わせ
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::three_way_combinations;
///
/// let result = three_way_combinations(&[1, 2], &[10, 20], &[100, 200]);
/// assert_eq!(result, vec![111, 211, 121, 221, 112, 212, 122, 222]);
/// ```
pub fn three_way_combinations(xs: &[i32], ys: &[i32], zs: &[i32]) -> Vec<i32> {
    xs.iter()
        .flat_map(|&x| {
            ys.iter()
                .flat_map(move |&y| zs.iter().map(move |&z| x + y + z))
        })
        .collect()
}

// =============================================================================
// 5.5 円内の点の判定
// =============================================================================

/// 点を表す構造体
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

/// 点が円の内側にあるかを判定
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Point, is_inside};
///
/// assert!(is_inside(&Point::new(1, 1), 2));   // 1² + 1² = 2 <= 4
/// assert!(!is_inside(&Point::new(5, 2), 2)); // 5² + 2² = 29 > 4
/// ```
pub fn is_inside(point: &Point, radius: i32) -> bool {
    radius * radius >= point.x * point.x + point.y * point.y
}

/// 全組み合わせを生成して判定結果を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Point, check_all_combinations};
///
/// let points = vec![Point::new(5, 2), Point::new(1, 1)];
/// let radiuses = vec![2, 1];
///
/// let results = check_all_combinations(&points, &radiuses);
/// assert_eq!(results.len(), 4);
/// ```
pub fn check_all_combinations(points: &[Point], radiuses: &[i32]) -> Vec<String> {
    radiuses
        .iter()
        .flat_map(|&r| {
            points.iter().map(move |point| {
                format!(
                    "Point({},{}) is within a radius of {}: {}",
                    point.x,
                    point.y,
                    r,
                    is_inside(point, r)
                )
            })
        })
        .collect()
}

/// フィルタリングして円内の点のみを返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Point, find_inside_points};
///
/// let points = vec![Point::new(5, 2), Point::new(1, 1)];
/// let radiuses = vec![2, 1];
///
/// let inside = find_inside_points(&points, &radiuses);
/// assert_eq!(inside, vec!["Point(1,1) is within a radius of 2"]);
/// ```
pub fn find_inside_points(points: &[Point], radiuses: &[i32]) -> Vec<String> {
    radiuses
        .iter()
        .flat_map(|&r| {
            points
                .iter()
                .filter(move |point| is_inside(point, r))
                .map(move |point| format!("Point({},{}) is within a radius of {}", point.x, point.y, r))
        })
        .collect()
}

/// flat_map でフィルタリング（filter の代わり）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::{Point, find_inside_points_flatmap};
///
/// let points = vec![Point::new(5, 2), Point::new(1, 1)];
/// let radiuses = vec![2, 1];
///
/// let inside = find_inside_points_flatmap(&points, &radiuses);
/// assert_eq!(inside, vec!["Point(1,1) is within a radius of 2"]);
/// ```
pub fn find_inside_points_flatmap(points: &[Point], radiuses: &[i32]) -> Vec<String> {
    radiuses
        .iter()
        .flat_map(|&r| {
            points.iter().flat_map(move |point| {
                if is_inside(point, r) {
                    vec![format!(
                        "Point({},{}) is within a radius of {}",
                        point.x, point.y, r
                    )]
                } else {
                    vec![]
                }
            })
        })
        .collect()
}

// =============================================================================
// 5.6 型の変化
// =============================================================================

/// Vec から始まると Vec が返る
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::list_result;
///
/// let result = list_result();
/// assert_eq!(result, vec![2, 1, 4, 2]); // 重複あり
/// ```
pub fn list_result() -> Vec<i32> {
    vec![1, 2]
        .into_iter()
        .flat_map(|a| vec![2, 1].into_iter().map(move |b| a * b))
        .collect()
}

/// HashSet を使うと重複が除去される
///
/// # Examples
///
/// ```
/// use grokking_fp::ch05_flatmap::set_result;
/// use std::collections::HashSet;
///
/// let result = set_result();
/// assert!(result.contains(&1));
/// assert!(result.contains(&2));
/// assert!(result.contains(&4));
/// assert_eq!(result.len(), 3); // 重複なし
/// ```
pub fn set_result() -> std::collections::HashSet<i32> {
    use std::collections::HashSet;

    let set_a: HashSet<i32> = [1, 2].into_iter().collect();
    let list_b = vec![2, 1];

    set_a
        .into_iter()
        .flat_map(|a| list_b.iter().map(move |&b| a * b))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_flatten_vec() {
        let nested = vec![vec![1, 2], vec![3], vec![]];
        assert_eq!(flatten_vec(nested), vec![1, 2, 3]);
    }

    #[test]
    fn test_get_all_authors() {
        let books = vec![
            Book::new("FP in Scala", vec!["Chiusano", "Bjarnason"]),
            Book::new("The Hobbit", vec!["Tolkien"]),
        ];
        let authors = get_all_authors(&books);
        assert_eq!(authors, vec!["Chiusano", "Bjarnason", "Tolkien"]);
    }

    #[test]
    fn test_expand() {
        assert_eq!(expand(&[1, 2]), vec![1, 11, 2, 12]);
    }

    #[test]
    fn test_filter_even_flatmap() {
        assert_eq!(filter_even_flatmap(&[1, 2, 3, 4, 5, 6]), vec![2, 4, 6]);
    }

    #[test]
    fn test_book_adaptations() {
        let movies = book_adaptations("Tolkien");
        assert_eq!(movies.len(), 2);
        assert_eq!(movies[0].title, "An Unexpected Journey");

        let empty = book_adaptations("Unknown");
        assert!(empty.is_empty());
    }

    #[test]
    fn test_get_recommendations() {
        let books = vec![
            Book::new("FP in Scala", vec!["Chiusano"]),
            Book::new("The Hobbit", vec!["Tolkien"]),
        ];
        let recs = get_recommendations(&books);
        assert_eq!(recs.len(), 2);
        assert!(recs[0].contains("An Unexpected Journey"));
        assert!(recs[1].contains("The Desolation of Smaug"));
    }

    #[test]
    fn test_all_combinations() {
        assert_eq!(all_combinations(&[1, 2], &[10, 20]), vec![11, 21, 12, 22]);
    }

    #[test]
    fn test_three_way_combinations() {
        let result = three_way_combinations(&[1], &[10], &[100, 200]);
        assert_eq!(result, vec![111, 211]);
    }

    #[test]
    fn test_is_inside() {
        assert!(is_inside(&Point::new(0, 0), 1));
        assert!(is_inside(&Point::new(1, 0), 1));
        assert!(!is_inside(&Point::new(2, 0), 1));
    }

    #[test]
    fn test_check_all_combinations() {
        let points = vec![Point::new(1, 1)];
        let radiuses = vec![2];
        let results = check_all_combinations(&points, &radiuses);
        assert_eq!(results.len(), 1);
        assert!(results[0].contains("true"));
    }

    #[test]
    fn test_find_inside_points() {
        let points = vec![Point::new(5, 2), Point::new(1, 1), Point::new(0, 0)];
        let radiuses = vec![2, 1];
        let inside = find_inside_points(&points, &radiuses);
        assert!(inside.contains(&"Point(1,1) is within a radius of 2".to_string()));
        assert!(inside.contains(&"Point(0,0) is within a radius of 2".to_string()));
        assert!(inside.contains(&"Point(0,0) is within a radius of 1".to_string()));
    }

    #[test]
    fn test_list_result() {
        let result = list_result();
        assert_eq!(result.len(), 4);
        assert_eq!(result, vec![2, 1, 4, 2]);
    }

    #[test]
    fn test_set_result() {
        let result = set_result();
        assert_eq!(result.len(), 3);
        assert!(result.contains(&1));
        assert!(result.contains(&2));
        assert!(result.contains(&4));
    }
}
