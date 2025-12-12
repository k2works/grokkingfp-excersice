//! 第3章: イミュータブルなデータ操作
//!
//! Rust ではデフォルトで不変（イミュータブル）です。
//! Vec のスライス操作と新しいコレクションの作成を学びます。

// =============================================================================
// 3.1 イミュータブルとは
// =============================================================================

/// Rust では `let` で宣言した変数はデフォルトで不変
/// 変更するには新しい値を作成する
///
/// # Examples
///
/// ```
/// let list = vec!["Apple", "Book"];
/// // list.push("Mango"); // コンパイルエラー！
///
/// // 新しい Vec を作成
/// let mut new_list = list.clone();
/// new_list.push("Mango");
///
/// assert_eq!(list.len(), 2);     // 元のリストは変わらない
/// assert_eq!(new_list.len(), 3); // 新しいリストに追加された
/// ```
pub fn immutable_demo() {
    let list = vec!["Apple", "Book"];
    let mut new_list = list.clone();
    new_list.push("Mango");
    assert_eq!(list.len(), 2);
    assert_eq!(new_list.len(), 3);
}

// =============================================================================
// 3.2 Vec の基本操作
// =============================================================================

/// 要素を追加した新しい Vec を返す（元の Vec は変更しない）
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::appended;
///
/// let apple_book = vec!["Apple", "Book"];
/// let apple_book_mango = appended(&apple_book, "Mango");
///
/// assert_eq!(apple_book.len(), 2);      // 元のリストは変わらない
/// assert_eq!(apple_book_mango.len(), 3);
/// assert_eq!(apple_book_mango, vec!["Apple", "Book", "Mango"]);
/// ```
pub fn appended<T: Clone>(list: &[T], element: T) -> Vec<T> {
    let mut result = list.to_vec();
    result.push(element);
    result
}

/// 2つのスライスを結合した新しい Vec を返す
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::appended_all;
///
/// let ab = vec!["a", "b"];
/// let cd = vec!["c", "d"];
/// let abcd = appended_all(&ab, &cd);
///
/// assert_eq!(abcd, vec!["a", "b", "c", "d"]);
/// assert_eq!(ab.len(), 2); // 元のリストは変わらない
/// ```
pub fn appended_all<T: Clone>(list: &[T], other: &[T]) -> Vec<T> {
    let mut result = list.to_vec();
    result.extend_from_slice(other);
    result
}

/// 最初の2要素を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::first_two;
///
/// assert_eq!(first_two(&["a", "b", "c"]), vec!["a", "b"]);
/// assert_eq!(first_two(&["x", "y"]), vec!["x", "y"]);
/// assert_eq!(first_two(&["z"]), vec!["z"]);
/// ```
pub fn first_two<T: Clone>(list: &[T]) -> Vec<T> {
    list.iter().take(2).cloned().collect()
}

/// 最後の2要素を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::last_two;
///
/// assert_eq!(last_two(&["a", "b", "c"]), vec!["b", "c"]);
/// assert_eq!(last_two(&["x", "y"]), vec!["x", "y"]);
/// assert_eq!(last_two(&["z"]), vec!["z"]);
/// ```
pub fn last_two<T: Clone>(list: &[T]) -> Vec<T> {
    let len = list.len();
    if len <= 2 {
        list.to_vec()
    } else {
        list[len - 2..].to_vec()
    }
}

// =============================================================================
// 3.3 リストの変換
// =============================================================================

/// 最初の2要素を末尾に移動
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::move_first_two_to_end;
///
/// assert_eq!(
///     move_first_two_to_end(&["a", "b", "c"]),
///     vec!["c", "a", "b"]
/// );
/// assert_eq!(
///     move_first_two_to_end(&["a", "b", "c", "d"]),
///     vec!["c", "d", "a", "b"]
/// );
/// ```
pub fn move_first_two_to_end<T: Clone>(list: &[T]) -> Vec<T> {
    if list.len() <= 2 {
        return list.to_vec();
    }
    let first_two = &list[..2];
    let without_first_two = &list[2..];
    appended_all(without_first_two, first_two)
}

/// 最後の要素の前に新しい要素を挿入
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::insert_before_last;
///
/// assert_eq!(
///     insert_before_last(&["a", "b"], "c"),
///     vec!["a", "c", "b"]
/// );
/// assert_eq!(
///     insert_before_last(&["x"], "y"),
///     vec!["y", "x"]
/// );
/// ```
pub fn insert_before_last<T: Clone>(list: &[T], element: T) -> Vec<T> {
    if list.is_empty() {
        return vec![element];
    }
    let len = list.len();
    let without_last = &list[..len - 1];
    let last = &list[len - 1..];
    let mut result = without_last.to_vec();
    result.push(element);
    result.extend_from_slice(last);
    result
}

// =============================================================================
// 3.4 旅程の再計画
// =============================================================================

/// 指定した都市の前に新しい都市を挿入
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::replan;
///
/// let plan_a = vec!["Paris", "Berlin", "Kraków"];
/// let plan_b = replan(&plan_a, "Vienna", "Kraków");
///
/// assert_eq!(plan_b, vec!["Paris", "Berlin", "Vienna", "Kraków"]);
/// assert_eq!(plan_a, vec!["Paris", "Berlin", "Kraków"]); // 元の計画は変わらない
/// ```
pub fn replan<'a>(plan: &[&'a str], new_city: &'a str, before_city: &str) -> Vec<&'a str> {
    let before_city_index = plan.iter().position(|&c| c == before_city);

    match before_city_index {
        Some(index) => {
            let cities_before = &plan[..index];
            let cities_after = &plan[index..];
            let mut result = cities_before.to_vec();
            result.push(new_city);
            result.extend_from_slice(cities_after);
            result
        }
        None => {
            // 指定した都市が見つからない場合は末尾に追加
            let mut result = plan.to_vec();
            result.push(new_city);
            result
        }
    }
}

/// ジェネリック版の旅程再計画
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::replan_generic;
///
/// let plan: Vec<String> = vec!["Paris".into(), "Berlin".into(), "Kraków".into()];
/// let krakow = "Kraków".to_string();
/// let plan_b = replan_generic(&plan, "Vienna".into(), &krakow);
///
/// assert_eq!(plan_b, vec!["Paris", "Berlin", "Vienna", "Kraków"]);
/// ```
pub fn replan_generic<T: Clone + PartialEq>(plan: &[T], new_city: T, before_city: &T) -> Vec<T> {
    let before_city_index = plan.iter().position(|c| c == before_city);

    match before_city_index {
        Some(index) => {
            let mut result = plan[..index].to_vec();
            result.push(new_city);
            result.extend_from_slice(&plan[index..]);
            result
        }
        None => {
            let mut result = plan.to_vec();
            result.push(new_city);
            result
        }
    }
}

// =============================================================================
// 3.5 String とスライスの類似性
// =============================================================================

/// 名前を省略形にする
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::abbreviate;
///
/// assert_eq!(abbreviate("Alonzo Church"), "A. Church");
/// assert_eq!(abbreviate("Alan Turing"), "A. Turing");
/// ```
pub fn abbreviate(name: &str) -> String {
    if let Some(space_index) = name.find(' ') {
        let initial = &name[..1];
        let last_name = &name[space_index + 1..];
        format!("{}. {}", initial, last_name)
    } else {
        name.to_string()
    }
}

/// 文字列の部分を取得
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::substring;
///
/// assert_eq!(substring("hello", 1, 4), "ell");
/// assert_eq!(substring("Rust", 0, 2), "Ru");
/// ```
pub fn substring(s: &str, start: usize, end: usize) -> &str {
    &s[start..end]
}

// =============================================================================
// 3.6 中央への挿入
// =============================================================================

/// リストの中央に要素を挿入
///
/// # Examples
///
/// ```
/// use grokking_fp::ch03_immutable_data::insert_at_middle;
///
/// assert_eq!(
///     insert_at_middle(&["a", "b", "c", "d"], "X"),
///     vec!["a", "b", "X", "c", "d"]
/// );
/// assert_eq!(
///     insert_at_middle(&["a", "b"], "X"),
///     vec!["a", "X", "b"]
/// );
/// ```
pub fn insert_at_middle<T: Clone>(list: &[T], element: T) -> Vec<T> {
    let middle = list.len() / 2;
    let before = &list[..middle];
    let after = &list[middle..];
    let mut result = before.to_vec();
    result.push(element);
    result.extend_from_slice(after);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_appended() {
        let list = vec![1, 2, 3];
        let result = appended(&list, 4);
        assert_eq!(result, vec![1, 2, 3, 4]);
        assert_eq!(list, vec![1, 2, 3]); // 元のリストは変わらない
    }

    #[test]
    fn test_appended_all() {
        let list1 = vec![1, 2];
        let list2 = vec![3, 4];
        let result = appended_all(&list1, &list2);
        assert_eq!(result, vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_first_two() {
        assert_eq!(first_two(&[1, 2, 3, 4, 5]), vec![1, 2]);
        assert_eq!(first_two(&[1]), vec![1]);
        assert_eq!(first_two::<i32>(&[]), Vec::<i32>::new());
    }

    #[test]
    fn test_last_two() {
        assert_eq!(last_two(&[1, 2, 3, 4, 5]), vec![4, 5]);
        assert_eq!(last_two(&[1]), vec![1]);
        assert_eq!(last_two::<i32>(&[]), Vec::<i32>::new());
    }

    #[test]
    fn test_move_first_two_to_end() {
        assert_eq!(move_first_two_to_end(&[1, 2, 3, 4]), vec![3, 4, 1, 2]);
        assert_eq!(move_first_two_to_end(&[1, 2]), vec![1, 2]);
        assert_eq!(move_first_two_to_end(&[1]), vec![1]);
    }

    #[test]
    fn test_insert_before_last() {
        assert_eq!(insert_before_last(&[1, 2, 3], 99), vec![1, 2, 99, 3]);
        assert_eq!(insert_before_last(&[1], 99), vec![99, 1]);
        assert_eq!(insert_before_last::<i32>(&[], 99), vec![99]);
    }

    #[test]
    fn test_replan() {
        let plan = vec!["Paris", "Berlin", "Kraków"];
        let new_plan = replan(&plan, "Vienna", "Kraków");
        assert_eq!(new_plan, vec!["Paris", "Berlin", "Vienna", "Kraków"]);
        assert_eq!(plan, vec!["Paris", "Berlin", "Kraków"]); // 元は変わらない
    }

    #[test]
    fn test_replan_city_not_found() {
        let plan = vec!["Paris", "Berlin"];
        let new_plan = replan(&plan, "Vienna", "NotExist");
        assert_eq!(new_plan, vec!["Paris", "Berlin", "Vienna"]); // 末尾に追加
    }

    #[test]
    fn test_abbreviate() {
        assert_eq!(abbreviate("Alonzo Church"), "A. Church");
        assert_eq!(abbreviate("SingleName"), "SingleName");
    }

    #[test]
    fn test_insert_at_middle() {
        assert_eq!(insert_at_middle(&[1, 2, 3, 4], 99), vec![1, 2, 99, 3, 4]);
        assert_eq!(insert_at_middle(&[1, 2, 3], 99), vec![1, 99, 2, 3]);
        assert_eq!(insert_at_middle::<i32>(&[], 99), vec![99]);
    }
}
