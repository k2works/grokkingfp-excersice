package ch02

/**
 * 第2章: 問題のあるショッピングカート（アンチパターン）
 *
 * 状態を持つクラスの問題点を示すための例です。
 */
class ShoppingCartBad {
    /** 問題: 可変リストを外部に公開しているため、誰でも中身を変更できる */
    val items: MutableList<String> = mutableListOf()

    /** 問題: items とは別に管理されるフラグ。items と食い違う可能性がある */
    private var bookAdded = false

    fun addItem(item: String) {
        items.add(item)
        if (item == "Book") {
            bookAdded = true
        }
    }

    fun getDiscountPercentage(): Int = if (bookAdded) 5 else 0

    /** 読み取り専用の型で返しても、中身は内部の可変リストそのもの */
    fun itemsView(): List<String> = items
}
