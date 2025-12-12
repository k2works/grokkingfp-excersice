defmodule Ch02.ShoppingCart do
  @moduledoc """
  ショッピングカートの例

  状態を持つオブジェクト指向的なアプローチと
  純粋関数による関数型アプローチの違いを示します。
  """

  # =============================================================================
  # 純粋関数によるショッピングカート
  # =============================================================================

  @doc """
  アイテムリストから割引率を計算

  - "Book" が含まれている場合: 5%
  - それ以外: 0%

  ## Examples

      iex> Ch02.ShoppingCart.get_discount_percentage(["Apple", "Book", "Banana"])
      5

      iex> Ch02.ShoppingCart.get_discount_percentage(["Apple", "Banana"])
      0

      iex> Ch02.ShoppingCart.get_discount_percentage([])
      0

  """
  @spec get_discount_percentage([String.t()]) :: non_neg_integer()
  def get_discount_percentage(items) do
    if "Book" in items, do: 5, else: 0
  end

  @doc """
  複数の割引ルールを適用

  - "Book" が含まれている場合: 5%
  - "Laptop" が含まれている場合: 10%
  - アイテムが10個以上の場合: 15%
  - 複数の条件を満たす場合は最大の割引率を適用

  ## Examples

      iex> Ch02.ShoppingCart.get_max_discount(["Apple", "Book"])
      5

      iex> Ch02.ShoppingCart.get_max_discount(["Laptop"])
      10

      iex> Ch02.ShoppingCart.get_max_discount(Enum.map(1..15, &Integer.to_string/1))
      15

      iex> Ch02.ShoppingCart.get_max_discount(["Book", "Laptop"])
      10

  """
  @spec get_max_discount([String.t()]) :: non_neg_integer()
  def get_max_discount(items) do
    discounts = [
      book_discount(items),
      laptop_discount(items),
      bulk_discount(items)
    ]

    Enum.max(discounts)
  end

  defp book_discount(items), do: if("Book" in items, do: 5, else: 0)
  defp laptop_discount(items), do: if("Laptop" in items, do: 10, else: 0)
  defp bulk_discount(items), do: if(length(items) >= 10, do: 15, else: 0)

  @doc """
  カートにアイテムを追加（純粋関数 - 新しいリストを返す）

  ## Examples

      iex> Ch02.ShoppingCart.add_item(["Apple"], "Book")
      ["Apple", "Book"]

      iex> Ch02.ShoppingCart.add_item([], "Book")
      ["Book"]

  """
  @spec add_item([String.t()], String.t()) :: [String.t()]
  def add_item(items, item), do: items ++ [item]

  @doc """
  カートからアイテムを削除（純粋関数 - 新しいリストを返す）

  ## Examples

      iex> Ch02.ShoppingCart.remove_item(["Apple", "Book", "Banana"], "Book")
      ["Apple", "Banana"]

      iex> Ch02.ShoppingCart.remove_item(["Apple"], "Book")
      ["Apple"]

  """
  @spec remove_item([String.t()], String.t()) :: [String.t()]
  def remove_item(items, item), do: List.delete(items, item)

  @doc """
  カートの合計金額を計算

  ## Examples

      iex> prices = %{"Apple" => 100, "Book" => 1500, "Banana" => 80}
      iex> Ch02.ShoppingCart.calculate_total(["Apple", "Book"], prices)
      1600

      iex> prices = %{"Apple" => 100}
      iex> Ch02.ShoppingCart.calculate_total([], prices)
      0

  """
  @spec calculate_total([String.t()], %{String.t() => number()}) :: number()
  def calculate_total(items, prices) do
    items
    |> Enum.map(&Map.get(prices, &1, 0))
    |> Enum.sum()
  end

  @doc """
  割引後の合計金額を計算

  ## Examples

      iex> prices = %{"Apple" => 100, "Book" => 1500}
      iex> Ch02.ShoppingCart.calculate_final_total(["Apple", "Book"], prices)
      1520.0

  """
  @spec calculate_final_total([String.t()], %{String.t() => number()}) :: float()
  def calculate_final_total(items, prices) do
    total = calculate_total(items, prices)
    discount = get_discount_percentage(items)
    total * (1 - discount / 100)
  end
end
