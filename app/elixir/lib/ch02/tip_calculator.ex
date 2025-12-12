defmodule Ch02.TipCalculator do
  @moduledoc """
  チップ計算の例

  グループの人数に基づいてチップの割合を計算します。
  純粋関数の例として使用します。
  """

  @doc """
  グループの人数からチップの割合を計算

  - 6人以上: 20%
  - 1-5人: 10%
  - 0人: 0%

  ## Examples

      iex> Ch02.TipCalculator.get_tip_percentage(["Alice", "Bob", "Charlie", "David", "Eve", "Frank"])
      20

      iex> Ch02.TipCalculator.get_tip_percentage(["Alice", "Bob"])
      10

      iex> Ch02.TipCalculator.get_tip_percentage([])
      0

  """
  @spec get_tip_percentage([String.t()]) :: non_neg_integer()
  def get_tip_percentage(names) do
    count = length(names)

    cond do
      count > 5 -> 20
      count > 0 -> 10
      true -> 0
    end
  end

  @doc """
  チップ額を計算

  ## Examples

      iex> Ch02.TipCalculator.calculate_tip(1000, ["Alice", "Bob", "Charlie", "David", "Eve", "Frank"])
      200.0

      iex> Ch02.TipCalculator.calculate_tip(1000, ["Alice", "Bob"])
      100.0

  """
  @spec calculate_tip(number(), [String.t()]) :: float()
  def calculate_tip(bill_amount, names) do
    bill_amount * get_tip_percentage(names) / 100
  end

  @doc """
  チップ込みの合計額を計算

  ## Examples

      iex> Ch02.TipCalculator.calculate_total_with_tip(1000, ["Alice", "Bob"])
      1100.0

  """
  @spec calculate_total_with_tip(number(), [String.t()]) :: float()
  def calculate_total_with_tip(bill_amount, names) do
    bill_amount + calculate_tip(bill_amount, names)
  end

  @doc """
  1人あたりの支払額を計算

  ## Examples

      iex> Ch02.TipCalculator.calculate_per_person(1000, ["Alice", "Bob"])
      550.0

  """
  @spec calculate_per_person(number(), [String.t()]) :: float()
  def calculate_per_person(bill_amount, names) do
    total = calculate_total_with_tip(bill_amount, names)

    case length(names) do
      0 -> 0.0
      n -> total / n
    end
  end

  @doc """
  カスタムチップ率で計算

  ## Examples

      iex> Ch02.TipCalculator.calculate_custom_tip(1000, 15)
      150.0

  """
  @spec calculate_custom_tip(number(), number()) :: float()
  def calculate_custom_tip(bill_amount, tip_percentage) do
    bill_amount * tip_percentage / 100
  end
end
