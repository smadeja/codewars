defmodule Series do
  @spec summation(non_neg_integer(), non_neg_integer()) :: non_neg_integer()
  def summation(n, acc \\ 0)

  def summation(0, acc), do: acc

  def summation(n, acc) do
    summation(n - 1, acc + n)
  end
end
