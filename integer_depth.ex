defmodule IntegerDepth do
  @spec compute_depth(pos_integer()) :: pos_integer()

  def compute_depth(n) do
    depth(n, 0, MapSet.new())
  end

  @spec depth(pos_integer(), non_neg_integer(), MapSet.t()) :: pos_integer()

  defp depth(_n, k, %MapSet{map: map}) when map_size(map) == 10, do: k

  defp depth(n, k, seen_digits) do
    new_k = k + 1
    new_seen_digits = MapSet.union(seen_digits, unique_digits(n * new_k))

    depth(n, new_k, new_seen_digits)
  end

  @spec unique_digits(integer()) :: MapSet.t()

  defp unique_digits(n) do
    n |> Integer.digits() |> MapSet.new()
  end
end
