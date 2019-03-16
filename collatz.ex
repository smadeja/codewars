defmodule Collatz do
  @spec collatz(pos_integer()) :: String.t()
  def collatz(n) do
    collatz_string(n, n |> Integer.to_string())
  end

  @spec collatz_string(pos_integer(), String.t()) :: String.t()

  defp collatz_string(1, acc), do: acc

  defp collatz_string(n, acc) do
    new_n = collatz_f(n)
    collatz_string(new_n, acc <> "->#{new_n}")
  end

  @spec collatz_f(pos_integer()) :: pos_integer()

  defp collatz_f(n) when n |> rem(2) == 0 do
    n |> div(2)
  end

  defp collatz_f(n) do
    3 * n + 1
  end
end
