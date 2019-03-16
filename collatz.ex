defmodule Collatz do
  @spec collatz(pos_integer()) :: String.t()
  def collatz(n) do
    collatz_seq(n) |> Enum.join("->")
  end

  @spec collatz_seq(pos_integer(), String.t()) :: list(pos_integer())
  defp collatz_seq(n, acc \\ [])

  defp collatz_seq(1, acc) do
    [1 | acc] |> Enum.reverse()
  end

  defp collatz_seq(n, acc) do
    collatz_seq(collatz_f(n), [n | acc])
  end

  @spec collatz_f(pos_integer()) :: pos_integer()

  defp collatz_f(n) when n |> rem(2) == 0 do
    n |> div(2)
  end

  defp collatz_f(n) do
    3 * n + 1
  end
end
