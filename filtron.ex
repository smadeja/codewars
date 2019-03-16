defmodule Filtron do
  @spec divisible_by(list(integer()), integer()) :: list(integer())
  def divisible_by(numbers, divisor) do
    numbers |> Enum.filter(&(&1 |> rem(divisor) == 0))
  end
end
