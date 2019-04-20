defmodule Pentabonacci do
  require Integer

  def count_odd_pentaFib(0), do: 0
  def count_odd_pentaFib(1), do: 1
  def count_odd_pentaFib(2), do: 1
  def count_odd_pentaFib(3), do: 1
  def count_odd_pentaFib(4), do: 1

  def count_odd_pentaFib(number) when number > 4 do
    fifo = :queue.from_list([0, 1, 1, 2, 4])
    count_odd_pentaFib(number, 5, fifo, 1)
  end

  def count_odd_pentaFib(number, current_number, _fifo, acc)
      when number < current_number do
    acc
  end

  def count_odd_pentaFib(number, current_number, fifo, acc)
      when number >= current_number do
    sum = :queue.to_list(fifo) |> Enum.sum()

    {_, new_fifo} = :queue.out(fifo)
    new_fifo = :queue.in(sum, new_fifo)

    new_acc = if sum |> Integer.is_odd(), do: acc + 1, else: acc
    count_odd_pentaFib(number, current_number + 1, new_fifo, new_acc)
  end
end
