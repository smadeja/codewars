defmodule BrainLuckInterpreter do
  alias BrainLuckInterpreter.{BracketMatching, Memory}

  @spec interpret(String.t(), String.t(), list(integer()), integer(), integer(), String.t()) ::
    String.t()

  def interpret(
    program,
    input \\ "",
    memory \\ [],
    program_index \\ 0,
    memory_index \\ 0,
    output \\ "") do

    case program |> String.at(program_index) do
      ">" ->
        program_index = program_index + 1
        memory_index = memory_index + 1

        interpret(program, input, memory, program_index, memory_index, output)

      "<" ->
        program_index = program_index + 1
        memory_index = memory_index - 1

        interpret(program, input, memory, program_index, memory_index, output)

      "+" ->
        memory = Memory.increment_at(memory, memory_index)
        program_index = program_index + 1

        interpret(program, input, memory, program_index, memory_index, output)

      "-" ->
        memory = Memory.decrement_at(memory, memory_index)
        program_index = program_index + 1

        interpret(program, input, memory, program_index, memory_index, output)

      "." ->
        program_index = program_index + 1
        output = output <> <<Memory.at(memory, memory_index)>>

        interpret(program, input, memory, program_index, memory_index, output)

      "," ->
        {current_input, input} = String.split_at(input, 1)
        memory = Memory.store_at(memory, memory_index, current_input)
        program_index = program_index + 1

        interpret(program, input, memory, program_index, memory_index, output)

      "[" ->
        program_index =
          if Memory.at(memory, memory_index) == 0 do
            BracketMatching.find_matching_bracket(program, program_index) + 1
          else
            program_index + 1
          end

        interpret(program, input, memory, program_index, memory_index, output)

      "]" ->
        program_index =
          if Memory.at(memory, memory_index) != 0 do
            BracketMatching.find_matching_bracket(program, program_index) + 1
          else
            program_index + 1
          end

        interpret(program, input, memory, program_index, memory_index, output)

      whitespace_char when whitespace_char in [" ", "\n"] ->
        program_index = program_index + 1
        interpret(program, input, memory, program_index, memory_index, output)

      nil ->
        output
    end
  end
end

defmodule BrainLuckInterpreter.Memory do
  defmodule IndexOutOfBounds do
    defexception [:message]
  end

  @spec at(list(integer()), integer()) :: integer() | no_return()
  def at(memory, index) do
    if 0 <= index do
      memory |> Enum.at(reversed_index(index), 0)
    else
      raise(IndexOutOfBounds, "the index of #{index} is out of bounds")
    end
  end

  @spec increment_at(list(integer()), integer()) :: list(integer()) | no_return()
  def increment_at(memory, index) do
    alter_at(memory, index, fn current_value ->
      (current_value + 1) |> mod(256)
    end)
  end

  @spec decrement_at(list(integer()), integer()) :: list(integer()) | no_return()
  def decrement_at(memory, index) do
    alter_at(memory, index, fn current_value ->
      (current_value - 1) |> mod(256)
    end)
  end

  @spec store_at(list(integer()), integer(), integer() | binary()) ::
    list(integer()) | no_return()

  def store_at(memory, index, <<value>>) do
    store_at(memory, index, value)
  end

  def store_at(memory, index, value) do
    alter_at(memory, index, fn _current_value ->
      value |> mod(256)
    end)
  end

  @spec alter_at(list(integer()), integer(), (integer() -> integer())) ::
    list(integer()) | no_return()

  defp alter_at(memory, index, fun) do
    memory_length = length(memory)

    cond do
      0 <= index and index < memory_length ->
        memory |> List.update_at(reversed_index(index), fun)
      memory_length <= index ->
        memory |> pad(index + 1) |> alter_at(index, fun)
      index < 0 ->
        raise(IndexOutOfBounds, "the index of #{index} is out of bounds")
    end
  end

  @spec pad(list(integer()), integer()) :: list(integer())

  defp pad(memory, target_length) when length(memory) == target_length do
    memory
  end

  defp pad(memory, target_length) when target_length > length(memory) do
    pad([0 | memory], target_length)
  end

  @spec reversed_index(integer()) :: integer()
  defp reversed_index(index) do
    (-index) - 1
  end

  # Copied from the Integer module so it works on a pre–1.4 version of Elixir
  @spec mod(integer(), neg_integer() | pos_integer()) :: integer()
  defp mod(dividend, divisor) do
    remainder = rem(dividend, divisor)

    if remainder * divisor < 0 do
      remainder + divisor
    else
      remainder
    end
  end
end

defmodule BrainLuckInterpreter.BracketMatching do
  @spec find_matching_bracket(String.t(), integer()) :: integer()
  def find_matching_bracket(text, index) do
    case String.at(text, index) do
      "[" -> find_closing_bracket(text, index + 1, ["["])
      "]" -> find_opening_bracket(text, index - 1, ["]"])
    end
  end

  @spec find_closing_bracket(String.t(), integer(), list(String.t())) ::
    integer() | nil

  defp find_closing_bracket(text, index, stack) do
    case String.at(text, index) do
      "[" ->
        find_closing_bracket(text, index + 1, ["[" | stack])
      "]" ->
        case hd(stack) do
          "]" ->
            nil
          "[" ->
            if length(tl(stack)) == 0 do
              index
            else
              find_closing_bracket(text, index + 1, tl(stack))
            end
        end
      nil ->
        nil
      _ ->
        find_closing_bracket(text, index + 1, stack)
    end
  end

  @spec find_opening_bracket(String.t(), integer(), list(String.t())) ::
    integer() | nil

  defp find_opening_bracket(text, index, stack) do
    case String.at(text, index) do
      "]" ->
        find_opening_bracket(text, index - 1, ["]" | stack])
      "[" ->
        case hd(stack) do
          "[" ->
            nil
          "]" ->
            if length(tl(stack)) == 0 do
              index
            else
              find_opening_bracket(text, index - 1, tl(stack))
            end
        end
      nil ->
        nil
      _ ->
        find_opening_bracket(text, index - 1, stack)
    end
  end
end

defmodule Brainluck do
  @spec brain_luck(String.t(), String.t()) :: String.t()
  def brain_luck(program, inputs) do
    BrainLuckInterpreter.interpret(program, inputs)
  end
end
