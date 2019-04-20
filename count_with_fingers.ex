defmodule Chisanbop do
  @spec valid?(String.t(), integer() | nil, integer()) :: boolean()
  def valid?(representation, previous_grapheme \\ nil, acc \\ 0)

  def valid?(<<grapheme::utf8, remaining_graphemes::bitstring>>, previous_grapheme, acc)
      when grapheme in [?0, ?1] and previous_grapheme in [?0, ?1, nil] do
    if valid_sequence?(acc, grapheme, previous_grapheme) do
      valid?(remaining_graphemes, grapheme, acc + 1)
    else
      false
    end
  end

  def valid?(<<_grapheme::utf8, _remaining_graphemes::bitstring>>, _previous_grapheme, _acc) do
    false
  end

  def valid?(<<>>, _previous_grapheme, acc) do
    acc == 10
  end

  @spec to_integer(String.t(), integer(), integer()) :: integer()
  def to_integer(representation, finger_index \\ 0, acc \\ 0)

  def to_integer(<<?1, remaining_graphemes::bitstring>>, finger_index, acc) do
    finger_value =
      case finger_index do
        finger_index when finger_index in [0, 1, 2, 3] -> 10
        4 -> 50
        5 -> 5
        finger_index when finger_index in [6, 7, 8, 9] -> 1
      end

    to_integer(remaining_graphemes, finger_index + 1, acc + finger_value)
  end

  def to_integer(<<?0, remaining_graphemes::bitstring>>, finger_index, acc) do
    to_integer(remaining_graphemes, finger_index + 1, acc)
  end

  def to_integer(<<>>, 10, acc) do
    acc
  end

  @spec valid_sequence?(integer(), integer(), integer() | nil) :: boolean()
  defp valid_sequence?(finger_index, grapheme, previous_grapheme) do
    case finger_index do
      finger_index when finger_index in [1, 2, 3] ->
        !(previous_grapheme == ?1 && grapheme == ?0)

      finger_index when finger_index in [7, 8, 9] ->
        !(previous_grapheme == ?0 && grapheme == ?1)

      finger_index when finger_index in [0, 4, 5, 6] ->
        true
    end
  end
end

defmodule CountWithFingers do
  def decode(representation) do
    if representation |> Chisanbop.valid?() do
      representation |> Chisanbop.to_integer()
    else
      -1
    end
  end
end
