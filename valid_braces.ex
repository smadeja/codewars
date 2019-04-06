defmodule BracketMatching do
  @opening_brackets [?(, ?[, ?{]
  @closing_brackets [?), ?], ?}]

  @matching_brackets %{
    ?) => ?(,
    ?] => ?[,
    ?} => ?{
  }

  @spec brackets_match?(String.t(), list(integer())) :: boolean()
  def brackets_match?(brackets, stack \\ [])

  def brackets_match?("", []), do: true
  def brackets_match?("", [_ | _]), do: false

  def brackets_match?(<<bracket::utf8, remaining_brackets::binary>>, stack)
      when is_list(stack) and bracket in @opening_brackets do
    brackets_match?(remaining_brackets, [bracket | stack])
  end

  def brackets_match?(<<bracket::utf8, _remaining_brackets::binary>>, [])
      when bracket in @closing_brackets do
    false
  end

  def brackets_match?(
        <<bracket::utf8, remaining_brackets::binary>>,
        [stack_top | remaining_stack]
      )
      when bracket in @closing_brackets do
    if stack_top == @matching_brackets[bracket] do
      brackets_match?(remaining_brackets, remaining_stack)
    else
      false
    end
  end
end

defmodule Challenge do
  def valid_braces(brackets) do
    brackets |> BracketMatching.brackets_match?()
  end
end
