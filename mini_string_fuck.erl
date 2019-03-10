-module(mini_string_fuck).
-export([my_first_interpreter/1]).


-spec my_first_interpreter(string()) -> string().

my_first_interpreter(Code) ->
  interpret_mini_string_fuck(Code, 0, "").


-spec interpret_mini_string_fuck(string(), non_neg_integer(), string()) -> string().

interpret_mini_string_fuck([Command | RemainingCommands], MemoryCell, Accumulator)
  when Command =:= $+ ->

  interpret_mini_string_fuck(RemainingCommands, (MemoryCell + 1) rem 256, Accumulator);

interpret_mini_string_fuck([Command | RemainingCommands], MemoryCell, Accumulator)
  when Command =:= $. ->

  interpret_mini_string_fuck(RemainingCommands, MemoryCell, [MemoryCell | Accumulator]);

interpret_mini_string_fuck([_IllegalCommand | RemainingCommands], MemoryCell, Accumulator) ->
  interpret_mini_string_fuck(RemainingCommands, MemoryCell, Accumulator);

interpret_mini_string_fuck([], _MemoryCell, Accumulator) ->
  lists:reverse(Accumulator).
