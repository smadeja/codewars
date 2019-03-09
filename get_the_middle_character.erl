-module(get_the_middle_character).
-export([middle/1]).

-spec middle(string()) -> string().
middle(String) ->
  case string:length(String) of
    Length when Length rem 2 =:= 0 ->
      string:slice(String, Length div 2 - 1, 2);

    Length ->
      string:slice(String, Length div 2, 1)
  end.
