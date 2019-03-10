-module(whiner).
-export([whiner/1, whine/1]).
-import(timer, [sleep/1]).

-spec whiner(pid()) -> done.
whiner(Parent) ->
  receive_messages(undefined, Parent).

-spec whine(pid()) -> no_return().
whine(Target) ->
  Target ! {whine, "Is anybody out there?"},
  sleep(1000),
  whine(Target).

-spec receive_messages(pid() | undefined, pid()) -> done.
receive_messages(Whiner, WhineTarget) ->
  receive
    {awake} when Whiner =:= undefined ->
      NewWhiner = spawn(whiner, whine, [WhineTarget]),
      receive_messages(NewWhiner, WhineTarget);

    {give_up} ->
      if
        Whiner =/= undefined ->
          exit(Whiner, shut_up),
          done;

        true ->
          done
      end;

    _MalformedMessage ->
      receive_messages(Whiner, WhineTarget)
  end.
