-module(c_shell).
-export([start/0,eval/2]).

start() ->
    process_flag(trap_exit, true),
    go().

go() ->
    Val = eval(io:parse_erl_exprs('-> ')),
    if 
        Val == err ->
            ok;
        true ->
            go()
    end.

eval({ok, Exprs, _}) ->
    Id = spawn_link(c_shell, eval, [self(), Exprs]),
    receive
        {value, Res, _} ->
            io:format("Result: ~w~n", [Res]),
            receive
                {'EXIT', Id, _} ->
                    value
            end;
        {'EXIT', Id, Reason} ->
            io:format("Error: ~w!~n", [Reason]),
            err
    end;
eval(_) ->
    io:format("Syntax Error!~n", []),
    err.

eval(Id, Exprs) ->
    Id ! erl_eval:exprs(Exprs, []).


