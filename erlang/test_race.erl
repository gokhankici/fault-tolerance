-module(test_race).
-export([test1/0, foo1/0, proc_reg/1,
        test2/0, main2/0]).

foo1() ->
    io:fwrite("hello world~n").

%% test_race.erl:11: The call erlang:register(Name::atom(),Pid::pid()) might fail
%% due to a possible race condition caused by its combination with the
%% erlang:whereis(Name::any()) call in test_race.erl on line 8
proc_reg(Name) ->
    case whereis(Name) of
        undefined ->
            Pid = spawn(?MODULE, foo1, []),
            register(Name,Pid);
        _ ->
            true
    end.

test1() ->
    spawn(?MODULE, proc_reg, [ali]),
    spawn(?MODULE, proc_reg, [ali]),
    true.

%% ####################################################

foo2() ->
    receive
        {From, {record, Name, Pid}} ->
            case whereis(Name) of
                undefined ->
                    register(Name,Pid),
                    From ! {self(), done};
                _ ->
                    true
            end
    end.
    
test2() ->
    Pid = spawn(fun() -> foo2() end),
    Me = self(),
    Pid ! {self(), {record, proc1, Pid}},

    %% case whereis(proc1) of
    %%     undefined ->
    %%         register(proc1, Me);
    %%     _ ->
    %%         true
    %% end,
    
    receive
        {Pid, done} ->
            true
    end.

main2() ->
    spawn(?MODULE, test2, []).
