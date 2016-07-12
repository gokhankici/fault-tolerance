-module(keep_alive2).
-export([start/0, loop_/0, new_process/4, main/0, fun1/0]).

start() ->
    Pid = spawn(?MODULE, loop_, []),
    %% register the process with name 'keep_alive'
    register(keep_alive, Pid).

loop_() ->
    %% trap EXIT signals using `process_flag`
    process_flag(trap_exit, true),
    loop([]).

loop(Processes) ->
    receive
        {From, {new_proc, Name, Mod, Func, Args}} ->
            Id = spawn_link(Mod, Func, Args),
            register(Name, Id),
            From ! {keep_alive, started},
            loop([{Id, Name, Mod, Func, Args} | Processes]);
        
        {'EXIT', Id, _} ->
            case lists:keysearch(Id, 1, Processes) of
                false ->
                    loop(Processes);
                {value, {Id, Name, Mod, Func, Args}} ->
                    P = lists:delete({Id,Name,Mod,Func,Args},
                                     Processes),
                    Id1 = spawn_link(Mod, Func, Args),
                    io:fwrite("restarted ~p~n", [Name]),
                    register(Name, Id1),
                    loop([{Id1, Name, Mod, Func, Args} | P])
            end
    end.

new_process(Name, Mod, Func, Args) ->
    keep_alive ! {self(), {new_proc, Name, Mod, Func, Args}},
    receive
        {keep_alive, started} ->
            true
    end.

fun1() ->
    receive
        {From,N} ->
            From ! (N + 1),
            exit(for_no_reason)
    end.
    

main() ->
    start(),
    new_process(fun1_name, ?MODULE, fun1, []),
    N = 42,
    fun1_name ! {self(), N},
    receive
        N2 ->
            io:fwrite("sent ~p, received ~p!~n", [N, N2]),
            ok
    end.

