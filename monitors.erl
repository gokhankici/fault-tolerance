-module(monitors).
-export([main/0]).

-import(c, [flush/0]).

start_critic() ->
    RestarterPid = spawn(fun() -> restarter() end),
    io:fwrite("restarter pid: ~p~n", [RestarterPid]),
    RestarterPid.

%% Check condition until it returns true or a number of times
%% while sleeping between the checks.
poll_try(_, _, 0)            -> false;
poll_try(Cond, Sleep, Times) ->
    Res = Cond(),
    if Res  -> true;
       true -> timer:sleep(Sleep),
               poll_try(Cond, Sleep, Times - 1)
    end.

register_force(Name,Pid) ->
    case whereis(Name) of
        undefined -> register(Name, Pid);
        Other     -> unregister(Name),
                     exit(Other, normal),       % give it a peaceful death
                     register(Name, Pid)
    end.

restarter() ->
    process_flag(trap_exit, true),
    Me  = self(),
    Critic = spawn_link(fun() -> critic() end),
    io:fwrite("critic pid: ~p~n", [Critic]),
    register_force(critic_name, Critic),
    receive
        {'EXIT', Me, shutdown} ->               % not a crash
            case whereis(critic_name) of
                undefined -> ok;
                Pid       -> exit(Pid, shutdown),
                             ok
            end;
        {'EXIT', Critic, normal} ->             % not a crash
            ok;
        {'EXIT', Critic, shutdown} ->           % manual termination, not a crash
            ok;
        {'EXIT', Critic, _} ->
            restarter();
        _ -> ok                                 % ignore
    end.


judge(Band, Album) ->
    Ref = make_ref(),
    io:fwrite("requesting judgement ~n"),
    poll_try(fun() -> whereis(critic_name) /= undefined end, 500, 4),
    critic_name ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
            timeout
    end.

critic() ->
    receive
        {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic().

main() ->
    Restarter = start_critic(),

    timer:sleep(1000),

    Criticism = judge("The Doors", "Light my Firewall"),
    io:fwrite("judgement 1: ~p~n", [Criticism]),

    exit(whereis(critic_name), kill),

    Criticism2 = judge("Rage Against the Turing Machine", "Unit Testify"),
    io:fwrite("judgement 2: ~p~n", [Criticism2]),

    exit(Restarter, shutdown),
    ok.

