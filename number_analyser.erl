%% A server which could be used in a telephone exchange to analyze
%% telephone numbers dialled by users of the exchange
%% page 121 & 80

-module(number_analyser).
-export([start/0,server/1]).
-export([add_number/2,analyse/1]).

%% creates a number analyzer
start() ->
    register(number_analyser,
             spawn(number_analyser, server, [nil])).

%% #######################################
%% The interface functions
%% #######################################
add_number(Seq, Dest) ->
    request({add_number,Seq,Dest}).

analyse(Seq) ->
    request({analyse,Seq}).

lookup(_, _) ->
    not_implemented.

insert(_, _, _) ->
    not_implemented.

request(Req) ->
    number_analyser ! {self(), Req},
    receive
        {number_analyser,Reply} ->
            Reply
    end.

%% #######################################

%% The server.
server(AnalTable) ->
    receive
        {From, {analyse,Seq}} ->
            case catch lookup(Seq, AnalTable) of
                {'EXIT', _} ->
                    From ! {number_analyser, error};
                Result ->
                    From ! {number_analyser, Result}
            end,
            server(AnalTable);

        {From, {add_number, Seq, Key}} ->
            From ! {number_analyser, ack},
            case catch insert(Seq, Key, AnalTable) of
                {'EXIT', _} ->
                    From ! {number_analyser, error},
                    server(AnalTable); % Table not changed
                NewTable ->
                    server(NewTable)
            end
    end.
