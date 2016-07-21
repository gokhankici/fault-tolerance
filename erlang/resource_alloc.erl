%% In this allocator a resource which has been allocated to a process will not be
%% returned to the allocator if the process making the allocation terminates
%% (erroneously or normally) without freeing the resource.
%% 
%% This can be circumvented by:
%% + Setting the server to trap EXIT signals (process_flag(trap_exit, true)).
%% + Creating links between the allocator and processes which have allocated one
%%   or more resources.
%% + Handling EXIT signals from such processes.
%% 
%% page: 81,82 & 111

-module(resource_alloc).
-export([start/1, server/2, allocate/0, free/1, start_server/1]).

start(Resources) ->
    Pid = spawn(allocator, server, [Resources,[]]),
    register(resource_alloc, Pid).

%% ####################################################
%% The interface functions
%% ####################################################
allocate() ->
    request(alloc).

%% Kill the process that does erroneous freeing
free(Resource) ->
    %% request({free,Resource}).
    resource_alloc ! {self(), {free, Resource}}, % <--
    receive                                      % <--
        {resource_alloc, error} ->               % <--
            exit(bad_allocation);                % <--
        {resource_alloc, Reply} ->               % <--
            Reply                                % <--
    end.                                         % <--

request(Request) ->
    resource_alloc ! {self(),Request},
    receive
        {resource_alloc,Reply} ->
            Reply
    end.
%% ####################################################

%% [EXTRA] Free resources on process termination
check(Free, Allocated, From) ->                             % <--
    case lists:keysearch(From, 2, Allocated) of             % <--
        false ->                                            % <--
            server(Free, Allocated);                        % <--
        {value, {R, From}} ->                               % <--
            check([R|Free],                                 % <--
                  lists:delete({R, From}, Allocated), From) % <--
    end.                                                    % <--

server(Free, Allocated) ->
    receive
        {From,alloc} ->
            allocate(Free, Allocated, From);
        {From,{free,R}} ->
            free(Free, Allocated, From, R);
        {'EXIT', From, _ } ->                   % <-- extra check
            check(Free, Allocated, From)        % <-- 
    end.

%% [EXTRA] Additional function for setting the server 
%% to trap EXIT signals using `process_flag`
start_server(Resources) ->                      % <--
    process_flag(trap_exit, true),              % <--
    server(Resources, []).                      % <--

allocate([R|Free], Allocated, From) ->
    link(From),                                 % <-- create a link during the allocation
    From ! {resource_alloc,{yes,R}},
    server(Free, [{R,From}|Allocated]);
allocate([], Allocated, From) ->
    From ! {resource_alloc,no},
    server([], Allocated).

free(Free, Allocated, From, R) ->
    case lists:member({R,From}, Allocated) of
        true ->
            From ! {resource_alloc, yes},
            Allocated1 = lists:delete({R, From}, Allocated),
            case lists:keysearch(From, 2, Allocated1) of    % <-- unlink on deletion
                false ->                                    % <--
                    unlink(From);                           % <--
                _ ->                                        % <--
                    true                                    % <--
            end,                                            % <--
            server([R|Free], Allocated1);
        false ->
            From ! {resource_alloc, error},
            server(Free, Allocated)
    end.
