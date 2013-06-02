
-module(process_monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Args), {pm_server, {pm_server, start_link, Args}, permanent, 5000, worker, [pm_server]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link({Name, RestartSpec, Jobs}) ->
    SupervisorName = create_supervisor_name(Name),
    supervisor:start_link({local, SupervisorName}, ?MODULE, [RestartSpec, Jobs]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([RestartSpec, Jobs]) ->
    Children = construct_supervised_children(Jobs),
    {ok, { {one_for_one, 5, 10}, Children} }.

%% ===================================================================
%% Local functions
%% ===================================================================
create_supervisor_name(JobGroupName) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(JobGroupName)).

%1>lists:seq(1,5).
%[1,2,3,4,5]
%2>[ {a,b, X} || X <- lists:seq(1,5)].
%[{a,b,1},{a,b,2},{a,b,3},{a,b,4},{a,b,5}]
construct_supervised_children(Jobs) -> [].


