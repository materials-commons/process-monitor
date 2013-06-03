
-module(pm_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
    {ok, { RestartSpec, Children} }.

%% ===================================================================
%% Local functions
%% ===================================================================
create_supervisor_name(JobGroupName) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(JobGroupName)).

%%
construct_supervised_children(Jobs) ->
    lists:flatten(lists:map(
                fun({_JobName, Count, Command}) ->
                    [?CHILD(Command) || _Ignore <- lists:seq(1,Count)]
                end, Jobs)).


