%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Server for monitoring jobs.
%%%
%%% @copyright Copyright (c) 2013, Regents of the University of Michigan.
%%% All rights reserved.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% ===================================================================
-module(pm_core_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-define(CHILD(Name, Arg), {Name, {pm_sup, start_link, [Name,Arg]}, permanent, 5000, supervisor, [pm_sup]}).


-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @private
init([]) ->
    Children = setup_supervisor_children(),
    {ok, {{one_for_one, 5, 10}, Children}}.

setup_supervisor_children() ->
    construct_children(file:consult(filename:join([os:getenv("ETC_DIR"), "pm.config"]))).

construct_children({ok, Terms}) ->
    lists:flatten(lists:map(
            fun({SupervisorGroup, _RestartSpec, _Jobs} = SupervisorArgs) ->
                SupervisorName = create_supervisor_name(SupervisorGroup),
                ?CHILD(SupervisorName, SupervisorArgs)
            end, Terms));
construct_children(_Ignore) -> ok.

create_supervisor_name(SupervisorGroup) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(SupervisorGroup)).