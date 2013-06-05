%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc API for monitoring, controlling and getting information on the
%%       processes being monitored.
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
-module(process_monitor).

%% API
-export([list_supervisor_groups/0, list_group_children/1]).

%% ===================================================================
%% API
%% ===================================================================
list_supervisor_groups() ->
    [ {group_name(Supervisor), Supervisor} ||
        {Supervisor, _Pid, _Type, _Modules} <- supervisor:which_children(pm_core_sup)].

list_group_children(Group) ->
    [create_process_entry(Server, Pid) || {Server, Pid, _Type, _Modules} <-
                        supervisor:which_children(to_supervisor_name(Group))].

%% ===================================================================
%% Local
%% ===================================================================
group_name(Supervisor) ->
    list_to_atom(string:substr(atom_to_list(Supervisor), 8)). % remove pm_sup_ from supervisor name

to_supervisor_name(Group) ->
    case starts_with(pm_sup, Group) of
        true -> Group; % Someone gave us the supervisor name.
        false -> make_supervisor_name(Group)
    end.

make_supervisor_name(Group) ->
    list_to_atom("pm_sup_" ++ atom_to_list(Group)).

starts_with(What, In) ->
    string:str(atom_to_list(In), atom_to_list(What)) =:= 1.

create_process_entry(Server, Pid) ->
    {ok, {Command, OsPid}} = pm_server:info(Pid),
    [{server, Server}, {command, Command}, {os_pid, OsPid}].