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
-export([list_sgroups/0, list_sgroup_children/1, list_sgroup_job_groups/1, restart_sgroup/1,
		 restart_sgroup_job_group/2]).

%% ===================================================================
%% API
%% ===================================================================
list_sgroups() ->
    [ {group_name(Supervisor), Supervisor} ||
        {Supervisor, _Pid, _Type, _Modules} <- supervisor:which_children(pm_core_sup)].

list_sgroup_job_groups(SupervisorGroup) ->
    Children = list_sgroup_children(SupervisorGroup),
    unique_job_groups(Children).

list_sgroup_children(Group) ->
    [create_process_entry(Server, Pid) || {Server, Pid, _Type, _Modules} <-
											  supervisor:which_children(to_supervisor_name(Group))].

restart_sgroup(SupervisorGroup) ->
	for_each_server(fun(Entry, SupervisorName) ->
							{server, ServerName} = lists:keyfind(server, 1, Entry),
							restart_child(SupervisorName, ServerName)
					end, SupervisorGroup).

restart_sgroup_job_group(SupervisorGroup, JobGroup) ->
	for_each_server(fun(Entry, SupervisorName) ->
							{server, ServerName} = lists:keyfind(server, 1, Entry),
							case is_in_jobgroup(ServerName, JobGroup) of
								true -> restart_child(SupervisorName, ServerName);
								false -> ok
							end
					end, SupervisorGroup).

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

unique_job_groups(Children) ->
    sets:to_list(sets:from_list([create_job_group(lists:keyfind(server, 1, Entry)) || Entry <- Children])).

create_job_group({server, Server}) ->
    %% Server name is pm_server_<name>_<number>
    %% pm_server_ is 10 characters, so start at 11th character
    ServerString = atom_to_list(Server),
    EndIndex = string:rchr(ServerString, $_) - 1,
    list_to_atom(string:sub_string(ServerString, 11, EndIndex)).

for_each_server(Fn, SupervisorGroup) ->
	SupervisorName = make_supervisor_name(SupervisorGroup),
	lists:foreach(fun(Entry) -> Fn(Entry, SupervisorName) end, list_sgroup_children(SupervisorGroup)).

is_in_jobgroup(Server, JobGroup) ->
	create_job_group({server, Server}) =:= JobGroup.

restart_child(SupervisorName, ServerName) ->
	supervisor:terminate_child(SupervisorName, ServerName),
	supervisor:restart_child(SupervisorName, ServerName).



