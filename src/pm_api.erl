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
-module(pm_api).

%% API
-export([
            list_sgroups/0, list_sgroup_children/1, list_sgroup_job_groups/1,
            restart_sgroup/1, restart_sgroup_job_group/2,
            stop_sgroup/1, stop_sgroup_job_group/2,
            start_sgroup/1, start_sgroup_job_group/2
        ]).

%% ===================================================================
%% API
%% ===================================================================
list_sgroups() ->
    [ {group_name(Supervisor), Supervisor} ||
            {Supervisor, _Pid, _Type, _Modules} <-
                supervisor:which_children(pm_core_sup), Supervisor /= pm_api_server].

list_sgroup_job_groups(SupervisorGroup) ->
    Fun =
        fun() ->
            Children = list_sgroup_children(SupervisorGroup),
            unique_job_groups(Children)
        end,
    execute_api_command(Fun).

list_sgroup_children(Group) ->
    Fun =
        fun() ->
            [create_process_entry(Server, Pid) || {Server, Pid, _Type, _Modules} <-
                                              supervisor:which_children(to_supervisor_name(Group))]
        end,
    execute_api_command(Fun).

restart_sgroup(SupervisorGroup) ->
    Fun =
        fun() ->
            for_each_server(fun restart_child/2, SupervisorGroup)
        end,
    execute_api_command(Fun).

restart_sgroup_job_group(SupervisorGroup, JobGroup) ->
    Fun =
        fun() ->
            for_each_server_job_group(fun restart_child/2, SupervisorGroup, JobGroup)
        end,
    execute_api_command(Fun).

stop_sgroup(SupervisorGroup) ->
    Fun =
        fun() ->
            for_each_server(fun supervisor:terminate_child/2, SupervisorGroup)
        end,
    execute_api_command(Fun).

stop_sgroup_job_group(SupervisorGroup, JobGroup) ->
    Fun =
        fun() ->
            for_each_server_job_group(fun supervisor:terminate_child/2, SupervisorGroup, JobGroup)
        end,
    execute_api_command(Fun).

start_sgroup(SupervisorGroup) ->
    Fun =
        fun() ->
            for_each_server(fun supervisor:restart_child/2, SupervisorGroup)
        end,
    execute_api_command(Fun).

start_sgroup_job_group(SupervisorGroup, JobGroup) ->
    Fun =
        fun() ->
            for_each_server_job_group(fun supervisor:terminate_child/2, SupervisorGroup, JobGroup)
        end,
    execute_api_command(Fun).

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
	lists:foreach(
                fun(Entry) ->
                    {server, ServerName} = lists:keyfind(server, 1, Entry),
                    Fn(SupervisorName, ServerName)
                end, list_sgroup_children(SupervisorGroup)).

for_each_server_job_group(Fn, SupervisorGroup, JobGroup) ->
    Fun =
        fun(SupervisorName, ServerName) ->
            case is_in_jobgroup(ServerName, JobGroup) of
                true -> Fn(SupervisorName, ServerName);
                false -> ok
            end
        end,
    for_each_server(Fun, SupervisorGroup).

is_in_jobgroup(Server, JobGroup) ->
	create_job_group({server, Server}) =:= JobGroup.

restart_child(SupervisorName, ServerName) ->
	supervisor:terminate_child(SupervisorName, ServerName),
	supervisor:restart_child(SupervisorName, ServerName).

execute_api_command(Fun) ->
    try
        Fun()
    catch
        _Error:_Reason -> {error, badgroup}
    end.


