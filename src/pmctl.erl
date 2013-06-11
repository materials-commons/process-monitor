%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc CLI for controlling the process monitor.
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
-module(pmctl).

%% API main for script
-export([main/1]).

-record(cargs,
        {
            sgroup :: string(),
            jgroup :: string(),
            commands :: [atom()]
        }).

%%%===================================================================
%%% Main
%%%===================================================================
main([]) -> usage();
main(Args) ->
    setup(),
    CArgs = parse_results(getopt:parse(opt_spec(), Args)),
    execute_commands(CArgs),
    ok.

%%%===================================================================
%%% Local
%%%===================================================================

setup() ->
    net_kernel:start(['pmcli@127.0.0.1', longnames]),
    auth:set_cookie('process_monitor').

usage({error, {Error, Description}}) ->
    io:format(standard_error, "~nError: ~p ~p~n~n", [Error, Description]),
    usage();
usage(Message) ->
    io:format(standard_error, "~nError: ~s~n~n", [Message]),
    usage().

usage() ->
    getopt:usage(opt_spec(), "pmctl", "<command>" ),
    io:format(standard_error, " Commands: stop, start, restart, listsgroups, listjgroups, listchildren~n", []),
    halt().

opt_spec() ->
    [
        {sgroup, $s, "sgroup", string, "The sgroup to use."},
        {jgroup, $j, "jgroup", string, "The jgroup to use."}
    ].

parse_results({error, {_Error, _Description}} = ErrorValue) ->
    usage(ErrorValue);
parse_results({ok, {Values, Commands}}) ->
    SGroup = retrieve_key(sgroup, Values),
    JGroup = retrieve_key(jgroup, Values),
    #cargs{sgroup = to_atom(SGroup), jgroup = to_atom(JGroup), commands = Commands}.

retrieve_key(Key, Values) ->
    key_value(lists:keyfind(Key, 1, Values)).

key_value({_Key, Value}) -> Value;
key_value(false) -> false.

to_atom(Value) when is_atom(Value) -> Value;
to_atom(Value) when is_list(Value) -> list_to_atom(Value).

execute_commands(#cargs{sgroup = SGroup, jgroup = JGroup, commands = Commands}) ->
    lists:foreach(
            fun ("listsgroups") ->
                    listsgroups();
                ("listjgroups") ->
                    listjgroups(SGroup);
                ("listchildren") ->
                    listchildren(SGroup);
                ("stop") ->
                    stop(SGroup, JGroup);
                ("start") ->
                    start(SGroup, JGroup);
                ("restartsgroup") ->
                    restartsgroup(SGroup);
                ("restartjgroup") ->
                    restartjgroup(SGroup, JGroup);
                (Command) ->
                    usage("Unknown Command: " ++ Command)
            end, Commands).

listsgroups() ->
    lists:foreach(
        fun(SGroup) ->
            io:format("~p~n", [SGroup])
        end, process_monitor:list_sgroups()).

listjgroups(false) ->
    usage("No SGroup specified");
listjgroups(SGroup) ->
    case process_monitor:list_sgroup_job_groups(SGroup) of
        {error, badgroup} -> usage("Unknown SGroup: " ++ SGroup);
        Groups -> lists:foreach(fun(Group) -> io:format("~p~n", [Group]) end, Groups)
    end.

listchildren(false) ->
    usage("No SGroup specified");
listchildren(SGroup) ->
    case process_monitor:list_sgroup_children(SGroup) of
        {error, badgroup} ->
            usage("Unknown SGroup: " ++ SGroup);
        Children ->
            lists:foreach(
                    fun([{server, Server}, {command, Command}, {os_pid, Pid}]) ->
                        io:format("~p, ~p, ~p~n", [Server, Command, Pid])
                    end, Children)
    end.

stop(_V1, _V2) -> ok.
start(_V1, _V2) -> ok.

restartjgroup(_V1, _V2) -> ok.

restartsgroup(false) -> usage("No SGroup specified");
restartsgroup(SGroup) ->
    io:format("Restarting SGroup: ~s~n", [SGroup]),
    process_monitor:restart_sgroup(SGroup).

