%%% ===================================================================
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%%
%%% @doc Server for handling API requests.
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

-export([
            list_sgroups/0, list_sgroups/1,
            list_sgroup_children/1, list_sgroup_children/2,
            list_sgroup_job_groups/1, list_sgroup_job_groups/2,
            restart_sgroup/1, restart_sgroup/2,
            restart_sgroup_job_group/2, restart_sgroup_job_group/3,
            stop_sgroup/1, stop_sgroup/2,
            stop_sgroup_job_job_group/2, start_sgroup_job_group/3,
            start_sgroup/1, start_sgroup/2,
            start_sgroup_job_group/2, start_sgroup_job_group/3
        ]).

list_sgroups() ->
    list_sgroups(localnode()).

list_sgroups(Node) ->
    pm_api_server:list_sgroups(Node).

list_sgroup_children(SGroup) ->
    list_sgroup_children(localnode(), SGroup).

list_sgroup_children(Node, SGroup) ->
    pm_api_server:list_sgroup_children(Node, SGroup).

list_sgroup_job_groups(SGroup) ->
    list_sgroup_job_groups(localnode(), SGroup).

list_sgroup_job_groups(Node, SGroup) ->
    pm_api_server:list_sgroup_job_groups(Node, SGroup).

restart_sgroup(SGroup) ->
    restart_sgroup(localnode(), SGroup).

restart_sgroup(Node, SGroup) ->
    pm_api_server:restart_sgroup(Node, SGroup).

restart_sgroup_job_group(SGroup, JobGroup) ->
    restart_sgroup_job_group(localnode(), SGroup, JobGroup).

restart_sgroup_job_group(Node, SGroup, JobGroup) ->
    pm_api_server:restart_sgroup_job_group(Node, SGroup, JobGroup).

stop_sgroup(SGroup) ->
    stop_sgroup(localnode(), SGroup).

stop_sgroup(Node, SGroup) ->
    pm_api_server:stop_sgroup(Node, SGroup).

stop_sgroup_job_job_group(SGroup, JGroup) ->
    stop_sgroup_job_job_group(localnode(), SGroup, JGroup).

stop_sgroup_job_job_group(Node, SGroup, JGroup) ->
    pm_api_server:stop_sgroup_job_job_group(Node, SGroup, JGroup).

start_sgroup(SGroup) ->
    start_sgroup(localnode(), SGroup).

start_sgroup(Node, SGroup) ->
    pm_api_server:start_sgroup(Node, SGroup).

start_sgroup_job_group(SGroup, JGroup) ->
    start_sgroup_job_group(localnode(), SGroup, JGroup).

start_sgroup_job_group(Node, SGroup, JGroup) ->
    pm_api_server:start_sgroup_job_group(Node, SGroup, JGroup).

localnode() -> pm_api_server:localnode().