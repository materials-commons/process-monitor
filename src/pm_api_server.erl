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
-module(pm_api_server).

%% API
-export([
            start_link/0,
            list_sgroups/1, list_sgroup_children/2, list_sgroup_job_groups/2,
            restart_sgroup/2, restart_sgroup_job_group/3,
            stop_sgroup/2, stop_sgroup_job_group/3,
            start_sgroup/2, start_sgroup_job_group/3,
            localnode/0
        ]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LOCALNODE, 'process_monitor@127.0.0.1').

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

localnode() -> ?LOCALNODE.

list_sgroups(Node) ->
    gen_server:call({?SERVER, Node}, list_sgroups).

list_sgroup_children(Node, SGroup) ->
    gen_server:call({?SERVER, Node}, {list_sgroup_children, SGroup}).

list_sgroup_job_groups(Node, SGroup) ->
    gen_server:call({?SERVER, Node}, {list_sgroup_job_groups, SGroup}).

restart_sgroup(Node, SGroup) ->
    gen_server:cast({?SERVER, Node}, {restart_sgroup, SGroup}).

restart_sgroup_job_group(Node, SGroup, JobGroup) ->
    gen_server:cast({?SERVER, Node}, {restart_sgroup_job_group, SGroup, JobGroup}).

stop_sgroup(Node, SGroup) ->
    gen_server:cast({?SERVER, Node}, {stop_sgroup, SGroup}).

stop_sgroup_job_group(Node, SGroup, JobGroup) ->
    gen_server:cast({?SERVER, Node}, {stop_sgroup_job_group, SGroup, JobGroup}).

start_sgroup(Node, SGroup) ->
    gen_server:cast({?SERVER, Node}, {start_sgroup, SGroup}).

start_sgroup_job_group(Node, SGroup, JobGroup) ->
    gen_server:cast({?SERVER, Node}, {start_sgroup_job_group, SGroup, JobGroup}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @private
init([]) ->
    {ok, undefined}.

%% @private
handle_call(list_sgroups, _From, State) ->
    {reply, pm_api:list_sgroups(), State};
handle_call({list_sgroup_children, SGroup}, _From, State) ->
    {reply, pm_api:list_sgroup_children(SGroup), State};
handle_call({list_sgroup_job_groups, SGroup}, _From, State) ->
    {reply, pm_api:list_sgroup_job_groups(SGroup), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast({restart_sgroup, SGroup}, State) ->
    pm_api:restart_sgroup(SGroup),
    {noreply, State};
handle_cast({restart_sgroup_job_group, SGroup, JobGroup}, State) ->
    pm_api:restart_sgroup_job_group(SGroup, JobGroup),
    {noreply, State};
handle_cast({stop_sgroup, SGroup}, State) ->
    pm_api:stop_sgroup(SGroup),
    {noreply, State};
handle_cast({stop_sgroup_job_group, SGroup, JobGroup}, State) ->
    pm_api:stop_sgroup_job_group(SGroup, JobGroup),
    {noreply, State};
handle_cast({start_sgroup, SGroup}, State) ->
    pm_api:start_sgroup(SGroup),
    {noreply, State};
handle_cast({start_sgroup_job_group, SGroup, JobGroup}, State) ->
    pm_api:start_sgroup_job_group(SGroup, JobGroup),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
