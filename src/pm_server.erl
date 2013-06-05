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
-module(pm_server).

%% API
-export([start_link/2, stop/1]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
            {
                port :: port(), % External process port
                command :: string() % Command running on port
            }).

start_link(Name, ExternalProcess) ->
    gen_server:start_link({local, Name}, ?MODULE, [ExternalProcess], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @private
init([ExternalProcess]) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), {start_process, ExternalProcess}),
    {ok, #state{command = ExternalProcess}}.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast({start_process, ExternalProcess}, _State) ->
    Port = open_port({spawn, ExternalProcess}, []),
    {noreply, #state{port = Port}};
handle_cast(stop, State) ->
    {stop, normal, State}.

%% @private
handle_info({'EXIT', _Port, Reason}, #state{command = Command} = State) ->
    error_logger:info_msg("External process ~s unexpectedly exited.", [Command]),
    {stop, {port_terminated, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% Do something when the process terminates
terminate({port_terminated, _Reason}, _State) ->
    error_logger:info_msg("Terminating because of external process exit"),
    ok;
terminate(_Reason, #state{port = Port}) ->
    port_close(Port),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.