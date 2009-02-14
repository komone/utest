%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

%% MODIFIED FROM mochiweb/reloader.erl
%% <http://code.google.com/p/mochiweb>
%% @author("Matthew Dempsky <matthew@mochimedia.com>").
%% @copyright 2007 Mochi Media, Inc.

-module(utest_dev).
-vsn("0.2").
-author('steve@simulacity.com').

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).
-export([start/0, stop/0]).

-include_lib("kernel/include/file.hrl").

-define(CHECK_INTERVAL, 2). %% in seconds
-record(state, {last, tref}).

%% TODO: Add in an option to compile changed source files as well as reloading 
%% any modified beam files (a la Nitrogen sync/0)?

%% start automatic reloading of changed source files
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% stop automatic reloading
stop() ->
    gen_server:call(?MODULE, stop).

%%
%% Callbacks for gen_server
%%

%%
init([]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(?CHECK_INTERVAL), check),
    {ok, #state{last=timestamp(), tref=TRef}}.
%%
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.
%%
handle_cast(_Req, State) ->
    {noreply, State}.
%%
handle_info(check, State) ->
    Now = timestamp(),
    check(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.
%%
code_change(_Vsn, State, _Extra) ->
    {ok, State}.
%%
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.

%%
%% Internal API
%%

%%
check(From, To) ->
	Fn = fun(Module, File) ->
		case file:read_file_info(File) of
		{ok, FileInfo} when 
				FileInfo#file_info.mtime >= From, 
				FileInfo#file_info.mtime < To ->
			reload(Module);
		{ok, _} ->
			unmodified;
		{error, enoent} ->
			%% The Erlang compiler deletes existing .beam files if
			%% recompiling fails.  Maybe it's worth spitting out a
			%% warning here, but I'd want to limit it to just once.
			gone;
		{error, Reason} ->
			io:format("Error reading ~s's file info: ~p~n",
			[File, Reason]),
			error
		end 
	end,
	[Fn(Module, File) || {Module, File} <- code:all_loaded(), is_list(File)].

%%
reload(Module) ->
    io:format("Reloading ~p...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            io:format("ok~n"),
            reload;
        {error, Reason} ->
            io:format(" FAILED ~p~n", [Reason]),
            error
    end.

%%
timestamp() ->
    erlang:localtime().
