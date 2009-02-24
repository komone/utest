%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest_gen).
-vsn("0.2").
-author('steve@simulacity.com').

-include("../include/utest.hrl").

-export([generate/3]).

%% TODO: Review the generated file format after using utest a bit more...

%% TODO: Maybe add support for grabbing comments for the functions if 
%% the source code is present

%% used by utest:gen/1
generate(Test, TestDir, TestExt) ->
	Path = filename:join(Test#suite.path, "ebin"),
	BeamList = filelib:wildcard("*.beam", Path),
	TestPath = filename:join(Test#suite.path, TestDir),
	case filelib:is_dir(TestPath) of 
	false -> ok = file:make_dir(TestPath);
	true -> ok
	end,
	gen_files(TestPath, BeamList, TestExt).

%%
%% Internal API
%%

%% create test files but we must skip over those already created
gen_files(Path, [H|T], TestExt) ->
	Module = list_to_atom(filename:basename(H, ".beam")),
	Exports = clean_exports(Module:module_info(exports), []),
	Filename = filename:join([Path, filename:flatten([Module, TestExt])]),
	case filelib:is_file(Filename) of 
	true -> 
		io:format("SKIP ~p~n", [Filename]), skip;
	false -> 
		io:format("FILE ~p~n", [Filename]),
		Header = io_lib:format("%% Unit Tests~n%% '~p'~n", [Module]),
		E = gen_exports(Exports, []),
		file:write_file(Filename, list_to_binary([Header|E]))
	end, 
	gen_files(Path, T, TestExt); 
%
gen_files(_Path, [], _TestExt) ->
	ok.

%% remove standard functions module_info/0, module_info/1
clean_exports([H|T], Acc) ->
	case H of 
	{module_info, 0} -> clean_exports(T, Acc);
	{module_info, 1} -> clean_exports(T, Acc);
	_ -> clean_exports(T, [H|Acc])
	end;
%
clean_exports([], Acc) ->
	lists:reverse(Acc).

%% create comment lines for exported API functions
gen_exports([{Function, Arity}|T], Acc) ->
	F = io_lib:format("~n% '~p/~p'~n", [Function, Arity]),
	gen_exports(T, [F|Acc]);
%
gen_exports([], Acc) ->
	lists:flatten(lists:reverse(Acc)).
