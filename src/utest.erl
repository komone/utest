%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest).
-vsn("0.2"). 
-author('steve@simulacity.com').

-include("../include/utest.hrl").

-export([info/1, gen/1, run/1]).
-export([help/0, config/0, config/1, config/2, eval/1, print/3]).

%% Prints out a usage reminder to the console
help() ->
	io:format(
	" help/0 - this message~n"
	" info/1 - get information about an application~n"
	" gen/1 - generate test files for an application~n"
	" run/1 - run the test suite for an application~n"
	" eval/1 - try out a test string to see if it will work~n"
	" config/0 - view current unit test configuration settings~n"
	" config/2 - change one of the current configuration settings~n", []).
	
%% Prints out the configuation parameters to the console
config() ->
	io:format("~p~n", [get_config()]).

%%
config(Key) ->
	get_config(Key).

%% Updates the configuration parameter 'Key' with the value 'Value' 
config(Key, Value) ->
	set_config(Key, Value).
	
%% Try out a valid test statement to see if it works
%% The statement must use fully qualified function calls
eval(Text) ->
	utest_eval:string(Text).

%% Gets information about an application and its tests 
info(App) ->
	init_config(),	 
	Test = utest_suite:init(App, get_config(test_dir), get_config(file_ext)),
 	utest_report:format(Test, console, get_config(test_dir)).

%% Generates test files for all modules in the application 'App'
gen(App) ->
	init_config(),
	Suite = utest_suite:init(App, get_config(test_dir), get_config(file_ext)),
	utest_gen:generate(Suite, get_config(test_dir), get_config(file_ext)).

%% Runs tests on all modules in the application 'App'
run(App) when is_list(App) ->
	run(list_to_atom(App));
run(App) when is_atom(App) ->
	init_config(),
	Test = utest_suite:init(App, get_config(test_dir), get_config(file_ext)),
 	print(normal, "Running...~n", []),
	utest_suite:setup(Test),
	Records = utest_eval:load_records(Test#suite.path),
 	{ok, Results, {Total, Pass, Fail, Skip}} = run_tests(Test#suite.path,
 		Records, Test#suite.tests, [], {0, 0, 0, 0}),
 	utest_suite:teardown(Test),
 	F = utest_report:format(Test#suite{results=Results}, 
 		get_config(report), 
 		get_config(test_dir)),

	case get_config(browser) == auto andalso get_config(report) == html of 
	true ->
	 	case F of
		{ok, File} -> start_browser(os:type(), File);
			_ -> ok
		end;
	_ -> ok
 	end,
 	case get_config(verbosity) of
 	terse -> 
 		{App, [{tests, Total}, {pass, Pass}, {fail, Fail}, {skip, Skip}]};
 	_ -> 
 		print(normal, "RESULT [~p] Total ~p, Pass ~p, Fail ~p, Skipped ~p~n", 
	 		[App, Total, Pass, Fail, Skip]),
	 	F
 	end.
 	
%% BROWSER[LINUX] os:cmd("exec firefox /../test.html").
%% BROWSER[WIN] os:cmd("start /../test.html").
%% BROWSER[WIN] os:cmd("rundll32 url.dll,FileProtocolHandler /../test.html").
start_browser({unix, _}, File) -> start_browser(unix, File);
start_browser(unix, File) -> os:cmd("exec firefox " ++ File);
start_browser({win32, _}, File) -> start_browser(win32, File);
start_browser(win32, File) -> os:cmd("start " ++ File);
start_browser(_, _) -> {error, no_browser}.

%%
%% Internal API
%%

%%
run_tests(Path, Records, [H|T], Acc, Tally) ->
%%	Module = list_to_existing_atom(filename:basename(H, ?FILE_EXT)),
	Module = list_to_atom(filename:basename(H, get_config(file_ext))),
	BeamInfo = Module:module_info(),	
	TestFile = filename:join([Path, H]),
	Cases = parse_test_file(TestFile),	
	{ok, Result, Tally1} = run_cases(Path, Records, Module, Cases, [], Tally),
	Results = #results{module=Module, number=length(Cases), file=TestFile,
		info=BeamInfo, cases=Result},
	run_tests(Path, Records, T, [Results|Acc], Tally1);
%
run_tests(_, _, [], Acc, Tally) ->
	{ok, lists:reverse(Acc), Tally}.

%%
run_cases(Path, Records, Module, [H|T], Acc, {Total, Pass, Fail, Skip}) ->
	Code = binary_to_list(H),
	Result = utest_eval:expr(Path, Records, Module, Code),
	case Result of 
	true -> 
		Tally = {Total + 1, Pass + 1, Fail, Skip},
		print(verbose, "PASS [~p] " ++ Code ++ "~n", [Module]);
	false -> 
		Tally = {Total + 1, Pass, Fail + 1, Skip},
		print(normal, "FAIL [~p] " ++ Code ++ "~n", [Module]);
	{error, Reason} ->
		Tally = {Total + 1, Pass, Fail, Skip + 1},
		print(normal, lists:concat(["SKIP [", Module, "] ", Code]), []),
		print(normal, " REASON ", []),
		case is_list(Reason) of 
		true -> print(normal, Reason, []);
		false -> print(normal, "~p", [Reason])
		end,
		print(normal, "~n", [])
	end,
	run_cases(Path, Records, Module, T, [{Code, Result}|Acc], Tally);
%
run_cases(_, _, _, [], Acc, Tally) ->
	{ok, lists:reverse(Acc), Tally}.

%%
parse_test_file(TestFile) ->
	% io:format("Reading ~p~n", [TestFile]),
	{ok, Text} = file:read_file(TestFile),	
	Text1 = list_to_binary(re:replace(Text, ?COMMENT_REGEX, "", [global])),
	Lines = re:split(Text1, ?LINEBREAK, [trim]),
	lists:dropwhile(fun(X) -> X =:= <<>> end, Lines).

%%
print(Verbosity, Format, Args) ->
	Setting = get_config(verbosity),
	case Verbosity of 
	verbose when Setting == verbose -> io:format(Format, Args);
	normal when Setting /= terse -> io:format(Format, Args);
	terse -> io:format(Format, Args);
	_ -> ok
	end.

%%
%% TODO: Change all of the below when converting to OTP	
%%
-define(CONFIG, utest_config).
%%
init_config() ->
	case lists:member(?CONFIG, ets:all()) of 
	true -> 
		proceed;
	false -> 
		application:load(?MODULE), 
		ets:new(?CONFIG, [named_table]),
		set_all_config(application:get_all_env(?MODULE))
	end.
	
%%
get_config() ->
	init_config(),
	ets:tab2list(?CONFIG).

%%
get_config(Key) ->
	init_config(),
	[{Key, Value}] = ets:lookup(?CONFIG, Key),
	Value.

%%
set_all_config([{Key, Value}|T]) ->
	set_config(Key, Value),
	set_all_config(T);
%
set_all_config([]) ->
	ok.

%%
set_config(Key, Value) ->
	case Key of 
	verbosity -> 
		Valid = lists:member(Value, [terse, normal, verbose]);
	report -> 
		Valid = lists:member(Value, 
			[nul, none, tty, console, text, raw, xml, html]);
	target -> 
		Valid = is_list(Value);
	test_dir -> 
		Valid = is_list(Value);
	file_ext when is_list(Value) -> 
		Valid = lists:nth(1, Value) == $.;
	browser -> 
		Valid = lists:member(Value, [no, off, auto]);
	_-> 
		Valid = false
	end,
	case Valid of
	true -> 
		set_config({Key, Value});
	false -> 
		{error, invalid_parameter}
	end.
%%
set_config({K, V}) ->	
	init_config(),
	true = ets:insert(?CONFIG, {K, V}),
	{ok, get_config(K)}.
