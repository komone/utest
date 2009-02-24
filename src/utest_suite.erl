%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest_suite).
-vsn("0.3").
-author('steve@simulacity.com').

-include("../include/utest.hrl").
 
-export([init/3, setup/1, teardown/1]).

%% TODO: refactor this whole approach to file/path handling 

%%
init(App, TestDir, TestExt) ->
	case Dir = code:lib_dir(App) of 
	Dir when is_list(Dir) ->
		State = release,
		AppDir = Dir; 
	{error, _} -> 
		State = development,
		{ok, CurrentDir} = file:get_cwd(),
		case filename:basename(CurrentDir) of
		"src" -> AppDir = filename:dirname(CurrentDir);
		"ebin" -> AppDir = filename:dirname(CurrentDir);
		TestDir -> AppDir = filename:dirname(CurrentDir);
		_ -> 
			State = development,
			AppDir = CurrentDir
		end
	end,
	case get_app_info(App, AppDir) of
	{ok, App, AppInfo} ->
		Suite = configure(AppInfo, 
			#suite{application=App, path=AppDir, state=State});
	_ ->
		Suite = #suite{application=App, state=development, path=AppDir}
	end,
	TestFiles = utest_util:find_files(AppDir, TestExt),
 	Suite#suite{tests=TestFiles}.

%%
setup(Test) ->
	ensure(Test#suite.depends, start). 

% TODO: Teardown means leaving the system in the _exact_ same state as it was
% before testing started e.g. only stop appliation dependencies that were 
% started by utest -- needs some careful thought when moving towards OTP/remote 
% use by a Continuous Integration server...
% For now -- do nothing ;)
teardown(_Test) ->
%%	ensure(Test#suite.depends, stop).
	ok.

%%
%% Internal API
%%

%%
get_app_info(App, AppDir) ->
	case get_app_info(App, AppDir, "ebin") of
	{ok, App, Info} -> 
		{ok, App, Info};
	_ ->  
		case get_app_info(App, AppDir, "src") of
		{ok, App, Info} -> {ok, App, Info};
		_ -> {error, not_found}
		end
	end.
%%
get_app_info(App, AppDir, Path) ->
	AppFile = filename:join([AppDir, Path, filename:flatten([App, ".app"])]),
	case file:consult(AppFile) of 
	{ok, [{application, App, Info}]} -> {ok, App, Info};
	_ -> {error, invalid}
	end.

%%
configure([H|T], Suite) ->
	case H of 
	{vsn, S} 		 -> Suite1 = Suite#suite{version=S};
	{description, S} -> Suite1 = Suite#suite{desc=S};
	{mod, X} 		 -> Suite1 = Suite#suite{invoke=X};
	{modules, L} 	 -> Suite1 = Suite#suite{modules=L};
	{registered, L}  -> Suite1 = Suite#suite{registered=L};
	{env, L} 		 -> Suite1 = Suite#suite{env=L};
	{applications, L} -> Suite1 = Suite#suite{depends=L};	
	_ -> Suite1 = Suite
	end,
	configure(T, Suite1);
%
configure([], Suite) ->
	Suite.

%%
ensure(Apps, State) ->
    ensure(Apps, application:which_applications(), State).
%%
ensure([App|Rest], Running, State) ->
	case lists:keysearch(App, 1, Running) of
	{value, {App, _, _}} when State == stop, App /= kernel, App /= stdlib -> 
		%% TODO: move 'print' to utest_report(?) as it's a cyclic call...
		utest:print(normal, "Stopping ~p...", [App]), 
		ok = application:stop(App),
		utest:print(normal, "ok~n", []); 
	false when State == start ->
		utest:print(normal, "Starting ~p...", [App]),
		ok = application:start(App),
		utest:print(normal, "ok~n", []);
	_ ->
		ok
	end,
	ensure(Rest, Running, State);
%
ensure([], _, _) ->
	application:which_applications().	
