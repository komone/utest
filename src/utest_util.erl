%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest_util).
-vsn("0.2").
-author('<steve@simulacity.com>').

%% Various functions that I wish were stdlib functions
-compile(export_all).

%%
find_files(Path, [$.|T]) ->
	Pattern = lists:flatten(["\\.", T, $$]),
	filelib:fold_files(Path, Pattern, true, fun(F, Acc) -> [F|Acc] end, []);
%
find_files(Path, Dir) when is_list(Dir) -> 
	FullPath = filename:join([Path, Dir]), 
	filelib:fold_files(FullPath, ".*", false, fun(F, Acc) -> [F|Acc] end, []).

%% Load external data files (text or binary)
load_file(Path, Filename, Type) ->
	File = filename:join([Path, utest:get_config(test_dir), Filename]),
	case filelib:is_file(File) of 
	true -> 
		{ok, Content} = file:read_file(File),
		case Type of 
		binary -> 
			String = lists:flatten(io_lib:format("~w", [Content]));
		_ -> 
			String = binary_to_list(Content)
		end,
		try erl_scan:string(String) of 
		{ok, Tokens, _} -> 
			{ok, Tokens};
		{error, {_Line, _Module, Descriptor}, _} -> 
			Reason = erl_scan:format_error(Descriptor),
			{error, lists:flatten([Reason, " [in file: \"", File, "\"]"])}
		catch
			_:Reason -> {error, Reason}
		end;
	false -> {error, {bad_file, File}}
	end.

%%
%% an incomplete but usable hack!
is_string([H|T]) when is_integer(H), H > 0 -> is_string(T);
is_string([_|_]) -> false;
is_string([]) -> true;
is_string(_) -> false.

