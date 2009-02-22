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

%%
%% an incomplete but usable hack!
is_string([H|T]) when is_integer(H), H > 0 -> is_string(T);
is_string([_|_]) -> false;
is_string([]) -> true;
is_string(_) -> false.

