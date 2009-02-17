%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest_eval).
-vsn("0.2").
-author('<steve@simulacity.com>').

-include("../include/utest.hrl").

-export([load_records/1, expr/4, string/1]).

-compile(export_all).


%% 
load_records(Path) ->
	Headers = utest_util:find_files(Path, ".hrl"),
	Other = utest_util:find_files(Path, ".inc"), %% this appears popular
	load_records(lists:append([Headers, Other]), []).

%%
expr(Path, Records, Module, Expr) ->
	case tokenize(Path, Module, Expr) of 
	{ok, Tokens} -> 
		case parse(Tokens) of
		{ok, Parsed} -> 
			Expanded = expand_records(Records, Parsed),
			evaluate(Expanded, []);
		{error, Reason} -> {error, Reason}
		end;
	{error, Reason} -> {error, Reason}
	end.

%%
string(Expr) ->
	expr(".", ?MODULE, [], Expr).

%%
%% Internal API
%%

%%
load_records([H|T], Acc) ->
	{ok, Form} = epp:parse_file(H, "", []),
	Records = [X || X = {attribute, _, record, _} <- Form],
	load_records(T, lists:append([Acc, Records]));
load_records([], Acc) ->
	Acc.

%% 
tokenize(Path, Module, Expr) ->
	%% Exports = Module:module_info(exports),	
	try erl_scan:string(Expr) of 
	{ok, Tokens, _} -> 
		case preprocess(Path, Module, Tokens, []) of
		{ok, Value} -> 
			Result = lists:flatten([{'(', 1}, Value, {')', 1}, {dot, 1}]),
			{ok, Result};
		{error, Reason} -> 
			{error, Reason}
		end
	catch
		_:Reason -> {error, Reason}
	end.

%%
expand_records(Records, Tokens) -> 
	Call = [{function,1,utest,0,[{clause,1,[],[],Tokens}]}],
	Result = erl_expand_records:module(lists:append([Records, Call]), []),
	[{function, _, utest, 0, [{clause, _, [], [], Tokens1}]}] = Result,
	Tokens1.

%% Implements contextual function call
preprocess(Path, Module, [{atom, 1, Name}|[H|T]], Acc) ->
	case lists:keymember(Name, 1, Module:module_info(exports)) of
	true when H =:= {'(', 1} ->
		Acc1 = [{atom, 1, Name}, {':', 1}, {atom, 1, Module}] ++ Acc,
		preprocess(Path, Module, [H|T], Acc1);
	_ -> 
		preprocess(Path, Module, [H|T], [{atom, 1, Name}|Acc])
	end;
%% Implements Macros
preprocess(Path, Module, [{'?', 1}|[H|T]], Acc) ->
	case H of 
	{var, 1, 'FILE'} ->
		{[{'(', 1}, {string, 1, Filename}, {')', 1}], T1} = lists:split(3, T),
		case utest_util:load_file(Path, Filename, text) of 
		{ok, Value} -> 
			preprocess(Path, Module, T1, [Value|Acc]);
		{error, Reason} -> 
			{error, Reason}
		end;
	{var, 1, 'BINARY'} -> 
		{[{'(', 1}, {string, 1, Filename}, {')', 1}], T1} = lists:split(3, T),
		case utest_util:load_file(Path, Filename, binary) of 
		{ok, Value} -> 
			preprocess(Path, Module, T1, [Value|Acc]);
		{error, Reason} -> 
			{error, Reason}
		end;
	{var, 1, 'ANY'} ->
		preprocess(Path, Module, T, [{var, 1, '_'}|Acc]);		
	{var, 1, 'MODULE'} ->
		preprocess(Path, Module, T, [{atom, 1, Module}|Acc]);		
	{var, 1, V} -> 
		{error, {badmacro, V}};
	V -> 
		{error, {badmacro, V}}
	end;
%
preprocess(Path, Module, [{dot, 1}], Acc) ->
	preprocess(Path, Module, [], Acc);
preprocess(Path, Module, [H|T], Acc) ->
	preprocess(Path, Module, T, [H|Acc]);
preprocess(_, _, [], Acc) ->
	{ok, lists:flatten(lists:reverse(Acc))}.


%% 
parse(Code) ->
	try erl_parse:parse_exprs(Code) of 
	{ok, ParseTree} ->
		case lists:member({var, 1, '_'}, Code) of 
		true -> {ok, process_wildcards(ParseTree)};
		false -> {ok, ParseTree}
		end;
	{error, {_Line, Module, Descriptor}} -> 
		Reason = lists:flatten(apply(Module, format_error, [Descriptor])),
		{error, Reason}
	catch
		_:Reason -> {error, Reason}
	end.

%% 
% allows valid use of '_' in expresssions (the ?ANY Macro)
% by converting these to case expressions
process_wildcards(ParseTree) ->
	[Trunk] = ParseTree,
	ParseTree1 = process_branch(Trunk),
	%io:format("~nPARSED:~n~p~n", [ParseTree1]),
	[ParseTree1].
	
process_branch({op, 1, Op, Arg1, Arg2}) ->
	Branch1 = process_branch(Arg1),
	Branch2 = process_branch(Arg2),
	process_op_branch(Op, Branch1, Branch2);
process_branch(ParseTree) -> 
	ParseTree.

process_op_branch('==', Arg1, Arg2) ->
	case has_unbound(Arg1) xor has_unbound(Arg2) of 
	true ->	convert_to_case(Arg1, Arg2, true, false);
	false -> {op, 1, '==', Arg1, Arg2}
	end;
process_op_branch('/=', Arg1, Arg2) ->
	case has_unbound(Arg1) xor has_unbound(Arg2) of 
	true ->	convert_to_case(Arg1, Arg2, false, true);
	false -> {op, 1, '/=', Arg1, Arg2}
	end;
process_op_branch(Op, Arg1, Arg2) ->
	{op, 1, Op, Arg1, Arg2}.

convert_to_case(Arg1, Arg2, Bool1, Bool2) ->
	case has_unbound(Arg2) of 
	true -> make_case(Arg1, Arg2, Bool1, Bool2);
	false -> make_case(Arg2, Arg1, Bool1, Bool2)
	end.
	
make_case(Expr1, Expr2, Bool1, Bool2) ->
	{'case', 1, Expr1, [
		{clause, 1, [Expr2], [], [{atom, 1, Bool1}]},		
		{clause, 1, [{var ,1, '_'}], [], [{atom, 1, Bool2}]} 
	]}.

has_unbound({tuple, 1, Rest}) -> lists:member({var, 1, '_'}, Rest);
has_unbound({cons, 1, {var, 1, '_'}, _Rest}) -> true;
has_unbound({cons, 1, _Value, Rest}) -> has_unbound(Rest);
has_unbound({nil, 1}) -> false;
has_unbound(_) -> false.



%% This should alwayes end up as either true or false
%% i.e. PASS or FAIL
evaluate(ParseTree, Args) ->
	% Bindings are currently UNUSED
	BindArg = fun({Key, Val}, Acc) -> erl_eval:add_binding(Key, Val, Acc) end,
	Bindings = lists:foldl(BindArg, erl_eval:new_bindings(), Args),
	try erl_eval:exprs(ParseTree, Bindings) of
	{value, Result, _} -> 
		case Result of
			true -> Result;
			false -> Result;
			_ -> {error, {badresult, Result}}
		end
	catch
		_:Reason -> {error, Reason}
	end.
		