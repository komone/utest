%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest_xml).
-vsn("0.3"). 
-author('steve@simulacity.com').

-include_lib("xmerl/include/xmerl.hrl").

-export(['#xml-inheritance#'/0]).
-export(['#root#'/4, '#element#'/5, '#text#'/1]).
-export([cleanup/1]).

-define(INDENT, "  ").

%%
%% Xmerl Callbacks
%%

'#xml-inheritance#'() -> [].

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V ++ "\n",Data];
'#root#'(Data, _Attrs, [], _E) ->
	["<?xml version=\"1.0\"?>\n",
	"<?xml-stylesheet type=\"text/xsl\" href=\"report.xsl\" ?>\n", 
	Data].

'#element#'(Tag, [], Attrs, Parents, _E) ->
	Level = length(Parents),
	lists:flatten([indent(Level), 
		xmerl_lib:empty_tag(Tag, Attrs), indent(Level - 1)]);
'#element#'(Tag, Data, Attrs, Parents, _E) ->
	Level = length(Parents),
	lists:flatten([indent(Level), 
		xmerl_lib:markup(Tag, Attrs, Data), indent(Level - 1)]).

'#text#'(Text) ->
	xmerl_lib:export_text(Text).

 
%%
indent(Level) when Level > 0 ->
	["\n", lists:duplicate(Level, ?INDENT)];
indent(_) ->
	["\n"].
	
%% ok, this is an ugly hack...
cleanup(Text) ->
	re:replace(Text, "\n[\t ]*\n", "\n", [global, {return, list}]).
