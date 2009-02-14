%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License

-module(utest_xml).
-vsn("0.2"). 
-author('<steve@simulacity.com>').

-include_lib("xmerl/include/xmerl.hrl").

-export(['#xml-inheritance#'/0]).
-export(['#root#'/4, '#element#'/5, '#text#'/1]).

%% TODO: Pretty print XML

'#xml-inheritance#'() -> [].

'#root#'(Data, [#xmlAttribute{name=prolog,value=V}], [], _E) ->
    [V ++ "\n",Data];
'#root#'(Data, _Attrs, [], _E) ->
    ["<?xml version=\"1.0\"?>\n"
    "<!-- TODO: better formatting in utest_xml -->", Data].

'#element#'(Tag, [], Attrs, _Parents, _E) ->
	"\n" ++ xmerl_lib:empty_tag(Tag, Attrs);
'#element#'(Tag, Data, Attrs, _Parents, _E) ->
	"\n" ++ xmerl_lib:markup(Tag, Attrs, Data).

'#text#'(Text) ->
	xmerl_lib:export_text(Text).
