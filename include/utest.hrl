%% Part of 'utest' Unit Testing Framework
%% Copyright (c) 2009 Steve Davis <steve@simulacity.com>
%% See MIT License
 
-record(suite, {
	application, 
	version = "", 
	state, 
	desc = "", 
	path,
	timestamp,
	host,
	invoke, 
	modules = [], 
	depends = [], 
	registered = [], 
	env = [], 
	tests = [],
	results = []
}).

-record(results, {
	module,
	number = 0,
	file,
	info = [],
	cases = []
}).

-define(LINEBREAK, "[\r\n]+").
-define(COMMENT_REGEX, "[ \t]*%.[^\r\n]*").
-define(VERSION_REGEX, "^[0-9]+(\.[0-9]+)+[abc]?$").
