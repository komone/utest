{application, utest,
 [{description, "Unit Testing"},
  {vsn, "0.3"},
  {modules, [
  	utest,
  	utest_gen,
  	utest_suite,
  	utest_eval,
  	utest_report
  ]},
  {registered, []},
%  {mod, {utest_app, []}}, when converting to OTP
  {env, [
  	{verbosity, normal},   % terse | normal | verbose
  	{report, html},        % none | console | raw | text | xml | html
  	{target, [localhost]}, % later add 'remote' for continuous integration
  	{test_dir, "utest"},   % the name for the default test directory
  	{file_ext, ".test"},   % test file extension, must start with .
  	{browser, auto}        % auto | off (automatically open browser if html)
  ]},
  {applications, [kernel, stdlib]}
]}.
