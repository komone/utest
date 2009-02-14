{application, utest,
 [{description, "Unit Testing"},
  {vsn, "0.2"},
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
  	{report, xml},         % none | console | raw | text | xml | html
  	{target, [localhost]}, % later add 'remote' for continuous integration
  	{test_dir, "utest"},        % the name for the default test directory
  	{file_ext, ".test"}    % test file extension, must start with .
  ]},
  {applications, [kernel, stdlib]}
]}.
