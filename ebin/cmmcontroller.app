{application, cmmcontroller,
  [{description, "Контроллер сервера управления шасси"},
    {vsn, "1"},
    {applications, [kernel, stdlib]},
    {mod,{main_app,[]}},
	{env,
		[
			{listen_port,  20202}
		]}
 ]}.
