-module(main_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:start(sasl),
    application:start(crypto),


    main_sup:start_link().

stop(_State) ->
    ok.
