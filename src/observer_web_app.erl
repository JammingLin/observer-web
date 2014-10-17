%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------

-module(observer_web_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
				{"/", cowboy_static, {priv_file, observer_web, "index.html"}},
				{"/css/[...]", cowboy_static, {priv_dir, observer_web, "css"}},
				{"/js/[...]", cowboy_static, {priv_dir, observer_web, "js"}},
				{"/info", observer_web_handler, []}
				]}
		]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8090}], [
			{env, [{dispatch, Dispatch}]}
		]),
	dets:open_file(observer_table, [{type, set}, {file, "observer_table"}]),
	dets:close(observer_table),
	observer_web_sup:start_link().

stop(_State) ->
	ok.