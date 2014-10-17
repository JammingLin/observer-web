%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------

-module(observer_perf_web).

-export([perf_info/2]).

perf_info(Node, PerfType) ->
	observer_web:try_rpc(Node, erlang, system_flag, [scheduler_wall_time, true]),
	case PerfType of
		scheduler ->
			lists:sort(observer_web:try_rpc(Node, erlang, statistics, [scheduler_wall_time]));
		memory ->
			MemT = mem_types(),
			MemInfo = observer_web:try_rpc(Node, erlang, memory, []),
			[{Type, Value} || {Type, Value} <- MemInfo, lists:member(Type, MemT)];
		io ->
			{{input, Input},{output, Output}} = observer_web:try_rpc(Node, erlang, statistics, [io]),
			[{input, Input},{output, Output}]
	end.

mem_types() ->
    [total, processes, atom, binary, code, ets].