%%%----------------------------------------------------------------------------
%%% @author Bill Wang <freecnpro@gmail.com>
%%% @copyright 2014 Freecnpro
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------

-module(observer_web).

-export([try_rpc/4]).

try_rpc(Node, Mod, Func, Args) ->
    case rpc:call(Node, Mod, Func, Args) of
		{badrpc, Reason} ->
	    	error_logger:error_report([{node, Node},
				       {call, {Mod, Func, Args}},
				       {reason, {badrpc, Reason}}]),
	    	error({badrpc, Reason});
		Res ->
	    	Res
    end.