%%%-------------------------------------------------------------------
%%% @author phyzx
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2015 12:14 PM
%%%-------------------------------------------------------------------
-module(esess).
-author("phyzx").

%% API
-export([
	start/0,
	stop/0
]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts an application with all needed dependencies
%%
%% @end
%%--------------------------------------------------------------------

start() ->
	application:ensure_started(syntax_tools),
	application:ensure_started(compiler),
	application:ensure_started(goldrush),
	application:ensure_started(lager),
	application:start(esess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops an application
%%
%% @end
%%--------------------------------------------------------------------

stop() ->
	application:stop(esess).