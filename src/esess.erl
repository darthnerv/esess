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
	stop/0,
	genv/2
]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts an application with all needed dependencies
%%
%% @end
%%--------------------------------------------------------------------

start() ->

	application:ensure_started(crypto),
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get application enviropment from config by name.
%%
%% @end
%%--------------------------------------------------------------------

genv(undefined,Defualt) ->

	Defualt;

genv({ok,Val},_) ->

	Val;

genv(Name,Default) ->

	genv(application:get_env(
		?MODULE,Name),Default).



