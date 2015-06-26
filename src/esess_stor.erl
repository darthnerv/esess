%%%-------------------------------------------------------------------
%%% @author phyzx
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2015 3:29 PM
%%%-------------------------------------------------------------------
-module(esess_stor).

-author("phyzx").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([
	create/0,
	flush/0,
	set/2,
	get/3,
	get_session/1,
	extend/1,
	delete/1
]).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {sessions = [], prototype = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call synchronized process wich returns num of values
%%
%% @end
%%--------------------------------------------------------------------
get(SSID,Values,ReurnType)
		when is_list(Values) ->

	gen_server:call(
		?MODULE,{get,SSID,Values,ReurnType});

get(_,_,_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call asynchronized process wich updates session args
%%
%% @end
%%--------------------------------------------------------------------
set(SSID,Values)
		when is_list(Values) ->

	?MODULE ! {set,SSID,Values};

set(_,_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call synchronized process wich creates new session in gen_server state
%% and returns empty session
%%
%% @end
%%--------------------------------------------------------------------
create() ->

	gen_server:call(?MODULE,create).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call asynchronized process wich
%% clear all sessions from gen_server state
%%
%% @end
%%--------------------------------------------------------------------
flush() ->

	?MODULE ! flush.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call asynchronized process wich delete session
%% from gen_server state
%%
%% @end
%%--------------------------------------------------------------------
delete(SSID) ->

	?MODULE ! {delete,SSID}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call asynchronized process wich extends lifetime of session
%%
%% @end
%%--------------------------------------------------------------------
extend(SSID) ->

	?MODULE ! {extend,SSID}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call synchronized process wich get session
%% record by specific session_id
%%
%% @end
%%--------------------------------------------------------------------
get_session(SSID) ->

	gen_server:call(?MODULE,{get_session,SSID}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->

	gen_server:start_link(
		{local, ?MODULE}, ?MODULE, [init], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).

init([init]) ->

	init(esess:genv(rec,[]));

init([]) ->

	{ok,#state{}};

init(Args) ->

	{ok,#state{prototype =

		lists:foldl(fun

			({Name,Value},Acc) ->

				[{to_bin(Name),Value}|Acc];

			(_,Acc) -> Acc

		end,[],[{<<"ssid">>,""}|Args])

	}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
		State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_call(create,_Form,#state{
		sessions = Sessions,
			prototype = Prototype } = State) ->

	SSID = list_to_binary(
		base64:encode_to_string(
			crypto:strong_rand_bytes(32))),

	Session =
		set_value(<<"ssid">>,
			SSID,Prototype),

	lager:info("~nSession created:~n ~p ~n",
		[Session]),

	{reply, ok,
		State#state{sessions =
			[{SSID,Session}|Sessions]}};

handle_call({get,SSID,Values,ReurnType},_From,#state{
	sessions = Sessions } = State) ->

	case proplists:get_value(
		SSID,Sessions) of

		undefined ->

			lager:info(
				"~nSession with id [~p]. Not Found.~n",[SSID]),

			{reply,{error,[]},State};

		Session ->

			Args = lists:foldl(fun

				(Name,Acc) ->

					Acc ++ get_value(
						Name,Session,ReurnType)

			end,[],Values),

			{reply,{ok,Args},State}

	end;

handle_call({get_session,SSID},_From,#state{
		sessions = Sessions } = State) ->

	case proplists:get_value(
		SSID,Sessions) of

		undefined ->

			lager:info(
				"~nSession with id [~p]. Not Found.~n",[SSID]),

			{reply,{error,not_found},State};

		Session ->

			lager:info(
				"~nSession found:~n ~p ~n",[Session]),

			{reply,{ok,Session},State}

	end;

handle_call(_Request, _From, State) ->

	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->

	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

handle_info({set,SSID,Args},#state{
		sessions = Sessions} = State) ->

	case proplists:get_value(
		SSID,Sessions) of

		undefined ->

			{noreply,State};

		Session ->

			UpdatedSession = lists:foldl(fun

				({Name,Val},Acc) ->

					set_value(
						proplists:lookup(Name,Acc)
							,Val,Acc);

				(_,Acc) -> Acc

			end,Session,Args),

			{noreply,State#state{sessions =
				[{SSID,UpdatedSession}] ++
					proplists:delete(SSID,Sessions) }}

	end;

handle_info({delete,SSID},#state{
		sessions = Sessions} = State) ->

	lager:info(
		"~nSessions deleted.~n",[]),

	{noreply,State#state{
		sessions = proplists:delete(
			SSID,Sessions)}};

handle_info(flush,State) ->

	lager:info(
		"~nSessions flushed.~n",[]),

	{noreply,State#state{
		sessions = []}};

handle_info(sessions,State) ->

	lager:info("~nSessions: ~p ~n",
		[State#state.sessions]),

	{noreply,State};

handle_info(_Info, State) ->

	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
		State :: #state{}) -> term()).

terminate(_Reason, _State) ->

	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
		Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->

	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
to_bin(Name)
		when is_binary(Name) ->

	Name;

to_bin(Name)
		when is_list(Name) ->

	list_to_binary(Name);

to_bin(Name)
		when is_float(Name) ->

	to_bin(
		io_lib:format("~.2f",Name));

to_bin(Name)
	when is_integer(Name) ->

		to_bin(
			integer_to_list(Name));

to_bin(Name)
	when is_atom(Name) ->

		atom_to_binary(Name,latin1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
get_value(Name,Session,args) ->

	case proplists:get_value(
		Name,Session) of

		undefined -> [];

		Val -> [Val]

	end;

get_value(Name,Session,proplist) ->

	case proplists:get_value(
		Name,Session) of

		undefined -> [];

		Val -> [{Name,Val}]

	end;

get_value(_,_,_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
set_value({Name,_OldValue},
		NewValue,Proplist) ->

	lager:info(
		"~nSession value [~p] modified to [~p] .~n",[Name,NewValue]),

	[{Name,NewValue} |
		proplists:delete(Name,Proplist)];

set_value(none,_,Proplist) -> Proplist;

set_value(Name,Value,Proplist) ->

	set_value(proplists:lookup(
		Name,Proplist),Value,Proplist).



