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
	get/1,
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

-record(state, {sessions = [],
	life_time = 1}).

-record(session,{
	ssid=""
}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Call synchronized process wich creates new session in gen_server state
%% and returns empty record #sess{}.
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
get(SSID) ->
	gen_server:call(?MODULE,{get,SSID}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
init([]) ->
	{ok, #state{}}.

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
		sessions = Sessions } = State) ->
	SSID = base64:encode_to_string(
		crypto:strong_rand_bytes(32)),
	Session =
		#session{ ssid = SSID },
	lager:info("~nSession created:~n ~p ~n",
		[Session]),
	{reply, {ok,Session},
		State#state{ sessions =
			[{SSID,Session}|Sessions]}};
handle_call({get,SSID},_From,#state{
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
handle_info({delete,SSID},#state{
		sessions = Sessions} = State) ->
	{noreply,State#state{
		sessions = proplists:delete(
			SSID,Sessions)}};
handle_info(flush,State) ->
	lager:info(
		"~nSessions flushed.~n",[]),
	{noreply,State#state{
		sessions = []}};
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
