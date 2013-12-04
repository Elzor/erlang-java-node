-module(java_controller).

-behaviour(gen_server).

-include_lib("common/include/log.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Only for tests
-export([stop/1]).

-define(SERVER, ?MODULE).
-define(PING_INTERVAL, 1000).

-record(state, { remote_pid   = undefined :: pid(),
                 waiters      = []        :: list(),
                 ext_port_ref = undefined :: reference() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    Port  = start_java_app(),
    gen_server:cast(self(), ping),
    {ok, #state{ext_port_ref = Port}}.

%% only for test
stop(Reason)->
    gen_server:cast(?SERVER, {stop_test, Reason}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({stop_test, Reason}, State)->
    {stop, Reason, State};

handle_cast(ping, State) ->
    {ok, JavaNode}   = application:get_env(javanode, java_node),
    {ok, JavaMbox}   = application:get_env(javanode, java_mbox),
    {ok, JavaHost}   = case application:get_env(javanode, java_host) of
                           undefined ->
                               inet:gethostname();
                           Other ->
                               Other
                       end,
    ping(JavaHost, JavaNode, JavaMbox),
    erlang:send_after(?PING_INTERVAL, self(), ping),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?ERR("unhandled cast, ~p", [_Msg]),
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
handle_info(ping, #state {remote_pid = undefined} = State) ->
    gen_server:cast(self(), ping),
    {noreply, State};

handle_info(ping, State) ->
    {noreply, State};

handle_info({pong, Pid}, #state {remote_pid = undefined,
                                 waiters = Waiters} = State) ->
    ?LOG("connection to java node established, pid ~p", [Pid]),
    link(Pid),
    lists:foreach(fun(Waiter) ->
                          gen_server:cast(self(), {wait_for_login, Waiter})
                  end, Waiters),
    {noreply, State#state {waiters = [],
                           remote_pid = Pid}};

handle_info({pong, _}, State) ->
    {noreply, State};

handle_info({Port, {exit_status, Status}}, #state {ext_port_ref = Port} = State) ->
    ?ERR("external java app exited with status ~p", [Status]),
    {stop, {error, {java_app_exit, Status}}, State};

handle_info({'EXIT', Pid, Reason}, #state {remote_pid = Pid} = State) ->
    ?ERR("external java mbox exited with reason ~p", [Reason]),
    {stop, {error, {java_mbox_exit, Reason}}, State};

handle_info(_Info, State) ->
    ?ERR("unhandled info, ~p", [_Info]),
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_java_app() ->
    {ok, JavaNodeCfg} = application:get_env(javanode, java_node),
    {ok, HostName}    = inet:gethostname(),
    JavaNode          = full_node_name(HostName, JavaNodeCfg),
    {ok, JavaMbox}    = application:get_env(javanode, java_mbox),
    {ok, JavaCmd}     = application:get_env(javanode, java_cmd),
    {ok, JavaPort}    = application:get_env(javanode, java_epmd_port),
    {ok, JavaConf}    = application:get_env(javanode, java_conf),

    PrivDir = code:priv_dir(javanode),
    %% If config file is the relative path then append priv directory
    LogFileName = atom_to_list(node()) ++ "_java.log",
    CmdWithParams = "java "++
        "-Dnodename=\""++atom_to_list(JavaNode) ++ "\" " ++
        "-Dmboxname=\""++atom_to_list(JavaMbox) ++ "\" " ++
        "-Dcookie=\""++atom_to_list(erlang:get_cookie()) ++ "\" " ++
        "-Dnodeport="++lists:flatten(io_lib:format("~p", [JavaPort])) ++ " " ++
        "-Dlogfile=\""++PrivDir ++ "/" ++ LogFileName++"\" " ++
        "-Dconfig=\""++PrivDir ++ "/" ++ JavaConf++"\" " ++
        "-classpath " ++ PrivDir ++ "/" ++ JavaCmd ++ " ",
    ?LOG("starting java app with cmd ~p", [CmdWithParams]),
    open_port({spawn, CmdWithParams}, [exit_status]).

ping(Host, Node, Mbox) ->
    erlang:send({Mbox, full_node_name(Host, Node)}, {ping, self()}).

full_node_name(Host, Node) ->
    list_to_atom(atom_to_list(Node) ++ "@" ++ Host).
