%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Lager backend for Humio.com logging service
%%% Configuration is a proplist with the following keys:
%%% <ul>
%%%    <li>`token' - Humio Ingestion API token (from Settings)</li>
%%%    <li>`dataspce' - Humio dataspace (from Settings)</li>
%%%    <li>`level' - log level to use</li>
%%%    <li>`retry_interval' - intervarl for retry in case endpoint is not
%%%                           availabes</li>
%%%    <li>`max_retries' - maximum number of retries</li>
%%% </ul>
%%% @end
%%% Created : 25 Feb 2017 by Martin Wiso <martin@wiso.cx>
%%%----------------------------------------------------------------------------

-module(lager_humio_backend).

-behaviour(gen_event).

%% API
-export([ init/1
        , handle_call/2
        , handle_event/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

%%% this is only exported for the spawn call
-export([deferred_log/4]).

-define(DEFAULT_FORMAT, "[$level] $message").
-define(BASE_API_URI,   "https://go.humio.com/api/v1/dataspaces").
-define(LOG_LEVELS, [ debug
                    , info
                    , notice
                    , warning
                    , error
                    , critical
                    , alert
                    , emergency
                    , none
                    ]).

-record(state, { token           :: string()
               , dataspace       :: string()
               , level           :: {'mask', integer()}
               , retry_interval  :: integer()
               , max_retries     :: integer()
               , httpc_opts      :: []
               }).

%% @private
init(Options) ->
    true = validate_options(Options),
    State = #state{ token           = get_option(token, Options, "")
                  , dataspace       = get_option(dataspace, Options, "")
                  , level           = lager_util:level_to_num(
                                        get_option(level, Options, debug))
                  , retry_interval  = get_option(retry_interval, Options, 60)
                  , max_retries     = get_option(max_retries, Options, 10)
                  , httpc_opts      = get_option(httpc_opts, Options, [])
                  },
    io:format("DEBUG: state=~p", [State]),
    {ok, State}.

%% @private
handle_call({set_token, Token}, State) ->
    {ok, ok, State#state{token = Token}};
handle_call({set_dataspace, DS}, State) ->
    {ok, ok, State#state{dataspace = DS}};
handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case validate_loglevel(Level) of
        false ->
            {ok, {error, bad_loglevel}, State};
        NewLevel ->
            {ok, ok, State#state{level = NewLevel}}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message}, #state{level = MinLevel} = State) ->
    case lager_util:is_loggable(Message, MinLevel, ?MODULE) of
        true ->
            Payload = jiffy:encode(create_payload(Message)),
            Request = create_httpc_request(State, Payload),
            RetryInterval = State#state.retry_interval,
            MaxRetries = State#state.max_retries,
            Opts = State#state.httpc_opts,

            Args = [Request, MaxRetries, RetryInterval, Opts],
            spawn(?MODULE, deferred_log, Args),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functionality
%%%============================================================================
create_payload(Message) ->
    MD    = lager_msg:metadata(Message),
    Level = to_binary(lager_msg:severity(Message)),
    Msg   = to_binary(lager_msg:message(Message)),
    Ts    = to_binary(lager_msg:timestamp(Message)),
    Raw   = to_binary(create_raw_message(Level, Msg, Ts)),
    [
     #{<<"tags">>    => create_tags(Level, MD)
      , <<"events">> => [create_event(Ts, MD, Raw)]
      }
    ].

create_tags(Level, MD) ->
    #{ <<"host">>   => to_binary(get_hostname())
     , <<"level">>  => Level
     , <<"source">> => <<>>
     }.

create_event(Ts, MD, RawMessage) ->
    #{ <<"timestamp">>  => format_ts(Ts)
     , <<"attributes">> => create_attributes(MD)
     , <<"rawstring">>  => RawMessage
     }.

create_attributes(MD) ->
    lists:foldl(fun({K, V}, Acc) ->
                        maps:put(to_binary(K), to_binary(V), Acc)
                end, #{}, MD).

create_raw_message(Level, Msg, Ts) ->
    binary_join([Ts, <<"[", (Level)/binary, "]">>, Msg], << >>).

format_ts(Ts) ->
    <<(Ts)/binary, "+00:00">>.

deferred_log(_Request, 0, _, _Opts) ->
    ok;
deferred_log(Request, Retries, Interval, Opts) ->
    case httpc:request(post, Request, [], Opts) of
        {ok, {{_, 200, _}, _H, _B}} -> ok;
        _ ->
            timer:sleep(Interval * 1000),
            deferred_log(Request, Retries - 1, Interval, Opts)
    end.

create_httpc_request(Payload, #state{token = Token, dataspace = DS}) ->
    {get_uri(DS), get_headers(Token), "application/json", Payload}.

get_uri(DS) ->
    ?BASE_API_URI ++ "/" ++ DS ++ "/ingest".

get_headers(Token) ->
    [{"Authorization", "Bearer " ++ Token}].

get_hostname() ->
    {ok, Hostname} = inets:get_hostname(),
    Hostname.

validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Levels ->
            Levels
    catch
        _:_ ->
            false
    end.

validate_options([]) -> true;
validate_options([{token, Token}|T]) when is_list(Token) ->
    validate_options(T);
validate_options([{dataspace, DT} | T]) when is_list(DT) ->
    validate_options(T);
validate_options([{retry_interval, N} | T]) when is_integer(N) ->
    validate_options(T);
validate_options([{max_retries, N} | T]) when is_integer(N) ->
    validate_options(T);
validate_options([{level, L} | T]) when is_atom(L) ->
    case lists:member(L, ?LOG_LEVELS) of
        false ->
            throw({error, {fatal, {bad_level, L}}});
        true ->
            validate_options(T)
    end;
validate_options([H | _]) ->
    throw({error, {fatal, {bad_console_config, H}}}).

get_option(Key, Options, Default) ->
    case lists:keyfind(Key, 1, Options) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.

binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join([Head | Tail], Sep) ->
  lists:foldl(fun (Value, Acc) ->
                      <<Acc/binary, Sep/binary, Value/binary>>
              end, Head, Tail).

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Pid) when is_pid(Pid) ->
    to_binary(pid_to_list(Pid));
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
to_binary(Value) ->
    Value.
