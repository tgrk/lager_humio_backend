%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Lager backend for Humio.com logging service
%%% Configuration is a proplist with the following keys:
%%% <ul>
%%%    <li>`token' - Humio Ingestion API token (from Settings)</li>
%%%    <li>`dataspace' - Humio dataspace (from Settings)</li>
%%%    <li>`source' - Humio log source that feed into the dataspace</li>
%%%    <li>`level' - log level to use</li>
%%%    <li>`formatter' - the module to use when formatting log messages.
%%%                      Defaults to `lager_default_formatter'</li>
%%%    <li>`formatter_config' - the format configuration string. Defaults to
%%%                             `time [ severity ] message'</li>
%%%    <li>`retry_interval' - intervarl for retry in case endpoint is not
%%%                           available (defaults to 60 seconds)</li>
%%%    <li>`metadata_filter' - a list of excluded metadata keys</li>
%%%    <li>`max_retries' - maximum number of retries (defaults to 10 retries)</li>
%%%    <li>`httpc_opts' - set custom `httpc:http_options()` to change default
%%%                       HTTP client behaviour</li>
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

-define(BASE_API_URI, "https://go.humio.com/api/v1/dataspaces").

-record(state, { token           :: string()
               , dataspace       :: string()
               , source          :: string()
               , level           :: integer()
               , formatter       :: atom()
               , format_config   :: list()
               , metadata_filter :: list()
               , retry_interval  :: integer()
               , max_retries     :: integer()
               , httpc_opts      :: []
               }).

-include_lib("lager/include/lager.hrl").

%% Exported for testing
-ifdef(TEST).
-export([ call_ingest_api/4
        , create_httpc_request/3
        , get_configuration/1
        , validate_options/1
        , create_event/4
        , get_hostname/0
        , to_binary/1
        ]).
-endif.

%% @private
init(Options) ->
    case validate_options(Options) of
        ok ->
            {ok, get_configuration(Options)};
        {error, _Reason} = Error ->
            Error
    end.

%% @private
handle_call(get_loglevel, #state{level = Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case is_valid_log_level(Level) of
        false ->
            {ok, {error, bad_loglevel}, State};
        true ->
            {ok, ok, State#state{level = Level}}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Message}, #state{level = MinLevel} = State) ->
    case lager_util:is_loggable(Message, MinLevel, ?MODULE) of
        true ->
            Payload = jiffy:encode(create_payload(Message, State)),
            Request = create_httpc_request(Payload, State#state.token, State#state.dataspace),
            RetryInterval = State#state.retry_interval,
            MaxRetries = State#state.max_retries,
            Opts = State#state.httpc_opts,

            spawn(fun () ->
                          call_ingest_api(Request, MaxRetries, RetryInterval, Opts)
                  end),
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
create_payload(Message, #state{source = Source, metadata_filter = MDFilter} = State) ->
    MD0   = lager_msg:metadata(Message),
    MD    = [{<<"source">>,Source}, {<<"host">>, to_binary(get_hostname())} | MD0],
    Level = to_binary(lager_msg:severity(Message)),
    Ts    = lager_msg:timestamp(Message),
    Raw   = to_binary(create_raw_message(Message, State)),
    [
     #{ <<"tags">>   => #{ <<"level">>  => to_binary(Level)}
      , <<"events">> => [create_event(Ts, MD, MDFilter, Raw)]
      }
    ].

create_event(Ts, MD, MDFilter, RawMessage) ->
    #{ <<"timestamp">>  => format_timestamp(Ts)
     , <<"attributes">> => create_attributes(MD, MDFilter)
     , <<"rawstring">>  => RawMessage
     }.

create_attributes(MD, Excluded) ->
    maps:without(Excluded, convert_attributes(MD, #{})).

convert_attributes([], Acc) ->
    Acc;
convert_attributes([{K, V} | T], Acc) ->
    Acc1 = maps:put(K, to_binary(V), Acc),
    convert_attributes(T, Acc1).

create_raw_message(Msg, #state{formatter = Formatter, format_config = Config}) ->
    Formatter:format(Msg, Config).

format_timestamp(Ts) ->
    iso8601:format(Ts).

call_ingest_api(_Request, 0, _, _Opts) ->
    ok;
call_ingest_api(Request, Retries, Interval, Opts) ->
    case httpc:request(post, Request, Opts, []) of
        {ok, {{_, 200, _}, _H, _B}} ->
            ok;
        _Other ->
            timer:sleep(Interval * 1000),
            call_ingest_api(Request, Retries - 1, Interval, Opts)
    end.

create_httpc_request(Payload, Token, DS) ->
    {get_uri(DS), get_headers(Token), "application/json", Payload}.

get_uri(DS) ->
    ?BASE_API_URI ++ "/" ++ DS ++ "/ingest".

get_headers(Token) ->
    [{"Authorization", "Bearer " ++ Token}].

get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    Hostname.

is_valid_log_level(Level) ->
    lists:member(Level, ?LEVELS).

validate_options([]) -> ok;
validate_options([{token, ""} | _T]) ->
    {error, missing_token};
validate_options([{token, Token} | T]) when is_list(Token) ->
    validate_options(T);
validate_options([{dataspace, ""} | _T]) ->
    {error, missing_dataspace};
validate_options([{dataspace, DT} | T]) when is_list(DT) ->
    validate_options(T);
validate_options([{source, ""} | _T]) ->
    {error, missing_source};
validate_options([{source, DT} | T]) when is_list(DT) ->
    validate_options(T);
validate_options([{retry_interval, N} | T]) when is_integer(N) ->
    validate_options(T);
validate_options([{max_retries, N} | T]) when is_integer(N) ->
    validate_options(T);
validate_options([{level, L} | T]) when is_atom(L) ->
    case is_valid_log_level(L) of
        false ->
            {error, {bad_level, L}};
        true ->
            validate_options(T)
    end;
validate_options([{formatter, _Val} | T]) ->
    validate_options(T);
validate_options([{format_config, _Val} | T]) ->
    validate_options(T);
validate_options([{metadata_filter, Excludes} | T]) when is_list(Excludes) ->
    validate_options(T);
validate_options([{httpc_opts, Opts} | T]) when is_list(Opts) ->
    validate_options(T);
validate_options([H | _]) ->
    {error, {bad_config, H}}.

get_configuration(Options) ->
    #state{ token           = get_option(token, Options, "")
          , dataspace       = get_option(dataspace, Options, "")
          , source          = get_option(source, Options, "unknown")
          , level           = lager_util:level_to_num(
                                get_option(level, Options, debug))
          , formatter       = get_option(formatter, Options, lager_default_formatter)
          , format_config   = get_option(format_config, Options, [])
          , metadata_filter = get_option(metadata_filter, Options, [])
          , retry_interval  = get_option(retry_interval, Options, 60)
          , max_retries     = get_option(max_retries, Options, 10)
          , httpc_opts      = get_option(httpc_opts, Options, [])
          }.

get_option(Key, Options, Default) ->
    case lists:keyfind(Key, 1, Options) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Pid) when is_pid(Pid) ->
    to_binary(pid_to_list(Pid));
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
to_binary(Value) ->
    Value.
