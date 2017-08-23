%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 25 Feb 2017 by Martin Wiso <martin@wiso.cx>
%%%----------------------------------------------------------------------------
-module(lager_humio_backend_tests).

-include_lib("eunit/include/eunit.hrl").

-compile({parse_transform, lager_transform}).

%% =============================================================================
lager_humio_backend_test_() ->
    {foreach,
     fun() ->
             mock_httpc()
     end,
     fun(_) ->
             unmock_httpc()
     end,
     [
       {"Integration",                    fun test_integration/0}
     , {"Call Ingest API retry",          fun test_call_ingest_api_retry/0}
     , {"Loading of init/config options", fun test_get_configuration/0}
     , {"Validate init/config options",   fun test_validate_options/0}
     , {"Create payload - event",         fun test_create_event/0}
     ]
    }.

%% =============================================================================
test_integration() ->
    HumioConfig = {lager_humio_backend,
                   [{token, "foo"},
                    {dataspace, "bar"},
                    {source, "foobar"},
                    {level, info}]
                  },

    application:load(lager),
    application:set_env(lager, handlers, [HumioConfig]),
    application:set_env(lager, error_logger_redirect, false),

    ok = meck:expect(
           httpc, request,
           fun (post, Request, [], []) ->
                   assert_request(Request),
                   {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}};
               (_, _, _, _) ->
                   ?assert(false),
                   error
           end
          ),
    ?assertEqual(ok, lager:start()),

    lager:info("Hello World!"),

    ?assertEqual(ok, lager:set_loglevel(lager_humio_backend, debug)),
    ?assertEqual(debug, lager:get_loglevel(lager_humio_backend)),
    ?assertEqual({error,bad_loglevel}, lager:set_loglevel(lager_humio_backend, foobar)),

    ok = application:stop(lager),
    ?assert(meck:validate(httpc)),
    ok.

assert_request({Url, Headers, ContentType, Payload}) ->
    ?assertEqual("https://go.humio.com/api/v1/dataspaces/bar/ingest", Url),
    ?assertEqual([{"Authorization","Bearer foo"}], Headers),
    ?assertEqual("application/json", ContentType),

    Decoded = jiffy:decode(Payload, [return_maps]),
    ?assert(is_list(Decoded)),

    [Event] = maps:get(<<"events">>, hd(Decoded)),
    ?assert(maps:is_key(<<"rawstring">>, Event)),
    ?assertNotEqual(
       nomatch,
       binary:match(maps:get(<<"rawstring">>, Event), [<<"Hello World!">>])
      ),
    ?assertEqual(
       lists:sort([<<"function">>, <<"line">>, <<"module">>, <<"node">>, <<"pid">>,<<"source">>,<<"host">>]),
       lists:sort(maps:keys(maps:get(<<"attributes">>, Event)))
      ),
    ?assertEqual(
       lists:sort([<<"level">>]),
       lists:sort(maps:keys(maps:get(<<"tags">>, hd(Decoded))))
      ),
    ok.

test_call_ingest_api_retry() ->
    Request = lager_humio_backend:create_httpc_request(<<"{}">>, "foo", "bar"),

    ok = meck:expect(
           httpc, request,
           fun (post, MockedRequest, [], []) ->
                   ?assertEqual(Request, MockedRequest),
                   {ok, {{"HTTP/1.1", 400, "Bad Request"}, [], <<>>}};
               (_, _, _, _) ->
                   ?assert(false),
                   error
           end
          ),

    %% tests case when we hit maximum number of retries
    ?assertEqual(ok, lager_humio_backend:call_ingest_api(Request, 0, 1, [])),

    %% tests case when we hit maximum number of retries
    ?assertEqual(ok, lager_humio_backend:call_ingest_api(Request, 3, 1, [])),
    ?assert(meck:validate(httpc)),
    ok.

test_get_configuration() ->
    Options = [ {token, ""}
              , {dataspace, ""}
              , {source, ""}
              , {level, debug}
              , {formatter, lager_default_formatter}
              , {format_config, []}
              , {metadata_filter, []}
              , {retry_interval, 60}
              , {max_retries, 10}
              , {httpc_opts, []}
              ],

    ?assertEqual(
       {state, [], [], [], 128, lager_default_formatter, [], [], 60*1000, 10, []},
       lager_humio_backend:get_configuration(Options)
      ).

test_validate_options() ->
    ValidOptions = [ {token, "foo"}
                   , {dataspace, "bar"}
                   , {level, debug}
                   , {formatter, lager_default_formatter}
                   , {format_config, []}
                   , {metadata_filter, []}
                   , {retry_interval, 60}
                   , {max_retries, 10}
                   , {httpc_opts, []}
                   ],

    ?assertEqual(ok, lager_humio_backend:validate_options(ValidOptions)),
    ?assertEqual(ok, lager_humio_backend:validate_options([])),

    Invalid = [
                {{token, ""},           {error, missing_token}}
              , {{dataspace, ""},       {error, missing_dataspace}}
              , {{source, ""},          {error, missing_source}}
              , {{level, unknown},      {error, {bad_level, unknown}}}
              , {{retry_interval, foo}, {error, {bad_config, {retry_interval, foo}}}}
              , {{max_retries, bar},    {error, {bad_config, {max_retries, bar}}}}
              , {{httpc_opts, #{}},     {error, {bad_config, {httpc_opts, #{}}}}}
              ],
    lists:foreach(
      fun ({Option, ExpectedError}) ->
              ?assertEqual(
                 ExpectedError,
                 lager_humio_backend:validate_options([Option])
                )
      end, Invalid),
    ok.

test_create_event() ->
    MD = [{pid, "<0.3774.0>"},
          {line, 119},
          {file, "lager_handler_watcher.erl"},
          {module, lager_handler_watcher}
         ],
    MDFilter = [line, file],
    Ts = {1501,189140,422258},

    ?assertEqual(
       #{<<"attributes">> =>
             #{module => <<"lager_handler_watcher">>,
               pid => <<"<0.3774.0>">>
              },
         <<"rawstring">> => <<"raw">>,
         <<"timestamp">> => <<"2017-07-27T20:59:00Z">>},
       lager_humio_backend:create_event(Ts, MD, MDFilter, <<"raw">>)
      ).

%%%============================================================================
%%% Internal functionality
%%%============================================================================
mock_httpc() ->
    ?assertEqual(ok, meck:new(httpc, [unstick, passthrough])),
    ?assertEqual(true, meck:validate(httpc)),
    ok.

unmock_httpc() ->
    ?assertEqual(ok, meck:unload(httpc)),
    ok.
