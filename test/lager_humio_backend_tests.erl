%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <tajgur@gmai.com>
%%% @doc
%%% Erlang library for Pocket API - http://getpocket.com/developer/docs/
%%% @end
%%% Created : 25 Feb 2017 by Martin Wiso <martin@wiso.cx>
%%%----------------------------------------------------------------------------
-module(lager_humio_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
lager_humio_backend_test_() ->
    {setup,
     fun() ->
             mock_httpc()
     end,
     fun(_) ->
             unmock_httpc()
     end,
     [
       {"Call Ingest API success",        fun test_call_ingest_api_success/0}
     , {"Call Ingest API retry",          fun test_call_ingest_api_retry/0}
     , {"Loading of init/config options", fun test_get_configuration/0}
     , {"Validate init/config options",   fun test_validate_options/0}
     , {"Create payload - tags",          fun test_create_tags/0}
     , {"Create payload - event",         fun test_create_event/0}
     ]
    }.

%% =============================================================================
test_call_ingest_api_success() ->
    Request = lager_humio_backend:create_httpc_request(<<"{}">>, "foo", "bar"),

    ?assertEqual(
       {"https://go.humio.com/api/v1/dataspaces/bar/ingest",
        [{"Authorization", "Bearer foo"}],
        "application/json", <<"{}">>},
       Request
      ),

    ok = meck:expect(
           httpc, request,
           fun (post, MockedRequest, [], []) ->
                   ?assertEqual(Request, MockedRequest),
                   {ok, {{"HTTP/1.1", 200, "OK"}, [], <<>>}};
               (_, _, _, _) ->
                   ?assert(false),
                   error
           end
          ),

    ?assertEqual(ok, lager_humio_backend:call_injest_api(Request, 3, 10, [])),

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
    ?assertEqual(ok, lager_humio_backend:call_injest_api(Request, 0, 10, [])),

    %% tests case when we hit maximum number of retries
    ?assertEqual(ok, lager_humio_backend:call_injest_api(Request, 3, 1, [])),

    ok.

test_get_configuration() ->
    Options = [ {token, ""}
              , {dataspace, ""}
              , {level, debug}
              , {formatter, lager_default_formatter}
              , {format_config, []}
              , {metadata_filter, []}
              , {retry_interval, 60}
              , {max_retries, 10}
              , {httpc_opts, []}
              ],

    ?assertEqual(
       {state, [], [], 128, lager_default_formatter, [], [], 60, 10, []},
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

test_create_tags() ->
    MD = [{pid, "<0.3774.0>"},
          {line, 119},
          {file, "lager_handler_watcher.erl"},
          {module, lager_handler_watcher}
         ],

    Map = lager_humio_backend:create_tags(info, MD),

    ?assertEqual(
       to_binary(lager_humio_backend:get_hostname()),
       maps:get(<<"host">>, Map)
      ),
    ?assertEqual(info, maps:get(<<"level">>, Map)),
    ?assert(is_pid(to_pid(maps:get(<<"source">>, Map)))),

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

to_binary(Value) when is_list(Value) ->
    list_to_binary(Value).

to_pid(Value) when is_binary(Value) ->
    list_to_pid(binary_to_list(Value)).
