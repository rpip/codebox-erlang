%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(codebox_tests).
-include_lib("eunit/include/eunit.hrl").
-include("codebox.hrl").
%% API Token for testing
-define(CB_TEST_TOKEN, "3ca7a5bd-9bde-49c3-9f6d-e9a951c319bc").
%-define(CB_TEST_BOXID, "2a2cbcc5-afa1-431e-89aa-f35d4b07aa56").
-define(CB_TEST_COLLABORATOR, <<"mawuli.ypa@gmail.com">>).

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
codebox_test_() ->
    {spawn, 
     {setup,
      fun setup/0,
      fun teardown/1,
      [
       {"List boxes",
        fun list_boxes/0},
       {"Create a box",
        fun create_box/0},
       {"Retrive information abouta a box",
        fun box_info/0},
       {"Retrive events about/for a box",
        fun box_events/0},
       {"Retrive activity log about/for a box",
        fun box_activity/0},
       {"Retrieve list of people collaborating on a box",
        fun list_collaborators/0},
       {"Add a collaborator",
        fun add_collaborator/0},
       %{"Remove a collaborator",
        %fun remove_collaborator/0},
       {"Remove/delete a box",
        fun remove_box/0}
     ]
     }
    }.


%%%-------------------------------------------------------------------
%%% Setup / Cleanup
%%%-------------------------------------------------------------------
setup() ->
    application:set_env(?APP_NAME, api_token, ?CB_TEST_TOKEN),
    application:start(?APP_NAME),
    ok.

teardown(_) ->
    application:set_env(?APP_NAME, api_token, undefined),
    application:stop(?APP_NAME).


%%%-------------------------------------------------------------------
%%% tests
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
%%% Boxes
%%%-------------------------------------------------------------------
list_boxes() ->
    Response = codebox:list_boxes(),
    ?assertEqual(true, Response#cb_http_response.success).

create_box() ->
    Box = #cb_box{name = <<"codebox-erl">>, type = <<"type1">>, 
                  stack = <<"python">>, description = <<"Codebox Python test box">>, 
                  public = <<"true">>},
    Response = codebox:create_box(Box),
    Boxid = jsonq:q([<<"id">>], Response#cb_http_response.data),
    set_demobox_id(binary_to_list(Boxid)),
    ?assertEqual(binary_to_list(Boxid), get_demobox_id()),
    ?assertEqual(true, Response#cb_http_response.success).

box_activity() ->
    Response = codebox:box_activity(get_demobox_id()),
    ?assertEqual(true, Response#cb_http_response.success).
    
box_events() ->
    Response = codebox:box_events(get_demobox_id()),
    ?assertEqual(true, Response#cb_http_response.success).

box_info() ->
    Response = codebox:box_info(get_demobox_id()),
    ?assertEqual(true, Response#cb_http_response.success).

remove_box() ->
    Response = codebox:remove_box(get_demobox_id()),
    ?assertEqual(true, Response#cb_http_response.success).


%%%-------------------------------------------------------------------
%%% collaborators
%%%-------------------------------------------------------------------
add_collaborator() ->
    Response = codebox:add_collaborator(get_demobox_id(), ?CB_TEST_COLLABORATOR),
    ?assertEqual(true, Response#cb_http_response.success).

list_collaborators() ->
    Response = codebox:list_collaborators(get_demobox_id()),
    ?assertEqual(true, Response#cb_http_response.success). 

remove_collaborator() ->
    Response = codebox:remove_collaborator(get_demobox_id(), ?CB_TEST_COLLABORATOR),
    ?assertEqual(true, Response#cb_http_response.success).



%%%-------------------------------------------------------------------
%%% Miscellaneous
%%%-------------------------------------------------------------------
%% @doc Returns the demo box id
get_demobox_id() ->
    case application:get_env(?APP_NAME, demobox_id) of
        {ok, API_TOKEN} when is_list(API_TOKEN) ->
            API_TOKEN;
        _ ->
           undefined
    end.

%% @doc Set's the demo box id
set_demobox_id(Boxid)->
    application:set_env(?APP_NAME, demobox_id, Boxid).
