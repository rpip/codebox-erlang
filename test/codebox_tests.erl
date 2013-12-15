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
-define(CB_TEST_BOXID, "2a2cbcc5-afa1-431e-89aa-f35d4b07aa56").
-define(CB_TEST_COLLABORATOR, "mawuli.ypa@gmail.com").

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
       %{"Remove/delete a box",
        %fun remove_box/0},
       {"Retrieve list of people collaborating on a box",
        fun list_collaborators/0},
       {"Add a collaborator",
        fun add_collaborator/0},
       {"Remove a collaborator",
        fun remove_collaborator/0}
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
                  stack = <<"python">>, description = <<"Codebox erlang test box">>, 
                  public = <<"true">> },
    Response = codebox:create_box(Box),
    ?assertEqual(true, Response#cb_http_response.success).

box_activity() ->
    Response = codebox:box_activity(?CB_TEST_BOXID),
    ?assertEqual(true, Response#cb_http_response.success).
    
box_events() ->
    Response = codebox:box_events(?CB_TEST_BOXID),
    ?assertEqual(true, Response#cb_http_response.success).

box_info() ->
    Response = codebox:box_info(?CB_TEST_BOXID),
    ?assertEqual(true, Response#cb_http_response.success).

remove_box() ->
    Response = codebox:remove_box(?CB_TEST_BOXID),
    ?assertEqual(true, Response#cb_http_response.success).


%%%-------------------------------------------------------------------
%%% collaborators
%%%-------------------------------------------------------------------
add_collaborator() ->
    Response = codebox:add_collaborator(?CB_TEST_BOXID, ?CB_TEST_COLLABORATOR),
    ?assertEqual(true, Response#cb_http_response.success).

list_collaborators() ->
    Response = codebox:list_collaborators(?CB_TEST_BOXID),
    ?assertEqual(true, Response#cb_http_response.success). 

remove_collaborator() ->
    Response = codebox:remove_collaborator(?CB_TEST_BOXID, ?CB_TEST_COLLABORATOR),
    ?assertEqual(true, Response#cb_http_response.success).



