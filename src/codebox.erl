%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <>
%%% @copyright (C) 2013, Mawuli Adzaku <mawuli@mawuli.me>
%%% @doc Erlang library for Codebox
%%%
%%% @end
%%% Created : 14 Dec 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------
-module(codebox).
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include("codebox.hrl").

%% Codebox API
-export([create_box/1, list_boxes/0, box_info/1,
         remove_box/1, box_events/1, box_activity/1]).
-export([list_collaborators/1, add_collaborator/2,
         remove_collaborator/2]).

%% Internal functions
-export([request/2, request/3, request_headers/0,
         parse_api_response/2, build_url/1, build_url/2]).


%%%-------------------------------------------------------------------
%%% Manage Boxes
%%%-------------------------------------------------------------------
%% @doc Create a new box
%%  Optional paramters are:
%%      description: default is empty
%%      git: default is empty
%%      public: default is false
-spec create_box(#cb_box{} | proplist() ) -> #cb_http_response{}.
create_box(Box) when is_record(Box, cb_box) ->
    BoxData = ?R2P(Box, cb_box),
    Url = build_url(create_box),
    request(post, Url, [{box, {BoxData}}]);
create_box(Box) ->
    Url = build_url(create_box),
    request(post, Url, Box).

%% @doc List boxes
-spec list_boxes() -> #cb_http_response{}.
list_boxes() ->
    Url = build_url(list_boxes),
    request(get, Url).

%% @doc Returns the information about a box
-spec box_info(BoxId :: boxid()) -> #cb_http_response{}.
box_info(BoxId) ->
    Url = build_url(manage_box, BoxId),
    request(get, Url).

%% @doc Remove a box
-spec remove_box(BoxId :: boxid()) -> #cb_http_response{}.
remove_box(BoxId) ->
    Url = build_url(manage_box, BoxId),
    request(delete, Url).
    
%% @doc Retrive events for a given a box
-spec box_events(BoxId :: boxid()) -> #cb_http_response{}.
box_events(BoxId) ->
    Url = build_url(box_events, BoxId),
    request(get, Url).

%% @doc Retrive activities on/for a box.
-spec box_activity(BoxId :: boxid()) -> #cb_http_response{}.
box_activity(BoxId) -> 
    Url = build_url(box_activity, BoxId),
    request(get, Url).


%%%-------------------------------------------------------------------
%%% Manage collaborators(for private boxes)
%%%-------------------------------------------------------------------
%% @doc Retrives list of collaborators emails
-spec list_collaborators(BoxId :: boxid()) -> #cb_http_response{}.
list_collaborators(BoxId)->
    Url = build_url(collaborators, BoxId),
    request(get, Url).

%% @doc Add a collaborator by email
-spec add_collaborator(BoxId, Email) -> #cb_http_response{} when
      BoxId :: boxid(),
      Email :: string().
add_collaborator(BoxId, Email)->
    Url = build_url(collaborators, BoxId),
    request(post, Url, [{email, Email}]).

%% @doc Remove a collaborator by email
-spec remove_collaborator(BoxId, Email) -> #cb_http_response{} when
      BoxId :: boxid(),
      Email :: string().
remove_collaborator(BoxId, Email)->
    Url = build_url(collaborators, BoxId),
    request(delete, Url, [{email, Email}]).

%%%-------------------------------------------------------------------
%%% Internal API
%%%-------------------------------------------------------------------
%% @doc Make an HTTP request
-spec request(Method, Url) -> #cb_http_response{} when
      Method :: get | delete | post,
      Url :: string().
request(get, Url) ->
    Headers = request_headers(),
    Res = lhttpc:request(Url, "GET", Headers,?HTTP_TIMEOUT),
    {ok, {{StatusCode, _}, _, Json}} = Res,
    parse_api_response(StatusCode, Json);
request(delete, Url) ->
    request(delete, Url, []).

-spec request(Method, Url, Data) -> #cb_http_response{} when
      Method :: get | delete | post,
      Url :: string(),
      Data :: list().
request(delete, Url, Data) when is_list(Data) ->
    Headers = request_headers(),
    Body = jiffy:encode({Data}),
    Res = lhttpc:request(Url, "DELETE", Headers, Body, ?HTTP_TIMEOUT),
    {ok, {{StatusCode, _}, _, Json}} = Res,
    parse_api_response(StatusCode, Json);
request(post, Url, Data) when is_list(Data) ->
    Headers = request_headers(),
    Body = jiffy:encode({Data}),
    Res = lhttpc:request(Url, "POST", Headers, Body, ?HTTP_TIMEOUT),
    {ok, {{StatusCode, _}, _, Json}} = Res,
    parse_api_response(StatusCode, Json).


%% @doc Construct the HTTP request headers
-spec request_headers() -> [tuple()].
request_headers() ->
    case application:get_env(?APP_NAME, api_token) of
        {ok, API_TOKEN} when is_list(API_TOKEN) ->
            [
             {"Authorization", API_TOKEN},              
             {"User-Agent", ?CB_USER_AGENT},
             {"Content-Type", "application/json"}
            ];
        _ ->
            [{"User-Agent", ?CB_USER_AGENT},
             {"Content-Type", "application/json"}
            ]
    end.


%% @doc Parses the HTTP response into a cb_http_response record
-spec parse_api_response(HttpCode, ResponseText) -> #cb_http_response{} when
      HttpCode :: integer(),
      ResponseText :: json().
parse_api_response(200, ResponseText) ->
    ResponseText2 = decode(ResponseText),
    #cb_http_response{success=true, http_code=200, data=ResponseText2};
parse_api_response(StatusCode, ResponseText) ->
    ResponseText2 = decode(ResponseText),
    #cb_http_response{success=false, http_code=StatusCode, data=ResponseText2}.
    
%% @doc Decode the response from the server
-spec decode(Text :: binary()) -> json().
decode(Text) ->
    Json = jiffy:decode(Text),
    Json.

%% doc Returns the API resource URL
build_url(create_box) ->
    ?CB_API_SERVER ++ "/boxes";
build_url(list_boxes) ->
    ?CB_API_SERVER ++ "/boxes";
build_url(_) ->
    undefined.
build_url(collaborators, BoxId) ->
    ?CB_API_SERVER ++ "/box/" ++ BoxId ++ "/collaborators";
%% 'manage_box' returns url for both 'box_info' and 'remove_box'.
build_url(manage_box, BoxId) ->
    ?CB_API_SERVER ++ "/box/" ++ BoxId;
build_url(box_events, BoxId) ->
    ?CB_API_SERVER ++ "/box/" ++ BoxId ++ "/events";
build_url(box_activity, BoxId) ->
    ?CB_API_SERVER ++ "/box/" ++ BoxId ++ "/activity";
build_url(_, _) ->
    undefined.
