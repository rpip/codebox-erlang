%%%-------------------------------------------------------------------
%%% @author Mawuli Adzaku <mawuli@mawuli.me>
%%% @copyright (C) 2013, Mawuli Adzaku
%%% @doc
%%%
%%% @end
%%% Created : 12 Dec 2013 by Mawuli Adzaku <mawuli@mawuli.me>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% types
%%%-------------------------------------------------------------------
-type response_code() :: integer().
-type response_text() :: binary().
-type response_data() :: binary().
-type json() :: tuple().
-type proplist() :: [atom() | tuple()].
-type boxid() :: string().

%%%-------------------------------------------------------------------
%%% records
%%%-------------------------------------------------------------------
-record(cb_http_response, {success :: true | false,
                           http_code :: response_code(), 
                           data :: response_data()
                         }).
-record(cb_box, {name :: string(),
                 type :: string(),
                 stack :: string(),
                 description :: string(),
                 git :: string(),
                 public :: boolean()
                }).
%%%-------------------------------------------------------------------
%%% miscellaneous
%%%-------------------------------------------------------------------
-define(APP_NAME, codebox).
%% api keys used for testing
-define(API_VERSION, "v1").

%% user-agent headers
-define(CB_USER_AGENT, "codebox-erlang/v0.1.0").

%% CodeBox API server
-define(CB_API_SERVER, "https://api.codebox.io/api").

%%timeout for HTTP requests
-define(HTTP_TIMEOUT, 10000).

%% Turns a record into a proplist
-define(R2P(Record,RecordType), lists:zip(record_info(fields, RecordType), tl(tuple_to_list(Record)))).
