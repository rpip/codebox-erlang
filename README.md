Codebox Erlang library
=======================

This is an Erlang library for interacting with the [CodeBox](http://codebox.com) REST API.

This library currently supports the following operations:

* Create a new box
* List boxes
* Manage collaborators(for private boxes): List, add, remove collaborators
* Manage a box: Get Information about a box, remove a box
* Events: List events for a box
* Activity: Get box activity

## setup
````bash
$ git clone https://github.com/mawuli-ypa/codebox-erlang
$ cd codebox-erlang; make; make eunit # to run eunit tests
$ OR rebar get-deps; rebar compile; rebar eunit skip_deps=true #if you use the rebar build tool
````

## Usage
    $ erl -pa ebin deps/*/ebin
````erlang
Erlang R16B (erts-5.10.1) [source] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.1  (abort with ^G)
1> application:set_env(codebox, api_token, YOUR_CODEBOX_API_TOKEN)
````

### Create a new box
    2> Box = #cb_box{name="codebox-erl", type="type1",
                  stack="python", description, public=true},
    3> Response = codebox:create_box(Box),
    4> Response#cb_http_response.success.


### List boxes
    5> Response = codebox:list_boxes(),
    6> Response#cb_http_response.data.

### Retrieve activity log of a box
    7> Response = codebox:box_activity(YOUR_BOX_ID),
    8> Response#cb_http_response.data.


### Retrive Codebox events
    9> Response = codebox:box_events(YOUR_BOX_ID),
    10> Response#cb_http_response.data

### Retrieve box info
    11> Response = codebox:box_info(YOUR_BOX_ID),
    12> Response#cb_http_response.data.

### Remove a box
    13> Response = codebox:remove_box(YOUR_BOX_ID),
    14> Response#cb_http_response.success.


### Add collaborator
    15> Response = codebox:add_collaborator(BoxId, "username@email.com"),
    16> Response#cb_http_response.success.

### List collaborators
    17> Response = codebox:list_collaborators(YOUR_BOX_ID),
    18> Response#cb_http_response.data.

### Remove collaborator
    19> Response = codebox:remove_box(YOUR_BOX_ID, "username@email.com"),
    20> Response#cb_http_response.success.


## Note
* All API calls/actions  return an #cb_http_response{} record.
* Uses [lhttpc](https://github.com/esl/lhttpc) for the HTTP requests.
* For more information, please see [CodeBox REST API docs](https://github.com/FriendCode/codebox-client/blob/master/docs/api.md)


## LICENSE
MIT LICENSE(see LICENSE)


## Authors
Mawuli Adzaku <mawuli at mawuli dot me>
