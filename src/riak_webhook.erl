-module(riak_webhook).

-export([postcommit/1, start_httpc_profile/0]).

postcommit(RObj) ->

    case application:start(?MODULE) of
        ok -> ok; {error,{already_started,_}} -> ok
    end,

    {ok, URL} = get_url(riak_object:bucket(RObj)),

    Headers = [],

    Struct = riak_object_json:encode(RObj),
    JSONData = mochijson2:encode(Struct),

    {ok, _} = httpc:request(post, {URL, Headers, "application/json", erlang:iolist_to_binary(JSONData)}, [], [], ?MODULE ),

    ok.


get_url(_Bucket) ->
    application:get_env(?MODULE, url).


start_httpc_profile() ->
    ServiceConfig = [{profile, ?MODULE}],
    {ok, PID} = inets:start(httpc, ServiceConfig, stand_alone),

    %% configure the client to have up to 10 concurrenct sockets
    %% using both keep-alive and pipelining

    HTTPClientOptions =
        [
         {max_sessions, 10},
         {max_keep_alive_length, 10},
         {pipeline_timeout, 120000},
         {max_pipeline_length, 10}
        ],

    httpc:set_options(HTTPClientOptions, PID),

    erlang:register( erlang:list_to_atom( "httpc_" ++ erlang:atom_to_list(?MODULE)), PID),

    {ok, PID}.
