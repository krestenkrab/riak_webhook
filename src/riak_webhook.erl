-module(riak_webhook).

-export([postcommit/1,

         %% utility to start/stop the webhook app
         start/0, stop/0,

         %% internal API used by the supervisor
         start_httpc_profile/0
]).

postcommit(RObj) ->

    case application:start(?MODULE) of
        ok -> ok; {error,{already_started,_}} -> ok
    end,

    {ok, C} = riak:local_client(),
    Props = C:get_bucket(riak_object:bucket(RObj)),

    case proplists:get_value(webhook_url, Props) of
        %% fails if there is no url property
        URL when is_list(URL) -> ok
    end,

    Headers = [],

    Struct = riak_object_json:encode(RObj),

    UseStruct =
        case proplists:get_value(webhook_sendbody, Props) of
            true ->
                Struct;
            <<"true">> ->
                Struct;
            _ ->
                {struct, StructMembers} = Struct,
                Siblings = proplists:get_value(<<"values">>, StructMembers, []),
                NewSiblings = [ {struct, proplists:delete(<<"data">>, SiblingMembers) }
                                || {struct, SiblingMembers} <- Siblings ],
                {struct, proplists:delete(<<"values">>, StructMembers) ++ NewSiblings}
        end,

    Encoder  = mochijson2:encoder([{utf8, true}]),
    JSONData = Encoder(UseStruct),

    {ok, _} = httpc:request(post, {URL, Headers, "application/json; charset=utf-8", JSONData}, [], [], ?MODULE ),

    ok.


start_httpc_profile() ->

    %% make sure to intern the atoms used by the bucket properties
    _ = erlang:binary_to_atom(<<"webhook_url">>, latin1),
    _ = erlang:binary_to_atom(<<"webhook_sendbody">>, latin1),

    %% start a HTTP client profile for the app (stand_alone will make it
    %% be linked to this process (the supervisor).
    ServiceConfig = [{profile, ?MODULE}],
    {ok, PID} = inets:start(httpc, ServiceConfig, stand_alone),

    %% configure the client profile

    MaxSessions = application:get_env(max_sessions, 10),
    MaxKeepAlive = application:get_env(max_keep_alive_length, 10),
    PipleLineTimeout = application:get_env(pipeline_timeout, 120000),
    MaxPipelineLength = application:get_env(max_pipeline_length, 10),

    HTTPClientOptions =
        [
         {max_sessions, MaxSessions},
         {max_keep_alive_length, MaxKeepAlive},
         {pipeline_timeout, PipleLineTimeout},
         {max_pipeline_length, MaxPipelineLength}
        ],

    httpc:set_options(HTTPClientOptions, PID),

    %% register by this special name, so that the hook does not need to know the PID
    erlang:register( erlang:list_to_atom( "httpc_" ++ erlang:atom_to_list(?MODULE)), PID),

    {ok, PID}.


start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

