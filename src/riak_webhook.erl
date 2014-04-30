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
        URL when is_list(URL) ->
            ok;
        Bin when is_binary(Bin) ->
            URL = erlang:binary_to_list(Bin)
    end,

    Headers = [],

    Struct = riak_object:to_json(RObj),

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
                {struct, proplists:delete(<<"values">>, StructMembers)  ++ [ {<<"values">>, NewSiblings} ]}
        end,

    Encoder  = mochijson2:encoder([{utf8, true}]),
    JSONData = Encoder(UseStruct),

    {ok, _} = httpc:request(post, {URL, Headers, "application/json; charset=utf-8", iolist_to_binary(JSONData)}, [], [], ?MODULE ),

    ok.


get_env({Property, Default}) ->
    case application:get_env(Property) of
        undefined ->
            {Property, Default};
        {ok, Other} ->
            {Property, Other}
    end.

start_httpc_profile() ->

    %% make sure to intern the atoms used by the bucket properties
    _ = erlang:binary_to_atom(<<"webhook_url">>, latin1),
    _ = erlang:binary_to_atom(<<"webhook_sendbody">>, latin1),

    %% start a HTTP client profile for the app (stand_alone will make it
    %% be linked to this process (the supervisor).
    ServiceConfig = [{profile, ?MODULE}],
    {ok, PID} = inets:start(httpc, ServiceConfig, stand_alone),

    %% register by this special name, so that the hook does not need to know the PID
    erlang:register( erlang:list_to_atom( "httpc_" ++ erlang:atom_to_list(?MODULE)), PID),

    %% configure the client profile

    {ok, Opts} = httpc:get_options(all, ?MODULE),

    Opts2 = lists:foldl(fun ({Prop,Val}, Dict) -> orddict:store(Prop,Val,Dict) end,
                        orddict:from_list(Opts),
                        [ {max_sessions, 10},
                          {keep_alive_timeout, 1000},
                          {pipeline_timeout, 1000},
                          {max_pipeline_length, 10} ] ),

    HTTPClientOptions = [ get_env(PropVal) || PropVal <- Opts2 ],

    httpc:set_options(HTTPClientOptions, PID),

    {ok, PID}.


start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

