-module(riak_webhook_tests).

-include_lib("eunit/include/eunit.hrl").


with_server(Transport, ServerFun, ClientFun) ->
    ServerOpts = [{ip, "127.0.0.1"}, {port, 4444}, {loop, ServerFun}],
    {ok, Server} = mochiweb_http:start(ServerOpts),
    Port = mochiweb_socket_server:get(Server, port),
    Res = (catch ClientFun(Transport, Port)),
    mochiweb_http:stop(Server),
    Res.

simple_test() ->
    application:start( riak_webhook ),
    Owner = self(),
    ok = with_server(plain,
                     fun(Req) ->
                             Body = Req:recv_body(),
                             Owner ! {body, Body},
                             Req:ok({"text/plain", Req:get(path)})
                     end,
                     fun(_Transport, _Port) ->
                             ok = riak_webhook:postcommit( riak_object:new(<<"xbuck">>, <<"xkey">>, <<"xvalue">>) )
                     end),
    receive
        {body, Body} ->
            {struct, Members} = mochijson2:decode( Body ),
            <<"xbuck">> = proplists:get_value(<<"bucket">>, Members),
            <<"xkey">> = proplists:get_value(<<"key">>, Members),
            [{struct, ValueProps}] = proplists:get_value(<<"values">>, Members),
            <<"xvalue">> = proplists:get_value(<<"data">>, ValueProps),
            ok
    after 1000 ->
            exit(did_not_receive_body)
    end.
