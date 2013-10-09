-module(simple_server).

-export([run/0]).

run() ->
    ServerFun = fun(Req) ->
                        Body = Req:recv_body(),
                        io:format("~p~n", [Body]),
                        Req:ok({"text/plain", Req:get(path)})
                end,
    ServerOpts = [{ip, "127.0.0.1"}, {port, 4444}, {loop, ServerFun}],
    {ok, Server} = mochiweb_http:start(ServerOpts).


