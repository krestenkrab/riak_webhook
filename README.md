# Riak Web Hook

This is a postcommit hook that POST's newly written riak objects to a remote HTTP server.

You set this module/function as your postcommit hook using whatever tools you're used to.   For instance, to install the postcommit hook in the `test` bucket, you can do:

````sh
curl -v -X PUT -H "Content-Type: application/json" \
     -d '{ "props":{ "postcommit"  : [{"mod":"riak_webhook","fun":"postcommit"}],
                     "webhook_url" : "http://localhost:4444/update",
                     "webhook_sendbody" : "false" }}' \
     http://127.0.0.1:8098/riak/test
````

The bucket property `webhook_url` configures where to send the request,
and `webhook_sendbody` controls if the body (the data of the stored object) should
be included in the POST body.

The first time you configure bucket properties, you might have to go into the riak console (e.g. by attaching to a riak node), and run `riak_webhook:start()` before doing the above curl post, which makes sure that it accepts the configuration names.


## Installation

Clone the source, build, and then symlink the required dependencies into your Riak install:

````sh
git clone https://github.com/krestenkrab/riak_webhook.git
cd riak_webhook
./rebar get-deps
./rebar compile
[...wait for a long time while spidermonkey builds...]
cd $RIAK_LIBS
ln -s $BUILD_DIR riak_webhook-0.0.2
````

## Configuration

You can configure the application `etc/app.config` to contain a section like this:

````erlang
   {riak_webhook, [  
         {max_sessions, 10},
         {max_keep_alive_length, 10},
         {pipeline_timeout, 120000},
         {max_pipeline_length, 10}
    ]},
````

The meaning of these properties can be found [here](http://erlang.org/doc/man/httpc.html#set_options-2), but the default configuration allows up to 10 concurrent persistent sockets, each having a maximum of 10 outstanding pipelined HTTP requests.

The value being posted is the same as you would have seen in a JavaScript hook or map/reduce setting.  The value is always posted with content-type `application/json; charset=utf-8`.  For instance:

````sh
curl -v -H 'Content-Type: application/json' \
    -X PUT -d '{ "foo" : "bar" }' \
    -H 'x-riak-index-email_bin: krab@trifork.com' \
    http://localhost:8098/riak/test/foo
````

Could result in the following JSON being POST'ed:

````javascript
{
  "bucket": "test",
  "key": "foo",
  "vclock": "a85hYGBgzGDKBVIcypz/fgYF5eRnMCXy5bEyVHw7dZovCwA=",
  "values": [
    {
      "metadata": {
        "Links": [],
        "X-Riak-VTag": "44srWHnt3975dLgx2LHucT",
        "content-type": "application/json",
        "index": {
          "email_bin": "krab@trifork.com"
        },
        "X-Riak-Last-Modified": "Thu, 10 Oct 2013 09:59:20 GMT",
        "X-Riak-Meta": {}
      }
      "data": "{ \"foo\" : \"bar\" }"      
    },
    // any siblings here
  ]
}
````

Only if you set the *bucket* property `webhook_sendbody` to true will the `data` element be included.   You'll notice that even if the content type is `application/json`, the value held in `data` will be the string representation of the JSON object.


