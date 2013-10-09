# Riak Web Hook

This is a postcommit hook that `POST`s newly written riak objects to a remote HTTP server.

You set this module/function as your postcommit hook using whatever tools you're used to.   For instance, to install the postcommit hook in the `test` bucket, you can do:

````
> curl -v -X PUT -H "Content-Type: application/json"
     -d '{ "props":{ "postcommit":[{"mod":"riak_webhook","fun":"postcommit"}] }}'
     http://127.0.0.1:8098/riak/test
````

## Installation

Clone the source, build, and then symlink the required dependencies into your Riak install:

````
git clone https://github.com/krestenkrab/riak_webhook.git
cd riak_webhook
./rebar get-deps
make
[...wait for a long time while spidermonkey builds...]
cd $RIAK_LIBS
ln -s $BUILD_DIR riak_webhook-0.0.1
````

## Configuration

To tell the commit hook where to send your updates, configure the application `etc/app.config` to contain a section like this:

````erlang
   {riak_webhook, [   {url, "http://somehost:port/path" }   ]},
````

The value being posted is the same as you would have seen in a JavaScript hook or map/reduce setting.  The value is always posted with content-type `application/json`. For instance:

````javascript
{  
    "bucket" : "somebucket",
    "key" : "mykey",
    "vclock":VclockAsString,
    "values" : [
        {  
            "metadata":{
                        "content-type": "text/plain"
                        "X-Riak-VTag":VtagAsString,
                        ...other metadata...
                       },
            "data" : " .. data as string here .." 
        },
        // any siblings here
    ]
````

You'll notice that even if the content type is `application/json`, the value held in `data` will be the string representation of the JSON object.
