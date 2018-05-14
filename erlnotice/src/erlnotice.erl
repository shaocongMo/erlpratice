-module (erlnotice).

-export ([start/0]).

start() ->
    application:start(inets),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(erlnotice).