# OpenTracing for Haskell

[OpenTracing](https://opentracing.io) is an attempt to define a common API to
instrument code for
[Dapper](https://research.google.com/pubs/pub36356.html)-style distributed
tracing, abstracting over various implementations of this concept (such as
Twitter's [Zipkin](https://zipkin.io), Uber's
[Jaeger](https://uber.github.io/jaeger/), or even [Apache
HTrace](http://htrace.incubator.apache.org).
