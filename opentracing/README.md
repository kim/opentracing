## OpenTracing

The [OpenTracing spec](https://github.com/opentracing/specification/blob/master/specification.md) defines a platform agnostic approach for distributed tracing. Distributed
tracing gives us insights into how complex programs spread across multiple processes are
performing together.

This package provides a core implementation of the OpenTracing spec. It includes
functionality to

  * Create `Span`s describing application code executions, including `Tag`s and
    `LogRecord`s

  * Serialize and deserialize `SpanContext`s across process boundaries

  * Batch and log `FinishedSpan`s

It does not provide any functionality for consuming `Span`s. There are platform specific
backends (CloudTrace, Zipkin, Jaeger, etc) that are provided in other packages.
