* 0.3.0 (2023-09-28)

  - GHC 8.x is no longer tested, the minimum version is 9.2.8
  - Various fixes the Jaeger backend
  - Fix Zipkin backend to use POST (#49)
  - Use a bounded queue for the batch reporter (#46)

  This release is considered _breaking_ due to #46, and (potential)
  incompatibility with older GHCs.

* 0.2.2 (2022-05-09)

  Support for base 4.16 / GHC 9.2

  GHC versions below 8.10 are no longer tested in CI.

* opentracing/0.2.1 (2021-11-08)

  Support aeson 2.x (#39)

* 0.2.0 (2021-06-03)

  - Drop support for GHC 8.0 (#35)
  - Add support for GHC 9.0 (#36)
  - Replace the unmaintained `thrift` package with `pinch` (#34)
  - wai: Better default operation name, and allow customising it (#33, #37)

  The major version is bumped because the packages won't compile with GHC 8.0
  anymore.

* 0.1.0.0 (2021-04-20)

  After years of friendly reminders, a zero version gets finally released =]
