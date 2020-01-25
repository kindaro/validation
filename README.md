# Validation

Several data-types like Either but with differing properties and type-class
instances.

This package is identical to `validation`, but with the `lens` dependency
removed. Therefore, it is easier to understand and faster to build, but some
features are absent.

* `Validation`

  The `Validation` data type is isomorphic to `Either`, but has an instance
  of `Applicative` that accumulates on the error side. That is to say, if two
  (or more) errors are encountered, they are appended using a `Semigroup`
  operation.

  As a consequence of this `Applicative` instance, there is no corresponding
  `Bind` or `Monad` instance. `Validation` is an example of, "An applicative
  functor that is not a monad."

