# Changelog for bag-of-holding

## 1.1.0 (2019-12-03)

The former `t` command for writing Pact Transactions has been moved to `p`. In
its place, `t` now opens the Transfer Wizard. Use this to easily perform
single-chain transfers without writing your own Pact code.

**Note:** General transactions are still broken, but will be fixed in the next
version.

## 1.0.1 (2019-11-29)

The dependency on `chainweb` has been dropped, vastly reducing the number of
transitive dependencies required by `boh`. This also reduces binary size.
