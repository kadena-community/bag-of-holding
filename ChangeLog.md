# Changelog for Bag of Holding

## 1.2.0 (2019-12-19)

### New Feature

The new `boh keys` command can be used to generate a key pair in the format that
`boh` expects:

```bash
$ boh keys
{
    "private": "09fbe65c7adb5f4d6928d1544c4a844740a6777cbc57dfeb96286cea63c4a520",
    "public": "007104e41eb2e1284212a2d86debafc67c4a9b5b837913b851b146f6b12fbed4"
}
```

To pipe this to a file for later use:

```bash
$ boh keys > keys.json
```

### Breaking Changes

The old way of opening `boh` to access the Wallet UI now requires the `wallet`
command:

```bash
$ boh wallet --keyfile=keys.json --account=you --node=us-w1.chainweb.com:443
```

## 1.1.1 (2019-12-10)

Fixed a bug involving decimal places entered into the Transfer Wizard.

## 1.1.0 (2019-12-03)

The former `t` command for writing Pact Transactions has been moved to `p`. In
its place, `t` now opens the Transfer Wizard. Use this to easily perform
single-chain transfers without writing your own Pact code.

**Note:** General transactions are still broken, but will be fixed in the next
version.

## 1.0.1 (2019-11-29)

The dependency on `chainweb` has been dropped, vastly reducing the number of
transitive dependencies required by `boh`. This also reduces binary size.
