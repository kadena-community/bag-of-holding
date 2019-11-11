# Bag of Holding

*A terminal wallet for the Kadena Public Blockchain.*

![Running a transaction.](screenshot.png)

## Usage

```
boh --keyfile=keys.json --account=YOUR-ACCOUNT-NAME --node=us-e1.chainweb.com:443
```

Where `keys.json` has the format:

```json
{
  "private": "YOUR-PRIVATE-KEY",
  "public": "YOUR-PUBLIC-KEY"
}
```

Clearly not yet secure. Use at your own discretion.
