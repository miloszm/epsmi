# epsmi
Electrum Personal Server written in Scala 2.13


For more information please see https://github.com/chris-belcher/electrum-personal-server 

This is a Scala implementation of the above server 


## Electrum server API methods supported:

- blockchain.transaction.get
- blockchain.transaction.get_merkle
- blockchain.scripthash.subscribe
- blockchain.scripthash.get_history
- blockchain.scripthash.get_balance
- server.ping
- blockchain.headers.subscribe
- blockchain.block.get_header
- blockchain.block.header
- blockchain.block.headers
- blockchain.block.get_chunk
- blockchain.transaction.broadcast
- mempool.get_fee_histogram
- blockchain.estimatefee
- blockchain.relayfee
- server.banner
- server.donation_address
- server.version
- blockchain.transaction.id_from_pos

### How to Run ###

```
sbt run
```

EPSMI needs Bitcoin’s bitcoind/bitcoind.exe executable on system’s PATH

adjust settings in application.conf
