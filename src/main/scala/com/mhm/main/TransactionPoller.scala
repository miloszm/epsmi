package com.mhm.main


/**
 * We need a thread which will periodically (every second or so)
 * call TransactionMonitor.checkForUpdatedTxs similarly to what
 * on_heartbeat_connected does in Python EPS.
 * TransactionMonitor.checkForUpdatedTxs is also called by on_heartbeat_listening,
 * but no information is send to client when listening, as there is no client socket available.
 *
 * is_tip_updated, header = check_for_new_blockchain_tip(rpc,protocol.are_headers_raw)
 * if is_tip_updated:
 *   protocol.on_blockchain_tip_updated(header) // send headers update via the "blockchain.headers.subscribe" channel
 *   updated_scripthashes = txmonitor.check_for_updated_txes()
 *   protocol.on_updated_scripthashes(updated_scripthashes) // send new history hash via the "blockchain.scripthash.subscribe" channel
 *
 * actual send update boils down to sending the following rpc:
 *   {"method": "blockchain.headers.subscribe", "params": [header]}  // for on_blockchain_tip_updated
 *   {"method": "blockchain.scripthash.subscribe", "params": [scrhash, history_hash]}  // for on_updated_scripthashes
 *
 * actual sending back to socket will require modification in jsonrpc4j
 *
 * def send_reply_fun(reply):
 *   line = json.dumps(reply)
 *   sock.sendall(line.encode('utf-8') + b'\n')
 *
 */


class TransactionPoller {

}
