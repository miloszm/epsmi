package com.mhm.main

import com.mhm.wallet.DeterministicWallet

case class SpksToMonitorResult(importNeeded: Boolean, spksToMonitor: Seq[String], wallets: Seq[DeterministicWallet])
