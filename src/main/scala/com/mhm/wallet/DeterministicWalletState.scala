package com.mhm.wallet

case class DeterministicWalletState(scriptPubKeyIndex: Map[String, ChangeIndexPair], nextIndex: Map[Int, Int])

object DeterministicWalletState {
  def createEmpty(): DeterministicWalletState = {
    DeterministicWalletState(Map(), Map())
  }
}
