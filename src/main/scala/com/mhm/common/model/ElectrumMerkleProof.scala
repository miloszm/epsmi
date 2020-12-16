package com.mhm.common.model

case class ElectrumMerkleProof(
  pos: Int,
  merkle: Array[String],
  txId: String,
  merkleRoot: String
)
