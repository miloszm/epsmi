package com.mhm.util

sealed trait MerkleNode {
  def isTuple: Boolean
  def value: String
}

case class StrMerkleNode(str: String) extends MerkleNode {
  override def isTuple = false
  override def value = str
  override def toString: String = s""""$str""""
}

case class TupleMerkleNode(left: MerkleNode, right: MerkleNode) extends MerkleNode {
  override def isTuple = true
  override def value = ""
  override def toString: String = s"($left, $right)"
}
