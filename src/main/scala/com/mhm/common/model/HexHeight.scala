package com.mhm.common.model

case class HexHeight(hex: String, height: Int) {
  def asJson: String = s"""{"hex": "$hex", "height": $height}"""
}
