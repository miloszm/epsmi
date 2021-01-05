package com.mhm.common.model

case class HashHeight(hash: String, height: Int){
  def asJson: String = s"""{"hex": "$hash", "height": $height}"""
}
