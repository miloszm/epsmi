package com.mhm.bitcoin

import spray.json.DefaultJsonProtocol

object JsonFormats  {
  import DefaultJsonProtocol._

  implicit val blockchainInfoJsonFormat = jsonFormat1(BlockchainInfo)
  implicit val bestBlockHashJsonFormat = jsonFormat1(BestBlockHash)
}

