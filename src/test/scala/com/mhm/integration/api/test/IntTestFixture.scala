package com.mhm.integration.api.test

import com.googlecode.jsonrpc4j.StreamServerWithHeartbeats
import com.mhm.api4electrum.Api4Electrum
import com.mhm.rpcclient.{EpsmiClient, RpcClient}
import com.mhm.rpcserver.RpcServer
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}

trait IntTestFixture extends FlatSpecLike with BeforeAndAfterAll {

  val EXTERNAL_EPS_PORT = 50002
  val EPSMI_PORT = 1420
  val port = EPSMI_PORT

  // -Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks -Djavax.net.ssl.keyStorePassword=123456

  case class Fixture(server: StreamServerWithHeartbeats, epsmiClient: EpsmiClient) {
    def client(): Api4Electrum = epsmiClient.client
  }

  lazy val fixture = Fixture(RpcServer.startServer(port), RpcClient.createClient(port))

  override protected def afterAll(): Unit = {
    fixture.epsmiClient.close()
    fixture.server.stop()
    super.afterAll()
  }

}
