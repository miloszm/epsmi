/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.wallet

import java.util.concurrent.atomic.AtomicReference

import com.mhm.bitcoin.{NoopWalletStateListener, WalletStateListener}
import com.mhm.connectors.RpcWrap.wrap
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{DeriveAddressesResult, ValidateAddressResult}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{DescriptorRpc, UtilRpc}
import scodec.bits.{ByteVector, HexStringSyntax}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class XpubDescTemplPair(xpub: String, descTempl: String)

case class AddrsSpksPair(addrs: Seq[String], spks: Seq[String])

case class ChangeIndexPair(change: Int, index: Int) {
  override def toString: String = s"(change=$change, index=$index)"
}

abstract class DeterministicWallet(gapLimit: Int,
                                   val walletName: String,
                                   walletStateListener: WalletStateListener = NoopWalletStateListener)
    extends Logging {

  val currentState =
    new AtomicReference[DeterministicWalletState](DeterministicWalletState.createEmpty())
  walletStateListener.updated(currentState.get)
  def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String]

  def getAddresses(rpcCli: DescriptorRpc with UtilRpc, change: Int, fromIndex: Int, count: Int): AddrsSpksPair = {
    val addresses = deriveAddresses(rpcCli, change, fromIndex, count)
    val spks = addresses.map { addr =>
      val valAddr: ValidateAddressResult =
        wrap(rpcCli.validateAddress(BitcoinAddress(addr)))
      val spk =
        valAddr.scriptPubKey.getOrElse(throw new IllegalArgumentException("missing script pub key"))

      /**
        * TODO NOTE for some reason spk hex has "19" prefix which we need to remove
        * I am not sure why eps does not have this problem, as it uses validate address as well
        */
      spk.hex.drop(2)
    }
    @tailrec
    def fillOutSpkIndex(spks: Seq[String],
                        spki: Map[String, ChangeIndexPair],
                        index: Int): Map[String, ChangeIndexPair] =
      index match {
        case i if i < spks.size =>
          val pair = ChangeIndexPair(change, fromIndex + i)
          fillOutSpkIndex(spks, (spki - spks(i)) + (spks(i) -> pair), i + 1)
        case _ => spki
      }
    val newScriptPubKeyIndex =
      fillOutSpkIndex(spks, currentState.get.scriptPubKeyIndex, 0)
    val newNextIndex = (currentState.get.nextIndex - change) + (change -> Math
      .max(currentState.get.nextIndex.getOrElse(change, 0), fromIndex + count))
    walletStateListener.updated(
      currentState.updateAndGet(_.copy(nextIndex = newNextIndex, scriptPubKeyIndex = newScriptPubKeyIndex))
    )
    val result = AddrsSpksPair(addresses, spks)
    walletStateListener.setAddressSpkMap(result)
    result
  }

  def getNewAddresses(rpcCli: DescriptorRpc with UtilRpc, change: Int, count: Int): AddrsSpksPair = {
    getAddresses(rpcCli, change, currentState.get.nextIndex.getOrElse(change, 0), count)
  }

  def rewindOne(change: Int): Unit = {
    val newEntry     = change -> (currentState.get.nextIndex.getOrElse(change, 1) - 1)
    val newNextIndex = (currentState.get.nextIndex - change) + newEntry
    walletStateListener.updated(currentState.updateAndGet(_.copy(nextIndex = newNextIndex)))
  }

  def findFirstNotImported(rpcCli: DescriptorRpc with UtilRpc,
                           change: Int,
                           importedAddresses: Set[BitcoinAddress]): String = {
    @tailrec
    def go(): String = {
      val pair = getNewAddresses(rpcCli, change, 1)
      if (importedAddresses.map(_.value).contains(pair.addrs.head)) go()
      else {
        pair.spks.head
      }
    }
    go()
  }

  /**
    * called when a new tx of ours arrives
    * to see if we need to import more addresses
    */
  def haveScriptpubkeysOverrunGaplimit(scriptpubkeys: Seq[String]): Map[Int, Int] = {
    case class CAPair(change: Int, addressesNeeded: Int)
    val expanded = scriptpubkeys.flatMap { spk =>
      currentState.get.scriptPubKeyIndex.get(spk).flatMap {
        case ChangeIndexPair(change, index) =>
          val distanceFromNext = currentState.get.nextIndex.getOrElse(change, 0) - index
          if (distanceFromNext <= this.gapLimit)
            Some(CAPair(change, this.gapLimit - distanceFromNext + 1))
          else None
      }
    }
    val reduced =
      expanded.groupBy(_.change).view.mapValues(_.map(_.addressesNeeded).max)
    reduced.toMap
  }
}

abstract class DescriptorDeterministicWallet(xpubVbytes: ByteVector,
                                             args: XpubDescTemplPair,
                                             gapLimit: Int,
                                             walletName: String,
                                             walletStateListener: WalletStateListener = NoopWalletStateListener)
    extends DeterministicWallet(gapLimit, walletName, walletStateListener) {
  def obtainDescriptorsWithoutChecksum(args: XpubDescTemplPair): Seq[String]

  def obtainDescriptors(rpcCli: DescriptorRpc): Future[Seq[String]] = {
    val dwc = obtainDescriptorsWithoutChecksum(args)
    Future.sequence(dwc.map { d =>
      logger.info(s"getting descriptor info for: $d")
      for {
        descriptorInfo <- rpcCli.getDescriptorInfo(d)
      } yield {
        logger.info(s"descriptor=${descriptorInfo.descriptor}")
        descriptorInfo.descriptor
      }
    })
  }
  val descriptors: List[String] = Nil

  def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String] = {
    val range: Vector[Double]         = Vector(fromIndex, fromIndex + count - 1)
    val result: DeriveAddressesResult = wrap(rpcCli.deriveAddresses(descriptors(change), Some(range)))
    result.addresses.map(_.value)
  }
}

class SingleSigWallet(rpcCli: DescriptorRpc,
                      xpubVbytes: ByteVector,
                      args: XpubDescTemplPair,
                      gapLimit: Int,
                      walletName: String,
                      walletStateListener: WalletStateListener = NoopWalletStateListener)
    extends DescriptorDeterministicWallet(xpubVbytes, args, gapLimit, walletName, walletStateListener) {
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTemplPair): Seq[String] = {
    val xpub = WalletOps.convertToStandardXpub(args.xpub, xpubVbytes)
    val descriptorsWithoutChecksum = (0 to 1).map { change =>
      val s = args.descTempl.replaceFirst("\\{change\\}", change.toString)
      s.replaceFirst("\\{xpub\\}", xpub)
    }
    descriptorsWithoutChecksum
  }
  override val descriptors = wrap(this.obtainDescriptors(rpcCli)).toList
  override def toString: String = {
    s"SingleSigWallet(xpubVbytes=${xpubVbytes.toHex} xpub=${args.xpub} " +
      s"descTemp=${args.descTempl} gapLimit=$gapLimit descriptors=${descriptors.mkString("|")})"
  }
}

class MultisigWallet(xpubVbytes: ByteVector, args: XpubDescTemplPair, walletName: String)
    extends DescriptorDeterministicWallet(xpubVbytes, args, gapLimit = 0, walletName) {
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTemplPair): Seq[String] = ???
}

class SingleSigOldMnemonicWallet extends DeterministicWallet(gapLimit = 0, walletName = "") {
  override def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String] = ???
}

object DeterministicWallet {

  def parseElectrumMasterPublicKey(
    rpcCli: DescriptorRpc,
    keyData: String,
    gapLimit: Int,
    chain: String,
    walletName: String,
    walletStateListener: WalletStateListener = NoopWalletStateListener
  ): DescriptorDeterministicWallet = {
    val xpubVBytes: ByteVector =
      if (chain == "main") hex"0488b21e"
      else if (chain == "test" || chain == "regtest") hex"043587cf"
      else throw new IllegalStateException("unrecognized bitcoin chain")

    val descriptorTemplateOpt: Option[String] =
      if (Set("xpub", "tpub").contains(keyData.take(4)))
        Some("pkh({xpub}/{change}/*)")
      else if (Set("zpub", "vpub").contains(keyData.take(4)))
        Some("wpkh({xpub}/{change}/*)")
      else if (Set("ypub", "upub").contains(keyData.take(4)))
        Some("sh(wpkh({xpub}/{change}/*))")
      else None

    if (keyData.contains(" "))
      throw new IllegalArgumentException("multisig not implemented yet")

    val wallet = descriptorTemplateOpt match {
      case Some(descriptorTemplate) =>
        new SingleSigWallet(
          rpcCli,
          xpubVBytes,
          XpubDescTemplPair(keyData, descriptorTemplate),
          gapLimit,
          walletName,
          walletStateListener
        )
      case None =>
        throw new IllegalArgumentException("SingleSigOldMnemonicWallet not implemented yet")
    }
    wallet
  }
}
