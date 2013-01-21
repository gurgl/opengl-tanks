package se.bupp.lek

import org.specs2.mutable.Specification
import server.Server

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2013-01-21
 * Time: 02:45
 * To change this template use File | Settings | File Templates.
 */
class CommandLineTest extends Specification {

  "lol" should {
    "lal" in {

      val cmd = "--tcp-port 54556 --udp-port 54778 --master-host localhost --master-port 1199 --occassion-id 2 --log logs/srv_97292179_2.log".split(" ")
      val settings = Server.handleCommandLine(cmd)

      settings.ports.tcpPort === 54556
    }
  }
}