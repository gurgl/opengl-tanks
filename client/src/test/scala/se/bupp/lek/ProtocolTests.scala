
package se.bupp.lek
import client.VisualWorldSimulation.{SpawnPlayers, KillPlayers}
import client.{NetworkGameState, VisualWorldSimulation}
import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import common.Model.{PlayerGO, KillPlayer, SpawnPlayer, ServerGameWorld}
import java.util

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-09-28
 * Time: 07:16
 * To change this template use File | Settings | File Templates.
 */
class ProtocolTests extends Specification with Mockito {

  "protocol should" should {

    "handle conversions2" in {
      System.getProperty("bupp") match {
        case s:String => failure("not set")
        case _ => success
      }
      System.setProperty("bapp","tja")
      System.getProperty("bapp") match {
        case s:String => success
        case _ => failure("it is set")
      }
    }
    "handle conversions" in {
      var ngs = new NetworkGameState(null)
      val s1 = new ServerGameWorld()
      val s2 = new ServerGameWorld()


      // new SpawnPlayer(1,"asdf",1)

      import scala.collection.JavaConversions.seqAsJavaList
      s1.stateChanges = new java.util.ArrayList(List(new KillPlayer(1), new SpawnPlayer(2,"asdf",2)))
      s2.stateChanges = new java.util.ArrayList(List(new KillPlayer(2), new KillPlayer(1)))

      var p2: PlayerGO = new PlayerGO()
      p2.playerId = 2
      s1.alivePlayers = new java.util.ArrayList(Seq(p2))
      val res = ngs.convertStateChanges(Seq(s1, s2))

      res.size should be equalTo 3

      res.apply(0) should be equalTo KillPlayers(List(1))

      //res.apply(1) should be equalTo SpawnPlayers(List((SpawnPlayer(2,"asdf",2),p2)))
      res.apply(2) should be equalTo KillPlayers(List(2,1))


    }
  }
}
