package test.luxoft

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import test.luxoft.SensorReportActor.GetReport
import test.luxoft.models.{SensorData, SensorReport, SensorStats}


class SensorReportActorSpec()
  extends TestKit(ActorSystem("SensorReportActorSpec"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "SensorReportActor" must {

    "accept new data" in {
      val sensorReportActor = system.actorOf(Props[SensorReportActor])
      sensorReportActor ! SensorData("s1", Some(1))
      expectMsg(())
    }

    "return empty initial map" in {
      val sensorReportActor = system.actorOf(Props[SensorReportActor])
      sensorReportActor ! GetReport
      expectMsg(Map.empty[String, SensorReport])
    }

    "return map with results" in {
      val sensorReportActor = system.actorOf(Props[SensorReportActor])
      sensorReportActor ! SensorData("s1", Some(1))
      expectMsg(())
      sensorReportActor ! SensorData("s1", Some(3))
      expectMsg(())
      sensorReportActor ! GetReport
      expectMsg(Map("s1" -> SensorReport(Some(SensorStats(1, 2, 3, 2)), 0)))
    }
  }
}