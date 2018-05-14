package yarn

import java.net.InetSocketAddress
import java.util.Map.Entry

import org.apache.commons.lang3.reflect.FieldUtils
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.yarn.api.records.{FinalApplicationStatus, Priority, Resource}
import org.apache.hadoop.yarn.client.api.AMRMClient.ContainerRequest
import org.apache.hadoop.yarn.client.api.{AMRMClient, NMClient}
import org.apache.hadoop.yarn.conf.YarnConfiguration
import org.apache.hadoop.yarn.util.Records
import org.apache.hadoop.yarn.util.resource.Resources
import org.apache.log4j.Level
import org.apache.log4j.ConsoleAppender
import org.apache.log4j.Logger
import org.apache.log4j.PatternLayout

/*
yarn jar \
        /usr/local/env/hadoop-2.9.0/share/hadoop/yarn/hadoop-yarn-applications-unmanaged-am-launcher-2.9.0.jar \
        Client \
        -classpath scala-test-0.0.1.jar \
        -cmd "java yarn.Main"
 */

object Main {
  def main(args: Array[String]): Unit = {

    val rootLogger = Logger.getRootLogger
    rootLogger.setLevel(Level.INFO)
    rootLogger.addAppender(new ConsoleAppender(new PatternLayout("%-6r [%p] %c - %m%n")))
    // Initialize clients to ResourceManager and NodeManagers
    val conf = new YarnConfiguration

    //    conf.getSocketAddr(YarnConfiguration.RM_SCHEDULER_ADDRESS, YarnConfiguration.DEFAULT_RM_SCHEDULER_ADDRESS, YarnConfiguration.DEFAULT_RM_SCHEDULER_PORT)
    conf.setSocketAddr(YarnConfiguration.RM_SCHEDULER_ADDRESS, new InetSocketAddress("211.159.173.48", YarnConfiguration.DEFAULT_RM_SCHEDULER_PORT))

    val rmClient: AMRMClient[ContainerRequest] = AMRMClient.createAMRMClient()
    rmClient.init(conf)
    rmClient.start()

    //    println("=========================================")
    //    println(rmClient.getClass)
    //    println(FieldUtils.readField(rmClient, "rmClient", true).getClass)
    //    println(conf)
    //    val iter = conf.iterator()
    //    while (iter.hasNext) {
    //      val entry = iter.next
    //      println(entry.getKey, entry.getValue)
    //    }
    //    println("=========================================")

    val nmClient = NMClient.createNMClient
    nmClient.init(conf)
    nmClient.start()

    // Register with ResourceManager
    println("before registerApplicationMaster")
    rmClient.registerApplicationMaster("", 0, "")
    println("after registerApplicationMaster")

    //    val resource = Records.newRecord(classOf[Resource])
    //    val priority = Records.newRecord(classOf[Priority])
    //    resource.setMemorySize(128)
    //    resource.setVirtualCores(1)
    //    priority.setPriority(0)
    //    val cr = new ContainerRequest(resource, null, null, priority)
    //    rmClient.addContainerRequest(cr)
    //    rmClient.addContainerRequest(cr)
    val sleep = 60
    println(s"Thread Sleep Start, ${sleep}")
    Thread.sleep(sleep * 1000)
    println("Thread Sleep Finish")

    println("before unregisterApplicationMaster")
    rmClient.unregisterApplicationMaster(FinalApplicationStatus.SUCCEEDED, "", "")
    println("after unregisterApplicationMaster")
  }
}
