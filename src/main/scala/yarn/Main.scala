package yarn

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.yarn.api.records.{FinalApplicationStatus, Priority, Resource}
import org.apache.hadoop.yarn.client.api.AMRMClient.ContainerRequest
import org.apache.hadoop.yarn.client.api.{AMRMClient, NMClient}
import org.apache.hadoop.yarn.conf.YarnConfiguration
import org.apache.hadoop.yarn.util.Records
import org.apache.hadoop.yarn.util.resource.Resources

/*
yarn jar \
        /usr/local/env/hadoop-2.9.0/share/hadoop/yarn/hadoop-yarn-applications-unmanaged-am-launcher-2.9.0.jar \
        Client \
        -classpath scala-test-0.0.1.jar \
        -cmd "java yarn.Main"
 */

object Main {
  def main(args: Array[String]): Unit = {
    // Initialize clients to ResourceManager and NodeManagers
    val conf = new YarnConfiguration

    val rmClient: AMRMClient[ContainerRequest] = AMRMClient.createAMRMClient()
    rmClient.init(conf)
    rmClient.start()

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
