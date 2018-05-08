package yarn

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.yarn.api.records.FinalApplicationStatus
import org.apache.hadoop.yarn.client.api.{AMRMClient, NMClient}
import org.apache.hadoop.yarn.conf.YarnConfiguration

object Main {
  def main(args: Array[String]): Unit = {
    // Initialize clients to ResourceManager and NodeManagers
    val conf = new YarnConfiguration

    val rmClient = AMRMClient.createAMRMClient
    rmClient.init(conf)
    rmClient.start()

    val nmClient = NMClient.createNMClient
    nmClient.init(conf)
    nmClient.start()

    // Register with ResourceManager
    System.out.println("before registerApplicationMaster")
    rmClient.registerApplicationMaster("", 0, "")
    System.out.println("after registerApplicationMaster")

    System.out.println("before unregisterApplicationMaster")
    rmClient.unregisterApplicationMaster(FinalApplicationStatus.SUCCEEDED, "", "")
    System.out.println("after unregisterApplicationMaster")
  }
}
