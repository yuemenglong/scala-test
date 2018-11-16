package test1;

import org.apache.ignite.Ignite;
import org.apache.ignite.IgniteCache;
import org.apache.ignite.Ignition;
import org.apache.ignite.configuration.IgniteConfiguration;
import org.apache.ignite.spi.discovery.tcp.TcpDiscoverySpi;
import org.apache.ignite.spi.discovery.tcp.ipfinder.vm.TcpDiscoveryVmIpFinder;

import java.util.Arrays;

public class IgniteTest {
    public static void main(String args[]) {
        Ignite ignite = getIgnite();
        IgniteCache<String, Object> cache = ignite.getOrCreateCache("a");
        cache.clear();
        cache.put("a", "asdf".getBytes());
        byte[] bs = (byte[]) cache.get("a");
        System.out.println(bs.length);
//        System.out.println(cache.size());
//        cache.invoke("a", (e, objs) -> {
//            System.out.println(e);
//            System.out.println(objs);
//            return 0;
//        });
        ignite.close();
    }

    public static Ignite getIgnite() {
        TcpDiscoverySpi spi = new TcpDiscoverySpi();
        TcpDiscoveryVmIpFinder ipFinder = new TcpDiscoveryVmIpFinder();
        ipFinder.setAddresses(Arrays.asList("127.0.0.1:47500..47509"));
        spi.setIpFinder(ipFinder);
        IgniteConfiguration cfg = new IgniteConfiguration();
        cfg.setDiscoverySpi(spi);
        cfg.setClientMode(true);
        Ignite ignite = Ignition.start(cfg);
        return ignite;
    }
}
