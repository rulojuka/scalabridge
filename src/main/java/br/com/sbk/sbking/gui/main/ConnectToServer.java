package br.com.sbk.sbking.gui.main;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import br.com.sbk.sbking.networking.client.SBKingClient;
import br.com.sbk.sbking.networking.client.SBKingClientFactory;
import br.com.sbk.sbking.networking.core.properties.FileProperties;
import br.com.sbk.sbking.networking.core.properties.NetworkingProperties;
import br.com.sbk.sbking.networking.core.properties.SystemProperties;
import br.com.sbk.sbking.networking.rest.RestHTTPClient;

public final class ConnectToServer {

    private ConnectToServer() {
        throw new IllegalStateException("Utility class");
    }

    public static SBKingClient connectToServer() {
        NetworkingProperties networkingProperties = new NetworkingProperties(new FileProperties(),
                new SystemProperties());
        String hostname = networkingProperties.getHost();
        int port = networkingProperties.getPort();
        if (isValidIP(hostname)) {
            RestHTTPClient restHTTPClient = new RestHTTPClient(hostname);
            return SBKingClientFactory.createWithKryonetConnection(hostname, port, restHTTPClient);
        } else {
            throw new InvalidIpException();
        }
    }

    private static boolean isValidIP(String ipAddr) {
        Pattern ptn = Pattern.compile("^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$");
        Matcher mtch = ptn.matcher(ipAddr);
        return mtch.find();
    }

}
