package jnode;

import java.io.FileInputStream;
import java.io.OutputStreamWriter;

import java.util.Properties;
import org.apache.log4j.Logger;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.FileAppender;
import org.apache.log4j.PropertyConfigurator;

public class Main {
    private static final String NODE_NAME  = "nodename";
    private static final String MBOX_NAME  = "mboxname";
    private static final String COOKIE     = "cookie";
    private static final String NODE_PORT  = "nodeport";
    private static final String CONFIGFILE = "config";
    private static Logger logger           = Logger.getLogger(Constant.LOGGER);

    private static void initLogging(String file)
    {
        PropertyConfigurator.configure(file);
    }

    public static void main(String[] args) {
        try {
            String nodeName     = System.getProperty(NODE_NAME);
            String mboxName     = System.getProperty(MBOX_NAME);
            String erlangCookie = System.getProperty(COOKIE);
            int erlangPort      = Integer.parseInt(System.getProperty(NODE_PORT));
            String Config       = System.getProperty(CONFIGFILE);
            initLogging(Config);
            logger.info("main started with params: \n\tnodename: "+nodeName+
                                                  "\n\tmbox: "+mboxName+
                                                  "\n\tcookie: "+erlangCookie+
                                                  "\n\tepmd_port: "+erlangPort
                    );
            Server srv = new Server(nodeName, mboxName, erlangCookie, erlangPort);
            srv.run();
            logger.info("main normal stopped");
            System.exit(0);
        }
        catch (Exception e) {
            try {
                logger.error(ExceptionUtils.toString(e));
            }
            catch (Exception inner) {
                inner.printStackTrace();
            }
            e.printStackTrace();
            logger.error("main stoped with error");
            System.exit(1);
        }
    }
}
