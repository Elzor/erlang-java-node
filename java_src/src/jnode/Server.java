package jnode;

import com.ericsson.otp.erlang.*;
import org.apache.log4j.Logger;

public class Server {
    private String erlangCookie = "web";
    private String nodeName     = "jnode";
    private String mboxName     = "jnodebox";
    private int pingTimeout     = 10000;
    private int erlangPort      = 15000;
    private Logger  logger = Logger.getLogger(Constant.LOGGER);
    private OtpNode node;
    private OtpMbox mbox;
    private OtpErlangPid controllerPid;

    final OtpErlangAtom atom_ok         = new OtpErlangAtom("ok");
    final OtpErlangAtom atom_pong       = new OtpErlangAtom("pong");
    final OtpErlangAtom atom_error      = new OtpErlangAtom("error");
    final OtpErlangAtom atom_java_error = new OtpErlangAtom("java_error");
    final OtpErlangAtom atom_invreq     = new OtpErlangAtom("invalid_request");

    public Server(String nodeName, String mboxName, String erlangCookie, int erlangPort) {
        this.nodeName     = nodeName;
        this.mboxName     = mboxName;
        this.erlangCookie = erlangCookie;
        this.erlangPort   = erlangPort;
        logger.info("server constructed");
    }

    public void run() throws Exception {
        if (node != null) {
            throw new IllegalStateException("Server.node");
        }
        if (mbox != null) {
            throw new IllegalStateException("Server.mbox");
        }
        node = new OtpNode(nodeName,erlangCookie,erlangPort);
        mbox = node.createMbox(mboxName);
        logger.info("mbox created");
        // make sure erlang node is alive
        checkErlangNode(mbox, pingTimeout);
        while (handleNodeMessages()) {
            Thread.sleep(50);
        }
        mbox.close();
        node.close();
    }

    boolean handleNodeMessages() {
        try {
            OtpErlangObject msg = mbox.receive(50);
            if (!(msg instanceof OtpErlangTuple)) {
                return true;
            }
            logger.info("handleNodeMessages: received: " + msg.toString());
            OtpErlangTuple t = (OtpErlangTuple) msg;
            OtpErlangAtom Cmd = (OtpErlangAtom) t.elementAt(0);
            switch(Cmd.atomValue()) {
                case "ping":
                    handlePing(t);
                    break;
                default:
                    break;
            }
            return true;
        }
        catch (OtpErlangExit e) {
            logger.error("remote pid is down, exiting");
            return false;
        }
        catch (Exception e) {
            logger.error(ExceptionUtils.toString(e));
            return false;
        }
    }

    void handlePing(OtpErlangTuple t) throws OtpErlangExit {
        OtpErlangPid pid = (OtpErlangPid) t.elementAt(1);
        controllerPid    = pid;
        mbox.send(pid, new OtpErlangTuple(new OtpErlangObject[]{atom_pong, mbox.self()}));
        mbox.link(pid);
        logger.info("handle ping from " + pid);
    }

    private void checkErlangNode(OtpMbox erlangMbox, Integer pingTimeout) throws Exception
    {
        OtpErlangObject msgObj = erlangMbox.receive(pingTimeout);
        logger.info("checkErlangNode: received: " + msgObj.toString());
        OtpErlangTuple  msgTuple = (OtpErlangTuple)msgObj;
        OtpErlangAtom   cmd = (OtpErlangAtom)msgTuple.elementAt(0);
        if (!cmd.atomValue().equals("ping"))
        {
            String text = "First message should be ping!";
            logger.warn(text);
            throw new Exception(text);
        }
        OtpErlangPid pid = (OtpErlangPid)msgTuple.elementAt(1);
        mbox.link(pid);
        logger.info("linked with erlang pid " + pid);
        return;
    }
}
