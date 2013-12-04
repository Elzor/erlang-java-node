package jnode;

import java.io.StringWriter;
import java.io.PrintWriter;

public class ExceptionUtils {
    static String toString(Throwable e)
    {
        StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw));
        return sw.toString();
    }
}
