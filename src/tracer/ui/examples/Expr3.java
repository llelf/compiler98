package nhctracer;
import java.awt.*;
import java.io.*;
import Tree;
import EDTParser;
import Connection;

public class Expr3 extends TraceApplet {

  public void init() {
    super.init();
    trace = parser.parseTrace(null);
    parser.parseTraceTree(trace);
    trace.color = Color.black;
  }
}

