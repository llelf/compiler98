package nhctracer;
import java.awt.*;
import java.io.*;
import Tree;
import EDTParser;
import Connection;

public class Expr1 extends TraceApplet {

  public void init() {
    super.init();
    trace = parser.parseTrace(null);
    trace.color = Color.black;
  }
}

