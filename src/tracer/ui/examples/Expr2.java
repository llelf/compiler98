package nhctracer;
import java.awt.*;
import java.io.*;
import Tree;
import EDTParser;
import Connection;

public class Expr2 extends TraceApplet {

  public void init() {
    super.init();
    node = parser.parseEDTNode(null, null, -1);
  }
}

