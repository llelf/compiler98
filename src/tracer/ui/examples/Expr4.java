package nhctracer;
import java.awt.*;
import java.io.*;
import Tree;
import EDTParser;
import Connection;

public class Expr4 extends TraceApplet {
  public void init() {
    super.init();
    trace = parser.parseTrace(null);
    trace.color = Color.black;
    EDTNode selectedNode = nt.nodeAt(14);
    Trace t = parser.parseTrace(selectedNode.tree);
    selectedNode.setTrace(t);
    selectedNode.tree.addTrace(t);
  }
}

