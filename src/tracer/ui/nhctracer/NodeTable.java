package nhctracer;

import java.awt.*;
import java.util.Vector;

public class NodeTable {
  Vector nt;
  int currentSize = 0;  
  Vector delayedRefs, delayedRedexes;

  public NodeTable() {
    nt = new Vector(50, 1000);
    nt.addElement(null); 
  }

  public void initDelayedRefs() {
    delayedRefs = new Vector();
    delayedRedexes = new Vector();
  }

  public void performDelayedSpawn() {
    for (int i = 0; i < delayedRefs.size(); i++) {
      EDTStructuredNode copy = (EDTStructuredNode)delayedRefs.elementAt(i);
      EDTStructuredNode org = (EDTStructuredNode)delayedRedexes.elementAt(i);
      //org.spawn2(copy);
    }
  }

  public void delaySpawn(EDTStructuredNode org, EDTStructuredNode copy) {
    delayedRefs.addElement(copy);
    delayedRedexes.addElement(org);
  }

  public void setNodeAt(EDTNode obj, int refnr) {
    try {
      if (refnr > currentSize) {
	for (int i = currentSize+1; i <= refnr; i++)
	  nt.addElement(null); 
	currentSize = refnr;
      }
      nt.setElementAt(obj, refnr);
    } catch (ArrayIndexOutOfBoundsException e) {
      System.err.println("NodeTable.setNodeAt: bad node index\n" + e);
      System.exit(-1);
    }      
  }

  public EDTNode nodeAt(int refnr) {
    try {
      return (EDTNode)nt.elementAt(refnr);
    } catch (ArrayIndexOutOfBoundsException e) {
      System.err.println("NodeTable.nodeAt: bad node index\n" + e);
      System.exit(-1);
    }        
    return null;
  }
}


