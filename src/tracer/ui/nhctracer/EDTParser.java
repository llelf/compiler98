package nhctracer;

import java.awt.*;
import java.util.Vector;

public class EDTParser {
    Connection conn;
    NodeTable nodeTable;

    public EDTParser(Connection conn, NodeTable nodeTable) {
        this.nodeTable = nodeTable;
	this.conn = conn;
    }

    public Output parseOutput() {
        Output output = new Output();
        int chars = parseNr();
	for (int i = 0; i < chars; i++) {
	    output.addChar((char)parseNr(), parseNr());
	}
	return output;
    }

    public Trace parseTrace(TraceTree ptree, int expectrefnr) {
        Trace trace = new Trace(ptree);
	TraceTree tree = parseTraceTree(trace);
	/*  if (expectrefnr >= 0 && tree.node.refnr != expectrefnr) {
	      System.err.println(
		"Expecting "+expectrefnr+
		" but got "+tree.node.refnr);
	    }
	*/
	return trace;
    }

    public TraceTree parseTraceTree(Trace parent) {
        TraceTree tree = new TraceTree(parent);
	EDTNode node = parseEDTNode((EDTStructuredNode)null, tree, -1);	    
	if (node instanceof Hidden) {
	    conn.out.println("Gn 5");
	    conn.out.println(""+node.trefnr);
	    node = parseEDTNode((EDTStructuredNode)null, tree, -1);	    
	}
	tree.setNode(node);
	return tree;
    }

    public void expect(int ch) {
      String t = conn.nextToken();
      if (t.charAt(0) != ch) {
	System.err.println("expect: expected '" + (char)ch + "', but got " + t);
	System.err.println("input: " + conn.rest());
	System.exit(-1);
      }
    }

    public Vector parseRedexes(EDTStructuredNode parent) {
	Vector result = new Vector(3, 10);
	int index = 0;	
	expect('(');
	String t = conn.nextToken();
	while (t.charAt(0) != ')') {
	    conn.pushBack(t);
	    result.addElement(parseEDTNode(parent, parent.tree, index++));
	    t = conn.nextToken();
	}
	return result;
    }

    public String parseIdent() {
	String t = conn.nextToken();
	return t;
    }

    public SourceRef parseSR() {
	String modname = conn.nextToken();
	switch (modname.charAt(0)) {
	    case '*':
		return new SourceRef((String)null, new Integer(0));
	    default:
		String rcs = conn.nextToken();
		try {
	    	    Integer rc = new Integer(rcs);
	    	    return new SourceRef(modname, rc);
		} catch (NumberFormatException e) {
	    	    System.err.println("parseSR: expected a number, got " + 
					rcs + "\n" + e);
		    System.err.println("input: " + conn.rest());
	    	    System.exit(-1);
		} catch (StringIndexOutOfBoundsException e) {
	    	    System.err.println("parseSR: bad string index\n" + e);
	    	    System.exit(-1);
		}
		break;
	}
	return null;
    }

    public EDTNode parseEDTNode(EDTStructuredNode parent, TraceTree tree, int index) {
      SourceRef srcref;
      int refnr, trefnr, irefnr, drefnr;
      String t = conn.nextToken();
      switch (t.charAt(0)) {
      case '(':
	// Start of a redex or a producer
	EDTNode result = null;
	Trace trace;
	t = conn.nextToken();
	switch (t.charAt(0)) {
	case 'N':
	    refnr = parseNr();
	    srcref = parseSR();
	    String module = parseIdent();
	    String constr = parseIdent();
	    int isIdentifier = parseNr();
	    int defpos = parseNr();
	    int pri = parseNr();
	    result = new IdName(parent, tree, index, srcref, 
				refnr, module, constr, isIdentifier, defpos, pri);
	    trefnr = parseNr();
	    irefnr = parseNr();
	    drefnr = parseNr();
	    result.setTRefNr(trefnr, irefnr, drefnr);
	    nodeTable.setNodeAt(result, refnr);
	    break;
	case 'A':
	    boolean isString = t.length() > 1 && t.charAt(1) == 's'; 
	    refnr = parseNr();
	    srcref = parseSR();
	    if (isString)
	      result = new CharList(parent, tree, index, srcref, refnr);
	    else
	      result = new Redex(parent, tree, index, srcref, refnr);
	    nodeTable.setNodeAt(result, refnr);
	    ((EDTStructuredNode)result).setArgs(parseRedexes((EDTStructuredNode)result));
	    trefnr = parseNr();
	    irefnr = parseNr();
	    drefnr = parseNr();
	    result.setTRefNr(trefnr, irefnr, drefnr);
	    break;
	case 'R':
	    refnr = parseNr();
	    irefnr = parseNr();
	    drefnr = parseNr();
	    EDTNode n = nodeTable.nodeAt(refnr);
	    // Check if the referenced node is built
	    if (n instanceof Redex && ((Redex)n).args.size() == 0)
	      // No, just build a place holder
	      result = new CutOffTree(parent, tree, index, refnr);
	    else
	      result = n.spawn(parent, tree, index, irefnr, drefnr, nodeTable);
	    break;
	case 'D':
	    refnr = parseNr();
	    result = new CutOffTree(parent, tree, index, refnr);
	    break;
	case 'C':
	    int caseifgd = parseNr();
	    refnr = parseNr();
	    srcref = parseSR();
	    result = new Case(parent, tree, index, srcref, refnr,
	                      caseifgd==0?"if ":caseifgd==1?"case ":"| ");
	    EDTNode e = parseEDTNode((EDTStructuredNode)result, tree, index);
	    EDTNode ctxt = parseEDTNode((EDTStructuredNode)result, tree, index);
	    irefnr = parseNr();
	    drefnr = parseNr();
	    ((Case)result).setArgs(e, ctxt, ctxt.refnr, irefnr, drefnr);
	    nodeTable.setNodeAt(result, refnr);
	    break;
	case 'B':
	    refnr = parseNr();	      
	    trefnr = parseNr();
	    drefnr = parseNr();
	    result = new Bottom(parent, tree, index, refnr, trefnr, drefnr);
	    nodeTable.setNodeAt(result, refnr);
	    break;
	case 'H':
	    refnr = parseNr();	      
	    trefnr = parseNr();
	    result = new Hidden(parent, tree, index, refnr, trefnr);
	    nodeTable.setNodeAt(result, refnr);
	    break;
	}
	expect(')');
	return result;
      case 'P':
	  return new Pruned(parent, tree, index);
      case '_': // The root node
	  System.err.println("ParseEDT: hmm, Root...");
	  System.exit(-1);
      default:
	  System.err.println("ParseEDT: unexpected character: " + t);
	  System.exit(-1);
      }
      return null;
    }

    public int parseNr() {
      String rcs = conn.nextToken();
      try {
	  Integer refnr = new Integer(rcs);
	  return refnr.intValue();
      } catch (NumberFormatException e) {
	  System.err.println("parseNR: expected a number, got " + 
			     rcs + "\n" + e);
	  System.err.println("input: " + conn.rest());
	  System.exit(-1);
      } catch (StringIndexOutOfBoundsException e) {
	  System.err.println("parseNR: expected a number, got " + 
			     rcs + "\n" + e);
		    System.err.println("input: " + conn.rest());
	  System.exit(-1);
      }
      return -1;
    }

    static boolean isCons(EDTStructuredNode node) {
        if (node != null && !node.args.isEmpty()) {
	    EDTNode id = (EDTNode)node.args.elementAt(0);
	    if (id instanceof IdName &&
		((IdName)id).name.equals(":"))
	        return true;
	}
	return false;
    }

}

