package nhctracer;

import java.io.*;
import java.util.*;

public class Script implements Cloneable {
  String name;
  static Hashtable parseTable, classTable;

  static void registerItemTypes() {
    parseTable = new Hashtable();
    classTable = new Hashtable();
    new Events.NodeSelect().registerItemType("nodeselect");
    new Events.NodeTrail().registerItemType("nodetrail");
    new Events.NodeExpand().registerItemType("nodeexpand");
    new Events.NodeSourceRef().registerItemType("nodesourceref");
    new Events.NodeDefRef().registerItemType("nodedefref");
    new Events.TraceSelect().registerItemType("traceselect");
    new Events.TraceTrail().registerItemType("tracetrail");
    new Events.OutputSelect().registerItemType("outputselect");
    new Events.OutputTrail().registerItemType("outputtrail");
    new Events.MetaPause().registerItemType("pause");
    new Events.MetaAlarm().registerItemType("alarm");
    new Events.MetaMessage().registerItemType("message");
    new Events.MetaConnect().registerItemType("connect");
    new Events.MetaDisconnect().registerItemType("disconnect");
    new Events.MetaNoWait().registerItemType("nowait");
    new Events.MetaAutoStart().registerItemType("autostart");
    new Events.MetaAutoQuit().registerItemType("autoquit");
  }

  protected void registerItemType(String name) {
    this.name = name;
    Script.parseTable.put(name, this);
    Script.classTable.put(this.getClass(), name);
  }

  static Script parse(StreamTokenizer st) throws IOException, ScriptException {
    Script script;
    if (st.ttype == StreamTokenizer.TT_WORD && 
	(script = (Script)Script.parseTable.get(st.sval)) != null) {
	try {
	  return ((Script)script.clone()).parseArgs(st);
	} catch (CloneNotSupportedException e) {
	  return null; // Cannot happen, but have to catch the exception
	}
    } else {
      throw new ScriptException("Script parse error: bad item type: " + st);
    }
  }

  static Vector parseReader(Reader reader) throws ScriptException {
    StreamTokenizer st = new StreamTokenizer(reader);
    st.slashSlashComments(false);
    st.slashStarComments(true);
    st.commentChar('#');
    st.quoteChar('"');
    st.lowerCaseMode(true);
    st.parseNumbers();
    st.eolIsSignificant(false);
    Vector scriptv = new Vector();
    try {
      while (st.nextToken() != StreamTokenizer.TT_EOF) {
	Script script = parse(st);
	scriptv.addElement(script);
      }
    } catch (IOException e) {
      throw new ScriptException(""+e);
    }
    return scriptv;
  }

  Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
    return this;
  }

  public Script() {
    name = (String)Script.classTable.get(this.getClass());
  }

  static boolean expect(StreamTokenizer st, char type) throws IOException, ScriptException { 
    if (checkType(st, type))
      return true;
    else {
      st.nextToken();
      throw new ScriptException("Expected '" + type + "', got " + st);
    }
  }
  
  static boolean checkType(StreamTokenizer st, char type) throws IOException, ScriptException {
    st.nextToken();
    if (st.ttype == type)
      return true;
    else {
      st.pushBack();
      return false;
    }
  }

  boolean checkWord(StreamTokenizer st, String token) throws IOException, ScriptException {
    st.nextToken();
    if ((st.ttype == StreamTokenizer.TT_WORD) && (st.sval.equals(token)))
      return true;
    else {
      st.pushBack();
      return false;
    }
  }

  static int getNumber(StreamTokenizer st) throws IOException, ScriptException { 
    st.nextToken();
    if (st.ttype == StreamTokenizer.TT_NUMBER)
      return (int)Math.round(st.nval);
    else
      throw new ScriptException("Expected a number, got " + st);
  }

  static String getString(StreamTokenizer st) throws IOException, ScriptException { 
    st.nextToken();
    if (st.ttype == '"')
      return st.sval;
    else
      throw new ScriptException("Expected a string, got " + st);
  }

  static Vector pathParser(StreamTokenizer st) throws IOException, ScriptException {
    Vector p = new Vector();
    expect(st, '{');
    if (checkType(st, '}'))
      return p;
    p.addElement(new Integer(getNumber(st)));
    while (checkType(st, ',')) {
      p.addElement(new Integer(getNumber(st)));
    }
    expect(st, '}');
    return p;
  }

  static String showPath(Vector p) {
    String s = "{";
    String mid = null;
    for (int i = 0; i < p.size(); i++) {
      if (mid == null)
	mid = ", ";
      else
	s += mid;
      s += p.elementAt(i);
    }
    return s + "}";    
  }

  public static void main(String[] args) throws IOException {
    registerItemTypes();
    try {
      parseReader(new InputStreamReader(System.in));
    } catch (ScriptException e) {
      System.err.println(""+e);
    }
  }

  public String toString() {
    return name;
  }
}


