package nhctracer;

import java.io.*;
import java.util.*;

public class Script implements Cloneable {
  String name;
  static Hashtable parseTable, classTable;

  static void registerItemTypes() {
    parseTable = new Hashtable();
    classTable = new Hashtable();
    new ScriptNodeSelect().registerItemType("nodeselect");
    new ScriptNodeTrail().registerItemType("nodetrail");
    new ScriptNodeExpand().registerItemType("nodeexpand");
    new ScriptNodeSourceRef().registerItemType("nodesourceref");
    new ScriptNodeDefRef().registerItemType("nodedefref");
    new ScriptTraceSelect().registerItemType("traceselect");
    new ScriptTraceTrail().registerItemType("tracetrail");
    new ScriptOutputSelect().registerItemType("outputselect");
    new ScriptOutputTrail().registerItemType("outputtrail");
    new ScriptMetaPause().registerItemType("pause");
    new ScriptMetaAlarm().registerItemType("alarm");
    new ScriptMetaMessage().registerItemType("message");
    new ScriptMetaConnect().registerItemType("connect");
    new ScriptMetaDisconnect().registerItemType("disconnect");
    new ScriptMetaNoWait().registerItemType("nowait");
    new ScriptMetaAutoStart().registerItemType("autostart");
    new ScriptMetaAutoQuit().registerItemType("autoquit");
  }

  private void registerItemType(String name) {
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

  boolean expect(StreamTokenizer st, char type) throws IOException, ScriptException { 
    if (checkType(st, type))
      return true;
    else {
      st.nextToken();
      throw new ScriptException("Expected '" + type + "', got " + st);
    }
  }
  
  boolean checkType(StreamTokenizer st, char type) throws IOException, ScriptException {
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

  int getNumber(StreamTokenizer st) throws IOException, ScriptException { 
    st.nextToken();
    if (st.ttype == StreamTokenizer.TT_NUMBER)
      return (int)Math.round(st.nval);
    else
      throw new ScriptException("Expected a number, got " + st);
  }

  String getString(StreamTokenizer st) throws IOException, ScriptException { 
    st.nextToken();
    if (st.ttype == '"')
      return st.sval;
    else
      throw new ScriptException("Expected a string, got " + st);
  }

  Vector pathParser(StreamTokenizer st) throws IOException, ScriptException {
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

class ScriptException extends Exception {
  public ScriptException(String error) {
    super(error);
  }
}

class ScriptPathContainer extends Script implements Cloneable {
  Vector path;  

  ScriptPathContainer() { }
  ScriptPathContainer(Vector path) { this.path = path; }

  Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
    path = pathParser(st);
    return this;
  }

  public String toString() {
    return super.toString() + " " + showPath(path);
  }
}

class ScriptNodeSelect extends ScriptPathContainer implements Cloneable {
  ScriptNodeSelect() { }
  ScriptNodeSelect(Vector path) { super(path); }
}

class ScriptNodeTrail extends Script implements Cloneable {
}

class ScriptNodeExpand extends Script implements Cloneable {
}

class ScriptNodeSourceRef extends Script implements Cloneable {
}

class ScriptNodeDefRef extends Script implements Cloneable {
}

class ScriptTraceSelect extends ScriptPathContainer implements Cloneable {
  ScriptTraceSelect() { }
  ScriptTraceSelect(Vector path) { super(path); }
}

class ScriptTraceTrail extends Script implements Cloneable {
}

class ScriptOutputSelect extends Script implements Cloneable {
  int refnr, lineno, colno;

  ScriptOutputSelect() { }
  ScriptOutputSelect(int refnr, int lineno, int colno) {
    this.refnr = refnr;
    this.lineno = lineno;
    this.colno = colno;
  }

  Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
    refnr = getNumber(st);
    lineno = getNumber(st);
    colno = getNumber(st);
    return this;
  }

  public String toString() {
    return super.toString() + " " + refnr + " " + lineno + " " + colno;
  }
}

class ScriptOutputTrail extends Script implements Cloneable {
}

class ScriptMetaPause extends Script implements Cloneable {
}

class ScriptMetaAlarm extends Script implements Cloneable {
}

class ScriptMetaMessage extends Script implements Cloneable {
  String message;

  ScriptMetaMessage() {};
  ScriptMetaMessage(String message) {
    this.message = quote(message);
  }

  Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
    message = getString(st);
    return this;
  }

  String unquote(String s) {
    char[] cb = new char[s.length()*2];
    s.getChars(0, s.length(), cb, 0);
    char ch;
    int i = 0;
    while ((ch = cb[i]) != '\0') {
      switch (ch) {
      case '\\':
	switch (cb[i+1]) {
	case '\"':	 
	  cb[i] = '\"';
	  break;
	case '\\':	 
	  break;
	case 'n':
	  cb[i] = '\n';
	  break;
	case 't':
	  cb[i] = '\t';
	  break;     
	}
	System.arraycopy(cb, i+2, cb, i+1, cb.length-(i+2));
	i += 2;
	break;
      default:
	i++;
	break;
      }
    }
    return new String(cb, 0, i);
  }
	  
  String quote(String s) {
    char[] cb = new char[s.length()*2];
    s.getChars(0, s.length(), cb, 0);
    char ch;
    int i = 0;
    while ((ch = cb[i]) != '\0') {
      switch (ch) {
      case '\"':
	System.arraycopy(cb, i, cb, i+1, cb.length-(i+1));
	cb[i] = '\\';
	i += 2;
	break;
      case '\n':
	System.arraycopy(cb, i, cb, i+1, cb.length-(i+1));
	cb[i] = '\\';
	cb[i+1] = 'n';
	i += 2;
	break;
      case '\t':
	System.arraycopy(cb, i, cb, i+1, cb.length-(i+1));
	cb[i] = '\\';
	cb[i+1] = 't';
	i += 2;
	break;
      case '\\':
	System.arraycopy(cb, i, cb, i+1, cb.length-(i+1));
	cb[i] = '\\';
	cb[i+1] = '\\';
	i += 2;
	break;
      default:
	i++;
      }
    }
    return new String(cb, 0, i);
  }

  public String toString() {
    return super.toString() + " \"" + message + "\"";
  }
}

class ScriptMetaConnect extends Script implements Cloneable {
  String host;
  int port;

  ScriptMetaConnect() {};
  ScriptMetaConnect(String host, int port) {
    this.host = host;
    this.port = port;
  }
  Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
    host = getString(st);
    port = getNumber(st);
    return this;
  }
  public String toString() {
    return super.toString() + " \"" + host + "\" " + port;
  }
}

class ScriptMetaDisconnect extends Script implements Cloneable {
}

class ScriptMetaNoWait extends Script implements Cloneable {
}

class ScriptMetaAutoStart extends Script implements Cloneable {
}

class ScriptMetaAutoQuit extends Script implements Cloneable {
}
