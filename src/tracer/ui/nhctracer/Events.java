package nhctracer;

import java.io.*;
import java.util.*;

public class Events {
  // Script entry types

  static public class PathContainer extends Script implements Cloneable {
    Vector path;  

    PathContainer() { }
    PathContainer(Vector path) { this.path = path; }

    Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
      path = Script.pathParser(st);
      return this;
    }

    public String toString() {
      return super.toString() + " " + Script.showPath(path);
    }
  }

  static public class NodeSelect extends PathContainer implements Cloneable {
    NodeSelect() { }
    NodeSelect(Vector path) { super(path); }
  }

  static public class NodeTrail extends Script implements Cloneable {
  }

  static public class NodeExpand extends Script implements Cloneable {
  }

  static public class NodeSourceRef extends Script implements Cloneable {
  }

  static public class NodeDefRef extends Script implements Cloneable {
  }

  static public class TraceSelect extends PathContainer implements Cloneable {
    TraceSelect() { }
    TraceSelect(Vector path) { super(path); }
  }

  static public class TraceTrail extends Script implements Cloneable {
  }

  static public class OutputSelect extends Script implements Cloneable {
    int refnr, lineno, colno;

    OutputSelect() { }
    OutputSelect(int refnr, int lineno, int colno) {
      this.refnr = refnr;
      this.lineno = lineno;
      this.colno = colno;
    }

    Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
      refnr = Script.getNumber(st);
      lineno = Script.getNumber(st);
      colno = Script.getNumber(st);
      return this;
    }

    public String toString() {
      return super.toString() + " " + refnr + " " + lineno + " " + colno;
    }
  }

  static public class OutputTrail extends Script implements Cloneable {
  }

  static public class MetaPause extends Script implements Cloneable {
  }

  static public class MetaAlarm extends Script implements Cloneable {
  }

  static public class MetaMessage extends Script implements Cloneable {
    String message;

    MetaMessage() {};
    MetaMessage(String message) {
      this.message = quote(message);
    }

    Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
      message = Script.getString(st);
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

  static public class MetaConnect extends Script implements Cloneable {
    String host;
    int port;

    MetaConnect() {};
    MetaConnect(String host, int port) {
      this.host = host;
      this.port = port;
    }
    Script parseArgs(StreamTokenizer st) throws IOException, ScriptException {
      host = Script.getString(st);
      port = Script.getNumber(st);
      return this;
    }
    public String toString() {
      return super.toString() + " \"" + host + "\" " + port;
    }
  }

  static public class MetaDisconnect extends Script implements Cloneable {
  }

  static public class MetaNoWait extends Script implements Cloneable {
  }

  static public class MetaAutoStart extends Script implements Cloneable {
  }

  static public class MetaAutoQuit extends Script implements Cloneable {
  }
}
