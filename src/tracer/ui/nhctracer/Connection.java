package nhctracer;

import java.io.*;
import java.net.*;
import java.awt.*;
import java.util.*;

public class Connection {
  String host;
  int port;
  String error;
  Socket server;
  protected String pushBacked = null;
  protected String line = null;
  BufferedReader in;
  PrintWriter out;

  public Connection(String host, int port) {
    server = null;
    error = null;
    this.host = host;
    this.port = port;
    for (int i = 0; i <= 20; i++) {
      try {
	server = new Socket(host, port);
	break;
      } catch (IOException e) { 
	if (i==20) {
	  error = "Cannot connect to trace.";
	  return;
	}
      }
      try { Thread.sleep(100); } catch (InterruptedException ex) {}
    }
    try { 
      in = new BufferedReader(new InputStreamReader(server.getInputStream()));
      out = new DbgPrintWriter(server.getOutputStream(), true);
    }
    catch (IOException e) {
      try { 
	server.close(); 
      } catch (IOException e2) {}
      error = "Cannot connect to trace";
      return;
    }
    // Attempt to read a token.
    // If too many users, we'll be disconnected by now.
    try {
      line = in.readLine();
      if (Options.tracecomm.getState())
	  System.err.println("Got: " + line);
    } catch (IOException e) {}
    if (line == null)
      error = "Too many users are using the server. Please try later.";
  }
    
  public Connection(String expr) {
    in = new BufferedReader(new StringReader(expr));
    out = null;
  }

  public void close() {
    try { 
      if (in != null)
	in.close();
      if (out != null)
	out.close();
      if (server != null)
	server.close();
    } catch (IOException e) {
      System.err.println("Couldn't close socket!\n" + e);
    }
  }

  public void pushBack(String tok) {
    pushBacked = tok;	
  }

  public String rest() {
    if (pushBacked != null)
      return pushBacked+line;
    else
      return line;
  }

  public String nextToken() {
    String result;
    if (pushBacked != null) {
      result = pushBacked;
      pushBacked = null;
      return result;
    }
    for (;;) {
      if (line == null || line.length() == 0) {
	try {
	  //System.err.println("Waiting for line...");
	  line = in.readLine();
	  if (Options.tracecomm.getState())
	    System.err.println("Got: " + line);
	} 
	catch (IOException e) {
	  // Should do something more sensible here...
	  System.err.println("tokenizer: Cannot read: " + e);
	  System.exit(-1);
	}
	if (line == null) {
	  System.err.println("Connection.nextToken: couldn't read");
	  //try { Thread.sleep(1000); }
	  //catch (InterruptedException e) {}
	  System.exit(-1);
	}
      }
      int len = line.length();
      int pos = 0, np;
      while (pos < len) {
	switch (line.charAt(pos)) {
	case '(':
	  result = "(";
	  line = line.substring(pos+1);
	  return result;
	case ')':
	  result = ")";
	  line = line.substring(pos+1);
	  return result;
	case '\'':
	  np = pos+1;
	  while (np < len && line.charAt(np) != '\'')
	    np++;
	  result = line.substring(pos, np+1);
	  line = line.substring(np+1);
	  return result;
	case '"':
	  np = pos+1;
	  while (np < len && line.charAt(np) != '"' || 
		 (line.charAt(np) == '"' && np > 0 && line.charAt(np-1) == '\\'))
	    np++;
	  result = line.substring(pos+1, np);
	  line = line.substring(np+1);
	  return result;
	case ' ':
	case '\n':
	case '\t':
	  pos++;
	  break;
	default:
	  np = pos;
	  while (np < len && "() ".indexOf(line.charAt(np)) < 0)
	    np++;
	  result = line.substring(pos, np);
	  line = line.substring(np);
	  return result;
	}
      }
      if (pos >= len)
	line = null;
    }
  }
  class DbgPrintWriter extends PrintWriter {
    DbgPrintWriter(OutputStream os, boolean b) {
      super(os, b);
    }

    public void println(String s) {
      if (Options.tracecomm.getState())
	System.err.println("Sent: " + s);
      super.println(s);
    }
  }
}
