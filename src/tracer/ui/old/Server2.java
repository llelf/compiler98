package nhctracer;

import java.io.*;
import java.net.*;
import java.awt.*;
import java.util.*;

public class Server2 extends Thread {
    public final static int DEFAULT_PORT = 6789;
    public final static int CONNECTED = 1;
    public final static int LINE_READ = 2;
    public final static int NEW_TOKEN = 3;
    protected int port;
    protected ServerSocket listen_socket;
    Connection connection;
    boolean connected = false;
    //Component user;

    // Exit with an error message, when an exception occurs.
    public static void fail(Exception e, String msg) {
        System.err.println(msg + ": " +  e);
        System.exit(1);
    }
    
    // Create a ServerSocket to listen for connections on;  start the thread.
    public Server2(int port) {
        if (port == 0) port = DEFAULT_PORT;
        this.port = port;
	//user = user_;
        try { listen_socket = new ServerSocket(port); }
        catch (IOException e) { fail(e, "Exception creating server socket"); }
        //System.out.println("Server: listening on port " + port);
        this.start();
    }
    
    public Connection connect() {
        try {
                Socket client_socket = listen_socket.accept();
                return new Connection(this, client_socket);
        }
        catch (IOException e) { fail(e, "Exception while listening for connections"); }
	System.err.println("Connection to debugger failed.");
	return null;
    }

    // The body of the server thread.  Loop forever, listening for and
    // accepting connections from clients.  For each connection, 
    // create a Connection object to handle communication through the
    // new Socket.

    public void run() {
	connection = connect();	
	connected = true;
	try {
	  join();
	}
        catch (InterruptedException e) {
	  System.exit(0);
	}
    }
    
    // Start the server up, listening on an optionally specified port
    public static void main(String[] args) {
        int port = 0;
        if (args.length == 1) {
            try {
		port = Integer.parseInt(args[0]); 
	    }
            catch (NumberFormatException e) {
		port = 0;
	    }
        }
        new Server2(port);
    }
}

// This class is the thread that handles all communication with a client
class Connection extends Thread {
    protected Socket client;
    protected Server2 server;
    protected String pushBacked = null;
    protected String line = null;
    DataInputStream in;
    PrintStream out;

    // Initialize the streams and start the thread
    public Connection(Server2 server_, Socket client_socket) {
	server = server_;
        client = client_socket;
        try { 
            in = new DataInputStream(client.getInputStream());
            out = new PrintStream(client.getOutputStream());
        }
        catch (IOException e) {
            try { 
		client.close(); 
	    } catch (IOException e2) {};
            System.err.println("Exception while getting socket streams: " + e);
            return;
        }
        this.start();
    }
    
    public void pushBack(String tok) {
	//System.err.println("pb: " + tok);	
	pushBacked = tok;	
    }

    public String nextToken() {
	String result;
	if (pushBacked != null) {
	    result = pushBacked;
	    pushBacked = null;
	    //System.err.println("nt(pb): " + result);
	    //System.err.println("nt(pb): " + line);
	    return result;
 	}
	for (;;) {
	  if (line == null || line.length() == 0) {
	      try {
	          line = in.readLine();
		  System.err.println(line);
              } catch (IOException e) {
	          System.err.println("tokenizer: Cannot read: " + e);
	          System.exit(-1);
	      }
              if (line == null) {
	          System.err.println("Connection.run: couldn't read");
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
		    //System.err.println("nt: " + result);
	    	    //System.err.println("nt: " + line);
		    return result;
		    //server.user.handleEvent(new Event(server, Server2.NEW_TOKEN, "("));
		    //pos++;
		    // break;
		case ')':
		    result = ")";
		    line = line.substring(pos+1);
		    //System.err.println("nt: " + result);
	    	    //System.err.println("nt: " + line);
		    return result;
		    //server.user.handleEvent(new Event(server, Server2.NEW_TOKEN, ")"));
		    //pos++;
		    //break;
		case '\'':
		    np = pos+1;
		    while (np < len && line.charAt(np) != '\'')
			np++;
		    //server.user.handleEvent(new Event(server, Server2.NEW_TOKEN, 
		    //			    line.substring(pos, np+1)));
		    result = line.substring(pos, np+1);
		    line = line.substring(np+1);
		    //System.err.println("nt: " + result);
	    	    //System.err.println("nt: " + line);
		    return result;
		    //pos = np+1;
		    //break;
		case '"':
		    np = pos+1;
		    while (np < len && line.charAt(np) != '"' || 
			   (line.charAt(np) == '"' && np > 0 && line.charAt(np-1) == '\\'))
			np++;
		    //server.user.handleEvent(new Event(server, Server2.NEW_TOKEN, 
		    //			    line.substring(pos, np+1)));
		    result = line.substring(pos, np+1);
		    line = line.substring(np+1);
		    //System.err.println("nt: " + result);
	    	    //System.err.println("nt: " + line);
		    return result;
		    //pos = np+1;
		    //break;
		case ' ':
		case '\n':
		case '\t':
		    pos++;
		    break;
		default:
		    np = pos;
		    while (np < len && "() ".indexOf(line.charAt(np)) < 0)
			np++;
		    //server.user.handleEvent(new Event(server, Server2.NEW_TOKEN, 
		    //			    line.substring(pos, np)));
		    result = line.substring(pos, np);
		    line = line.substring(np);
		    //System.err.println("nt: " + result);
	    	    //System.err.println("nt: " + line);
		    return result;
		    //pos = np;
		    //break;
	    }
	  }
	}
    }

    public synchronized void run() {
	//server.user.handleEvent(new Event(server, Server2.CONNECTED, null));
	String line;
	try {
	    while (true) {
		line = null;
		while (line == null) {
	    		System.err.println("Waiting for a line");
	    		line = in.readLine();	
		}
	    	System.err.println(line);
	    	out.println(line);
	    }
	} catch (IOException e) {
	    System.err.println("Error reading from socket:" + e);
	  System.exit(0);
	}
	try {
	  join();
	}
        catch (InterruptedException e) {
	  System.exit(0);
	}
/*
        try {
	    while (true) {
            for(;;) {
                String line = in.readLine();
                if (line == null) {
		    System.err.println("Connection.run: couldn't read");
		    System.exit(-1);
		}
		tokenizer(line);
            }
        }
        catch (IOException e) ;
        finally try client.close(); catch (IOException e2) ;
*/
    }

}
