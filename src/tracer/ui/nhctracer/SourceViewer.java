package nhctracer;

import java.awt.*;
import java.io.*;
import java.util.Vector;
import java.util.Hashtable;

public class SourceViewer extends Panel {
    Status status;
    TextArea viewer;
    Hashtable files;
    FileInfo currentFile;
    static final String spaces = "        ";
    TabbedPanel tabPanel;

    public SourceViewer(Status status) {
	this.status = status;
 	setLayout(new BorderLayout());

	viewer = new TextArea(); //(12, 80);
	add(viewer, BorderLayout.CENTER);
	viewer.setEditable(false);
	viewer.setFont(GetParams.getFont("nhctracer.sourcefont",
	                                 Font.PLAIN, "Courier", 10));
	files = new Hashtable(20);
    }
  
    public void setTabPanel(TabbedPanel tabPanel) {
        this.tabPanel = tabPanel;
    }

    public void reset() {
	viewer.setText("");
	currentFile = null;
        files.clear();
    }

    public void showSelection() {
	System.err.println("Selection:" + viewer.getSelectedText());
    }

    public void showSourceLocation(Connection conn, String filename,
                                   int r, int c) {
	getToolkit().sync();
	if (filename != null && readFile(conn, filename)) {
	    markPosition(r-1, c);
	}
	getToolkit().sync();
    }

    public void noSourceLocation() {
	viewer.setText("");
	currentFile = null;
    }

    public String replaceTabs(String s) {
	int i, j;
	for (i = 0; i < s.length();) {
	    if (s.charAt(i) == '\t') {
		j = 8 - (i % 8);
		try {
		    s = s.substring(0, i) + 
		        spaces.substring(0, j) +		
		        s.substring(i+1);
		} catch (StringIndexOutOfBoundsException e) {
		    fail(e, "replaceTabs: bad args.");
		}
		i += j;
	    } else
		i++;
	}
	return s;
    }

    public boolean readFile(Connection conn, String filename) {
	BufferedReader file;

	if ((currentFile = (FileInfo)files.get(filename)) != null) {
	    viewer.setText(currentFile.contents);
	    return true;
	}
	status.setText("Loading " + filename);
	    conn.out.println("F");
	    conn.out.println(filename);
	    file = conn.in;
	    FileInfo fi = new FileInfo();
	    String line;

	    fi.lines = new Vector(50, 50);
	    fi.contents = "";
	    viewer.setText("");

	    try {
		int chars = 0;
		while ((line = file.readLine()) != null && !line.equals("<EOF>")) {
		    int i;
		    line = replaceTabs(line);
		    fi.contents = fi.contents + line + "\n";
		    fi.lines.addElement(new Integer(chars));
		    chars += line.length()+1;
		}
		viewer.setText(fi.contents);
		//setTitle(filename);
	    } catch (EOFException e) {}
	    catch (IOException e) {
	      fail(e, "io error reading source code file.");
	    }
	    currentFile = fi;
	    files.put(filename, fi);	      
	status.setText("");
	return true;
    }

    public void markPosition(int r, int c) {
        Integer i = (Integer)currentFile.lines.elementAt(r);
	viewer.select(i.intValue() + c-1, i.intValue() + c);
	tabPanel.select("Source code");
    }

    public static void fail(Exception e, String msg) {
        System.err.println(msg + ": " +  e);
        System.exit(1);
    }

}

class FileInfo {
  Vector lines;
  String contents;

  public FileInfo() {
  }
}
