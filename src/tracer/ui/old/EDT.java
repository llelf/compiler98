package nhctracer;

import java.awt.*;
import java.applet.*;

public class EDT extends Applet {
    EDTFrame edtframe;
 
    public EDT() {
	super();
	EDTFrame edtframe = new EDTFrame(//this,
			    	         "Haskell Tracer", 
			    	         "Jan Sparud",
				         "Version -0.01 alpha");
	edtframe.resize(500, 600);
	edtframe.show();
	this.start();
    }

    public void run() {
    }

    public void init() {
      //new EDT();
    }

    public static void main(String[] argv) {
      new EDT();
    }
}

/*
public class EDT extends Applet {
    EDTFrame edtframe;
 
    public void init() {
	setLayout(new BorderLayout());

	edtframe = new EDTFrame(this,
			    	"Haskell Tracer", 
			    	"Jan Sparud",
				"Version 0.01 alfa");
	add("Center", edtframe);
	edtframe.pack();
	edtframe.show();
    }
}
*/

class EDTFrame extends Frame {
    MenuBar menuBar;

    Menu fileMenu;
    Menu editMenu;
    Menu viewMenu;
    Menu helpMenu;

    MenuItem databaseItem;
    MenuItem tableItem;
    MenuItem exitItem;

    MenuItem copyItem;
    MenuItem cutItem;
    MenuItem pasteItem;

    MenuItem dataItem;
    MenuItem schemaItem;

    MenuItem aboutItem;

    DbgPanel dbgPanel;
    OutputFrame outputFrame;

    String appTitle;
    String appAuthor;
    String appVersion;

    public EDTFrame(//Applet app,
		    String appTitle, 
		    String appAuthor, 
		    String appVersion) {

	this.appTitle = appTitle;
	this.appAuthor = appAuthor;
	this.appVersion = appVersion;
	
	menuBar = new MenuBar(); setMenuBar(menuBar);
	
	menuBar.setFont(new Font("Helvetica", Font.PLAIN, 14));
	
	fileMenu = new Menu("File"); menuBar.add(fileMenu);
	editMenu = new Menu("Edit"); menuBar.add(editMenu);
	viewMenu = new Menu("View"); menuBar.add(viewMenu);
	helpMenu = new Menu("Help"); menuBar.add(helpMenu);
	
	exitItem = new MenuItem("Exit"); fileMenu.add(exitItem);
	
	copyItem = new MenuItem("Copy"); editMenu.add(copyItem);
	cutItem = new MenuItem("Cut"); editMenu.add(cutItem);
	pasteItem = new MenuItem("Paste"); editMenu.add(pasteItem);
	
	copyItem.disable();
	cutItem.disable();
	pasteItem.disable();
	
	aboutItem = new MenuItem("About"); helpMenu.add(aboutItem);
	menuBar.setHelpMenu(helpMenu);
	
	setLayout(new BorderLayout());
	
	dbgPanel = new DbgPanel();
	outputFrame = new OutputFrame(this, dbgPanel);

	add("Center", dbgPanel);
	/* resize(500, 600); */
	setTitle(appTitle);
	/* show(); */
    }

    public boolean handleEvent(Event evt) {
        if(evt.target instanceof MenuItem) {
            if(evt.target == exitItem || 
                evt.id == Event.WINDOW_DESTROY) {
	        if (dbgPanel.server != null && dbgPanel.server.connection != null)
		    dbgPanel.server.connection.close();
                System.exit(0);
                return true;
            } else if(evt.target == aboutItem) {
                AboutDialog about = new AboutDialog(
                    this, 
                    appTitle, 
                    appAuthor,
                    appVersion
                );
                return true;
            }
        }

        return super.handleEvent(evt);
    }

    public void destroy() {
        dispose();
	System.exit(0);
    }
}

class AboutDialog extends Dialog {

	public AboutDialog(Frame parent, String title, String author, String version) {
		super(parent, title, true);

		int i;
		int width, height;

		setLayout(new BorderLayout());

		Panel titlePanel = new Panel();
		Panel buttonPanel = new Panel();

		FontMetrics m = getFontMetrics(getFont());

		width = m.stringWidth(title);
		if((i=m.stringWidth(author)) > width) width = i;
		if((i=m.stringWidth(version)) > width) width = i;
	
		height = m.getHeight();

		resize((width*3)/2, height*10);

		titlePanel.setLayout(new GridLayout(3,1));
		titlePanel.add("North",new Label(title,Label.CENTER));
		titlePanel.add("Center",new Label(author,Label.CENTER));
		titlePanel.add("South",new Label(version,Label.CENTER));

		buttonPanel.setLayout(new BorderLayout());
		buttonPanel.add("Center", new Button("OK"));

		add("Center", titlePanel);
		add("South", buttonPanel);

		show();
	}

	public boolean handleEvent(Event e) {

		if(e.target instanceof Button) {
			dispose();
			return true;
		}
		return false;
	}
}
