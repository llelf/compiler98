package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.io.*;
import java.util.*;
import java.net.*;

public class TraceFrame extends Frame {
  MenuBar menuBar;
  
  Menu fileMenu, optionMenu;  // viewMenu, demoMenu, helpMenu;
  
  // File menu items
  MenuItem /* newWindowItem, */ serverItem, createScriptItem, endScriptItem;
  MenuItem runScriptItem, msgScriptItem, /* loadDemoItem, printItem, */ exitItem;
  
  // Options menu items
  MenuItem fontItem;

  // Help menu items
  // MenuItem aboutItem;
  
  int port;
  String host;
  String appTitle = "Hat 1.0";

  MainPanel mainPanel;

  FontDialog fontDialog;
  Button connect;
  Button disconnect;
  Connection serverConnection;
  TraceFrame me;
  PrintWriter script;
  ActionHandler handler;
  // DemoHandler demoHandler;
  Hashtable demoTable;
  boolean listenersEnabled = true;

  public TraceFrame(String host,
		    int port) {

    this.port = port;
    this.host = host == null ? "localhost" : host;
    this.appTitle = appTitle;
    me = this;

    menuBar = new MenuBar(); 
    setMenuBar(menuBar);

    menuBar.setFont(new Font("Helvetica", Font.PLAIN, 14));
	
    fileMenu = new Menu("File"); menuBar.add(fileMenu);
    // editMenu = new Menu("Edit"); menuBar.add(editMenu);
    optionMenu = new Menu("Options"); menuBar.add(optionMenu);
    // viewMenu = new Menu("View"); menuBar.add(viewMenu);
    // demoMenu = new Menu("Demo");
    // helpMenu = new Menu("Help"); //menuBar.add(helpMenu);
	
    handler = new ActionHandler();
    // demoHandler = new DemoHandler();
    // newWindowItem = createMenuItem("New window", fileMenu, handler, true, null);
    serverItem =
      createMenuItem("Change server/port", fileMenu, handler, true,
                     null);
    createScriptItem =
      createMenuItem("Create script", fileMenu, handler, true,
                     new MenuShortcut(KeyEvent.VK_S));
    endScriptItem =
      createMenuItem("End script", fileMenu, handler, true,
                     new MenuShortcut(KeyEvent.VK_E));
    runScriptItem =
      createMenuItem("Run script", fileMenu, handler, true,
                     new MenuShortcut(KeyEvent.VK_R));
    msgScriptItem =
      createMenuItem("Add script message", fileMenu, handler, true,
                     new MenuShortcut(KeyEvent.VK_M));
    // loadDemoItem = createMenuItem("Load demo programs", fileMenu, handler, true, null);
    // printItem = createMenuItem("Print", fileMenu, handler, true, new MenuShortcut(KeyEvent.VK_P));
    exitItem = createMenuItem("Exit", fileMenu, handler, true, null);

    fontItem = createMenuItem("Select font", optionMenu, handler, true, null);

    // optionMenu.add(createCheckboxMenuItem(Options.memoise, true));
    // optionMenu.add(createCheckboxMenuItem(Options.showcase, true));
    // optionMenu.add(createCheckboxMenuItem(Options.tracecomm, true));
    // optionMenu.add(createCheckboxMenuItem(Options.dumprefs, true));
    // optionMenu.add(createCheckboxMenuItem(Options.arrow, true));
    // optionMenu.add(createCheckboxMenuItem(Options.oarrow, true));
    
    // aboutItem = createMenuItem("About", helpMenu, handler, true, null);

    // menuBar.setHelpMenu(helpMenu);
	
    setTitle(appTitle);

    mainPanel = new MainPanel(this);

    connect = new Button("Connect");
    connect.addActionListener(handler);
    disconnect = new Button("Disconnect");
    disconnect.addActionListener(handler);

    Panel pb = new Panel();
    pb.setLayout(new FlowLayout());
    pb.add(connect);
    pb.add(disconnect);

    setLayout(new BorderLayout());

    add(pb, BorderLayout.NORTH);
    add(mainPanel, BorderLayout.CENTER);
    
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	me.dispose();
      }
    });
    
    Script.registerItemTypes();
  }
  
  public MenuItem createMenuItem(String lab, Menu m, ActionListener al, boolean enabled, MenuShortcut msc) {
    MenuItem item = msc==null ? new MenuItem(lab) : new MenuItem(lab, msc);
    m.add(item);
    item.setEnabled(enabled);
    item.addActionListener(al);
    return item;
  }

  public CheckboxMenuItem createCheckboxMenuItem(Option opt, boolean enabled) {
    CheckboxMenuItem item = new CheckboxMenuItem(opt.name, opt.getState());
    //m.add(item);
    item.setEnabled(enabled);
    item.addItemListener(opt);
    return item;
  }

  public void stop() {
    if (serverConnection != null) {
      mainPanel.outputPanel.disconnected();      
      serverConnection = null;
      mainPanel.viewer.reset();
    }
  }

  void disableListeners() {
    mainPanel.disableListeners();
    listenersEnabled = false;
  }

  void enableListeners() {
    if (!listenersEnabled) {
      mainPanel.enableListeners();
      listenersEnabled = true;
    }
  }

  void runScript(String file) {
    if (!file.endsWith(".scr"))
      file += ".scr";
    try {
      mainPanel.status.setStateText("(script running) ");
      Vector scriptv;
      if (file.startsWith("http:")) {
	URL url = new URL(file);
	URLConnection uc = url.openConnection();
	uc.setDoInput(true);
	uc.connect();
	scriptv = Script.parseReader(new InputStreamReader(uc.getInputStream()));
      } else
	scriptv = Script.parseReader(new FileReader(file));
      disableListeners();
      ScriptEngine scriptEngine = new ScriptEngine(me, scriptv, file);
    } catch (IOException ex) {
      enableListeners();
      new ModalMessage(me, "Cannot open/read " + file);
    } catch (ScriptException ex) {
      enableListeners();
      new ModalMessage(me, ex.getMessage());
    }
    mainPanel.status.setStateText("");
  }

  class ScriptFilter implements FilenameFilter {
    public boolean accept(File dir, String name) {
      return name.endsWith(".scr");
    }
  }

  /*
  class DemoHandler implements ActionListener {
    public void actionPerformed(ActionEvent evt) {
      Object url = demoTable.get(evt.getActionCommand());
      if (url == null) {
	mainPanel.status.setText("Bad url in demo.");
      } else {
	runScript((String)url);
      }
    }
  }
  */

  class ActionHandler implements ActionListener {
    public void actionPerformed(ActionEvent evt) {
      Object target = evt.getSource();
      if (target instanceof MenuItem) {
	if (target == exitItem) {
	  if (serverConnection != null) {
	    mainPanel.outputPanel.disconnected();
	    serverConnection = null;
	    mainPanel.viewer.reset();
	  }
	  System.exit(0);
	} else if (target == serverItem) {
	  ServerDialog sd = new ServerDialog(me, host, port);
	  if (sd.server != null) {
	    host = sd.server;
	    port = sd.port;
	  }
	} /* else if (target == newWindowItem) {
	  TraceFrame traceFrame = 
	    new TraceFrame(host, port);
	  traceFrame.setSize(600, 700);
	  traceFrame.setVisible(true);
	} */ else if (target == createScriptItem) {
	  FileDialog fd = new FileDialog(me, "Create script file", FileDialog.SAVE);
	  fd.setFilenameFilter(new ScriptFilter());
	  fd.setVisible(true);
	  if (fd.getFile() != null) {
	    try {
	      String filename = fd.getFile();
	      if (!filename.endsWith(".scr"))
		filename += ".scr";
	      mainPanel.status.setStateText("(script recording) ");	      
	      script = new PrintWriter(new FileWriter(filename));
	      script.println("# Script started at " + new Date());
	    } catch (IOException ex) {
	      new ModalMessage(me, "Error creating script file.");
	      script = null;
	    }
	  }
	} else if (target == endScriptItem) {
	  if (script != null) {
	    script.println("# Script finished at " + new Date());
	    script.close();
	    new ModalMessage(me, "Script finished");
	    script = null;
	    mainPanel.status.setStateText("");
	  }
	} else if (target == runScriptItem) {
	  FileDialog fd = new FileDialog(me, "Run script file", FileDialog.LOAD);
	  fd.setFilenameFilter(new ScriptFilter());
	  fd.setVisible(true);
	  if (fd.getFile() != null) {
	      String filename = fd.getFile(), dir = fd.getDirectory();
	      runScript(fd.getDirectory() + fd.getFile());
	  }
	} else if (target == msgScriptItem) {
	  if (script != null) {
	    ScriptMessage sm = new ScriptMessage(me);
	    if (sm.message != null) {
	      script.println(new ScriptMetaMessage(sm.message));
	    }
	  }
	} else if (target == fontItem) {
	  if (fontDialog == null)
	    fontDialog = new FontDialog(me, mainPanel.dbgPanel.ui);
	  else
	    fontDialog.show();
	}
       /* else if (target == loadDemoItem) {
	  mainPanel.status.setText("Loading demo programs from the demo server, please wait...");
	  try {
	    String serverURL = System.getProperty("nhctracer.demoserver");
	    if (serverURL == null)
	      serverURL = "http://www.cs.york.ac.uk/fp/ART/demo/demo.dat";
	    URL u = new URL(serverURL);
	    URLConnection uc = u.openConnection();
	    uc.setDoInput(true);
	    uc.connect();
	    BufferedReader file = new BufferedReader (new InputStreamReader(uc.getInputStream()));
	    demoTable = new Hashtable();
	    String line;
	    Stack menus = new Stack();
	    Menu curMenu = demoMenu;
	    while ((line = file.readLine()) != null) {
	      if (line.equals("push")) {
	        menus.push(curMenu);		
		curMenu = new Menu(file.readLine());
		((Menu)menus.peek()).add(curMenu);
	      } else if (line.equals("pop")) {
		curMenu = (Menu)menus.pop();
	      } else {
		MenuItem mi = new MenuItem(line);
		mi.addActionListener(demoHandler);
		String url = file.readLine();
		demoTable.put(line, url);
		curMenu.add(mi);
	      }
	    }
	    menuBar.add(demoMenu);
	    loadDemoItem.setEnabled(false);
	    mainPanel.status.setText("Demo programs loaded, see the Demo menu.");
	  } catch (Exception e) {
	    mainPanel.status.setText("Error loading demo programs.");
	    new ModalMessage(me, "Cannot load demo programs (" + e + ")");
	  }
	} else if (target == aboutItem) {
	  AboutDialog about = new AboutDialog(me, appTitle, appAuthor, appVersion);
	}
	*/
      } else if (target == connect) {
	if (serverConnection == null) {
	  mainPanel.status.setText("Connecting to server. Please wait.");
	  serverConnection = new Connection(host, port);
	  if (serverConnection.error == null) {
	    if (script != null)
	      script.println(new ScriptMetaConnect(host, port));
	    mainPanel.outputPanel.connected(serverConnection);      
	  } else {
	    mainPanel.status.setText(serverConnection.error);
	    serverConnection = null;
	  }
	} else {
	  mainPanel.status.setText("Already connected");
	}
      } else if (target == disconnect) {
	if (serverConnection == null) {
	  mainPanel.status.setText("Not connected");
	} else {
	  if (script != null)
	    script.println(new ScriptMetaDisconnect());
	  mainPanel.outputPanel.disconnected();      
	  serverConnection = null;
	  mainPanel.viewer.reset();
	}
      }
    }
  }

  public void destroy() {
    dispose();
    System.exit(0);
  }
}

/*
class AboutDialog extends Dialog implements ActionListener {
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

    setSize((width*3)/2, height*10);

    titlePanel.setLayout(new GridLayout(3, 1));
    titlePanel.add(new Label(title, Label.CENTER));
    titlePanel.add(new Label(author,Label.CENTER));
    titlePanel.add(new Label(version,Label.CENTER));

    buttonPanel.setLayout(new BorderLayout());
    Button ok = new Button("OK");
    buttonPanel.add(ok, BorderLayout.CENTER);
    ok.addActionListener(this);

    add(titlePanel, BorderLayout.CENTER);
    add(buttonPanel, BorderLayout.CENTER);

    setVisible(true);
  }

  public void actionPerformed(ActionEvent evt) {
    dispose();
  }  
}
*/
