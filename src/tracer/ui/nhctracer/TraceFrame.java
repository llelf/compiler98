package nhctracer;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.net.*;

public class TraceFrame extends Frame {
  MenuBar menuBar;
  
  Menu fileMenu, optionMenu, helpMenu;  // viewMenu
  
  // File menu items
  MenuItem
    connectToTraceItem, disconnectItem,
    createScriptItem, endScriptItem, runScriptItem, msgScriptItem,
    exitItem;
  
  // Options menu items
  MenuItem chooseFontItem, chooseStringLenItem;

  // Help menu items
  MenuItem mouseHelpItem, highlightHelpItem, symbolHelpItem;
   
  int port;
  String host;
  String appTitle = "Hat 1.08 Trail Browser";

  MainPanel mainPanel;

  StringDialog stringDialog;
  FontDialog fontDialog;
  Button connect;
  Button disconnect;
  Connection serverConnection;
  TraceFrame me;
  PrintWriter script;
  ActionHandler handler;
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
    helpMenu = new Menu("Help"); // menuBar.add(helpMenu);
	
    handler = new ActionHandler();
    // serverItem =
    //   createMenuItem("Change server/port", fileMenu, handler, true,
    //                  null);
    connectToTraceItem =
      createMenuItem("Connect to trace", fileMenu, handler, true, null);
    disconnectItem =
      createMenuItem("Disconnect", fileMenu, handler, true, null);
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
    // printItem = createMenuItem("Print", fileMenu, handler, true, new MenuShortcut(KeyEvent.VK_P));
    exitItem = createMenuItem("Exit", fileMenu, handler, true, null);

    chooseFontItem = createMenuItem("Choose font", optionMenu, handler, true, null);
    chooseStringLenItem =
      createMenuItem("Choose string-length limit", optionMenu, handler, true, null);

    optionMenu.add(createCheckboxMenuItem(Options.lineNos, true));
    // optionMenu.add(createCheckboxMenuItem(Options.memoise, false));
    
    optionMenu.add(createCheckboxMenuItem(Options.showcase, true));
    // optionMenu.add(createCheckboxMenuItem(Options.tracecomm, true));
    // optionMenu.add(createCheckboxMenuItem(Options.highshare, true));
    // optionMenu.add(createCheckboxMenuItem(Options.oarrow, true));
    
    mouseHelpItem =
      createMenuItem("mouse clicks", helpMenu, handler, true, null);
    highlightHelpItem =
      createMenuItem("expression highlights", helpMenu, handler, true, null);
    symbolHelpItem =
      createMenuItem("special symbols", helpMenu, handler, true, null);
    menuBar.setHelpMenu(helpMenu);
	
    setTitle(appTitle);

    mainPanel = new MainPanel(this);

    Panel pb = new Panel();
    pb.setLayout(new FlowLayout());

    setLayout(new BorderLayout());

    add(pb, BorderLayout.NORTH);
    add(mainPanel, BorderLayout.CENTER);
    
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        stop();
	me.dispose();
        System.exit(0);  // windowClosing requires explicit call to System.exit
                         // otherwise java loops forever
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

  boolean stopping = false;
  public void stop() {
    if (stopping) return;
    if (serverConnection != null) {
      stopping = true;
      mainPanel.outputPanel.disconnected();      
      serverConnection = null;
      mainPanel.viewer.reset();
      stopping = false;
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

  void connectToTrace(String filename,String startnode) {
    doDisconnect();
    try {
      if (startnode!="") {startnode=" -remote "+startnode;}
      Runtime.getRuntime().exec("hat-connect "+port+" "+filename+startnode);
      Thread.sleep(100);
      doConnect();
    } catch (IOException ex) {
      enableListeners();
      new ModalMessage(me, "Cannot open/read "+filename);
    } catch (InterruptedException ex) {
      enableListeners();
    }
  }
  
  class TraceFilter implements FilenameFilter {
    public boolean accept(File dir, String name) {
      return name.endsWith(".hat");
    }
  }

  class ActionHandler implements ActionListener {
    public void actionPerformed(ActionEvent evt) {
      Object target = evt.getSource();
      if (target instanceof MenuItem) {
	if (target == connectToTraceItem) {
	  FileDialog fd =
	    new FileDialog(me, "Connect to trace", FileDialog.LOAD);
	  fd.setVisible(true);
	  if (fd.getFile() != null) {
	    connectToTrace(fd.getDirectory()+fd.getFile(),"");
	  }
	} else if (target == disconnectItem) {
	  doDisconnect();	
	/*
	} else if (target == serverItem) {
	  ServerDialog sd = new ServerDialog(me, host, port);
	  if (sd.server != null) {
	    host = sd.server;
	    port = sd.port;
	  }
	 */
	} else if (target == createScriptItem) {
	  FileDialog fd =
	    new FileDialog(me, "Create script file", FileDialog.SAVE);
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
	      script.println(new Events.MetaMessage(sm.message));
	    }
	  }
	} else if (target == exitItem) {
	  stop();
	  System.exit(0);
	} else if (target == chooseFontItem) {
	  if (fontDialog == null)
	    fontDialog = new FontDialog(me, mainPanel.dbgPanel.ui);
	  else
	    fontDialog.show();
	} else if (target == chooseStringLenItem) {
	  if (stringDialog == null)
	    stringDialog = new StringDialog(me, mainPanel.dbgPanel.ui);
	  else
	    stringDialog.show();
	} else if (target == mouseHelpItem) {
	  new Help("mouse clicks", mainPanel.dbgPanel.ui);
	} else if (target == highlightHelpItem) {
	  new Help("expression highlights", mainPanel.dbgPanel.ui);
	} else if (target == symbolHelpItem) {
	  new Help("special symbols", mainPanel.dbgPanel.ui);
	}
      }
    }
  }

  void doConnect() {
    if (serverConnection == null) {
      serverConnection = new Connection(host, port);
      if (serverConnection.error == null) {
	if (script != null)
	  script.println(new Events.MetaConnect(host, port));
	mainPanel.outputPanel.connected(serverConnection);      
      } else {
	mainPanel.status.setText(serverConnection.error);
	serverConnection = null;
      }
    }
  }
  
  void doDisconnect() {
    if (script != null)
      script.println(new Events.MetaDisconnect());
    stop();
  }
  
  public void destroy() {
    dispose();
    System.exit(0);
  }
}

