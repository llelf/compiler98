package nhctracer;

import java.awt.*;
import java.applet.*;
import java.util.Vector;
import java.io.*;

public class OutputFrame extends Frame {
    Frame parent;
    DbgPanel dbgPanel;
    OutputPanel outputPanel;  

    public OutputFrame(Frame parent, DbgPanel dbgPanel) {
        super();
	this.dbgPanel = dbgPanel;
	outputPanel = new OutputPanel();
	outputPanel.setDbgPanel(dbgPanel);
	setLayout(new BorderLayout());
	add("Center", outputPanel);
	setTitle("Program output");
	resize(600, 400);
	show();
    }
}

