package nhctracer;

import java.awt.Font;
import java.applet.Applet;

public class GetParams {
  public static Applet applet;
    
  public static void setApplet(Applet _applet) {
    applet = _applet;
  }

  public static int getInt(String param, int def) {
    String parint;
    if (applet != null)
      parint = applet.getParameter(param);
    else
      parint = System.getProperty(param);
    if (parint == null)
      return def;
    else
      try {
	Integer i = new Integer(parint);
	System.err.println("Setting " + param + " to " + i);
	return i.intValue();
      } catch (NumberFormatException e) {
	System.err.println("GetParams.getInt: bad integer parameter:" + param 
			   + ": " + parint);
	System.exit(-1);
	/* NOTREACHED */
	return 0;
      }    
  }

  public static Font getFont(String param, int deftype, String deffont, int defsize) {
    String parfont;
    if (applet != null)
      parfont = applet.getParameter(param);
    else
      parfont = System.getProperty(param);

    if (parfont == null)
      return new Font(deffont, deftype, defsize);
    int colonpos = parfont.indexOf(':');
    if (colonpos < 0) {
      System.err.println("GetParams.getFont: Bad " + param + ": " + parfont);
      System.exit(-1);
    }
    String fontname = parfont.substring(0, colonpos-1);
    String sizestr = parfont.substring(colonpos+1);
    try {
      Integer size = new Integer(sizestr);
      System.err.println("Setting font " + param + " to '" + fontname + "', size " + size);
      return new Font(fontname, deftype, size.intValue());
    } catch (NumberFormatException e) {
      System.err.println("GetParams.getFont: bad font size for " + param 
			 + ": " + sizestr);
      System.exit(-1);
    }    
    return null;
  }
}
