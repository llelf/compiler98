import java.net.*;
import java.io.*;

public class URLTest {
  public static void main(String[] args) throws IOException {
    try {
      URL url = new URL("http://www.cs.chalmers.se/~sparud/apa");
      URLConnection uc = url.openConnection();
      uc.setDoInput(true);
      uc.connect();
      BufferedReader reader = 
	  new BufferedReader (new InputStreamReader (uc.getInputStream()));
      String line;
      while ((line = reader.readLine()) != null) {
	System.out.println(">>> " + line);
      }
    } catch (IOException e) {
      System.err.println(""+e);
    }
  }
}
