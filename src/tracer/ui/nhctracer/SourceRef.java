package nhctracer;

public class SourceRef {
    String file;
    int col, line;
    static final int DIVISOR = 10000;
    
    public SourceRef(String f, Integer rc) {
	file = f;
	col = rc.intValue() % DIVISOR;
	line = rc.intValue() / DIVISOR;
    }
}

