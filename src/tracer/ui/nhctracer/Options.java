package nhctracer;

public class Options {
  public static Option memoise =
    new Option("Memoise unfolded structures", false);
  public static Option showcase =
    new Option("Show case/guard/if-redexes", true);
  public static Option tracecomm =
    new Option("Trace hat-connect communication", false);
  public static Option dumprefs =
    new Option("Print reference numbers when dumping trace structures", false);
  public static Option arrow =
    new Option("Paint cursor when printing to file", false);
  public static Option oarrow =
    new Option("Paint cursor in output window", false);
}
