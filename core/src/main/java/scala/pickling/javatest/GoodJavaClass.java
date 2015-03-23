package scala.pickling.javatest;

/**
 * An example "good" picklable class.
 */
public final class GoodJavaClass implements java.io.Serializable {
    private final String value;
    private transient final String name;

    public GoodJavaClass(String value, String name) {
        this.value = value;
        this.name = name;
    }
}
