package scala.pickling.javatest;

/**
 * An example "good" picklable class.
 */
public abstract class AbstractJavaClass implements java.io.Serializable {
    private final String value;
    private transient final String name;

    public AbstractJavaClass(String value, String name) {
        this.value = value;
        this.name = name;
    }
}
