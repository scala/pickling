package scala.pickling.javafieldfail;

public final class FakeByte {
    private static final long serialVersionUID = 1L;
    private final byte value;

    public FakeByte(byte value) {
        this.value = value;
    }

    public byte byteValue() {
        return value;
    }
}
