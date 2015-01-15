package scala.pickling;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/** Annotates the known subclasses of a type for pickling
 * purposes; if this annotation is present, we consider the type
 * to be 'closed' and can pickle it statically even if it isn't
 * sealed. We trust this annotation completely, using it in
 * preference to looking at "isSealed" or "knownDirectSubclasses"
 * reflection APIs ourselves. It's fine to list only some
 * subclasses of the annotated class, but not a good idea to list
 * non-subclasses.
 */
@Retention(RetentionPolicy.CLASS)
public @interface directSubclasses {
    public Class<?>[] value();
}
