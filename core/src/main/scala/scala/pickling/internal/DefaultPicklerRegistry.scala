package scala.pickling.internal

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.pickling.spi.PicklerRegistry._
import scala.reflect.runtime.universe.Mirror
import scala.pickling._
import scala.pickling.runtime.CustomRuntime
import scala.pickling.spi.{PicklerRegistry, RuntimePicklerGenerator}
import scala.pickling.util.ClassMapper

/** Default pickle registry just uses TrieMaps and delegates behavior to a runtime pickler generator. */
final class DefaultPicklerRegistry(generator: RuntimePicklerGenerator)
  extends PicklerRegistry with CustomRuntime {

  type PicklerGenerator = FastTypeTag[_] => Pickler[_]
  type UnpicklerGenerator = FastTypeTag[_] => Unpickler[_]

  private val picklerMap: mutable.Map[String, Pickler[_]] = new TrieMap[String, Pickler[_]]
  private val picklerGenMap: mutable.Map[String, PicklerGenerator] = new TrieMap[String, PicklerGenerator]
  private val unpicklerMap: mutable.Map[String, Unpickler[_]] = new TrieMap[String, Unpickler[_]]
  private val unpicklerGenMap: mutable.Map[String, UnpicklerGenerator] = new TrieMap[String, UnpicklerGenerator]

  val tupleGenerators: (PicklerGen[Any], UnpicklerGen[Any]) =
    (tuplePicklerGenerator.asInstanceOf[PicklerGen[Any]],
      tupleUnpicklerGenerator.asInstanceOf[UnpicklerGen[Any]])

  val templatesToRegister =
    Vector("scala.Tuple2" -> tupleGenerators) ++
      ClassMapper.specializedTupleNamesFor("scala.Tuple2")
        .map(_ -> tupleGenerators)

  registerTemplatesAtInit(templatesToRegister)

  override def genUnpickler(mirror: Mirror, tagKey: String)(implicit share: refs.Share): Unpickler[_] = {
    lookupUnpickler(tagKey) match {
      case Some(p) => p
      case None =>
        // TODO - This should probably just be taking the `tagKey` and no mirror or share, the mirror/share
        //        should be configured by the default runtime.
        val p = generator.genUnpickler(mirror, tagKey)
        registerUnpickler(tagKey, p)
        p
    }
  }
  def genPickler(classLoader: ClassLoader, clazz: Class[_], tag: FastTypeTag[_])(implicit share: refs.Share): Pickler[_] = {
    lookupPickler(tag.key) match {
      case Some(p) => p
      case None =>
        // TODO - genPickler should probably just be using the tag and `currentMirror` of internal.
        val p = generator.genPickler(classLoader, clazz, tag)
        registerPickler(tag.key, p)
        p
    }
  }

  /** Registers a pickler with this registry for future use. */
  override def registerPickler[T](key: String, p: Pickler[T]): Unit =
    picklerMap.put(key, p)

  /** Registers an unpickler with this registry for future use. */
  override def registerUnpickler[T](key: String, p: Unpickler[T]): Unit =
    unpicklerMap.put(key, p)

  override private[pickling] def clearRegisteredPicklerUnpicklerFor[T: FastTypeTag]: Unit = {
    val tag = implicitly[FastTypeTag[T]]
    picklerMap -= tag.key
    unpicklerMap -= tag.key
  }

  override val isLookupEnabled = true

  /** Checks the existence of a pickler ignoring the registered generators. */
  override def lookupExistingPickler(key: String): Option[Pickler[_]] =
    picklerMap.get(key)

  /** Checks the existence of an unpickler ignoring the registered generators. */
  override def lookupExistingUnpickler(key: String): Option[Unpickler[_]] =
    unpicklerMap.get(key)

  /** Checks the existence of an unpickler. */
  override def lookupUnpickler(key: String): Option[Unpickler[_]] = {
    unpicklerMap.get(key) match {
      case x: Some[Unpickler[_]] => x
      case None =>
        // Now we use the typeConstructor registry
       FastTypeTag(key) match {
          case a @ FastTypeTag(typename, args) =>
            unpicklerGenMap.get(typename) match {
              case Some(gen) =>
                // Generate, register and return a pickler
                val up = gen(a)
                registerUnpickler(key, up)
                Some(up)
              case None => None
            }
          case _ => None // This key is not an applied type.
        }
    }
  }

  /** Looks for a pickler with the given FastTypeTag string. */
  override def lookupPickler(key: String): Option[Pickler[_]] = {
    picklerMap.get(key) match {
      case x: Some[Pickler[_]] => x
      case None =>
        FastTypeTag(key) match {
          case a @ FastTypeTag(typename, _) =>
            picklerGenMap.get(typename) match {
              case Some(gen) =>
                // Genereate the pickler, register it with ourselves for future lookup, and return it.
                val up = gen(a)
                registerPickler(key, up)
                Some(up)
              case None => None
            }
          case _ => None // This key is not an applied type.
        }
    }

  }
  /** Registers a pickler and unpickler for a type with this registry for future use.
    *
    * @param key The type key for the pickler.
    *             Note: In reflective scenarios this may not include type
    *             parameters. In those situations, the pickler should be able
    *             to handle arbitrary (existential) type parameters.
    * @param p The unpickler to register.
    */
  override def registerPicklerUnpickler[T](key: String, p: (Pickler[T] with Unpickler[T])): Unit = {
    registerPickler(key, p)
    registerUnpickler(key, p)
  }

  /** Registers a function which can generate picklers for a given type constructor.
    *
    * @param typeConstructorKey  The type constructor.  e.g. "scala.List" for something that can make scala.List[A] picklers.
    * @param generator  A function which takes an applied type string (your type + arguments) and returns a pickler for
    *                   this type.
    */
  override def registerUnpicklerGenerator[T](typeConstructorKey: String, generator: UnpicklerGen[T]): Unit =
    unpicklerGenMap.put(typeConstructorKey, generator)


  /** Registers a function which can generate picklers for a given type constructor.
    *
    * @param typeConstructorKey  The type constructor.  e.g. "scala.List" for something that can make scala.List[A] picklers.
    * @param generator  A function which takes an applied type string (your type + arguments) and returns a pickler for
    *                   this type.
    */
  override def registerPicklerGenerator[T](typeConstructorKey: String, generator: PicklerGen[T]): Unit =
    picklerGenMap.put(typeConstructorKey, generator)

  /** Registers a function which can generate picklers for a given type constructor.
    *
    * @param typeConstructorKey  The type constructor.  e.g. "scala.List" for something that can make scala.List[A] picklers.
    * @param generator  A function which takes an applied type string (your type + arguments) and returns a pickler for
    *                   this type.
    */
  override def registerPicklerUnpicklerGenerator[T](typeConstructorKey: String, generator: PicklerUnpicklerGen[T]): Unit = {
    registerPicklerGenerator(typeConstructorKey, generator)
    registerUnpicklerGenerator(typeConstructorKey, generator)
  }

  /** Transfer the "state" between different [[scala.pickling.spi.PicklingRuntime]]s.
    *
    * Watch out, this operation is not thread-safe.
    *
    * Make a new [[scala.pickling.spi.PicklingRuntime]] aware of
    * the already registered [[Pickler]]s and [[Unpickler]]s present
    * in the one that will be replaced.
    */
  private[pickling] def dumpStateTo(r: PicklerRegistry): Unit = {

    for(p <- picklerMap) r.registerPickler(p._1, p._2.asInstanceOf[Pickler[Any]])
    for(p <- picklerGenMap) r.registerPicklerGenerator(p._1, p._2.asInstanceOf[PicklerGen[Any]])
    for(u <- unpicklerMap) r.registerUnpickler(u._1, u._2.asInstanceOf[Unpickler[Any]])
    for(u <- unpicklerGenMap) r.registerUnpicklerGenerator(u._1, u._2.asInstanceOf[UnpicklerGen[Any]])

  }

}
