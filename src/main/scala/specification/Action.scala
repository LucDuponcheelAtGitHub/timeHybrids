package specification

import scala.collection.immutable.Seq

trait Action[ActorMorphism[_, _]: Category, Morphism[_, _]: Category]:

  // declared

  def actionFunctor[Z]: Functor[ActorMorphism, Function, [Y] =>> Morphism[Z, Y]]

  // defined

  extension [Z, Y, X](yμx: ActorMorphism[Y, X])
    def a(zμy: Morphism[Z, Y]): Morphism[Z, X] =
      actionFunctor.φ(yμx)(zμy)

  type ActorTransition = [Z] =>> ActorMorphism[Z, Z]

  extension [Z, Y, X](τys: Seq[ActorTransition[Y]])
    def allActUpon(zμy: Morphism[Z, Y]): Morphism[Z, Y] =
      τys.foldRight(zμy)(_ `a` _)
