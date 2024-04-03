package specification

import scala.collection.immutable.Seq

trait Category[Morphism[_, _]]
    extends Composition[Morphism],
      Identity[Morphism]:

  // defined

  def composeAll[Z]: Function[Seq[Transition[Z]], Transition[Z]] =
    zτs => zτs `composeAllWith` ι

  // laws

  trait CategoryLaws[L[_]: Law]:

    def leftIdentity[Z, Y]: Morphism[Z, Y] => L[Morphism[Z, Y]] = zμy =>
      {
        ι`o` zμy
      } `=` {
        zμy
      }

    def rightIdentity[Z, Y]: Morphism[Z, Y] => L[Morphism[Z, Y]] =
      zμy =>
        {
          zμy `o` ι
        } `=` {
          zμy
        }
