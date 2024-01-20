package specification

trait Identity[Morphism[_, _]]:

  // declared

  def ι[Z]: Morphism[Z, Z]
