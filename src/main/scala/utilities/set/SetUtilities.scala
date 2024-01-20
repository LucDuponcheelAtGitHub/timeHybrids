package utilities.set

def emptySet[Z]: Set[Z] = Set.empty

def singleton[Z]: Z => Set[Z] = z => emptySet + z

def tuple2ToSet[Z]: Tuple2[Z, Z] => Set[Z] =
  (z0, z1) => singleton(z0) union singleton(z1)

def choices[Z]: Set[Set[Z]] => Set[Set[Z]] =
  zss =>
    val zsl: List[Set[Z]] = zss.toList
    zsl match
      case Nil =>
        singleton(emptySet)
      case zs :: zsl =>
        for {
          z <- zs
          zs <- choices(zsl.toSet).toList
        } yield {
          zs + z
        }

def U[Z]: Set[Set[Z]] => Set[Z] =
  zss =>
    for {
      zs <- zss
      z <- zs
    } yield {
      z
    }

def all : Set[Boolean] => Boolean =
  _.foldRight(true)(_ && _)