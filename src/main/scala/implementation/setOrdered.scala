package implementation

import specification.{Ordered}

given setOrdered[Z]: Ordered[Set[Z]] with

  extension (lzs: Set[Z]) def <(rzs: Set[Z]): Boolean =
    (lzs subsetOf rzs) && (lzs != rzs)
