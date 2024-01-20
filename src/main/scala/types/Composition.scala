package types

import specification.{Limit}

type Composition[Z] = Limit[[O] =>> CompositionEnum[Z, O]]
