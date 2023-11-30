package specification

trait MonadPlus[UTC[_]] extends Monad[UTC], Plus[UTC]:

  // laws

  trait MonadPlusLaws[L[_]: Law]:

    def mappingOverZero[Z, Y]: Function[Z, Y] => L[UTC[Y]] =
      zφy =>
        {
          for {
            z <- ζ[Z]
          } yield {
            zφy(z)
          }
        } `=` {
          ζ
        }

    def mappingOverPlus[Z, Y]: Function[Z, Y] => UTC[Z] => UTC[Z] => L[UTC[Y]] =
      zφy =>
        lutc =>
          rutc =>
            {
              for {
                z <- lutc `+` rutc
              } yield {
                zφy(z)
              }
            } `=` {
              {
                for {
                  lz <- lutc
                } yield {
                  zφy(lz)
                }
              } `+` {
                for {
                  lz <- rutc
                } yield {
                  zφy(lz)
                }
              }
            }

    def flatMappingWithIdentityOverZero[Z]: L[UTC[Z]] = {
      for {
        utcz <- ζ[UTC[Z]]
        z <- utcz
      } yield {
        identity(z)
      }
    } `=` {
      ζ
    }

    def flatMappingWithPlus[Z]: UTC[UTC[Z]] => UTC[UTC[Z]] => L[UTC[UTC[Z]]] =
      lutcutcz =>
        rutcutcz =>
          {
            for {
              lutcz <- lutcutcz
              rutcz <- rutcutcz
            } yield {
              lutcz `+` rutcz
            }
          } `=` {
            lutcutcz `+` rutcutcz
          }
