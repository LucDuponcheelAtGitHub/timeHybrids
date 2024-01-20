package timehybrids.specification

import specification.{Ordered}

trait Time[Moment]:

  given momentOrdered: Ordered[Moment]
  
  val momentOrderedVal = momentOrdered

  type MomentInterval = momentOrderedVal.Interval

  type MomentIntervalUtilities = momentOrderedVal.IntervalUtilities

  val momentIntervalUtilities: MomentIntervalUtilities

