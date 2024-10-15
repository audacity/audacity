/***************************************************/
/*! \class JCRev
    \brief John Chowning's reverberator class.

    This class is derived from the CLM JCRev
    function, which is based on the use of
    networks of simple allpass and comb delay
    filters.  This class implements three series
    allpass units, followed by four parallel comb
    filters, and two decorrelation delay lines in
    parallel at the output.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_JCREV_H
#define STK_JCREV_H

#include "Effect.h"
#include "Delay.h" 

namespace Nyq
{

class JCRev : public Effect
{
 public:
  //! Class constructor taking a T60 decay time argument (one second default value).
  JCRev( StkFloat T60 = 1.0 );

  //! Class destructor.
  ~JCRev();

  //! Reset and clear all internal state.
  void clear();

  //! Set the reverberation T60 decay time.
  void setT60( StkFloat T60 );

 protected:

  StkFloat computeSample( StkFloat input );

  Delay allpassDelays_[3];
  Delay combDelays_[4];
  Delay outLeftDelay_;
  Delay outRightDelay_;
  StkFloat allpassCoefficient_;
  StkFloat combCoefficient_[4];

};

} // namespace Nyq

#endif

