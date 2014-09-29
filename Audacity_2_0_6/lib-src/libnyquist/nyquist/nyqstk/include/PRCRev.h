/***************************************************/
/*! \class PRCRev
    \brief Perry's simple reverberator class.

    This class is based on some of the famous
    Stanford/CCRMA reverbs (NRev, KipRev), which
    were based on the Chowning/Moorer/Schroeder
    reverberators using networks of simple allpass
    and comb delay filters.  This class implements
    two series allpass units and two parallel comb
    filters.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_PRCREV_H
#define STK_PRCREV_H

#include "Effect.h" 
#include "Delay.h" 

namespace Nyq
{

class PRCRev : public Effect
{
public:
  //! Class constructor taking a T60 decay time argument (one second default value).
  PRCRev( StkFloat T60 = 1.0 );

  //! Class destructor.
  ~PRCRev();

  //! Reset and clear all internal state.
  void clear();

  //! Set the reverberation T60 decay time.
  void setT60( StkFloat T60 );

protected:

  StkFloat computeSample( StkFloat input );

  Delay allpassDelays_[2];
  Delay combDelays_[2];
  StkFloat allpassCoefficient_;
  StkFloat combCoefficient_[2];

};

} // namespace Nyq

#endif

