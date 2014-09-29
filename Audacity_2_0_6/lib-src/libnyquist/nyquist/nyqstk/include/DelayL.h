/***************************************************/
/*! \class DelayL
    \brief STK linear interpolating delay line class.

    This Delay subclass implements a fractional-
    length digital delay-line using first-order
    linear interpolation.  A fixed maximum length
    of 4095 and a delay of zero is set using the
    default constructor.  Alternatively, the
    delay and maximum length can be set during
    instantiation with an overloaded constructor.

    Linear interpolation is an efficient technique
    for achieving fractional delay lengths, though
    it does introduce high-frequency signal
    attenuation to varying degrees depending on the
    fractional delay setting.  The use of higher
    order Lagrange interpolators can typically
    improve (minimize) this attenuation characteristic.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_DELAYL_H
#define STK_DELAYL_H

#include "Delay.h"

namespace Nyq
{

class DelayL : public Delay
{
public:

  //! Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
  DelayL();

  //! Overloaded constructor which specifies the current and maximum delay-line lengths.
  /*!
    An StkError will be thrown if the delay parameter is less than
    zero, the maximum delay parameter is less than one, or the delay
    parameter is greater than the maxDelay value.
   */  
  DelayL(StkFloat delay, unsigned long maxDelay);

  //! Class destructor.
  ~DelayL();

  //! Set the delay-line length.
  /*!
    The valid range for \e theDelay is from 0 to the maximum delay-line length.
  */
  void setDelay(StkFloat delay);

  //! Return the current delay-line length.
  StkFloat getDelay(void) const;

  //! Return the value which will be output by the next call to tick().
  /*!
    This method is valid only for delay settings greater than zero!
   */
  StkFloat nextOut(void);

 protected:

  StkFloat computeSample( StkFloat input );

  StkFloat alpha_;
  StkFloat omAlpha_;
  StkFloat nextOutput_;
  bool doNextOut_;
};

} // namespace Nyq

#endif
