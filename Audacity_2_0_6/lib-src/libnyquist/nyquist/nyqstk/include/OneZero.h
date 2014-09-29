/***************************************************/
/*! \class OneZero
    \brief STK one-zero filter class.

    This protected Filter subclass implements
    a one-zero digital filter.  A method is
    provided for setting the zero position
    along the real axis of the z-plane while
    maintaining a constant filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_ONEZERO_H
#define STK_ONEZERO_H

#include "Filter.h"

namespace Nyq
{

class OneZero : protected Filter
{
 public:

  //! Default constructor creates a first-order low-pass filter.
  OneZero();

  //! Overloaded constructor which sets the zero position during instantiation.
  OneZero(StkFloat theZero);

  //! Class destructor.
  ~OneZero();

  //! Clears the internal state of the filter.
  void clear(void);

  //! Set the b[0] coefficient value.
  void setB0(StkFloat b0);

  //! Set the b[1] coefficient value.
  void setB1(StkFloat b1);

  //! Set the zero position in the z-plane.
  /*!
    This method sets the zero position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive zero value produces a high-pass filter, while a
    negative zero value produces a low-pass filter.  This method does
    not affect the filter \e gain value.
  */
  void setZero(StkFloat theZero);

  //! Set the filter gain.
  /*!
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   */
  void setGain(StkFloat gain);

  //! Return the current filter gain.
  StkFloat getGain(void) const;

  //! Return the last computed output value.
  StkFloat lastOut(void) const;

  //! Input one sample to the filter and return one output.
  StkFloat tick(StkFloat sample);

  //! Take a channel of the StkFrames object as inputs to the filter and replace with corresponding outputs.
  /*!
    The \c channel argument should be zero or greater (the first
    channel is specified by 0).  An StkError will be thrown if the \c
    channel argument is equal to or greater than the number of
    channels in the StkFrames object.
  */
  StkFrames& tick( StkFrames& frames, unsigned int channel = 0 );

};

} // namespace Nyq

#endif
