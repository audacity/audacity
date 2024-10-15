/***************************************************/
/*! \class OnePole
    \brief STK one-pole filter class.

    This protected Filter subclass implements
    a one-pole digital filter.  A method is
    provided for setting the pole position along
    the real axis of the z-plane while maintaining
    a constant peak filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_ONEPOLE_H
#define STK_ONEPOLE_H

#include "Filter.h"

namespace Nyq
{

class OnePole : protected Filter
{
public:

  //! Default constructor creates a first-order low-pass filter.
  OnePole();

  //! Overloaded constructor which sets the pole position during instantiation.
  OnePole( StkFloat thePole );

  //! Class destructor.
  ~OnePole();

  //! Clears the internal state of the filter.
  void clear(void);

  //! Set the b[0] coefficient value.
  void setB0(StkFloat b0);

  //! Set the a[1] coefficient value.
  void setA1(StkFloat a1);

  //! Set the pole position in the z-plane.
  /*!
    This method sets the pole position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive pole value produces a low-pass filter, while a negative
    pole value produces a high-pass filter.  This method does not
    affect the filter \e gain value.
  */
  void setPole(StkFloat thePole);

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
