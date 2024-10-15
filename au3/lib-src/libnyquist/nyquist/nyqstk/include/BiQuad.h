/***************************************************/
/*! \class BiQuad
    \brief STK biquad (two-pole, two-zero) filter class.

    This protected Filter subclass implements a
    two-pole, two-zero digital filter.  A method
    is provided for creating a resonance in the
    frequency response while maintaining a constant
    filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_BIQUAD_H
#define STK_BIQUAD_H

#include "Filter.h"

namespace Nyq
{

class BiQuad : protected Filter
{
public:

  //! Default constructor creates a second-order pass-through filter.
  BiQuad();

  //! Class destructor.
  virtual ~BiQuad();

  //! Clears all internal states of the filter.
  void clear(void);

  //! Set the b[0] coefficient value.
  void setB0(StkFloat b0);

  //! Set the b[1] coefficient value.
  void setB1(StkFloat b1);

  //! Set the b[2] coefficient value.
  void setB2(StkFloat b2);

  //! Set the a[1] coefficient value.
  void setA1(StkFloat a1);

  //! Set the a[2] coefficient value.
  void setA2(StkFloat a2);

  //! Sets the filter coefficients for a resonance at \e frequency (in Hz).
  /*!
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  If \e normalize is true,
    the filter zeros are placed at z = 1, z = -1, and the coefficients
    are then normalized to produce a constant unity peak gain
    (independent of the filter \e gain parameter).  The resulting
    filter frequency response has a resonance at the given \e
    frequency.  The closer the poles are to the unit-circle (\e radius
    close to one), the narrower the resulting resonance width.
  */
  void setResonance(StkFloat frequency, StkFloat radius, bool normalize = false);

  //! Set the filter coefficients for a notch at \e frequency (in Hz).
  /*!
    This method determines the filter coefficients corresponding to
    two complex-conjugate zeros with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  No filter normalization
    is attempted.
  */
  void setNotch(StkFloat frequency, StkFloat radius);

  //! Sets the filter zeroes for equal resonance gain.
  /*!
    When using the filter as a resonator, zeroes places at z = 1, z
    = -1 will result in a constant gain at resonance of 1 / (1 - R),
    where R is the pole radius setting.

  */
  void setEqualGainZeroes();

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
  virtual StkFloat tick(StkFloat sample);

  //! Take a channel of the StkFrames object as inputs to the filter and replace with corresponding outputs.
  /*!
    The \c channel argument should be zero or greater (the first
    channel is specified by 0).  An StkError will be thrown if the \c
    channel argument is equal to or greater than the number of
    channels in the StkFrames object.
  */
  virtual StkFrames& tick( StkFrames& frames, unsigned int channel = 0 );

 protected:

  // This function must be implemented in all subclasses. It is used
  // to get around a C++ problem with overloaded virtual functions.
  virtual StkFloat computeSample( StkFloat input );
};

} // namespace Nyq

#endif
