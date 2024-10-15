/***************************************************/
/*! \class PoleZero
    \brief STK one-pole, one-zero filter class.

    This protected Filter subclass implements
    a one-pole, one-zero digital filter.  A
    method is provided for creating an allpass
    filter with a given coefficient.  Another
    method is provided to create a DC blocking filter.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_POLEZERO_H
#define STK_POLEZERO_H

#include "Filter.h"

namespace Nyq
{

class PoleZero : protected Filter
{
 public:

  //! Default constructor creates a first-order pass-through filter.
  PoleZero();

  //! Class destructor.
  ~PoleZero();

  //! Clears the internal states of the filter.
  void clear(void);

  //! Set the b[0] coefficient value.
  void setB0(StkFloat b0);

  //! Set the b[1] coefficient value.
  void setB1(StkFloat b1);

  //! Set the a[1] coefficient value.
  void setA1(StkFloat a1);

  //! Set the filter for allpass behavior using \e coefficient.
  /*!
    This method uses \e coefficient to create an allpass filter,
    which has unity gain at all frequencies.  Note that the \e
    coefficient magnitude must be less than one to maintain stability.
  */
  void setAllpass(StkFloat coefficient);

  //! Create a DC blocking filter with the given pole position in the z-plane.
  /*!
    This method sets the given pole position, together with a zero
    at z=1, to create a DC blocking filter.  \e thePole should be
    close to one to minimize low-frequency attenuation.

  */
  void setBlockZero(StkFloat thePole = 0.99);

  //! Set the filter gain.
  /*!
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   */
  void setGain( StkFloat gain );

  //! Return the current filter gain.
  StkFloat getGain( void ) const;

  //! Return the last computed output value.
  StkFloat lastOut( void ) const;

  //! Input one sample to the filter and return one output.
  StkFloat tick( StkFloat sample );

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
