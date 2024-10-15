/***************************************************/
/*! \class Instrmnt
    \brief STK instrument abstract base class.

    This class provides a common interface for
    all STK instruments.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_INSTRMNT_H
#define STK_INSTRMNT_H

#include "Stk.h"

namespace Nyq
{

class Instrmnt : public Stk
{
 public:
  //! Default constructor.
  Instrmnt();

  //! Class destructor.
  virtual ~Instrmnt();

  //! Start a note with the given frequency and amplitude.
  virtual void noteOn(StkFloat frequency, StkFloat amplitude) = 0;

  //! Stop a note with the given amplitude (speed of decay).
  virtual void noteOff(StkFloat amplitude) = 0;

  //! Set instrument parameters for a particular frequency.
  virtual void setFrequency(StkFloat frequency);

  //! Return the last output value.
  StkFloat lastOut() const;

  //! Return the last left output value.
  StkFloat lastOutLeft() const;

  //! Return the last right output value.
  StkFloat lastOutRight() const;

  //! Compute one sample and output.
  StkFloat tick( void );

  //! Fill a channel of the StkFrames object with computed outputs.
  /*!
    The \c channel argument should be zero or greater (the first
    channel is specified by 0).  An StkError will be thrown if the \c
    channel argument is equal to or greater than the number of
    channels in the StkFrames object.
  */
  StkFrames& tick( StkFrames& frames, unsigned int channel = 0 );

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
  virtual void controlChange(int number, StkFloat value);

 protected:

  // This abstract function must be implemented in all subclasses.
  // It is used to get around a C++ problem with overloaded virtual
  // functions.
  virtual StkFloat computeSample( void ) = 0;

  StkFloat lastOutput_;

};

} // namespace Nyq

#endif
