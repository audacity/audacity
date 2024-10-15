/***************************************************/
/*! \class Delay
    \brief STK non-interpolating delay line class.

    This protected Filter subclass implements
    a non-interpolating digital delay-line.
    A fixed maximum length of 4095 and a delay
    of zero is set using the default constructor.
    Alternatively, the delay and maximum length
    can be set during instantiation with an
    overloaded constructor.
    
    A non-interpolating delay line is typically
    used in fixed delay-length applications, such
    as for reverberation.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_DELAY_H
#define STK_DELAY_H

#include "Filter.h"

namespace Nyq
{

class Delay : protected Filter
{
public:

  //! Default constructor creates a delay-line with maximum length of 4095 samples and zero delay.
  Delay();

  //! Overloaded constructor which specifies the current and maximum delay-line lengths.
  /*!
    An StkError will be thrown if the delay parameter is less than
    zero, the maximum delay parameter is less than one, or the delay
    parameter is greater than the maxDelay value.
   */
  Delay(unsigned long delay, unsigned long maxDelay);

  //! Class destructor.
  virtual ~Delay();

  //! Clears the internal state of the delay line.
  void clear();

  //! Set the maximum delay-line length.
  /*!
    This method should generally only be used during initial setup
    of the delay line.  If it is used between calls to the tick()
    function, without a call to clear(), a signal discontinuity will
    likely occur.  If the current maximum length is greater than the
    new length, no change will be made.
  */
  void setMaximumDelay(unsigned long delay);

  //! Set the delay-line length.
  /*!
    The valid range for \e theDelay is from 0 to the maximum delay-line length.
  */
  void setDelay(unsigned long delay);

  //! Return the current delay-line length.
  unsigned long getDelay(void) const;

  //! Calculate and return the signal energy in the delay-line.
  StkFloat energy(void) const;

  //! Return the value at \e tapDelay samples from the delay-line input.
  /*!
    The tap point is determined modulo the delay-line length and is
    relative to the last input value (i.e., a tapDelay of zero returns
    the last input value).
  */
  StkFloat contentsAt(unsigned long tapDelay);

  //! Return the last computed output value.
  StkFloat lastOut(void) const;

  //! Return the value which will be output by the next call to tick().
  /*!
    This method is valid only for delay settings greater than zero!
   */
  virtual StkFloat nextOut(void);

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

  unsigned long inPoint_;
  unsigned long outPoint_;
  StkFloat delay_;
};

} // namespace Nyq

#endif

