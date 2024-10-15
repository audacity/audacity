/***************************************************/
/*! \class SineWave
    \brief STK sinusoid oscillator class.

    This class computes and saves a static sine "table" that can be
    shared by multiple instances.  It has an interface similar to the
    WaveLoop class but inherits from the Generator class.  Output
    values are computed using linear interpolation.

    The "table" length, set in SineWave.h, is 2048 samples by default.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_SINEWAVE_H
#define STK_SINEWAVE_H

namespace Nyq
{

const unsigned long TABLE_SIZE = 2048;

} // namespace Nyq

#include "Generator.h"

namespace Nyq
{

class SineWave : public Generator
{
public:
  //! Default constructor.
  SineWave( void );

  //! Class destructor.
  virtual ~SineWave( void );

  //! Clear output and reset time pointer to zero.
  void reset( void );

  //! Set the data read rate in samples.  The rate can be negative.
  /*!
    If the rate value is negative, the data is read in reverse order.
  */
  void setRate( StkFloat rate ) { rate_ = rate; };

  //! Set the data interpolation rate based on a looping frequency.
  /*!
    This function determines the interpolation rate based on the file
    size and the current Stk::sampleRate.  The \e frequency value
    corresponds to file cycles per second.  The frequency can be
    negative, in which case the loop is read in reverse order.
   */
  void setFrequency( StkFloat frequency );

  //! Increment the read pointer by \e time samples, modulo file size.
  void addTime( StkFloat time );

  //! Increment current read pointer by \e angle, relative to a looping frequency.
  /*!
    This function increments the read pointer based on the file
    size and the current Stk::sampleRate.  The \e anAngle value
    is a multiple of file size.
   */
  void addPhase( StkFloat angle );

  //! Add a phase offset to the current read pointer.
  /*!
    This function determines a time offset based on the file
    size and the current Stk::sampleRate.  The \e angle value
    is a multiple of file size.
   */
  void addPhaseOffset( StkFloat angle );

protected:

  StkFloat computeSample( void );

  static StkFrames table_;
  StkFloat time_;
  StkFloat rate_;
  StkFloat phaseOffset_;

};

} // namespace Nyq

#endif
