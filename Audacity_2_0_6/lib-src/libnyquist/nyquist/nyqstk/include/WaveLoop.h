/***************************************************/
/*! \class WaveLoop
    \brief STK waveform oscillator class.

    This class inherits from FileWvIn and provides audio file looping
    functionality.  Any audio file that can be loaded by FileRead can
    be looped using this class.

    WaveLoop supports multi-channel data.  It is important to
    distinguish the tick() methods, which return samples produced by
    averaging across sample frames, from the tickFrame() methods,
    which return references or pointers to multi-channel sample
    frames.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_WAVELOOP_H
#define STK_WAVELOOP_H

#include "FileWvIn.h"

namespace Nyq
{

class WaveLoop : public FileWvIn
{
public:
  //! Default constructor.
  WaveLoop( unsigned long chunkThreshold = 1000000, unsigned long chunkSize = 1024 );

  //! Class constructor that opens a specified file.
  WaveLoop( std::string fileName, bool raw = false, bool doNormalize = true,
            unsigned long chunkThreshold = 1000000, unsigned long chunkSize = 1024 );

  //! Class destructor.
  virtual ~WaveLoop();

  //! Open the specified file and load its data.
  /*!
    Data from a previously opened file will be overwritten by this
    function.  An StkError will be thrown if the file is not found,
    its format is unknown, or a read error occurs.  If the file data
    is to be loaded incrementally from disk and normalization is
    specified, a scaling will be applied with respect to fixed-point
    limits.  If the data format is floating-point, no scaling is
    performed.
  */
  void openFile( std::string fileName, bool raw = false, bool doNormalize = true );

  //! Set the data read rate in samples.  The rate can be negative.
  /*!
    If the rate value is negative, the data is read in reverse order.
  */
  void setRate( StkFloat rate );

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

  virtual void computeFrame( void );

  StkFrames firstFrame_;
  StkFloat phaseOffset_;

};

} // namespace Nyq

#endif
