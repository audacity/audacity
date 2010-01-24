/***************************************************/
/*! \class Generator
    \brief STK abstract unit generator parent class.

    This class provides common functionality for
    STK unit generator sample-source subclasses.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_GENERATOR_H
#define STK_GENERATOR_H

#include "Stk.h"

namespace Nyq
{

class Generator : public Stk
{
 public:
  //! Class constructor.
  Generator( void );

  //! Class destructor.
  virtual ~Generator( void );

  //! Return the last output value.
  virtual StkFloat lastOut( void ) const { return lastOutput_; };

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

 protected:

  // This abstract function must be implemented in all subclasses.
  // It is used to get around a C++ problem with overloaded virtual
  // functions.
  virtual StkFloat computeSample( void ) = 0;

  StkFloat lastOutput_;

};

} // namespace Nyq

#endif

