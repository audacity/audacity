/***************************************************/
/*! \class Effect
    \brief STK abstract effects parent class.

    This class provides common functionality for
    STK effects subclasses.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Stk.h"

#ifndef STK_EFFECT_H
#define STK_EFFECT_H

namespace Nyq
{

class Effect : public Stk
{
 public:
  //! Class constructor.
  Effect();

  //! Class destructor.
  virtual ~Effect();

  //! Reset and clear all internal state.
  virtual void clear() = 0;

  //! Set the mixture of input and "effected" levels in the output (0.0 = input only, 1.0 = reverb only). 
  void setEffectMix(StkFloat mix);

  //! Return the last output value.
  StkFloat lastOut() const;

  //! Return the last left output value.
  StkFloat lastOutLeft() const;

  //! Return the last right output value.
  StkFloat lastOutRight() const;

  //! Take one sample input and compute one sample of output.
  StkFloat tick( StkFloat input );

  //! Take a channel of the StkFrames object as inputs to the effect and replace with corresponding outputs.
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
  virtual StkFloat computeSample( StkFloat input ) = 0;

  // Returns true if argument value is prime.
  bool isPrime( int number );

  StkFloat lastOutput_[2];
  StkFloat effectMix_;

};

} // namespace Nyq

#endif

