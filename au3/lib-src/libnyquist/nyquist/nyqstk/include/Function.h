/***************************************************/
/*! \class Function
    \brief STK abstract function parent class.

    This class provides common functionality for STK classes which
    implement tables or other types of input to output function
    mappings.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "Stk.h"

#ifndef STK_FUNCTION_H
#define STK_FUNCTION_H

namespace Nyq
{

class Function : public Stk
{
 public:
  //! Class constructor.
  Function();

  //! Class destructor.
  virtual ~Function();

  //! Return the last output value.
  virtual StkFloat lastOut() const { return lastOutput_; };

  //! Take one sample input and compute one sample of output.
  StkFloat tick( StkFloat input );

  //! Take a channel of the StkFrames object as inputs to the function and replace with corresponding outputs.
  /*!
    The \c channel argument should be zero or greater (the first
    channel is specified by 0).  An StkError will be thrown if the \c
    channel argument is equal to or greater than the number of
    channels in the StkFrames object.
  */
  virtual StkFrames& tick( StkFrames& frames, unsigned int channel = 0 );

 protected:

  // This abstract function must be implemented in all subclasses.
  // It is used to get around a C++ problem with overloaded virtual
  // functions.
  virtual StkFloat computeSample( StkFloat input ) = 0;

  StkFloat lastOutput_;

};

} // namespace Nyq

#endif

