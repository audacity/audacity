/***************************************************/
/*! \class WvIn
    \brief STK audio input abstract base class.

    This class provides common functionality for a variety of audio
    data input subclasses.

    WvIn supports multi-channel data.  It is important to distinguish
    the tick() methods, which return samples produced by averaging
    across sample frames, from the tickFrame() methods, which return
    references or pointers to multi-channel sample frames.

    Both interleaved and non-interleaved data is supported via the use
    of StkFrames objects.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_WVIN_H
#define STK_WVIN_H

#include "Stk.h"
#include <vector>

namespace Nyq
{

class WvIn : public Stk
{
public:
  //! Default constructor.
  WvIn();

  //! Class destructor.
  virtual ~WvIn();

  //! Return the number of audio channels in the data.
  unsigned int getChannels( void ) const { return data_.channels(); };

  //! Return the average across the last output sample frame.
  /*!
    If no file data is loaded, the returned value is 0.0.
  */
  StkFloat lastOut( void ) const;

  //! Return an StkFrames reference to the last output sample frame.
  /*!
    If no file data is loaded, an empty container is returned.
   */
  const StkFrames& lastFrame( void ) const { return lastOutputs_; };

  //! Read out the average across one sample frame of data.
  /*!
    If no file data is loaded, the returned value is 0.0.
  */
  StkFloat tick( void );

  //! Fill a channel of the StkFrames object with averaged sample frames.
  /*!
    The \c channel argument should be zero or greater (the first
    channel is specified by 0).  An StkError will be thrown if the \c
    channel argument is greater than or equal to the number of
    channels in the StkFrames object.  If no file data is loaded, the
    container is filled with zeroes.
  */
  StkFrames& tick( StkFrames& frames, unsigned int channel = 0 );

  //! Fill the StkFrames argument with data and return the same reference.
  /*!
    An StkError will be thrown if there is an incompatability
    between the number of channels in the loaded data and that in the
    StkFrames argument.  If no file data is loaded, the container is
    filled with zeroes.
  */
  StkFrames& tickFrame( StkFrames& frames );

protected:

  // This abstract function must be implemented in all subclasses.
  // It is used to get around a C++ problem with overloaded virtual
  // functions.
  virtual void computeFrame( void ) = 0;

  StkFrames data_;
  StkFrames lastOutputs_;

};

} // namespace Nyq

#endif
