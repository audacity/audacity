/***************************************************/
/*! \class Noise
    \brief STK noise generator.

    Generic random number generation using the
    C rand() function.  The quality of the rand()
    function varies from one OS to another.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_NOISE_H
#define STK_NOISE_H

#include "Generator.h"

namespace Nyq
{

class Noise : public Generator
{
public:

  //! Default constructor which seeds the random number generator with the system time.
  Noise();

  //! Constructor which seeds the random number generator with a given seed.
  /*!
    If the seed value is zero, the random number generator is
    seeded with the system time.
  */
  Noise( unsigned int seed );

  //! Class destructor.
  virtual ~Noise();

  //! Seed the random number generator with a specific seed value.
  /*!
    If no seed is provided or the seed value is zero, the random
    number generator is seeded with the current system time.
  */
  void setSeed( unsigned int seed = 0 );

protected:

  virtual StkFloat computeSample( void );

};

} // namespace Nyq

#endif
