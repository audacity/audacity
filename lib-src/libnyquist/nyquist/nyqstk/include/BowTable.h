/***************************************************/
/*! \class BowTable
    \brief STK bowed string table class.

    This class implements a simple bowed string
    non-linear function, as described by Smith (1986).

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_BOWTABL_H
#define STK_BOWTABL_H

#include "Function.h"

namespace Nyq
{

class BowTable : public Function
{
public:
  //! Default constructor.
  BowTable();

  //! Class destructor.
  ~BowTable();

  //! Set the table offset value.
  /*!
    The table offset is a bias which controls the
    symmetry of the friction.  If you want the
    friction to vary with direction, use a non-zero
    value for the offset.  The default value is zero.
  */
  void setOffset(StkFloat offset);

  //! Set the table slope value.
  /*!
   The table slope controls the width of the friction
   pulse, which is related to bow force.
  */
  void setSlope(StkFloat slope);

protected:

  StkFloat computeSample( StkFloat input );

  StkFloat offset_;
  StkFloat slope_;

};

} // namespace Nyq

#endif
