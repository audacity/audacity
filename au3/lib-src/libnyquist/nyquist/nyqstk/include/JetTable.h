/***************************************************/
/*! \class JetTable
    \brief STK jet table class.

    This class implements a flue jet non-linear
    function, computed by a polynomial calculation.
    Contrary to the name, this is not a "table".

    Consult Fletcher and Rossing, Karjalainen,
    Cook, and others for more information.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_JETTABL_H
#define STK_JETTABL_H

#include "Function.h"

namespace Nyq
{

class JetTable : public Function
{
public:
  //! Default constructor.
  JetTable();

  //! Class destructor.
  ~JetTable();

protected:

  StkFloat computeSample( StkFloat input );

};

} // namespace Nyq

#endif
