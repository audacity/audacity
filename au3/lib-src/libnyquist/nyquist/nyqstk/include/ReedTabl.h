/***************************************************/
/*! \class ReedTabl
    \brief STK reed table class.

    This class implements a simple one breakpoint,
    non-linear reed function, as described by
    Smith (1986).  This function is based on a
    memoryless non-linear spring model of the reed
    (the reed mass is ignored) which saturates when
    the reed collides with the mouthpiece facing.

    See McIntyre, Schumacher, & Woodhouse (1983),
    Smith (1986), Hirschman, Cook, Scavone, and
    others for more information.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/

#if !defined(__REEDTABL_H)
#define __REEDTABL_H

#include "Stk.h"

class ReedTabl : public Stk
{
public:
  //! Default constructor.
  ReedTabl();

  //! Class destructor.
  ~ReedTabl();

  //! Set the table offset value.
  /*!
    The table offset roughly corresponds to the size
    of the initial reed tip opening (a greater offset
    represents a smaller opening).
  */
  void setOffset(MY_FLOAT aValue);

  //! Set the table slope value.
  /*!
   The table slope roughly corresponds to the reed
   stiffness (a greater slope represents a harder
   reed).
  */
  void setSlope(MY_FLOAT aValue);

  //! Return the last output value.
  MY_FLOAT lastOut() const;

  //! Return the function value for \e input.
  /*!
    The function input represents the differential
    pressure across the reeds.
  */
  MY_FLOAT tick(MY_FLOAT input);

  //! Take \e vectorSize inputs and return the corresponding function values in \e vector.
  MY_FLOAT *tick(MY_FLOAT *vector, unsigned int vectorSize);

protected:  
  MY_FLOAT offSet;
  MY_FLOAT slope;
  MY_FLOAT lastOutput;

};

#endif
