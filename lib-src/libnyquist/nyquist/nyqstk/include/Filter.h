/***************************************************/
/*! \class Filter
    \brief STK filter class.

    This class implements a generic structure that
    can be used to create a wide range of filters.
    It can function independently or be subclassed
    to provide more specific controls based on a
    particular filter type.

    In particular, this class implements the standard
    difference equation:

    a[0]*y[n] = b[0]*x[n] + ... + b[nb]*x[n-nb] -
                a[1]*y[n-1] - ... - a[na]*y[n-na]

    If a[0] is not equal to 1, the filter coeffcients
    are normalized by a[0].

    The \e gain parameter is applied at the filter
    input and does not affect the coefficient values.
    The default gain value is 1.0.  This structure
    results in one extra multiply per computed sample,
    but allows easy control of the overall filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_FILTER_H
#define STK_FILTER_H

#include "Stk.h"
#include <vector>

namespace Nyq
{

class Filter : public Stk
{
public:
  //! Default constructor creates a zero-order pass-through "filter".
  Filter(void);

  //! Overloaded constructor which takes filter coefficients.
  /*!
    An StkError can be thrown if either of the coefficient vector
    sizes is zero, or if the a[0] coefficient is equal to zero.
  */
  Filter( std::vector<StkFloat> &bCoefficients, std::vector<StkFloat> &aCoefficients );

  //! Class destructor.
  virtual ~Filter(void);

  //! Sets all internal states of the filter to zero.
  void clear(void);

  //! Set filter coefficients.
  /*!
    An StkError can be thrown if either of the coefficient vector
    sizes is zero, or if the a[0] coefficient is equal to zero.  If
    a[0] is not equal to 1, the filter coeffcients are normalized by
    a[0].  The internal state of the filter is not cleared unless the
    \e clearState flag is \c true.
  */
  void setCoefficients( std::vector<StkFloat> &bCoefficients, std::vector<StkFloat> &aCoefficients, bool clearState = false );

  //! Set numerator coefficients.
  /*!
    An StkError can be thrown if coefficient vector is empty.  Any
    previously set denominator coefficients are left unaffected.  Note
    that the default constructor sets the single denominator
    coefficient a[0] to 1.0.  The internal state of the filter is not
    cleared unless the \e clearState flag is \c true.
  */
  void setNumerator( std::vector<StkFloat> &bCoefficients, bool clearState = false );

  //! Set denominator coefficients.
  /*!
    An StkError can be thrown if the coefficient vector is empty or
    if the a[0] coefficient is equal to zero.  Previously set
    numerator coefficients are unaffected unless a[0] is not equal to
    1, in which case all coeffcients are normalized by a[0].  Note
    that the default constructor sets the single numerator coefficient
    b[0] to 1.0.  The internal state of the filter is not cleared
    unless the \e clearState flag is \c true.
  */
  void setDenominator( std::vector<StkFloat> &aCoefficients, bool clearState = false );

  //! Set the filter gain.
  /*!
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   */
  virtual void setGain(StkFloat gain);

  //! Return the current filter gain.
  virtual StkFloat getGain(void) const;

  //! Return the last computed output value.
  virtual StkFloat lastOut(void) const;

  //! Input one sample to the filter and return one output.
  virtual StkFloat tick( StkFloat input );

  //! Take a channel of the StkFrames object as inputs to the filter and replace with corresponding outputs.
  /*!
    The \c channel argument should be zero or greater (the first
    channel is specified by 0).  An StkError will be thrown if the \c
    channel argument is equal to or greater than the number of
    channels in the StkFrames object.
  */
  virtual StkFrames& tick( StkFrames& frames, unsigned int channel = 0 );

protected:

  StkFloat gain_;
  std::vector<StkFloat> b_;
  std::vector<StkFloat> a_;
  std::vector<StkFloat> outputs_;
  std::vector<StkFloat> inputs_;

};

} // namespace Nyq

#endif
