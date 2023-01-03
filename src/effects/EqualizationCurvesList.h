/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationCurvesList.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_EQUALIZATION_CURVES_LIST__
#define __AUDACITY_EQUALIZATION_CURVES_LIST__

#include "EqualizationCurves.h"

class Envelope;
struct EqualizationFilter;

//! Maintains a list of preset curves for Equalization effects
struct EqualizationCurvesList {
   explicit EqualizationCurvesList(EqualizationFilter &params)
      : mParameters{ params }
   {}

   void EnvelopeUpdated();
   void EnvelopeUpdated(const Envelope &env, bool lin);
   void Select(int sel);

   void ForceRecalc() { mRecalcRequired = true; }

   EQCurveArray mCurves;
   EqualizationFilter &mParameters;
   bool mRecalcRequired{ false };
};
#endif
