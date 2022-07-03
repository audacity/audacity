/**********************************************************************

  Audacity: A Digital Audio Editor

  LogarithmicUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_LOGARITHMIC_UPDATER__
#define __AUDACITY_LOGARITHMIC_UPDATER__

#include "RulerUpdater.h"

class LogarithmicUpdater final : public RulerUpdater {
public:
   explicit LogarithmicUpdater(const Ruler& ruler, const ZoomInfo* z)
      : RulerUpdater{ ruler, NULL }
   {}
   ~LogarithmicUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs
   ) const override;
};

#endif

