/**********************************************************************

  Audacity: A Digital Audio Editor

  LogarithmicUpdater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LOGARITHMIC_UPDATER__
#define __AUDACITY_LOGARITHMIC_UPDATER__

#include "Updater.h"

struct LogarithmicUpdater : public Updater {
   explicit LogarithmicUpdater(const Ruler& ruler, const ZoomInfo* z)
      : Updater{ ruler, NULL }
   {}
   ~LogarithmicUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs
   ) const override;
};

#endif

