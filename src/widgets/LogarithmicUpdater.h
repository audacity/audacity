/**********************************************************************

  Audacity: A Digital Audio Editor

  LogarithmicUpdater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LOGARITHMIC_UPDATER__
#define __AUDACITY_LOGARITHMIC_UPDATER__

#include "RulerUpdater.h"

struct LogarithmicUpdater final : public RulerUpdater {
   using RulerUpdater::RulerUpdater;
   ~LogarithmicUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct& context
   ) const override;
};

#endif

