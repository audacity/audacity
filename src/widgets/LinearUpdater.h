/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearUpdater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LINEAR_UPDATER__
#define __AUDACITY_LINEAR_UPDATER__

#include "Updater.h"

struct LinearUpdater final : public Updater {
   using Updater::Updater;
   ~LinearUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct& context
   ) const override;
};

#endif
