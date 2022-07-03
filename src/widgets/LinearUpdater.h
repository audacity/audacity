/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearUpdater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LINEAR_UPDATER__
#define __AUDACITY_LINEAR_UPDATER__

#include "Updater.h"

struct LinearUpdater : public Updater {
   explicit LinearUpdater(const Ruler& ruler, const ZoomInfo* z)
      : Updater{ ruler, z }
   {}
   ~LinearUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs
   ) const override;
};

#endif
