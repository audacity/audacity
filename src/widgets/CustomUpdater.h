/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER__
#define __AUDACITY_CUSTOM_UPDATER__

#include "Updater.h"

struct CustomUpdater : public Updater {
   explicit CustomUpdater(const Ruler& ruler, const ZoomInfo* z)
      : Updater{ ruler, NULL }
   {}

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs
   ) const override;
};

#endif
