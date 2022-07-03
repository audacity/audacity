/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER__
#define __AUDACITY_CUSTOM_UPDATER__

#include "RulerUpdater.h"

class CustomUpdater : public RulerUpdater {
public:
   explicit CustomUpdater(const Ruler& ruler, const ZoomInfo* z)
      : RulerUpdater{ ruler, NULL }
   {}
   ~CustomUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs
   ) const override;
};

#endif
