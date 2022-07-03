/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_LINEAR_UPDATER__
#define __AUDACITY_LINEAR_UPDATER__

#include "RulerUpdater.h"

class LinearUpdater final : public RulerUpdater {
public:
   using RulerUpdater::RulerUpdater;
   ~LinearUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct& context
   ) const override;
};

#endif
