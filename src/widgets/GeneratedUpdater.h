/**********************************************************************

  Audacity: A Digital Audio Editor

  GeneratedUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_GENERATED_UPDATER__
#define __AUDACITY_GENERATED_UPDATER__

#include "RulerUpdater.h"

struct GeneratedUpdater : public RulerUpdater {
   using RulerUpdater::RulerUpdater;
   virtual ~GeneratedUpdater() override = 0;

   virtual void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct& context, const std::any& data
   ) const override = 0;

   bool Tick(wxDC& dc,
      int pos, double d, const TickSizes& tickSizes, wxFont font,
      TickOutputs outputs,
      const RulerStruct& context
   ) const;
};

#endif
