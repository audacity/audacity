/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdaterPosition.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER_POSITION__
#define __AUDACITY_CUSTOM_UPDATER_POSITION__

#include "CustomUpdater.h"

struct CustomUpdaterPosition final : public CustomUpdater {
   using CustomUpdater::CustomUpdater;
   ~CustomUpdaterPosition() override;

   bool TickCustom(wxDC& dc, int labelIdx, wxFont font,
      TickOutputs outputs,
      const RulerStruct& context
   ) const override;
};

#endif
