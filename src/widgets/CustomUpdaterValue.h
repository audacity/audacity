/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdaterPosition.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER_VALUE__
#define __AUDACITY_CUSTOM_UPDATER_VALUE__

#include "CustomUpdater.h"

struct CustomUpdaterValue final : public CustomUpdater {
   using CustomUpdater::CustomUpdater;
   ~CustomUpdaterValue() override;

   bool TickCustom(wxDC& dc, int labelIdx, wxFont font,
      TickOutputs outputs,
      const RulerStruct& context
   ) const override;
};

#endif
