/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER__
#define __AUDACITY_CUSTOM_UPDATER__

#include "Updater.h"

struct CustomUpdater : public Updater {
   explicit CustomUpdater(const ZoomInfo* z = nullptr)
      : Updater{ z }
   {}
   ~CustomUpdater() override;

   void Update(
      wxDC& dc, const Envelope* envelope,
      UpdateOutputs& allOutputs, const RulerStruct& context
   ) const override;

   bool TickCustom(wxDC& dc, int labelIdx, wxFont font,
      TickOutputs outputs,
      const RulerStruct& context
   ) const;
};

#endif
