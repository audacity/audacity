/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdaterPosition.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER_POSITION__
#define __AUDACITY_CUSTOM_UPDATER_POSITION__

#include "CustomUpdater.h"

class CustomUpdaterPosition final : public CustomUpdater
{
public:
    static const CustomUpdaterPosition& Instance();
    CustomUpdaterPosition() = default;
    ~CustomUpdaterPosition() override;

protected:
    bool TickCustom(wxDC& dc, int labelIdx, wxFont font, TickOutputs outputs, const RulerStruct& context) const override;
};

#endif
