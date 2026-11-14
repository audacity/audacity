/**********************************************************************

  Audacity: A Digital Audio Editor

  CustomUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_CUSTOM_UPDATER__
#define __AUDACITY_CUSTOM_UPDATER__

#include "RulerUpdater.h"

class CustomUpdater : public RulerUpdater
{
public:
    using RulerUpdater::RulerUpdater;
    ~CustomUpdater() override;

    void Update(
        wxDC& dc, const Envelope* envelope, UpdateOutputs& allOutputs, const RulerStruct& context) const final;

    void SetData(
        RulerUpdater::Labels majorLabels,
        RulerUpdater::Labels minorLabels,
        RulerUpdater::Labels minorMinorLabels)
    {
        mMajorLabels = move(majorLabels);
        mMinorLabels = move(minorLabels);
        mMinorMinorLabels = move(minorMinorLabels);
    }

protected:
    virtual bool TickCustom(wxDC& dc, int labelIdx, wxFont font, TickOutputs outputs, const RulerStruct& context) const = 0;

    RulerUpdater::Labels mMajorLabels, mMinorLabels, mMinorMinorLabels;
};

#endif
