/**********************************************************************

  Audacity: A Digital Audio Editor

  LinearUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_LINEAR_UPDATER__
#define __AUDACITY_LINEAR_UPDATER__

#include "GeneratedUpdater.h"

class ZoomInfo;

class LinearUpdater final : public GeneratedUpdater
{
public:
    LinearUpdater() = default;

    // Always has default data values
    static const LinearUpdater& Instance();

    ~LinearUpdater() override;

    void Update(
        wxDC& dc, const Envelope* envelope, UpdateOutputs& allOutputs, const RulerStruct& context) const override;

    void SetData(const ZoomInfo* pZoomInfo = nullptr, int leftOffset = 0)
    {
        mpZoomInfo = pZoomInfo;
        mLeftOffset = 0;
    }

private:
    const ZoomInfo* mpZoomInfo{};
    int mLeftOffset{};
};

#endif
