/**********************************************************************

  Audacity: A Digital Audio Editor

  GeneratedUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_GENERATED_UPDATER__
#define __AUDACITY_GENERATED_UPDATER__

#include "RulerUpdater.h"

class GeneratedUpdater : public RulerUpdater
{
public:
    using RulerUpdater::RulerUpdater;
    virtual ~GeneratedUpdater() override = 0;

protected:
    bool Tick(wxDC& dc, int pos, double d, const TickSizes& tickSizes, wxFont font, TickOutputs outputs, const RulerStruct& context) const;

    double ComputeWarpedLength(const Envelope& env, double t0, double t1) const
    {
        return env.IntegralOfInverse(t0, t1);
    }
};

#endif
