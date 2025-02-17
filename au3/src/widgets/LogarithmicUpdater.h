/**********************************************************************

  Audacity: A Digital Audio Editor

  LogarithmicUpdater.h

  Dominic Mazzoni
  Michael Papadopoulos split from Ruler.h

**********************************************************************/

#ifndef __AUDACITY_LOGARITHMIC_UPDATER__
#define __AUDACITY_LOGARITHMIC_UPDATER__

#include "GeneratedUpdater.h"

class LogarithmicUpdater final : public GeneratedUpdater
{
public:
    static const LogarithmicUpdater& Instance();

    ~LogarithmicUpdater() override;

    void Update(
        wxDC& dc, const Envelope* envelope, UpdateOutputs& allOutputs, const RulerStruct& context) const override;

private:
    LogarithmicUpdater() = default;
};

#endif
