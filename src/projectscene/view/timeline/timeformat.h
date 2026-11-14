/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#pragma once

#include "view/timeline/irulerformat.h"

namespace au::projectscene {
class TimeFormat : public IRulerFormat
{
public:
    IntervalInfo intervalInfo(TimelineContext* context) override;
    QString label(double d, const IntervalInfo& intervalInfo, TickType tickType, TimelineContext* context = nullptr) override;
};
}
