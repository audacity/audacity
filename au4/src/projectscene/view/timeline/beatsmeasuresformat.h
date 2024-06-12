/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#include "timeformat.h"

namespace au::projectscene {
class BeatsMeasuresFormat
{
public:
    explicit BeatsMeasuresFormat() {}
    ~BeatsMeasuresFormat();

    static IntervalInfo intervalInfo(TimelineContext* context);
    static QString label(double d, const IntervalInfo& timeIntervalInfo, TickType tickType, TimelineContext* context);
};
}
