#include "playcursorcontroller.h"

using namespace au::projectscene;
using namespace muse::actions;

au::processing::secs_t PlayCursorController::timePosition() const
{
    return m_timePosition.val;
}

void PlayCursorController::setTimePosition(processing::secs_t newTimePosition)
{
    m_timePosition.set(newTimePosition);
}

muse::async::Channel<au::processing::secs_t> PlayCursorController::timePositionChanged() const
{
    return m_timePosition.changed;
}
