/*
 * Audacity: A Digital Audio Editor
 */

#include "timespan.h"
#include "log.h"

namespace au::trackedit {
TimeSpan::TimeSpan(secs_t start, secs_t end)
    : m_start(std::move(start)), m_end(std::move(end))
{
    IF_ASSERT_FAILED(!muse::is_equal(start, end) && end > start) {
        LOGE() << "invalid time span: start=" << start << " end=" << end;
    }
}

TimeSpan::TimeSpan(double start, double end)
    : TimeSpan{secs_t { start }, secs_t { end }}
{}

secs_t TimeSpan::start() const
{
    return m_start;
}

secs_t TimeSpan::end() const { return m_end; }

secs_t TimeSpan::duration() const
{
    return m_end - m_start;
}

bool TimeSpan::operator==(const TimeSpan& other) const { return m_start == other.m_start && m_end == other.m_end; }

bool TimeSpan::operator!=(const TimeSpan& other) const { return !this->operator==(other); }
}
