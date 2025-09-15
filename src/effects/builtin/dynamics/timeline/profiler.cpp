/*
 * Audacity: A Digital Audio Editor
 */
#include "profiler.h"

#include <QDebug>
#include <algorithm>

namespace au::effects {
Profiler::~Profiler()
{
    if (!m_callDurations.empty()) {
        auto minIt
            =std::min_element(m_callDurations.begin(), m_callDurations.end());
        auto maxIt
            =std::max_element(m_callDurations.begin(), m_callDurations.end());
        double sum = 0.0;
        std::vector<double> durations;
        durations.reserve(m_callDurations.size());
        for (const auto& d : m_callDurations) {
            double val = static_cast<double>(d.count());
            sum += val;
            durations.push_back(val);
        }
        double avg = sum / durations.size();
        std::sort(durations.begin(), durations.end());
        double median;
        size_t n = durations.size();
        if (n % 2 == 0) {
            median = (durations[n / 2 - 1] + durations[n / 2]) / 2.0;
        } else {
            median = durations[n / 2];
        }
        qDebug("Call durations (us): min=%.2f max=%.2f avg=%.2f median=%.2f",
               static_cast<double>(minIt->count()),
               static_cast<double>(maxIt->count()), avg, median);
    }
}
} // namespace au::effects
