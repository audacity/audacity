#pragma once

#include <chrono>
#include <vector>

namespace au::effects {
class Profiler
{
public:
    ~Profiler();

    class Scope
    {
    public:
        Scope(Profiler& profiler)
            : m_profiler(profiler),
            m_start(std::chrono::high_resolution_clock::now()) {}

        ~Scope()
        {
            const auto end = std::chrono::high_resolution_clock::now();
            m_profiler.m_callDurations.push_back(
                std::chrono::duration_cast<std::chrono::microseconds>(end - m_start));
        }

    private:
        Profiler& m_profiler;
        const std::chrono::high_resolution_clock::time_point m_start;
    };

private:
    std::vector<std::chrono::microseconds> m_callDurations;
};
} // namespace au::effects
