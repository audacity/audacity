/*
 * Audacity: A Digital Audio Editor
 */
#include "axislabel.h"

#include "framework/global/translation.h"

#include <QString>

#include <algorithm>

namespace au {
namespace {
std::string valueToLabel(double value, int decimalDigits)
{
    if (value >= 1000 && decimalDigits < 3) {
        auto label = QString::number(value / 1000, 'f', decimalDigits);
        // Trim trailing zeros and a trailing dot, without using regex.
        while (((label.contains('.') && (label.endsWith('0'))) || label.endsWith('.'))) {
            label.chop(1);
        }
        return label.toStdString() + muse::trc("axis", "k");
    } else {
        return QString::number(value, 'f', decimalDigits).toStdString();
    }
}

bool mustBeRemoved(const shared::AxisTick& tick, const shared::AxisTick& prevTick, const shared::AxisTick* nextTick, double minDistance)
{
    const auto distanceToPrev = std::abs(tick.position - prevTick.position);
    if (distanceToPrev < minDistance) {
        return true;
    }
    if (nextTick) {
        const auto distanceToNext = std::abs(nextTick->position - tick.position);
        if (distanceToNext < minDistance) {
            return true;
        }
    }
    return false;
}

struct TickAndIndex {
    shared::AxisTick tick;
    int index = 0;
};

void removeOverlappingTicks(std::vector<TickAndIndex>& ticks, double labelSize, double axisSize)
{
    if (ticks.size() <= 1) {
        // Nothing to do
    } else if (ticks.size() == 2 && mustBeRemoved(ticks[1].tick, ticks[0].tick, nullptr, labelSize / axisSize)) {
        // Special case: the last tick may be removed.
        ticks.pop_back();
    } else {
        // First and last ticks must be kept.
        auto it = ticks.begin() + 1;
        while (it != ticks.end() - 1) {
            if (mustBeRemoved(it->tick, (it - 1)->tick, &((it + 1)->tick), labelSize / axisSize)) {
                it = ticks.erase(it);
            } else {
                ++it;
            }
        }
    }
}
}

std::vector<std::string> shared::labelsForTicks(const std::vector<AxisTick>& ticks, double labelSize, double axisSize,
                                                int maxDecimalDigits)
{
    if (ticks.empty()) {
        return {};
    }

    std::vector<TickAndIndex> ticksWithIndex;
    ticksWithIndex.reserve(ticks.size());
    std::transform(ticks.begin(), ticks.end(), std::back_inserter(ticksWithIndex), [index = 0](const AxisTick& tick) mutable {
        return TickAndIndex { tick, index++ };
    });
    removeOverlappingTicks(ticksWithIndex, labelSize, axisSize);

    std::vector<std::string> labels;
    labels.resize(ticks.size());
    int numDecimalDigits = 0;
    while (numDecimalDigits <= maxDecimalDigits) {
        for (const auto& tick : ticksWithIndex) {
            labels[tick.index] = valueToLabel(tick.tick.val, numDecimalDigits);
        }
        const auto someLabelsAreTheSame = std::adjacent_find(labels.begin(), labels.end(), [](const std::string& a, const std::string& b) {
            return !(a.empty() || b.empty()) && a == b;
        }) != labels.end();
        if (!someLabelsAreTheSame) {
            break;
        }
        ++numDecimalDigits;
    }
    return labels;
}
}
