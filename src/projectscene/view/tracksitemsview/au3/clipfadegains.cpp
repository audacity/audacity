/*
* Audacity: A Digital Audio Editor
*/
#include "clipfadegains.h"

#include "au3-stretching-sequence/ClipFades.h"

using namespace au::projectscene;

namespace {
::FadeType fadeType(FadeTypes::Type type)
{
    switch (type) {
    case FadeTypes::Type::LOGARITHMIC:
        return ::FadeType::Logarithmic;
    case FadeTypes::Type::LINEAR:
    default:
        return ::FadeType::Linear;
    }
}
}

ClipFadeGains::ClipFadeGains(QObject* parent)
    : QObject(parent)
{
}

QList<qreal> ClipFadeGains::gainsAt(FadeTypes::Type type, const QList<qreal>& positions) const
{
    const auto au3Type = fadeType(type);
    QList<qreal> gains;
    gains.reserve(positions.size());
    for (const auto position : positions) {
        gains.push_back(ClipFades::fadeGain(au3Type, position));
    }
    return gains;
}
