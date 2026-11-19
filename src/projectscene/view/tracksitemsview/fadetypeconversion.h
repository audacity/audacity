/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cassert>

#include "trackedit/dom/clip.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
constexpr FadeTypes::Type toFadeTypesType(trackedit::FadeType type)
{
    switch (type) {
    case trackedit::FadeType::Linear:
        return FadeTypes::Type::LINEAR;
    case trackedit::FadeType::Exponential:
        return FadeTypes::Type::EXPONENTIAL;
    case trackedit::FadeType::Logarithmic:
        return FadeTypes::Type::LOGARITHMIC;
    case trackedit::FadeType::SquareRoot:
        return FadeTypes::Type::SQUARE_ROOT;
    case trackedit::FadeType::EqualPower:
        return FadeTypes::Type::EQUAL_POWER;
    case trackedit::FadeType::SCurve:
        return FadeTypes::Type::S_CURVE;
    default:
        assert(false);
        return FadeTypes::Type::LINEAR;
    }
}

constexpr trackedit::FadeType toTrackeditFadeType(FadeTypes::Type type)
{
    switch (type) {
    case FadeTypes::Type::EXPONENTIAL:
        return trackedit::FadeType::Exponential;
    case FadeTypes::Type::LOGARITHMIC:
        return trackedit::FadeType::Logarithmic;
    case FadeTypes::Type::SQUARE_ROOT:
        return trackedit::FadeType::SquareRoot;
    case FadeTypes::Type::EQUAL_POWER:
        return trackedit::FadeType::EqualPower;
    case FadeTypes::Type::S_CURVE:
        return trackedit::FadeType::SCurve;
    case FadeTypes::Type::LINEAR:
    default:
        return trackedit::FadeType::Linear;
    }
}

constexpr trackedit::ClipSide toTrackeditClipSide(ClipSides::Side side)
{
    return side == ClipSides::Side::Left ? trackedit::ClipSide::Left : trackedit::ClipSide::Right;
}
}
