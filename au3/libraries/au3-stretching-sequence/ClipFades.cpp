#include "ClipFades.h"

#include "au3-xml/XMLAttributeValueView.h"
#include "au3-xml/XMLWriter.h"

#include <algorithm>
#include <cmath>
#include <numbers>
#include <optional>
#include <string>

static constexpr auto FadeIn_attr = "fadeIn";
static constexpr auto FadeOut_attr = "fadeOut";
static constexpr auto FadeInType_attr = "fadeInType";
static constexpr auto FadeOutType_attr = "fadeOutType";

namespace {
//! 10^(floorDb * (1 - x) / 20) = exp(floorDb * ln(10) / 20 * (1 - x)), `exp` being cheaper than `pow`
constexpr auto logarithmicFadeExponent = ClipFades::logarithmicFadeFloorDb * std::numbers::ln10 / 20.0;

constexpr const char* fadeTypeName(FadeType type)
{
    switch (type) {
    case FadeType::Logarithmic:
        return "logarithmic";
    case FadeType::Linear:
        return "linear";
    default:
        assert(false);
        return "";
    }
}

constexpr std::optional<FadeType> fadeTypeFromName(const std::string& name)
{
    for (const auto type : { FadeType::Linear, FadeType::Logarithmic }) {
        if (name == fadeTypeName(type)) {
            return type;
        }
    }
    return std::nullopt;
}
}

static const ClipInterface::ClipAttachments::RegisteredFactory keyClipFades{
    [](ClipInterface& clip) { return std::make_unique<ClipFades>(clip); }
};

ClipFades::ClipFades(ClipInterface& clip)
{
    Reparent(clip);
}

ClipFades& ClipFades::Get(ClipInterface& clip)
{
    return clip.ClipAttachments::Get<ClipFades>(keyClipFades);
}

const ClipFades& ClipFades::Get(const ClipInterface& clip)
{
    return Get(const_cast<ClipInterface&>(clip));
}

std::unique_ptr<ClipInterfaceAttachment> ClipFades::Clone() const
{
    // Still refers to our clip: the copy of the clip reparents it.
    auto clone = std::make_unique<ClipFades>(*m_clip);
    clone->m_fadeIn = m_fadeIn.load();
    clone->m_fadeOut = m_fadeOut.load();
    clone->m_draggingSide = m_draggingSide.load();
    clone->m_fadeInType = m_fadeInType.load();
    clone->m_fadeOutType = m_fadeOutType.load();
    return clone;
}

void ClipFades::Reparent(ClipInterface& clip)
{
    m_clip = &clip;
    mStretchChangedSubscription = clip.SubscribeToStretchRatioChange(
        [this](double oldRatio, double newRatio) {
        const auto ratioChange = newRatio / oldRatio;
        m_fadeIn = m_fadeIn * ratioChange;
        m_fadeOut = m_fadeOut * ratioChange;
    });
}

double ClipFades::playDuration() const
{
    return m_clip->GetPlayEndTime() - m_clip->GetPlayStartTime();
}

void ClipFades::WriteXMLAttributes(XMLWriter& writer) const
{
    // Don't write the raw values, or we'd also have to write the `m_draggingSide` state.
    writer.WriteAttr(FadeIn_attr, fadeIn(), 8);
    writer.WriteAttr(FadeOut_attr, fadeOut(), 8);
    writer.WriteAttr(FadeInType_attr, wxString { fadeTypeName(fadeInType()) });
    writer.WriteAttr(FadeOutType_attr, wxString { fadeTypeName(fadeOutType()) });
}

bool ClipFades::HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView)
{
    double value;
    if (attr == FadeIn_attr) {
        if (!valueView.TryGet(value)) {
            return false;
        }
        m_fadeIn = value;
        return true;
    }
    if (attr == FadeOut_attr) {
        if (!valueView.TryGet(value)) {
            return false;
        }
        m_fadeOut = value;
        return true;
    }
    if (attr == FadeInType_attr || attr == FadeOutType_attr) {
        const auto type = fadeTypeFromName(valueView.ToString());
        if (!type) {
            return false;
        }
        (attr == FadeInType_attr ? m_fadeInType : m_fadeOutType) = *type;
        return true;
    }
    return false;
}

double ClipFades::fadeIn() const
{
    if (m_draggingSide == ClipSide::Left) {
        return std::clamp(m_fadeIn.load(), 0., playDuration());
    } else {
        // Leave room for the effective fade-out, not the raw one, which may exceed the clip, e.g. after trimming.
        return std::clamp(m_fadeIn.load(), 0., playDuration() - fadeOut());
    }
}

void ClipFades::setFadeIn(double value)
{
    m_fadeIn = std::clamp(value, 0., playDuration());
    m_draggingSide = ClipSide::Left;
}

double ClipFades::fadeOut() const
{
    if (m_draggingSide == ClipSide::Right) {
        return std::clamp(m_fadeOut.load(), 0., playDuration());
    } else {
        // Leave room for the effective fade-in, not the raw one, which may exceed the clip, e.g. after trimming.
        return std::clamp(m_fadeOut.load(), 0., playDuration() - fadeIn());
    }
}

void ClipFades::setFadeOut(double value)
{
    m_fadeOut = std::clamp(value, 0., playDuration());
    m_draggingSide = ClipSide::Right;
}

void ClipFades::setDraggingSide(ClipSide side)
{
    if (side == m_draggingSide) {
        return;
    }
    m_fadeIn = fadeIn();
    m_fadeOut = fadeOut();
    m_draggingSide = side;
}

double ClipFades::fadeGain(FadeType type, double x)
{
    // Positions may be marginally outside of the fade
    x = std::clamp(x, 0.0, 1.0);
    switch (type) {
    case FadeType::Logarithmic:
        return x == 0.0 ? 0.0 : std::exp(logarithmicFadeExponent * (1.0 - x));
    case FadeType::Linear:
    default:
        return x;
    }
}

double ClipFades::Snapshot::gainAt(double t) const
{
    auto gain = 1.0;
    if (fadeIn > 0 && t < fadeIn) {
        gain *= fadeGain(fadeInType, t / fadeIn);
    }
    if (fadeOut > 0 && t > playDuration - fadeOut) {
        gain *= fadeGain(fadeOutType, (playDuration - t) / fadeOut);
    }
    return gain;
}

ClipFades::Snapshot ClipFades::snapshot() const
{
    return { playDuration(), fadeIn(), fadeOut(), fadeInType(), fadeOutType() };
}

FadeType ClipFades::fadeInType() const
{
    return m_fadeInType;
}

void ClipFades::setFadeInType(FadeType type)
{
    m_fadeInType = type;
}

FadeType ClipFades::fadeOutType() const
{
    return m_fadeOutType;
}

void ClipFades::setFadeOutType(FadeType type)
{
    m_fadeOutType = type;
}
