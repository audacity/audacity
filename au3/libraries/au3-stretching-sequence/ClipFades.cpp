#include "ClipFades.h"

#include "au3-xml/XMLAttributeValueView.h"
#include "au3-xml/XMLWriter.h"

#include <algorithm>

static constexpr auto FadeIn_attr = "fadeIn";
static constexpr auto FadeOut_attr = "fadeOut";

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
