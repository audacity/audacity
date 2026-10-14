/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ClipInterface.h"
#include "au3-utility/Observer.h"

enum class ClipSide {
    Left,
    Right
};

//! Fade lengths of a clip, in seconds of play time
//! As a clip attachment, it is copied along with the clip (split, paste, undo history)
//! and saved as attributes of the clip in the project file.
class ClipFades final : public ClipInterfaceAttachment
{
public:
    explicit ClipFades(ClipInterface& clip);

    static ClipFades& Get(ClipInterface& clip);
    static const ClipFades& Get(const ClipInterface& clip);

    std::unique_ptr<ClipInterfaceAttachment> Clone() const override;
    void Reparent(ClipInterface& clip) override;

    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    double fadeIn() const;
    void setFadeIn(double);

    double fadeOut() const;
    void setFadeOut(double);

    void setDraggingSide(ClipSide);

private:
    double playDuration() const;

    ClipInterface* m_clip = nullptr;

    double m_fadeIn = 0.0;
    double m_fadeOut = 0.0;
    ClipSide m_draggingSide = static_cast<ClipSide>(0);

    Observer::Subscription mStretchChangedSubscription;
};
