#pragma once

#include "ClipInterface.h"
#include "au3-utility/Observer.h"

#include <atomic>

enum class ClipSide {
    Left,
    Right
};

enum class FadeType {
    Linear,
    Logarithmic,
};

//! Fade lengths of a clip, in seconds of play time
//! As a clip attachment, it is copied along with the clip (split, paste, undo history)
//! and saved as attributes of the clip in the project file.
class STRETCHING_SEQUENCE_API ClipFades final : public ClipInterfaceAttachment
{
public:
    explicit ClipFades(ClipInterface& clip);

    static constexpr double logarithmicFadeFloorDb = -60.0;

    //! `x` from 0 (silent end) to 1 (unfaded end)
    static double fadeGain(FadeType type, double x);

    //! Everything that defines the fades of a clip, at some point in time
    struct Snapshot {
        double playDuration = 0.0;
        double fadeIn = 0.0;
        double fadeOut = 0.0;
        FadeType fadeInType = FadeType::Linear;
        FadeType fadeOutType = FadeType::Linear;

        //! The gain of the fades at `timeFromPlayStart`, in seconds from the clip's play start
        double gainAt(double timeFromPlayStart) const;

        bool operator==(const Snapshot&) const = default;
    };

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

    FadeType fadeInType() const;
    void setFadeInType(FadeType);

    FadeType fadeOutType() const;
    void setFadeOutType(FadeType);

    void setDraggingSide(ClipSide);

    Snapshot snapshot() const;

private:
    double playDuration() const;

    ClipInterface* m_clip = nullptr;

    // Atomic: written by the main thread, read by the audio thread, e.g. when the fades are dragged during playback.
    std::atomic<double> m_fadeIn = 0.0;
    std::atomic<double> m_fadeOut = 0.0;
    std::atomic<ClipSide> m_draggingSide = static_cast<ClipSide>(0);
    std::atomic<FadeType> m_fadeInType = FadeType::Linear;
    std::atomic<FadeType> m_fadeOutType = FadeType::Linear;

    Observer::Subscription mStretchChangedSubscription;
};
