#pragma once

#include "ClipInterface.h"
#include "au3-utility/Observer.h"

#include <atomic>

enum class ClipSide {
    Left,
    Right
};

//! Published whenever the fades of a clip change
struct ClipFadesChange {};

//! Fade lengths of a clip, in seconds of play time
//! As a clip attachment, it is copied along with the clip (split, paste, undo history)
//! and saved as attributes of the clip in the project file.
class STRETCHING_SEQUENCE_API ClipFades final : public ClipInterfaceAttachment, public Observer::Publisher<ClipFadesChange>
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

    //! The gain of linear fades (for now) at `t`, for a clip from `clipStart` to `clipEnd`
    static double gain(double t, double clipStart, double clipEnd, double fadeIn, double fadeOut);

    //! The gain of the fades at `timeFromPlayStart`, in seconds from the clip's play start
    double gainAt(double timeFromPlayStart) const;

    [[nodiscard]] Observer::Subscription SubscribeToChanges(std::function<void()> cb) const;

private:
    double playDuration() const;

    ClipInterface* m_clip = nullptr;

    // Atomic: written by the main thread, read by the audio thread, e.g. when the fades are dragged during playback.
    std::atomic<double> m_fadeIn = 0.0;
    std::atomic<double> m_fadeOut = 0.0;
    std::atomic<ClipSide> m_draggingSide = static_cast<ClipSide>(0);

    Observer::Subscription mStretchChangedSubscription;
};
