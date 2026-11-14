/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file WaveformApperance.h

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_WAVEFORM_APPEARANCE__
#define __AUDACITY_WAVEFORM_APPEARANCE__

#include "WaveClip.h"
#include "WaveTrack.h"

//! Persistent appearance settings that apply to all channels of a track
class WaveformAppearance : public TrackAttachment
{
public:
    static WaveformAppearance& Get(WaveTrack& track);
    static const WaveformAppearance& Get(const WaveTrack& track);

    static WaveformAppearance& Get(WaveChannel& channel);
    static const WaveformAppearance& Get(const WaveChannel& channel);

    explicit WaveformAppearance(WaveTrack& track);
    ~WaveformAppearance() override;

    void CopyTo(Track& track) const override;
    void Reparent(const std::shared_ptr<Track>& parent) override;
    void WriteXMLAttributes(XMLWriter& writer) const override;
    bool HandleXMLAttribute(
        const std::string_view& attr, const XMLAttributeValueView& valueView)
    override;

    int GetColorIndex() const { return mColorIndex; }
    void SetColorIndex(int colorIndex);

private:
    void Subscribe(const std::shared_ptr<WaveTrack>& pTrack);

    std::weak_ptr<WaveTrack> mwTrack;
    Observer::Subscription mSubscription;
    int mColorIndex{ 0 };
};

struct WaveColorAttachment final : WaveClipListener
{
    explicit WaveColorAttachment();
    ~WaveColorAttachment() override;

    std::unique_ptr<WaveClipListener> Clone() const override;

    static WaveColorAttachment& Get(WaveClip& clip);
    static const WaveColorAttachment& Get(const WaveClip& clip);
    static WaveColorAttachment& Get(WaveChannelInterval& clip);
    static const WaveColorAttachment& Get(const WaveChannelInterval& clip);

    void MarkChanged() noexcept override; // NOFAIL-GUARANTEE
    void Invalidate() override; // NOFAIL-GUARANTEE

    // Write color
    void WriteXMLAttributes(XMLWriter& writer) const override;

    // Read color
    bool HandleXMLAttribute(const std::string_view& attr, const XMLAttributeValueView& valueView) override;

    int GetColorIndex() const { return mColorIndex; }
    void SetColorIndex(int colorIndex) { mColorIndex = colorIndex; }

private:
    int mColorIndex{};
};

#endif
