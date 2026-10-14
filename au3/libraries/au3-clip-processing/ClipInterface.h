/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipInterface.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioSegmentSampleView.h"
#include "au3-registries/ClientData.h"
#include "au3-utility/CRTPBase.h"
#include "au3-utility/Observer.h"
#include "au3-math/SampleCount.h"

#include <string_view>

class ClipInterface;
class XMLAttributeValueView;
class XMLWriter;

class ClipTimes
{
public:
    virtual ~ClipTimes();

    /*!
     * The number of raw audio samples not hidden by trimming.
     */
    virtual sampleCount GetVisibleSampleCount() const = 0;

    virtual int GetRate() const = 0;

    virtual double GetPlayStartTime() const = 0;

    virtual double GetPlayEndTime() const = 0;

    virtual sampleCount TimeToSamples(double time) const = 0;

    virtual double GetStretchRatio() const = 0;
};

enum class PitchAndSpeedPreset
{
    Default,
    OptimizeForVoice,
};

struct ClipInterfaceAttachment;
CRTP_BASE(ClipInterfaceAttachmentBase, struct,
          ClientData::Cloneable<ClipInterfaceAttachment>);
//! Base of the objects attached to a clip by code that only knows the clip
//! through this interface
struct ClipInterfaceAttachment : ClipInterfaceAttachmentBase {
    ~ClipInterfaceAttachment() override = default;

    //! Called on the attachments of a copy of a clip, once copied, for those
    //! that hold a back-pointer to their clip: it should now point to `clip`
    /*!
     Default implementation does nothing
     */
    virtual void Reparent(ClipInterface& clip);

    //! Default implementation does nothing
    virtual void WriteXMLAttributes(XMLWriter& writer) const;

    //! Default implementation just returns false
    virtual bool HandleXMLAttribute(
        const std::string_view& attr, const XMLAttributeValueView& valueView);
};

/*!
 * Implementations are responsible for copying (and reparenting), saving and
 * loading the attachments.
 */
class ClipInterface : public ClipTimes, public ClientData::Site<ClipInterface, ClipInterfaceAttachment,
                                                                    ClientData::DeepCopying>
{
public:
    using ClipAttachments
        =ClientData::Site<ClipInterface, ClipInterfaceAttachment, ClientData::DeepCopying>;

    ~ClipInterface() override;

    virtual AudioSegmentSampleView GetSampleView(
        size_t iChannel, sampleCount start, size_t length, bool mayThrow = true) const = 0;

    virtual size_t NChannels() const = 0;

    virtual int GetCentShift() const = 0;

    [[nodiscard]] virtual Observer::Subscription
    SubscribeToCentShiftChange(std::function<void(int)> cb) const = 0;

    virtual PitchAndSpeedPreset GetPitchAndSpeedPreset() const = 0;

    [[nodiscard]] virtual Observer::Subscription
    SubscribeToPitchAndSpeedPresetChange(
        std::function<void(PitchAndSpeedPreset)> cb) const = 0;

    [[nodiscard]] virtual Observer::Subscription SubscribeToStretchRatioChange(
        std::function<void(double oldRatio, double newRatio)> cb) const = 0;
};

using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface> >;
