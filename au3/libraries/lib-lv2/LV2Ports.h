/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Ports.h
  @brief Immutable descriptions of LV2 ports; associated state structures

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_PORTS__
#define __AUDACITY_LV2_PORTS__

#if USE_LV2

#include <functional>
#include <optional>
#include <unordered_map>

#include "LV2Utils.h"
#include "EffectInterface.h"
#include "MemoryX.h"
#include "TranslatableString.h"
#include <wx/arrstr.h>
#include "lv2/atom/forge.h"
#include "zix/ring.h"

using Floats = ArrayOf<float>;

//! Immutable description of an LV2 port
class LV2Port
{
public:
    LV2Port(const LilvPort* port, int index, bool isInput,
            const wxString& symbol, const wxString& name,
            const TranslatableString& group)
        : mPort(port), mIndex(index), mIsInput(isInput)
        , mSymbol(symbol), mName(name), mGroup(group)
    {}
    const LilvPort* const mPort;
    const uint32_t mIndex;
    const bool mIsInput;
    const wxString mSymbol;
    const wxString mName;
    const TranslatableString mGroup;
};

//! Immutable description of an LV2 Audio port
class LV2AudioPort final : public LV2Port
{
public:
    using LV2Port::LV2Port;
};
using LV2AudioPortPtr = std::shared_ptr<LV2AudioPort>;
using LV2AudioPortArray = std::vector<LV2AudioPortPtr>;

//! Immutable description of an LV2 Atom port
class LV2AtomPort final : public LV2Port
{
public:
    LV2AtomPort(const LilvPort* port, int index, bool isInput,
                const wxString& symbol, const wxString& name,
                const TranslatableString& group,
                uint32_t minimumSize, bool isMidi, bool wantsPosition)
        : LV2Port{port, index, isInput, symbol, name, group}
        , mMinimumSize{minimumSize}
        , mIsMidi{isMidi}
        , mWantsPosition{wantsPosition}
    {}
    const uint32_t mMinimumSize;
    const bool mIsMidi;
    const bool mWantsPosition;
};
using LV2AtomPortPtr = std::shared_ptr<LV2AtomPort>;
using LV2AtomPortArray = std::vector<LV2AtomPortPtr>;

//! State of an instance of an LV2 Atom port
struct LV2_API LV2AtomPortState final {
    //! @pre `pPort != nullptr`
    explicit LV2AtomPortState(LV2AtomPortPtr pPort)
        : mpPort{move(pPort)}
        , mRing{zix_ring_new(mpPort->mMinimumSize)}
        , mBuffer{safenew uint8_t[mpPort->mMinimumSize]}
    {
        assert(mpPort);
        /*
         There is no complementary munlock anywhere in the library!
         Are we mis-using a class meant for plugin implementation?
         Do we make a resource leak of physical memory pages?
         Should we just skip this call?
         Or is mlock a good idea we should implement in our own RingBuffer,
         which we can also munlock in its destructor?
         */
        zix_ring_mlock(mRing.get());

        ResetForInstanceOutput();
    }

    //! Transfer incoming events from the ring buffer to the event buffer.
    /*!
     These will be made available to each slave in the chain.
     In addition, reset the output Atom ports.
    */
    void SendToInstance(LV2_Atom_Forge& forge, int64_t frameTime, float speed);
    void ResetForInstanceOutput();

    //! Take responses from the instance and send cross-thread for the dialog
    void ReceiveFromInstance();

    //! Dialog can poll one ring buffer for messages at idle time
    /*!
     Given function may be called multiple times
     */
    void SendToDialog(
        std::function<void(const LV2_Atom* atom, uint32_t size)> handler);

    //! Dialog pushes to other ring buffer when it gets a user interface event
    void ReceiveFromDialog(const void* buffer, uint32_t buffer_size);

    const LV2AtomPortPtr mpPort;
    const Lilv_ptr<ZixRing, zix_ring_free> mRing;
    const std::unique_ptr<uint8_t[]> mBuffer;
};
using LV2AtomPortStatePtr = std::shared_ptr<LV2AtomPortState>;
using LV2AtomPortStateArray = std::vector<LV2AtomPortStatePtr>;

//! Immutable description of an LV2 CV port (control data signal at sample rate)
class LV2CVPort final : public LV2Port
{
public:
    LV2CVPort(const LilvPort* port, int index, bool isInput,
              const wxString& symbol, const wxString& name,
              const TranslatableString& group,
              float min, float max, float def, bool hasLo, bool hasHi)
        : LV2Port(port, index, isInput, symbol, name, group)
        , mMin{min}, mMax{max}, mDef{def}, mHasLo{hasLo}, mHasHi{hasHi}
    {}
    const float mMin;
    const float mMax;
    const float mDef;
    const bool mHasLo;
    const bool mHasHi;
};
using LV2CVPortPtr = std::shared_ptr<LV2CVPort>;
using LV2CVPortArray = std::vector<LV2CVPortPtr>;

//! State of an instance of an LV2 CV port
struct LV2CVPortState final {
    //! @pre `pPort != nullptr`
    explicit LV2CVPortState(LV2CVPortPtr pPort)
        : mpPort{move(pPort)}
    {
        assert(mpPort);
    }

    const LV2CVPortPtr mpPort;
    Floats mBuffer;
};
//! No need yet for extra indirection
using LV2CVPortStateArray = std::vector<LV2CVPortState>;

//! Immutable description of an LV2 control port
class LV2_API LV2ControlPort final : public LV2Port
{
public:
    LV2ControlPort(const LilvPort* port, int index, bool isInput,
                   const wxString& symbol, const wxString& name,
                   const TranslatableString& group,
                   std::vector<double> scaleValues, wxArrayString scaleLabels,
                   const wxString& units,
                   float min, float max, float def, bool hasLo, bool hasHi,
                   bool toggle, bool enumeration, bool integer, bool sampleRate,
                   bool trigger, bool logarithmic)
        : LV2Port{port, index, isInput, symbol, name, group}
        , mScaleValues{move(scaleValues)}
        , mScaleLabels(std::move(scaleLabels))
        , mUnits{units}
        , mMin{min}, mMax{max}, mDef{def}
        , mHasLo{hasLo}, mHasHi{hasHi}
        , mToggle{toggle}, mEnumeration{enumeration}, mInteger{integer}
        , mSampleRate{sampleRate}
        , mTrigger{trigger}, mLogarithmic{logarithmic}
    {}

    // ScalePoints
    const std::vector<double> mScaleValues;
    const wxArrayString mScaleLabels;

    const wxString mUnits;
    const float mMin;
    const float mMax;
    const float mDef;
    const bool mHasLo;
    const bool mHasHi;
    const bool mToggle;
    const bool mEnumeration;
    const bool mInteger;
    const bool mSampleRate;
    const bool mTrigger;
    const bool mLogarithmic;

    //! Map a real number to one of the scale points
    size_t Discretize(float value) const;
};
using LV2ControlPortPtr = std::shared_ptr<LV2ControlPort>;
using LV2ControlPortArray = std::vector<LV2ControlPortPtr>;

//! Storage locations to be connected to LV2 control ports
struct LV2EffectSettings final {
    //! vector of values in correspondence with the control ports
    std::vector<float> values;
    //! Result of last load of a preset; may be null
    mutable std::shared_ptr<const LilvState> mpState;
};

//! Assume settings originated from LV2Effecct::MakeSettings()
//! and copies thereof
inline LV2EffectSettings& GetSettings(EffectSettings& settings)
{
    auto pSettings = settings.cast<LV2EffectSettings>();
    assert(pSettings);
    return *pSettings;
}

inline const LV2EffectSettings& GetSettings(const EffectSettings& settings)
{
    return GetSettings(const_cast<EffectSettings&>(settings));
}

//! Carry output control port information back to main thread
struct LV2_API LV2EffectOutputs : EffectOutputs {
    ~LV2EffectOutputs() override;
    std::unique_ptr<EffectOutputs> Clone() const override;
    void Assign(EffectOutputs&& src) override;
    //! vector of values in correspondence with the control ports
    std::vector<float> values;
};

//! Other UI related state of an instance of an LV2 Control port
struct LV2ControlPortState final {
    //! @pre `pPort != nullptr`
    explicit LV2ControlPortState(LV2ControlPortPtr pPort)
        : mpPort{move(pPort)}
    {
        assert(mpPort);
    }

    const LV2ControlPortPtr mpPort;
    //! Value of mTmp last seen by idle-time updater
    float mLst{ 0.0 };
    //! Value of UI control, as scaled by sample rate if that is required
    float mTmp{ 0.0 };
    //! Lower bound, as scaled by sample rate if that is required
    float mLo{ 0.0 };
    //! Upper bound, as scaled by sample rate if that is required
    float mHi{ 0.0 };
};
//! No need yet for extra indirection
using LV2ControlPortStateArray = std::vector<LV2ControlPortState>;

class LV2_API LV2Ports
{
public:
    //! @post every member of `mGroups` occurs as a key in `mGroupMap`
    explicit LV2Ports(const LilvPlugin& plug);

    void EmitPortValues(
        const LilvState& state, LV2EffectSettings& settings) const;

    const void* GetPortValue(const LV2EffectSettings& settings, const char* port_symbol, uint32_t* size, uint32_t* type) const;

    void SetPortValue(LV2EffectSettings& settings, const char* port_symbol, const void* value, uint32_t size, uint32_t type) const;

    LV2AudioPortArray mAudioPorts;
    unsigned mAudioIn{ 0 };
    unsigned mAudioOut{ 0 };

    LV2AtomPortArray mAtomPorts;
    std::optional<size_t> mControlInIdx{};
    std::optional<size_t> mControlOutIdx{};
    unsigned mMidiIn{ 0 };
    unsigned mMidiOut{ 0 };

    LV2CVPortArray mCVPorts;

    LV2ControlPortArray mControlPorts;
    TranslatableStrings mGroups;
    std::unordered_map<TranslatableString, std::vector<int> > mGroupMap;
    //! Mapping from index number among all ports, to position
    //! among the control ports only
    std::unordered_map<uint32_t, size_t> mControlPortMap;
    int mLatencyPort{ -1 };
};

class LV2_API LV2PortStates
{
public:
    explicit LV2PortStates(const LV2Ports& ports);
    LV2PortStates(const LV2PortStates&) = delete;
    LV2PortStates& operator=(const LV2PortStates&) = delete;

    LV2AtomPortStateArray mAtomPortStates;
    LV2CVPortStateArray mCVPortStates;
};

class LV2_API LV2PortUIStates
{
public:
    LV2PortUIStates(const LV2PortStates& states, const LV2Ports& ports);
    LV2PortUIStates(const LV2PortUIStates&) = delete;
    LV2PortUIStates& operator=(const LV2PortUIStates&) = delete;

    LV2AtomPortStatePtr mControlIn;
    LV2AtomPortStatePtr mControlOut;
    LV2ControlPortStateArray mControlPortStates;
};

#endif
#endif
