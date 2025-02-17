/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Instance.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/
#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2Instance.h"
#include "LV2Wrapper.h"
#include "AudacityException.h"

LV2Instance::LV2Instance(
    const PerTrackEffect& effect, const LV2FeaturesList& features,
    const LV2Ports& ports)
    : PerTrackEffect::Instance{effect}
    , mFeatures{features}
    , mPorts{ports}
{
    LV2Preferences::GetUseLatency(effect, mUseLatency);

    int userBlockSize;
    LV2Preferences::GetBufferSize(effect, userBlockSize);
    mUserBlockSize = std::max(1, userBlockSize);
    lv2_atom_forge_init(&mForge, mFeatures.Base().URIDMapFeature());
}

LV2Instance::~LV2Instance() = default;

void LV2Instance::MakeMaster(const EffectSettings& settings,
                             double sampleRate)
{
    // Come here only when doing non-realtime application of the effect, in which
    // case, we don't care about capturing the output ports
    if (mMaster && sampleRate == mFeatures.mSampleRate) {
        // Already made but be sure to connect control ports to the right place
        mMaster->ConnectControlPorts(mPorts, GetSettings(settings), nullptr);
        return;
    }
    mMaster = MakeWrapper(settings, sampleRate, nullptr);
    SetBlockSize(mUserBlockSize);
}

std::unique_ptr<LV2Wrapper>
LV2Instance::MakeWrapper(const EffectSettings& settings,
                         double sampleRate, EffectOutputs* pOutputs)
{
    return LV2Wrapper::Create(mFeatures, mPorts, mPortStates,
                              GetSettings(settings), sampleRate, pOutputs);
}

size_t LV2Instance::SetBlockSize(size_t maxBlockSize)
{
    mFeatures.mBlockSize = std::max(mFeatures.mMinBlockSize,
                                    std::min({ maxBlockSize, mUserBlockSize, mFeatures.mMaxBlockSize }));
    if (mMaster) {
        mMaster->SendBlockSize();
    }
    for (auto& pSlave : mSlaves) {
        pSlave->SendBlockSize();
    }
    return GetBlockSize();
}

size_t LV2Instance::GetBlockSize() const
{
    return mFeatures.mBlockSize;
}

unsigned LV2Instance::GetAudioInCount() const
{
    return mPorts.mAudioIn;
}

unsigned LV2Instance::GetAudioOutCount() const
{
    return mPorts.mAudioOut;
}

auto LV2Instance::GetLatency(const EffectSettings&, double) const
-> SampleCount
{
    if (mMaster && mUseLatency && mPorts.mLatencyPort >= 0) {
        return mMaster->GetLatency();
    }
    return 0;
}

// Start of destructive processing path
bool LV2Instance::ProcessInitialize(EffectSettings& settings,
                                    double sampleRate, ChannelNames chanMap)
{
    MakeMaster(settings, sampleRate);
    if (!mMaster) {
        return false;
    }
    for (auto& state : mPortStates.mCVPortStates) {
        state.mBuffer.reinit(GetBlockSize(), state.mpPort->mIsInput);
    }
    mMaster->Activate();
    return true;
}

size_t LV2Instance::ProcessBlock(EffectSettings&,
                                 const float* const* inbuf, float* const* outbuf, size_t size)
{
    using namespace LV2Symbols;
    if (size > GetBlockSize()) {
        return 0;
    }
    assert(mMaster); // else ProcessInitialize() returned false, I'm not called
    const auto instance = &mMaster->GetInstance();

    int i = 0;
    int o = 0;
    for (auto& port : mPorts.mAudioPorts) {
        lilv_instance_connect_port(instance,
                                   port->mIndex,
                                   const_cast<float*>(port->mIsInput ? inbuf[i++] : outbuf[o++]));
    }

    for (auto& state : mPortStates.mAtomPortStates) {
        state->SendToInstance(mForge, mPositionFrame, mPositionSpeed);
    }

    lilv_instance_run(instance, size);

    // Main thread consumes responses
    mMaster->ConsumeResponses();

    for (auto& state : mPortStates.mAtomPortStates) {
        state->ResetForInstanceOutput();
    }

    return size;
}

bool LV2Instance::RealtimeInitialize(EffectSettings&, double)
{
    for (auto& state : mPortStates.mCVPortStates) {
        state.mBuffer.reinit(GetBlockSize(), state.mpPort->mIsInput);
    }
    return true;
}

bool LV2Instance::RealtimeFinalize(EffectSettings&) noexcept
{
    return GuardedCall<bool>([&]{
        mSlaves.clear();
        for (auto& state : mPortStates.mCVPortStates) {
            state.mBuffer.reset();
        }
        return true;
    });
}

bool LV2Instance::RealtimeAddProcessor(EffectSettings& settings,
                                       EffectOutputs* pOutputs, unsigned, float sampleRate)
{
    // Connect to outputs only if this is the first processor for the track.
    // (What's right when a mono effect is on a stereo channel?  Unclear, but
    // this definitely causes connection with the first channel.)
    auto pInstance = LV2Wrapper::Create(mFeatures,
                                        mPorts, mPortStates, GetSettings(settings), sampleRate,
                                        mSlaves.empty() ? static_cast<LV2EffectOutputs*>(pOutputs) : nullptr);
    if (!pInstance) {
        return false;
    }
    pInstance->Activate();
    mSlaves.push_back(move(pInstance));
    return true;
}

bool LV2Instance::RealtimeSuspend()
{
    if (mMaster) {
        mMaster->Deactivate();
    }
    for (auto& pSlave : mSlaves) {
        pSlave->Deactivate();
    }

    mPositionSpeed = 0.0;
    mPositionFrame = 0;
    mRolling = false;

    return true;
}

bool LV2Instance::RealtimeResume()
{
    if (mMaster) {
        mMaster->Activate();
    }
    for (auto& pSlave : mSlaves) {
        pSlave->Activate();
    }

    mPositionSpeed = 1.0;
    mPositionFrame = 0;
    mRolling = true;

    return true;
}

bool LV2Instance::RealtimeProcessStart(MessagePackage&)
{
    mNumSamples = 0;
    for (auto& state : mPortStates.mAtomPortStates) {
        state->SendToInstance(mForge, mPositionFrame, mPositionSpeed);
    }
    return true;
}

size_t LV2Instance::RealtimeProcess(size_t group, EffectSettings&,
                                    const float* const* inbuf, float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }
    assert(numSamples <= (size_t)GetBlockSize());

    if (group < 0 || group >= (int)mSlaves.size()) {
        return 0;
    }

    const auto slave = mSlaves[group].get();
    const auto instance = &slave->GetInstance();

    int i = 0;
    int o = 0;
    for (auto& port : mPorts.mAudioPorts) {
        lilv_instance_connect_port(instance,
                                   port->mIndex,
                                   const_cast<float*>(port->mIsInput ? inbuf[i++] : outbuf[o++]));
    }

    mNumSamples = std::max(numSamples, mNumSamples);

    if (mRolling) {
        lilv_instance_run(instance, numSamples);
    } else {
        while (--i >= 0) {
            for (decltype(numSamples) s = 0; s < numSamples; s++) {
                outbuf[i][s] = inbuf[i][s];
            }
        }
    }

    // Background thread consumes responses from yet another worker thread
    slave->ConsumeResponses();

    for (auto& state : mPortStates.mAtomPortStates) {
        state->ResetForInstanceOutput();
    }

    if (group == 0) {
        mPositionFrame += numSamples;
    }

    return numSamples;
}

bool LV2Instance::RealtimeProcessEnd(EffectSettings&) noexcept
{
    return GuardedCall<bool>([&]{
        // Nothing to do if we did process any samples
        if (mNumSamples == 0) {
            return true;
        }

        // Why is this not also done on the destructive processing path?
        // Because it is soon dimissing the modal dialog anyway.
        for (auto& state : mPortStates.mAtomPortStates) {
            state->ReceiveFromInstance();
        }

        mNumSamples = 0;

        return true;
    });
}

#endif
