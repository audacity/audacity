/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.h

  Dominic Mazzoni

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#pragma once

#include "modularity/ioc.h"
#include "effects/ieffectsparametersservice.h"

#include "effects/view/abstracteffectmodel.h"

namespace au::effects {
class EffectAmplify : public AbstractEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double amp READ amp WRITE setAmp NOTIFY ampChanged FINAL)
    Q_PROPERTY(double ampMin READ ampMin NOTIFY ampChanged FINAL)
    Q_PROPERTY(double ampMax READ ampMax NOTIFY ampChanged FINAL)

    Q_PROPERTY(int ampSliderValue READ ampSliderValue WRITE setAmpSliderValue NOTIFY ampSliderValueChanged FINAL)
    Q_PROPERTY(double ampSliderValueMin READ ampSliderValueMin NOTIFY ampSliderValueChanged FINAL)
    Q_PROPERTY(double ampSliderValueMax READ ampSliderValueMax NOTIFY ampSliderValueChanged FINAL)

    Q_PROPERTY(double newPeakAmp READ newPeakAmp WRITE setNewPeakAmp NOTIFY newPeakAmpChanged FINAL)
    Q_PROPERTY(double newPeakAmpMin READ newPeakAmpMin NOTIFY newPeakAmpChanged FINAL)
    Q_PROPERTY(double newPeakAmpMax READ newPeakAmpMax NOTIFY newPeakAmpChanged FINAL)

    Q_PROPERTY(bool allowCliping READ allowCliping WRITE setAllowCliping NOTIFY allowClipingChanged FINAL)

    muse::Inject<IEffectsParametersService> effectsParametersService;

public:

    EffectAmplify();
    virtual ~EffectAmplify();

    Q_INVOKABLE void onAmpTextChanged();
    Q_INVOKABLE void onNewPeakTextChanged();
    Q_INVOKABLE void onAmpSliderValueChanged(int newValue);

    // EffectDefinitionInterface implementation

    // EffectType GetType() const override;
    // OptionalMessage LoadFactoryDefaults(EffectSettings& settings)
    // const override;
    // OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);

    // unsigned GetAudioInCount() const override;
    // unsigned GetAudioOutCount() const override;
    // size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

    // Effect implementation

    // bool Init() override;
    // // std::any BeginPreview(const EffectSettings& settings) override;
    // std::unique_ptr<EffectEditor> PopulateOrExchange(
    //     ShuttleGui& S, EffectInstance& instance, EffectSettingsAccess& access, const EffectOutputs* pOutputs) override;
    // bool TransferDataToWindow(const EffectSettings& settings) override;
    // bool TransferDataFromWindow(EffectSettings& settings) override;

    // std::shared_ptr<EffectInstance> MakeInstance() const override;

// private:
//     struct Instance : StatefulPerTrackEffect::Instance {
//         using StatefulPerTrackEffect::Instance::Instance;
//         ~Instance() override;
//     };

//     void ClampRatio();

//     // EffectAmplify implementation

    double ampSliderValueMin() const;

    double ampSliderValueMax() const;

private:
    void CheckClip();

    double amp() const;
    void setAmp(double amp);

    double ampMin() const;
    double ampMax() const;

    double newPeakAmp() const;
    void setNewPeakAmp(double amp);

    double newPeakAmpMin() const;
    double newPeakAmpMax() const;

    bool allowCliping() const;
    void setAllowCliping(bool allow);

    int ampSliderValue() const;
    void setAmpSliderValue(int value);

signals:
    void ampChanged();
    void newPeakAmpChanged();
    void allowClipingChanged();

    void ampSliderValueChanged();

private:
    EffectParameter m_ratio = { u"ratio", muse::Val(1.0f), muse::Val(0.9f), muse::Val(0.003162f), muse::Val(316.227766f), muse::Val(1.0f) };
    EffectParameter m_amp = { u"", muse::Val(0.0f), muse::Val(-0.91515f), muse::Val(-50.0f), muse::Val(50.0f), muse::Val(10.0f) };
    EffectParameter m_allowClipping = { u"allowClipping", muse::Val(false), muse::Val(false) };

    double m_peakAmp = 1.0;
    double m_newPeakAmp = 1.0;
    int m_ampSliderValue = 0;

    double m_ratioMaxForClip = 1.0;   // maximum value of mRatio which does not cause clipping
    double m_ampSliderValueMin = 0.0;
    double m_ampSliderValueMax = 0.0;
};
}
