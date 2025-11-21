#pragma once

#include "SettingsVisitor.h"
#include "StatefulEffect.h"

class LabelTrack;
class WaveChannel;

// the standard vinyl rpm choices
// If the percent change is not one of these ratios, the choice control gets
// "n/a".
enum kVinyl
{
    kVinyl_33AndAThird = 0,
    kVinyl_45,
    kVinyl_78,
    kVinyl_NA
};

class BUILTIN_EFFECTS_API ChangeSpeedBase : public StatefulEffect
{
public:
    static inline ChangeSpeedBase*
    FetchParameters(ChangeSpeedBase& e, EffectSettings&)
    {
        return &e;
    }

    static const ComponentInterfaceSymbol Symbol;

    ChangeSpeedBase();
    virtual ~ChangeSpeedBase();

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;
    ManualPageID ManualPage() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;
    OptionalMessage LoadFactoryDefaults(EffectSettings& settings) const override;
    OptionalMessage DoLoadFactoryDefaults(EffectSettings& settings);

    bool CheckWhetherSkipEffect(const EffectSettings& settings) const override;
    double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const override;
    bool Init() override;
    bool Process(EffectInstance& instance, EffectSettings& settings) override;

private:
    // ChangeSpeedBase implementation

    using Gap = std::pair<double, double>;
    using Gaps = std::vector<Gap>;
    Gaps
    FindGaps(const WaveTrack& track, const double curT0, const double curT1);

    bool ProcessOne(
        const WaveChannel& track, WaveChannel& outputTrack, sampleCount start, sampleCount end);
    bool ProcessLabelTrack(LabelTrack* t);

protected:
    // track related
    int mCurTrackNum;
    double mCurT0;
    double mCurT1;

    // control values
    double
        m_PercentChange; // percent change to apply to tempo
                         // -100% is meaningless, but sky's the upper limit.
                         // Slider is (-100, 200], but textCtrls can set higher.
    int mFromVinyl;     // from standard vinyl speed (RPM) enum
    double mFactor;     // scale factor calculated from percent change
    double mFromLength; // current selection length
    int mTimeCtrlFormat; // time control format index number
    double mMultiplier;

    bool mbLoopDetect;

    double mRate;

    // private effect parameters
    int mToVinyl;           // to standard vinyl speed (rpm)
    double mToLength;       // target length of selection
    NumericFormatID mFormat; // time control format

    const EffectParameterMethods& Parameters() const override;

    static constexpr EffectParameter Percentage {
        &ChangeSpeedBase::m_PercentChange, L"Percentage", 0.0, -99.0, 4900.0, 1
    };
};
