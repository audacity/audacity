/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseReductionBase.h

  Dominic Mazzoni
  Paul Licameli

**********************************************************************/
#pragma once

#include "StatefulEffect.h"

enum NoiseReductionChoice
{
    NRC_REDUCE_NOISE,
    NRC_ISOLATE_NOISE,
    NRC_LEAVE_RESIDUE,
};

class BUILTIN_EFFECTS_API NoiseReductionBase : public StatefulEffect
{
public:
    static const ComponentInterfaceSymbol Symbol;

    NoiseReductionBase();
    virtual ~NoiseReductionBase();

    using Effect::TrackProgress;

    // ComponentInterface implementation

    ComponentInterfaceSymbol GetSymbol() const override;
    TranslatableString GetDescription() const override;

    // EffectDefinitionInterface implementation

    EffectType GetType() const override;

    // Effect implementation

    bool Process(EffectInstance& instance, EffectSettings& settings) override;

    // This object is the memory of the effect between uses
    // (other than noise profile statistics)
    class BUILTIN_EFFECTS_API Settings
    {
    public:
        Settings();
        ~Settings()
        {
        }

        bool PrefsIO(bool read);
        bool Validate(NoiseReductionBase* effect) const;

        size_t WindowSize() const
        {
            return 1u << (3 + mWindowSizeChoice);
        }

        unsigned StepsPerWindow() const
        {
            return 1u << (1 + mStepsPerWindowChoice);
        }

        size_t SpectrumSize() const
        {
            return 1 + WindowSize() / 2;
        }

        size_t StepSize() const
        {
            return WindowSize() / StepsPerWindow();
        }

        bool mDoProfile;

        // Stored in preferences:

        // Basic:
        double mNewSensitivity;   // - log10 of a probability... yeah.
        double mFreqSmoothingBands; // really an integer
        double mNoiseGain;        // in dB, positive
        double mAttackTime;       // in secs
        double mReleaseTime;      // in secs

        // Advanced:
        double mOldSensitivity; // in dB, plus or minus

        // Basic:
        int mNoiseReductionChoice;

        // Advanced:
        int mWindowTypes;
        int mWindowSizeChoice;
        int mStepsPerWindowChoice;
        int mMethod;
    };

    class Statistics;
    class Worker;

protected:
    std::unique_ptr<Settings> mSettings;
    std::unique_ptr<Statistics> mStatistics;
};
