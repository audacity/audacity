/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationParameters.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_EQUALIZATION_PARAMETERS__
#define __AUDACITY_EQUALIZATION_PARAMETERS__

#include "ShuttleAutomation.h"

// Flags to specialise the UI
const int kEqOptionGraphic = 1;
const int kEqOptionCurve   = (1 << 1);
// The legacy version offers both Graphic and curve on the same UI.
const int kEqLegacy = kEqOptionGraphic | kEqOptionCurve;

// Define to enable the old combined UI with both curves and sliders, and
// management of a set of curves
#undef LEGACY_EQ

//! Parameters of the Equalization effects that persist in configuration files
struct BUILTIN_EFFECTS_API EqualizationParameters {
    enum kInterpolations {
        kBspline,
        kCosine,
        kCubic,
        nInterpolations
    };
    static const EnumValueSymbol kInterpStrings[nInterpolations];

    explicit EqualizationParameters(const EffectSettingsManager& manager);
    void LoadDefaults(int options);
    void SaveConfig() const;
    bool IsLinear() const;

    const EffectSettingsManager& mSettingsManager;

    wxString mCurveName;
    float mdBMin;
    float mdBMax;
    size_t mM;
    int mInterp;
    bool mDrawMode;
    bool mDrawGrid;
    bool mLin;

// Not all of these are visited now
    static constexpr EffectParameter FilterLength{ &EqualizationParameters::mM,
                                                   L"FilterLength",        8191,    21,      8191,    0 };
    static constexpr EffectParameter CurveName{ &EqualizationParameters::mCurveName,
                                                L"CurveName",           L"unnamed", L"", L"", L"" };
    static constexpr EffectParameter InterpLin{ &EqualizationParameters::mLin,
                                                L"InterpolateLin",      false,   false,   true,    false };
    static constexpr EnumParameter InterpMeth{ &EqualizationParameters::mInterp,
                                               L"InterpolationMethod", 0,       0,       0,       0, kInterpStrings, nInterpolations };
    static constexpr EffectParameter DrawMode{ &EqualizationParameters::mDrawMode,
                                               L"",                   true,    false,   true,    false };
    static constexpr EffectParameter DrawGrid{ &EqualizationParameters::mDrawGrid,
                                               L"",                   true,    false,   true,    false };
    static constexpr EffectParameter dBMin{ &EqualizationParameters::mdBMin,
                                            L"",                   -30.0f,   -120.0,  -10.0,   0 };
    static constexpr EffectParameter dBMax{ &EqualizationParameters::mdBMax,
                                            L"",                   30.0f,    0.0,     60.0,    0 };
};

#endif
