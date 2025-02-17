/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationParameters.cpp

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.cpp

***********************************************************************/
#include "EqualizationParameters.h"
#include "ConfigInterface.h"

const EnumValueSymbol EqualizationParameters::kInterpStrings[nInterpolations] =
{
    // These are acceptable dual purpose internal/visible names

    /* i18n-hint: Technical term for a kind of curve.*/
    { XO("B-spline") },
    { XO("Cosine") },
    { XO("Cubic") }
};

EqualizationParameters::EqualizationParameters(
    const EffectSettingsManager& manager)
    : mSettingsManager{manager}
    , mCurveName{CurveName.def}
    , mM{FilterLength.def}
    , mInterp{InterpMeth.def}
    , mLin{InterpLin.def}
{
    GetConfig(manager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("dBMin"), mdBMin, dBMin.def);
    GetConfig(manager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("dBMax"), mdBMax, dBMax.def);
    GetConfig(manager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("DrawMode"), mDrawMode, DrawMode.def);
    GetConfig(manager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("DrawGrid"), mDrawGrid, DrawGrid.def);
}

void EqualizationParameters::LoadDefaults(int options)
{
    mdBMin = dBMin.def;
    mdBMax = dBMax.def;
    mDrawMode = DrawMode.def;
    mDrawGrid = DrawGrid.def;

    if (options == kEqOptionCurve) {
        mDrawMode = true;
    }
    if (options == kEqOptionGraphic) {
        mDrawMode = false;
    }
}

void EqualizationParameters::SaveConfig()
const
{
    // TODO: just visit these effect settings the default way
    SetConfig(mSettingsManager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("dBMin"), mdBMin);
    SetConfig(mSettingsManager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("dBMax"), mdBMax);
    SetConfig(mSettingsManager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("DrawMode"), mDrawMode);
    SetConfig(mSettingsManager, PluginSettings::Private,
              CurrentSettingsGroup(), wxT("DrawGrid"), mDrawGrid);
}

bool EqualizationParameters::IsLinear() const
{
    // If sliders show (!mDrawMode), always use the log envelope
    return mDrawMode && mLin;
}
