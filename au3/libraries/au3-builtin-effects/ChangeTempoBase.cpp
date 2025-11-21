#include "ChangeTempoBase.h"
#include "ShuttleAutomation.h"
#include "TimeWarper.h"
#ifdef USE_SBSMS
#include "SBSMSBase.h"
#endif
#include <cmath>

// Soundtouch defines these as well, which are also in generated configmac.h
// and configunix.h, so get rid of them before including,
// to avoid compiler warnings, and be sure to do this
// after all other #includes, to avoid any mischief that might result
// from doing the un-definitions before seeing any wx headers.
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_BUGREPORT
#undef PACKAGE
#undef VERSION
#include "SoundTouch.h"

// Soundtouch is not reasonable below -99% or above 3000%.

const EffectParameterMethods& ChangeTempoBase::Parameters() const
{
    static CapturedParameters<ChangeTempoBase, Percentage, UseSBSMS> parameters;
    return parameters;
}

const ComponentInterfaceSymbol ChangeTempoBase::Symbol { XO("Change Tempo") };

ChangeTempoBase::ChangeTempoBase()
{
    // mUseSBSMS always defaults to false and its value is used only if USE_SBSMS
    // is defined
    Parameters().Reset(*this);
    m_FromBPM = 0.0; // indicates not yet set
    m_ToBPM = 0.0;  // indicates not yet set
    m_FromLength = 0.0;
    m_ToLength = 0.0;

    m_bLoopDetect = false;

    SetLinearEffectFlag(true);
}

ChangeTempoBase::~ChangeTempoBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol ChangeTempoBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString ChangeTempoBase::GetDescription() const
{
    return XO("Changes the tempo of a selection without changing its pitch");
}

ManualPageID ChangeTempoBase::ManualPage() const
{
    return L"Change_Tempo";
}

// EffectDefinitionInterface implementation

EffectType ChangeTempoBase::GetType() const
{
    return EffectTypeProcess;
}

bool ChangeTempoBase::SupportsAutomation() const
{
    return true;
}

// Effect implementation

double ChangeTempoBase::CalcPreviewInputLength(
    const EffectSettings&, double previewLength) const
{
    return previewLength * (100.0 + m_PercentChange) / 100.0;
}

bool ChangeTempoBase::CheckWhetherSkipEffect(const EffectSettings&) const
{
    return m_PercentChange == 0.0;
}

bool ChangeTempoBase::Init()
{
    // The selection might have changed since the last time ChangeTempoBase
    // was invoked, so recalculate the Length parameters.
    m_FromLength = mT1 - mT0;
    m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);

    return true;
}

bool ChangeTempoBase::Process(EffectInstance&, EffectSettings& settings)
{
    bool success = false;

#if USE_SBSMS
    if (mUseSBSMS) {
        double tempoRatio = 1.0 + m_PercentChange / 100.0;
        SBSMSBase proxy;
        proxy.mProxyEffectName = XO("High Quality Tempo Change");
        proxy.setParameters(tempoRatio, 1.0);
        //! Already processing; don't make a dialog
        success = Delegate(proxy, settings);
    } else
#endif
    {
        auto initer = [&](soundtouch::SoundTouch* soundtouch) {
            soundtouch->setTempoChange(m_PercentChange);
        };
        double mT1Dashed = mT0 + (mT1 - mT0) / (m_PercentChange / 100.0 + 1.0);
        RegionTimeWarper warper {
            mT0, mT1, std::make_unique<LinearTimeWarper>(mT0, mT0, mT1, mT1Dashed)
        };
        success = SoundTouchBase::ProcessWithTimeWarper(initer, warper, false);
    }

    if (success) {
        mT1 = mT0 + (mT1 - mT0) / (m_PercentChange / 100 + 1.);
    }

    return success;
}
