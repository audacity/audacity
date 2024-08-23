/*
* Audacity: A Digital Audio Editor
*/
#include "amplifyeffect.h"

#include "global/translation.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace muse;
using namespace au::effects;

AmplifyEffect::AmplifyEffect()
{
}

ComponentInterfaceSymbol AmplifyEffect::GetSymbol() const
{
    return Symbol;
}

TranslatableString AmplifyEffect::GetDescription() const
{
    return XO("Increases or decreases the volume of the audio you have selected");
}

ManualPageID AmplifyEffect::ManualPage() const
{
    return L"Amplify";
}

bool AmplifyEffect::show()
{
    LOGDA() << "-------------";

    //! NOTE Opening the view for yourself
    //! And we set ourselves as id the instance
    //! This implementation is temporary
    //! Probably we need to register instances with their IDs somewhere
    //! and get an instance from this register in a view model.

    String uri = u"audacity://effects/viewer?type=%1&instanceId=%2";
    String type = au3::wxToSting(Symbol.Internal());
    String instanceId = String::number(reinterpret_cast<size_t>(this));
    RetVal<Val> rv = interactive()->open(uri.arg(type, instanceId).toStdString());

    LOGDA() << "open ret: " << rv.ret.toString();

    return rv.ret;
}

double AmplifyEffect::ratio() const
{
    return mRatio;
}

void AmplifyEffect::setRatio(double r)
{
    mRatio = r;
    LOGDA() << "mRatio: " << r;
}

au::effects::EffectMeta AmplifyEffect::meta()
{
    EffectMeta meta;
    meta.title = muse::mtrc("effects", "Amplify");
    meta.categoryId = BUILTIN_CATEGORY_ID;

    return meta;
}
