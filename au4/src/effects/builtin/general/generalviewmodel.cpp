/*
* Audacity: A Digital Audio Editor
*/
#include "generalviewmodel.h"

#include "libraries/lib-effects/Effect.h"

#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"

using namespace au::effects;

void GeneralViewModel::init()
{
    IF_ASSERT_FAILED(!instanceId().isEmpty()) {
        return;
    }

    //! NOTE Temporary solution
    m_effect = reinterpret_cast<Effect*>(instanceId().toLongLong());

    LOGDA() << au3::wxToSting(m_effect->GetSymbol().Internal());

    //! NOTE This is where it's needed
    //! * Get a list of parameters
    //! Turn them into items for Qml (we can make QObjects, or just QVariantMap)
    //! In Qml show the list and depending on the parameter type load the required delegate
    //! Add a method to change the parameter value
}
