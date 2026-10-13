/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

#include "modularity/imoduleinterface.h"
#include "async/notification.h"
#include "types/ret.h"

#include "macromanagertypes.h"

namespace au::macromanager {
class IMacrosRepository : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IMacrosRepository)

public:
    virtual ~IMacrosRepository() = default;

    virtual const MacroList& macros() const = 0;
    virtual std::optional<Macro> macro(const std::string& name) const = 0;

    virtual muse::Ret save(const Macro& macro) = 0;
    virtual muse::Ret remove(const std::string& name) = 0;
    virtual muse::Ret rename(const std::string& oldName, const std::string& newName) = 0;

    virtual muse::async::Notification macrosChanged() const = 0;
};
}
