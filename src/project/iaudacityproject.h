#ifndef AU_PROJECT_IAUDACITYPROJECT_H
#define AU_PROJECT_IAUDACITYPROJECT_H

#include <memory>

#include "global/types/string.h"
#include "global/io/path.h"
#include "global/types/retval.h"
#include "global/async/notification.h"
#include "trackedit/itrackeditproject.h"
#include "types/projecttypes.h"
#include "projectscene/iprojectviewstate.h"

namespace au::project {
class IAudacityProject
{
public:
    virtual ~IAudacityProject() = default;

    virtual muse::Ret createNew() = 0;
    virtual muse::Ret load(const muse::io::path_t& path, bool forceMode = false, const std::string& format = "") = 0;

    virtual void close() = 0;
    virtual muse::async::Notification aboutCloseBegin() const = 0;
    virtual muse::async::Notification aboutCloseEnd() const = 0;

    virtual bool isNewlyCreated() const = 0;
    virtual bool isImported() const = 0;

    virtual muse::String title() const { return muse::String(); }

    virtual QString displayName() const = 0;
    virtual muse::async::Notification displayNameChanged() const { return muse::async::Notification(); }
    virtual muse::io::path_t path() const = 0;
    virtual muse::async::Notification pathChanged() const = 0;
    virtual muse::ValNt<bool> needSave() const = 0;
    virtual muse::Ret canSave() const = 0;
    virtual bool needAutoSave() const = 0;
    virtual void setNeedAutoSave(bool val) = 0;
    virtual muse::async::Notification needSaveChanged() const { return muse::async::Notification(); }
    virtual muse::Ret save(const muse::io::path_t& path = muse::io::path_t(), SaveMode saveMode = SaveMode::Save) = 0;

    virtual const au::trackedit::ITrackeditProjectPtr trackeditProject() const = 0;

    virtual projectscene::IProjectViewStatePtr viewState() const = 0;

    virtual uintptr_t au3ProjectPtr() const = 0;
};

using IAudacityProjectPtr = std::shared_ptr<IAudacityProject>;
}

#endif // AU_PROJECT_IAUDACITYPROJECT_H
