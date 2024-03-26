#ifndef AU_PROJECT_IAUDACITYPROJECT_H
#define AU_PROJECT_IAUDACITYPROJECT_H

#include <memory>

#include "global/types/string.h"
#include "global/io/path.h"
#include "global/types/retval.h"
#include "global/async/notification.h"
#include "processing/dom/processingproject.h"

namespace au::project {
class IAudacityProject
{
public:
    virtual ~IAudacityProject() = default;

    virtual mu::Ret load(const mu::io::path_t& path, bool forceMode = false, const std::string& format = "") = 0;

    virtual mu::String title() const { return mu::String(); }
    virtual mu::io::path_t path() const = 0;
    virtual mu::async::Notification pathChanged() const = 0;
    virtual mu::ValNt<bool> needSave() const { return mu::ValNt<bool>(); }

    virtual mu::async::Notification displayNameChanged() const { return mu::async::Notification(); }
    virtual mu::async::Notification needSaveChanged() const { return mu::async::Notification(); }

    virtual const au::processing::ProcessingProjectPtr processingProject() const = 0;
};

using IAudacityProjectPtr = std::shared_ptr<IAudacityProject>;
}

#endif // AU_PROJECT_IAUDACITYPROJECT_H
