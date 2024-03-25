#ifndef AU_PROJECT_IAUDACITYPROJECT_H
#define AU_PROJECT_IAUDACITYPROJECT_H

#include <memory>

#include "global/types/string.h"
#include "global/io/path.h"
#include "global/types/retval.h"
#include "global/async/notification.h"
#include "processing/dom/processingproject.h"

namespace mu::project {
class IAudacityProject
{
public:
    virtual ~IAudacityProject() = default;

    mu::String title() const { return mu::String(); }
    mu::io::path_t filePath() const { return mu::io::path_t(); }
    mu::async::Notification pathChanged() const { return mu::async::Notification(); }
    mu::ValNt<bool> needSave() const { return mu::ValNt<bool>(); }

    mu::async::Notification displayNameChanged() const { return mu::async::Notification(); }
    mu::async::Notification needSaveChanged() const { return mu::async::Notification(); }

    virtual const au::processing::ProcessingProject& processingProject() const = 0;
};

using IAudacityProjectPtr = std::shared_ptr<IAudacityProject>;
}

#endif // AU_PROJECT_IAUDACITYPROJECT_H
