#ifndef MU_PROJECT_IAUDACITYPROJECT_H
#define MU_PROJECT_IAUDACITYPROJECT_H

#include <memory>

#include "global/types/string.h"
#include "global/io/path.h"
#include "global/types/retval.h"
#include "global/async/notification.h"

namespace mu::project {
class IAudacityProject
{
public:
    virtual ~IAudacityProject() = default;

    String title() const { return String(); }
    io::path_t filePath() const { return io::path_t(); }
    mu::async::Notification pathChanged() const { return mu::async::Notification(); }
    ValNt<bool> needSave() const { return ValNt<bool>(); }

    mu::async::Notification displayNameChanged() const { return mu::async::Notification(); }
    mu::async::Notification needSaveChanged() const { return mu::async::Notification(); }
};

using IAudacityProjectPtr = std::shared_ptr<IAudacityProject>;
}

#endif // MU_PROJECT_IAUDACITYPROJECT_H
