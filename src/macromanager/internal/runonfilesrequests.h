/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include "macromanager/irunonfilesrequests.h"

namespace au::macromanager {
class RunOnFilesRequests : public IRunOnFilesRequests
{
public:
    void push(const muse::modularity::ContextPtr& ctx, const RunOnFilesRequest& request) override;
    std::optional<RunOnFilesRequest> take(const muse::modularity::ContextPtr& ctx) override;

private:
    struct Entry {
        muse::modularity::ContextPtr ctx;
        RunOnFilesRequest request;
    };

    std::vector<Entry> m_entries;
};
}
