/*
* Audacity: A Digital Audio Editor
*/
#include "runonfilesrequests.h"

#include <algorithm>

using namespace au::macromanager;

void RunOnFilesRequests::push(const muse::modularity::ContextPtr& ctx, const RunOnFilesRequest& request)
{
    m_entries.push_back({ ctx, request });
}

std::optional<RunOnFilesRequest> RunOnFilesRequests::take(const muse::modularity::ContextPtr& ctx)
{
    const auto it = std::ranges::find_if(m_entries, [&ctx](const Entry& entry) {
        return entry.ctx->id == ctx->id;
    });

    if (it == m_entries.end()) {
        return std::nullopt;
    }

    RunOnFilesRequest request = std::move(it->request);
    m_entries.erase(it);

    return request;
}
