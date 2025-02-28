/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: CancellationContext.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <atomic>
#include <memory>
#include <mutex>
#include <vector>

namespace audacity::concurrency {
class ICancellable;

class CancellationContext;
using CancellationContextPtr = std::shared_ptr<CancellationContext>;

class CONCURRENCY_API CancellationContext final
{
    struct Tag final
    {
    };

public:
    explicit CancellationContext(Tag);

    CancellationContext(const CancellationContext&)            = delete;
    CancellationContext(CancellationContext&&)                 = delete;
    CancellationContext& operator=(const CancellationContext&) = delete;
    CancellationContext& operator=(CancellationContext&&)      = delete;

    [[nodiscard]] static CancellationContextPtr Create();

    void Cancel();

    using CancellableWPtr = std::weak_ptr<ICancellable>;
    void OnCancelled(CancellableWPtr cancellable);

private:
    std::atomic<bool> mCancelled { false };

    std::mutex mCancellableObjectsMutex;
    std::vector<CancellableWPtr> mCancellableObjects;
}; // struct CancellationContext
} // namespace audacity::concurrency
