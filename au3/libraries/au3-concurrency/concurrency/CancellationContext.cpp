/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: CancellationContext.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#include "CancellationContext.h"

#include <algorithm>

#include "ICancellable.h"

namespace audacity::concurrency {
CancellationContext::CancellationContext(Tag)
{
}

std::shared_ptr<CancellationContext> CancellationContext::Create()
{
    return std::make_shared<CancellationContext>(Tag {});
}

void CancellationContext::Cancel()
{
    if (mCancelled.exchange(true)) {
        return;
    }

    std::vector<CancellableWPtr> cancellableObjects;

    {
        auto lock = std::lock_guard { mCancellableObjectsMutex };
        std::swap(cancellableObjects, mCancellableObjects);
    }

    std::for_each(
        cancellableObjects.begin(), cancellableObjects.end(),
        [](auto& wptr)
    {
        if (auto lock = wptr.lock()) {
            lock->Cancel();
        }
    });
}

void CancellationContext::OnCancelled(CancellableWPtr cancellable)
{
    auto locked = cancellable.lock();
    if (!locked) {
        return;
    }

    if (mCancelled.load(std::memory_order_acquire)) {
        locked->Cancel();
        return;
    }

    auto lock = std::lock_guard { mCancellableObjectsMutex };
    mCancellableObjects.erase(
        std::remove_if(
            mCancellableObjects.begin(), mCancellableObjects.end(),
            [](auto& wptr) { return wptr.expired(); }),
        mCancellableObjects.end());

    mCancellableObjects.push_back(std::move(cancellable));
}
} // namespace audacity::concurrency
