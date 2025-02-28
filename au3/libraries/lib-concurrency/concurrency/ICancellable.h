/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: ICancellable.cpp
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

namespace audacity::concurrency {
class CONCURRENCY_API ICancellable
{
public:
    virtual ~ICancellable() = default;

    virtual void Cancel() = 0;
};
} // namespace audacity::concurrency
