/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: AppEvents.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <functional>

#include "Observer.h"

namespace AppEvents {
/*! Register callback to be called when application is initialized.
 *  If application is already initialized, callback will be called immediately.
 *  @param callback Callback to be called when application is initialized.
 *  @pre `!!calback`
 */
UTILITY_API void OnAppInitialized(std::function<void()> callback);

/*! Register callback to be called when application is closing.
 *  @param callback Callback to be called when application is initialized.
 *  @pre `!!calback`
 */
UTILITY_API void OnAppClosing(std::function<void()> callback);

/*! Register callback to be called when application is idle.
 *  @param callback Callback to be called when application is idle.
 *  @pre `!!calback`
 */
UTILITY_API Observer::Subscription OnAppIdle(std::function<void()> callback);

/*! Base class for application events providers.
 *  This class has no virtual methods and should not be publicly derived from.
 *  Derived classes should use the provided interface to fire events.
 *  It is safe to have multiple instances of derived classes, but it does not
 *  make much sense.
 */
class UTILITY_API ProviderBase /* not final */
{
protected:
    virtual ~ProviderBase() = default;

    void HandleAppInitialized();
    void HandleAppIdle();
    void HandleAppClosing();
};
} // namespace AppEvents
