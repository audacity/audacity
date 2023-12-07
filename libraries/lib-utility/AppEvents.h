/*
 * SPDX-License-Identifier: GPL-2.0-or-later
 * SPDX-FileName: AppEvents.h
 * SPDX-FileContributor: Dmitry Vedenko
 */

#pragma once

#include <functional>

namespace AppEvents
{
/*! Register callback to be called when application is initialized.
 *  If application is already initialized, callback will be called immediately.
 *  @param callback Callback to be called when application is initialized.
 *  @pre `!!calback`
 */
UTILITY_API void OnAppInitialized(std::function<void()> callback);

class UTILITY_API AppEventsProvider /* not final */
{
protected:
   virtual ~AppEventsProvider() = default;

   void HandleAppInitialized();
};

} // namespace AppEvents
