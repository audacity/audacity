/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  URLSchemesRegistry.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <string_view>
#include <functional>

#include "Observer.h"

//! A message that is invoked when a custom-scheme URL is passed to Audacity
struct URLschemeHandlerMessage final
{
    std::string_view url;
};

//! Registry for custom URL schemes, that enables Audacity to handle open-url
//! system requests
class URL_SCHEMES_API URLSchemesRegistry final : public Observer::Publisher<URLschemeHandlerMessage>
{
    URLSchemesRegistry() = default;
    URLSchemesRegistry(const URLSchemesRegistry&) = delete;
    URLSchemesRegistry& operator=(const URLSchemesRegistry&) = delete;

public:
    //! Retrieves the registry instance
    static URLSchemesRegistry& Get();

    //! Returns true, if Audacity can handle custom URLs
    bool IsURLHandlingSupported() const noexcept;

    //! Associates a new scheme with Audacity.
    /*!
     * On Windows, it is ensured that the last opened
     * will handle the URL. Returns true if Audacity succeeded updating
     * the registry.
     *
     * On macOS, scheme registration is done using the Info.plist
     * file and this function always returns true.
     *
     * On other platforms - always returns false.
     */
    bool RegisterScheme(std::string_view scheme);

    //! Called by the application main class
    //! when a custom-scheme URL is passed to Audacity,
    //! effectively sending URLschemeHandlerMessage to
    //! subscribers.
    void HandleURL(std::string_view url);
};
