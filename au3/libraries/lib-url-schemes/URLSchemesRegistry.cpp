/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  URLSchemesRegistry.cpp

  Dmitry Vedenko

**********************************************************************/

#include "URLSchemesRegistry.h"

#include <cassert>
#include <string>
#include <vector>
#include <memory>

namespace {
std::function<bool(std::string_view)> SchemeRegistrar;
}

void SetSchemaRegistrar(std::function<bool(std::string_view)> registrar);
void SetSchemaRegistrar(std::function<bool(std::string_view)> registrar)
{
    // Only a single registrar is allowed.
    assert(!SchemeRegistrar);

    SchemeRegistrar = std::move(registrar);
}

bool URLSchemesRegistry::IsURLHandlingSupported() const noexcept
{
    return !!SchemeRegistrar;
}

URLSchemesRegistry& URLSchemesRegistry::Get()
{
    static URLSchemesRegistry registry;
    return registry;
}

bool URLSchemesRegistry::RegisterScheme(std::string_view schema)
{
    if (!SchemeRegistrar) {
        return false;
    }

    return SchemeRegistrar(schema);
}

void URLSchemesRegistry::HandleURL(std::string_view url)
{
    Publish({ url });
}
