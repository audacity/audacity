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

void URLSchemesRegistry::SetRegistrar(SchemeRegistrar registrar)
{
   // Only a single registrar is allowed.
   assert(!m_registrar);

   m_registrar = std::move(registrar);
}

bool URLSchemesRegistry::IsURLHandlingSupported() const noexcept
{
   return !!m_registrar;
}

URLSchemesRegistry& URLSchemesRegistry::Get()
{
   static URLSchemesRegistry registry;
   return registry;
}

bool URLSchemesRegistry::RegisterScheme(std::string_view schema)
{
   if (!m_registrar)
      return false;

   return m_registrar(schema);
}

void URLSchemesRegistry::HandleURL(std::string_view url)
{
   Publish({ url });
}

