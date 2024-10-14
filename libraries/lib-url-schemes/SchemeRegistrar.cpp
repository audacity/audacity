/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SchemeRegistrar.cpp

  Dmitry Vedenko

**********************************************************************/

#include <string_view>
#include <functional>

#include "URLSchemesRegistry.h"

auto registrar = ([]() {
   URLSchemesRegistry::Get().SetRegistrar([](std::string_view) { return true; });
   return true;
})();
