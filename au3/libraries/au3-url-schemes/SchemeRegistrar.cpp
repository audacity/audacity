/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SchemeRegistrar.cpp

  Dmitry Vedenko

**********************************************************************/

#include <string_view>
#include <functional>

void SetSchemaRegistrar(std::function<bool(std::string_view)> registrar);

auto registrar = ([]() {
    SetSchemaRegistrar([](std::string_view) { return true; });
    return true;
})();
