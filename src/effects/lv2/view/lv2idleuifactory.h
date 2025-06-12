/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <suil/suil.h>
#include <memory>

namespace au::effects {
class ILv2IdleUi;
std::unique_ptr<ILv2IdleUi> tryCreateLv2IdleUi(SuilInstance& suilInstance, bool isExternalUi);
}
