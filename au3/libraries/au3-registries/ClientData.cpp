/*!********************************************************************

Audacity: A Digital Audio Editor

@file ClientData.cpp

Paul Licameli

**********************************************************************/

#include "ClientData.h"

// These are needed out-of-line for the Windows link
ClientData::Base::~Base() = default;
template struct REGISTRIES_API ClientData::Cloneable<>;
