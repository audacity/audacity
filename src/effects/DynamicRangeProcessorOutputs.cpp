/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorOutputs.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorOutputs.h"
#include <cassert>
#include <unordered_set>

void DynamicRangeProcessorOutputs::SetEditorCallback(EditorCallback callback)
{
   mEditorCallback = std::move(callback);
}

std::unique_ptr<EffectOutputs> DynamicRangeProcessorOutputs::Clone() const
{
   return std::make_unique<DynamicRangeProcessorOutputs>(*this);
}

void DynamicRangeProcessorOutputs::Assign(EffectOutputs&& src)
{
   auto& srcPackets = static_cast<DynamicRangeProcessorOutputs&>(src).packets;
   if (mEditorCallback)
      mEditorCallback(srcPackets);
   packets = std::move(srcPackets);
}
