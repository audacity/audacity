/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorOutputs.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include "EffectInterface.h"
#include <array>

class DynamicRangeProcessorOutputs final : public EffectOutputs
{
public:
   using EditorCallback = std::function<void(
      const std::vector<DynamicRangeProcessorOutputPacket>&)>;

   void SetEditorCallback(EditorCallback callback);

   std::vector<DynamicRangeProcessorOutputPacket> packets;

private:
   std::unique_ptr<EffectOutputs> Clone() const override;
   void Assign(EffectOutputs&& src) override;

   EditorCallback mEditorCallback;
};
