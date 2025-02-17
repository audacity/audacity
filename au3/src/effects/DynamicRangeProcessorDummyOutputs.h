/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorDummyOutputs.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "EffectInterface.h"

//! A dummy implementation just to distinguish real-time from non-real-time
//! `CompressorInstance`s.
class DynamicRangeProcessorDummyOutputs final : public EffectOutputs
{
private:
    std::unique_ptr<EffectOutputs> Clone() const override
    {
        return std::make_unique<DynamicRangeProcessorDummyOutputs>();
    }

    void Assign(EffectOutputs&& src) override
    {
    }
};
