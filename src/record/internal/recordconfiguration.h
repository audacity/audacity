/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../irecordconfiguration.h"

namespace au::record {
class RecordConfiguration : public IRecordConfiguration
{
public:
    muse::draw::Color recordColor() const override;
};
}
