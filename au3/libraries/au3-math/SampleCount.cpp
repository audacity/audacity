/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SampleCount.cpp

  Paul Licameli split from audacity/Types.h

**********************************************************************/
#include "SampleCount.h"

#include <algorithm>

#include <wx/debug.h>

size_t sampleCount::as_size_t() const
{
    assert(value >= 0);
    assert(static_cast<std::make_unsigned_t<type> >(value) <= std::numeric_limits<size_t>::max());
    return value;
}

size_t limitSampleBufferSize(size_t bufferSize, sampleCount limit)
{
    return
        std::min(sampleCount(bufferSize), std::max(sampleCount(0), limit))
        .as_size_t();
}
