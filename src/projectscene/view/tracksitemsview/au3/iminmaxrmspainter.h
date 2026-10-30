#pragma once

#include <cstdint>
#include <vector>

#include "modularity/imoduleinterface.h"
#include "trackedit/trackedittypes.h"

#include "iau3wavepainter.h"
#include "WaveMetrics.h"
#include "../wavecolumn.h"

namespace au::projectscene {
class IMinMaxRMSPainter : public IAu3WavePainter, MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IMinMaxRMSPainter)
public:
    //! A clip's times and what the columns drawn for it depend on
    struct ClipData {
        bool valid = false;
        size_t channelCount = 0;
        double playStartTime = 0.0;
        double playEndTime = 0.0;
        double trimLeft = 0.0;
        double sequenceEndTime = 0.0;
        double samplesPerSecond = 0.0;
        //! Changes whenever the columns of an unchanged window may change; unique across clips
        uint64_t dataVersion = 0;
        //! Width of the cache tiles the columns are computed in, in pixels
        double tileWidth = 0.0;
    };

    virtual ClipData clipData(const trackedit::ClipKey& clipKey) = 0;

    //! Fills columns for one channel of the window in metrics, whose times are relative to the
    //! untrimmed sequence start. Returns false while some of the data is still being computed.
    virtual bool columns(const trackedit::ClipKey& clipKey, size_t channelIndex, const IWavePainter::Params& params,
                         const WaveMetrics& metrics, std::vector<WaveColumnVertex>& columns) = 0;
};
}
