/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene {
//! Per-column waveform data ready for vertex generation
struct WaveColumnVertex
{
    float x;           //!< Pixel x-coordinate within the clip
    float maxY;        //!< Top of waveform (pixel y)
    float minY;        //!< Bottom of waveform (pixel y)
    float rmsMaxY;     //!< Top of RMS band (pixel y)
    float rmsMinY;     //!< Bottom of RMS band (pixel y)
    bool selected;     //!< Whether this column is in the selection range
    bool clipping;     //!< Whether this column clips
};

//! One sample ready for vertex generation
struct WaveSamplePoint
{
    float x;           //!< Pixel x-coordinate within the clip
    float y;           //!< Pixel y-coordinate within the channel
    bool selected;     //!< Whether the sample is in the selection range
    bool clipping;     //!< Whether the sample clips
};
}
