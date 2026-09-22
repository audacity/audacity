/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM Vamp Plugin Set

    Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include <vamp/vamp.h>
#include <vamp-sdk/PluginAdapter.h>

#include "plugins/BeatTrack.h"
#include "plugins/OnsetDetect.h"
#include "plugins/ChromagramPlugin.h"
#include "plugins/ConstantQSpectrogram.h"
#include "plugins/TonalChangeDetect.h"
#include "plugins/KeyDetect.h"
#include "plugins/MFCCPlugin.h"
#include "plugins/SegmenterPlugin.h"
#include "plugins/SimilarityPlugin.h"
#include "plugins/BarBeatTrack.h"
#include "plugins/AdaptiveSpectrogram.h"
#include "plugins/DWT.h"
#include "plugins/Transcription.h"

static Vamp::PluginAdapter<BeatTracker> beatTrackerAdapter;
static Vamp::PluginAdapter<OnsetDetector> onsetDetectorAdapter;
static Vamp::PluginAdapter<ChromagramPlugin> chromagramPluginAdapter;
static Vamp::PluginAdapter<ConstantQSpectrogram> constantQAdapter;
static Vamp::PluginAdapter<TonalChangeDetect> tonalChangeDetectorAdapter;
static Vamp::PluginAdapter<KeyDetector> keyDetectorAdapter;
static Vamp::PluginAdapter<MFCCPlugin> mfccPluginAdapter;
static Vamp::PluginAdapter<SegmenterPlugin> segmenterPluginAdapter;
static Vamp::PluginAdapter<SimilarityPlugin> similarityPluginAdapter;
static Vamp::PluginAdapter<BarBeatTracker> barBeatTrackPluginAdapter;
static Vamp::PluginAdapter<AdaptiveSpectrogram> adaptiveSpectrogramAdapter;
static Vamp::PluginAdapter<DWT> dwtAdapter;
static Vamp::PluginAdapter<Transcription> transcriptionAdapter;

const VampPluginDescriptor *vampGetPluginDescriptor(unsigned int vampApiVersion,
                                                    unsigned int index)
{
    if (vampApiVersion < 1) return 0;

    switch (index) {
    case  0: return beatTrackerAdapter.getDescriptor();
    case  1: return onsetDetectorAdapter.getDescriptor();
    case  2: return chromagramPluginAdapter.getDescriptor();
    case  3: return constantQAdapter.getDescriptor();
    case  4: return tonalChangeDetectorAdapter.getDescriptor();
    case  5: return keyDetectorAdapter.getDescriptor();
    case  6: return segmenterPluginAdapter.getDescriptor();
    case  7: return similarityPluginAdapter.getDescriptor();
    case  8: return mfccPluginAdapter.getDescriptor();
    case  9: return barBeatTrackPluginAdapter.getDescriptor();
    case 10: return dwtAdapter.getDescriptor();
    case 11: return adaptiveSpectrogramAdapter.getDescriptor();
    case 12: return transcriptionAdapter.getDescriptor();
    default: return 0;
    }
}

