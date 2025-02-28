/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportUtils.h

  Dominic Mazzoni

  Vitaly Sverchinsky split from ImportPlugin.h

**********************************************************************/

#pragma once

#include <memory>
#include <vector>

#include "Import.h"
#include "SampleFormat.h"
#include "Internat.h"
#include "Track.h"

class wxString;

class TrackList;
class WaveTrackFactory;
class WaveTrack;
class WaveChannel;

class IMPORT_EXPORT_API ImportUtils final
{
public:

    //! Choose appropriate format, which will not be narrower than the specified one
    static sampleFormat ChooseFormat(sampleFormat effectiveFormat);

    //! Builds a wave track
    //! The format will not be narrower than the specified one.
    static std::shared_ptr<WaveTrack>
    NewWaveTrack(WaveTrackFactory& trackFactory, unsigned nChannels, sampleFormat effectiveFormat, double rate);

    static void ShowMessageBox(const TranslatableString& message, const TranslatableString& caption = XO("Import Project"));

    //! Iterates over channels in each wave track from the list
    static
    void ForEachChannel(TrackList& trackList, const std::function<void(WaveChannel&)>& op);

    //! Iterates over channels in one wave track
    static
    void ForEachChannel(WaveTrack& track, const std::function<void(WaveChannel&)>& op);

    //! Flushes the given channels and moves them to \p outTracks
    static
    void FinalizeImport(TrackHolders& outTracks, const std::vector<std::shared_ptr<WaveTrack> >& importedStreams);

    //! Flushes the given channels and moves them to \p outTracks
    //! \p trackList is emptied
    static
    void FinalizeImport(TrackHolders& outTracks, TrackList&& trackList);

    //! Flushes the given channels and moves them to \p outTracks
    static
    void FinalizeImport(TrackHolders& outTracks, WaveTrack& track);
};
