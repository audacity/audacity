/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportUtils.cpp

  Dominic Mazzoni
 
  Vitaly Sverchinsky split from ImportPlugin.cpp

**********************************************************************/

#include "ImportUtils.h"

#include "WaveTrack.h"
#include "QualitySettings.h"
#include "BasicUI.h"

sampleFormat ImportUtils::ChooseFormat(sampleFormat effectiveFormat)
{
   // Consult user preference
   auto defaultFormat = QualitySettings::SampleFormatChoice();

   // Don't choose format narrower than effective or default
   auto format = std::max(effectiveFormat, defaultFormat);

   // But also always promote 24 bits to float
   if (format > int16Sample)
      format = floatSample;

   return format;
}

WaveTrack::Holder
ImportUtils::NewWaveTrack(WaveTrackFactory &trackFactory,
                          unsigned nChannels,
                          sampleFormat effectiveFormat,
                          double rate)
{
   return trackFactory.Create(nChannels, ChooseFormat(effectiveFormat), rate);
}

void ImportUtils::ShowMessageBox(const TranslatableString &message, const TranslatableString& caption)
{
   BasicUI::ShowMessageBox(message,
                           BasicUI::MessageBoxOptions().Caption(caption));
}

void ImportUtils::FinalizeImport(TrackHolders& outTracks, const std::vector<WaveTrack::Holder>& importedStreams)
{
   for(auto& stream : importedStreams)
      FinalizeImport(outTracks, *stream);
}

void ImportUtils::FinalizeImport(TrackHolders& outTracks, TrackList &&trackList)
{
   if(trackList.empty())
      return;

   for(const auto track : trackList.Any<WaveTrack>())
      track->Flush();
   
   while (!trackList.empty())
      outTracks.push_back(trackList.DetachFirst());
}

void ImportUtils::FinalizeImport(TrackHolders& outTracks, WaveTrack &track)
{
   track.Flush();
   outTracks.push_back(track.shared_from_this());
}
