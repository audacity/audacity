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

TrackListHolder
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

void ImportUtils::FinalizeImport(TrackHolders& outTracks, const std::vector<TrackListHolder>& importedStreams)
{
   for(auto& stream : importedStreams)
      FinalizeImport(outTracks, stream);
}

void ImportUtils::FinalizeImport(TrackHolders& outTracks, TrackListHolder trackList)
{
   if(trackList->empty())
      return;

   for(const auto track : trackList->Any<WaveTrack>())
      track->Flush();
   
   outTracks.push_back(std::move(trackList));
}

void ImportUtils::ForEachChannel(TrackList& trackList, const std::function<void(WaveChannel&)>& op)
{
   for(auto track : trackList.Any<WaveTrack>())
   {
      for(auto channel : track->Channels())
      {
         op(*channel);
      }
   }
}
