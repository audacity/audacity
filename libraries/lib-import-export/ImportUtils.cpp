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

#include <wx/filename.h>

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

std::shared_ptr<WaveTrack> ImportUtils::NewWaveTrack(
   WaveTrackFactory &trackFactory, sampleFormat effectiveFormat, double rate)
{
   return trackFactory.Create(ChooseFormat(effectiveFormat), rate);
}

void ImportUtils::ShowMessageBox(const TranslatableString &message, const TranslatableString& caption)
{
   BasicUI::ShowMessageBox(message,
                           BasicUI::MessageBoxOptions().Caption(caption));
}

std::shared_ptr<TrackList>
ImportUtils::MakeTracks(const NewChannelGroup &channels)
{
   auto result = TrackList::Temporary(nullptr, channels);
   for (const auto pTrack : result->Any<WaveTrack>())
      pTrack->Flush();
   return result;
}
