/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.cpp

  Dominic Mazzoni
 
  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#include "ExportUtils.h"
#include "Track.h"
#include "WaveTrack.h"

//TODO: used in many places in anticipation that Exporter yields same result, fix that
TrackIterRange<const WaveTrack> ExportUtils::FindExportWaveTracks(const TrackList& tracks, bool selectedOnly)
{
   bool anySolo =
      !(tracks.Leaders<const WaveTrack>() + &WaveTrack::GetSolo).empty();

   return tracks.Leaders<const WaveTrack>()
      + (selectedOnly ? &Track::IsSelected : &Track::Any)
      - (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);
}

ExportPlugin::Parameters ExportUtils::ParametersFromEditor(const ExportOptionsEditor& editor)
{
   ExportPlugin::Parameters parameters;
   for(int i = 0, count = editor.GetOptionsCount(); i < count; ++i)
   {
      ExportOption option;
      ExportValue value;
      if(editor.GetOption(i, option) && editor.GetValue(option.id, value))
         parameters.emplace_back(option.id, value);
   }
   return parameters;
}

