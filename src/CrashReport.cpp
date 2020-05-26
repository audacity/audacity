/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 CrashReport.cpp
 
 *//*******************************************************************/

#include "Audacity.h"
#include "CrashReport.h"
#include "Experimental.h"

#if defined(EXPERIMENTAL_CRASH_REPORT)

#if defined(__WXMSW__)
#include <wx/evtloop.h>
#endif

#include "wxFileNameWrapper.h"
#include "AudacityLogger.h"
#include "AudioIOBase.h"
#include "FileNames.h"
#include "Internat.h"
#include "prefs/GUIPrefs.h"
#include "widgets/ErrorDialog.h"

namespace CrashReport {

void Generate(wxDebugReport::Context ctx)
{
   wxDebugReportCompress rpt;
   rpt.AddAll(ctx);
   
   wxFileNameWrapper fn{ FileNames::DataDir(), wxT("audacity.cfg") };
   rpt.AddFile(fn.GetFullPath(), _TS("Audacity Configuration"));
   rpt.AddFile(FileNames::PluginRegistry(), wxT("Plugin Registry"));
   rpt.AddFile(FileNames::PluginSettings(), wxT("Plugin Settings"));
   
   if (ctx == wxDebugReport::Context_Current)
   {
      auto saveLang = GUIPrefs::GetLangShort();
      GUIPrefs::InitLang( wxT("en") );
      auto cleanup = finally( [&]{ GUIPrefs::InitLang( saveLang ); } );
      
      auto gAudioIO = AudioIOBase::Get();
      rpt.AddText(wxT("audiodev.txt"), gAudioIO->GetDeviceInfo(), wxT("Audio Device Info"));
#ifdef EXPERIMENTAL_MIDI_OUT
      rpt.AddText(wxT("mididev.txt"), gAudioIO->GetMidiDeviceInfo(), wxT("MIDI Device Info"));
#endif
   }
   
   auto logger = AudacityLogger::Get();
   if (logger)
   {
      rpt.AddText(wxT("log.txt"), logger->GetLog(), _TS("Audacity Log"));
   }
   
   bool ok = wxDebugReportPreviewStd().Show(rpt);
   
#if defined(__WXMSW__)
   wxEventLoop::SetCriticalWindow(NULL);
#endif
   
   if (ok && rpt.Process())
   {
      AudacityTextEntryDialog dlg(nullptr,
         XO("Report generated to:"),
         XO("Audacity Support Data"),
         rpt.GetCompressedFileName(),
         wxOK | wxCENTER);
      dlg.SetName(dlg.GetTitle());
      dlg.ShowModal();
      
      wxLogMessage(wxT("Report generated to: %s"),
                   rpt.GetCompressedFileName());
      
      rpt.Reset();
   }
}

}
#endif
