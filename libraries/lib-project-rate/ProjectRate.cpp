/*!********************************************************************

Audacity: A Digital Audio Editor

@file ProjectRate.cpp

Paul Licameli split from ProjectSettings.cpp

**********************************************************************/

#include "ProjectRate.h"

#include "AudioIOBase.h"
#include "Prefs.h"
#include "Project.h"
#include "QualitySettings.h"
#include "XMLWriter.h"

wxDEFINE_EVENT(EVT_PROJECT_RATE_CHANGE, wxEvent);

namespace {
   struct MyEvent : wxEvent {
      MyEvent() : wxEvent{ 0, EVT_PROJECT_RATE_CHANGE } {}
      wxEvent *Clone() const override { return new MyEvent{*this}; }
   };

   void Notify( AudacityProject &project )
   {
      MyEvent e;
      project.ProcessEvent( e );
   }
}

static const AudacityProject::AttachedObjects::RegisteredFactory
sKey{
  []( AudacityProject &project ){
     auto result = std::make_shared< ProjectRate >(project);
     return result;
   }
};

ProjectRate &ProjectRate::Get( AudacityProject &project )
{
   return project.AttachedObjects::Get< ProjectRate >( sKey );
}

const ProjectRate &ProjectRate::Get( const AudacityProject &project )
{
   return Get( const_cast< AudacityProject & >( project ) );
}

ProjectRate::ProjectRate(AudacityProject &project)
   : mProject{ project }
{
   int intRate = 0;
   bool wasDefined = QualitySettings::DefaultSampleRate.Read( &intRate );
   mRate = intRate;
   if ( !wasDefined ) {
      // The default given above can vary with host/devices. So unless there is
      // an entry for the default sample rate in audacity.cfg, Audacity can open
      // with a rate which is different from the rate with which it closed.
      // See bug 1879.
      QualitySettings::DefaultSampleRate.Write( mRate );
      gPrefs->Flush();
   }
}

double ProjectRate::GetRate() const
{
   return mRate;
}

void ProjectRate::SetRate(double rate)
{
   if (rate != mRate) {
      mRate = rate;
      Notify(mProject);
   }
}

static ProjectFileIORegistry::WriterEntry entry {
[](const AudacityProject &project, XMLWriter &xmlFile){
   xmlFile.WriteAttr(wxT("rate"), ProjectRate::Get(project).GetRate());
}
};

static ProjectFileIORegistry::AttributeReaderEntries entries {
// Just a pointer to function, but needing overload resolution as non-const:
(ProjectRate& (*)(AudacityProject &)) &ProjectRate::Get, {
   { L"rate", [](auto &settings, auto value){
      double rate;
      Internat::CompatibleToDouble(value, &rate);
      settings.SetRate( rate );
   } },
} };
