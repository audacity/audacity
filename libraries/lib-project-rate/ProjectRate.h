/*!********************************************************************

Audacity: A Digital Audio Editor

@file ProjectRate.h
@brief an object holding per-project preferred sample rate

Paul Licameli split from ProjectSettings.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_RATE__
#define __AUDACITY_PROJECT_RATE__

class AudacityProject;

#include "ClientData.h"
#include <wx/event.h> // to declare custom event type

// Sent to the project when the rate changes
wxDECLARE_EXPORTED_EVENT(PROJECT_RATE_API,
   EVT_PROJECT_RATE_CHANGE, wxEvent);

///\brief Holds project sample rate
class PROJECT_RATE_API ProjectRate final
   : public ClientData::Base
{
public:
   static ProjectRate &Get( AudacityProject &project );
   static const ProjectRate &Get( const AudacityProject &project );
   
   explicit ProjectRate(AudacityProject &project);
   ProjectRate( const ProjectRate & ) PROHIBITED;
   ProjectRate &operator=( const ProjectRate & ) PROHIBITED;

   void SetRate(double rate);
   double GetRate() const;

private:
   AudacityProject &mProject;
   double mRate;
};

#endif

