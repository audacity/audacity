/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSelectionManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SELECTION_MANAGER__
#define __AUDACITY_PROJECT_SELECTION_MANAGER__

#include "ClientData.h" // to inherit
#include "Observer.h"
#include "ComponentInterfaceSymbol.h"
#include "Observer.h"

class AudacityProject;
struct ProjectNumericFormatsEvent;

class AUDACITY_DLL_API ProjectSelectionManager final
   : public ClientData::Base
{
public:
   static ProjectSelectionManager &Get( AudacityProject &project );
   static const ProjectSelectionManager &Get( const AudacityProject &project );

   explicit ProjectSelectionManager( AudacityProject &project );
   ProjectSelectionManager( const ProjectSelectionManager & ) = delete;
   ProjectSelectionManager &operator=(
      const ProjectSelectionManager & ) = delete;
   ~ProjectSelectionManager() override;

private:
   void OnFormatsChanged(ProjectNumericFormatsEvent);

public:
   void SetSelectionFormat(const NumericFormatID & format);
   void SetAudioTimeFormat(const NumericFormatID & format);
   void ModifySelection(double &start, double &end, bool done);
   void SetFrequencySelectionFormatName(
      const NumericFormatID & formatName);
   void SetBandwidthSelectionFormatName(
      const NumericFormatID & formatName);
   void ModifySpectralSelection(
      double &bottom, double &top, bool done);

private:
   void SnapSelection();

   Observer::Subscription mFormatsSubscription;
   AudacityProject &mProject;

   Observer::Subscription mSnappingChangedSubscription;
   Observer::Subscription mTimeSignatureChangedSubscription;
   Observer::Subscription mProjectRateChangedSubscription;
};

#endif
