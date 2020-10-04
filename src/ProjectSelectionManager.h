/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSelectionManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SELECTION_MANAGER__
#define __AUDACITY_PROJECT_SELECTION_MANAGER__

#include "ClientData.h" // to inherit
#include "ComponentInterfaceSymbol.h"
#include "Observer.h"

class AudacityProject;

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

   NumericFormatID AS_GetSelectionFormat();
   void AS_SetSelectionFormat(const NumericFormatID & format);

   NumericFormatID TT_GetAudioTimeFormat();
   void TT_SetAudioTimeFormat(const NumericFormatID & format);
   void AS_ModifySelection(double &start, double &end, bool done);


   double SSBL_GetRate() const;
   NumericFormatID SSBL_GetFrequencySelectionFormatName();
   void SSBL_SetFrequencySelectionFormatName(
      const NumericFormatID & formatName);
   NumericFormatID SSBL_GetBandwidthSelectionFormatName();
   void SSBL_SetBandwidthSelectionFormatName(
      const NumericFormatID & formatName);
   void SSBL_ModifySpectralSelection(
      double &bottom, double &top, bool done);

private:
   void SnapSelection();

   AudacityProject &mProject;

   Observer::Subscription mSnappingChangedSubscription;
   Observer::Subscription mTimeSignatureChangedSubscription;
   Observer::Subscription mProjectRateChangedSubscription;
};

#endif
