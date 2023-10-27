/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSelectionManager.cpp

Paul Licameli split from ProjectManager.cpp

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SELECTION_MANAGER__
#define __AUDACITY_PROJECT_SELECTION_MANAGER__

#include "ClientData.h" // to inherit
#include "toolbars/SelectionBarListener.h" // to inherit
#include "toolbars/SpectralSelectionBarListener.h" // to inherit
#include "ComponentInterfaceSymbol.h"
#include "Observer.h"

class AudacityProject;

class AUDACITY_DLL_API ProjectSelectionManager final
   : public ClientData::Base
   , public SelectionBarListener
   , public SpectralSelectionBarListener
   , public TimeToolBarListener
{
public:
   static ProjectSelectionManager &Get( AudacityProject &project );
   static const ProjectSelectionManager &Get( const AudacityProject &project );

   explicit ProjectSelectionManager( AudacityProject &project );
   ProjectSelectionManager( const ProjectSelectionManager & ) = delete;
   ProjectSelectionManager &operator=(
      const ProjectSelectionManager & ) = delete;
   ~ProjectSelectionManager() override;

   // SelectionBarListener callback methods
   NumericFormatID AS_GetSelectionFormat() override;
   void AS_SetSelectionFormat(const NumericFormatID & format) override;
   NumericFormatID TT_GetAudioTimeFormat() override;
   void TT_SetAudioTimeFormat(const NumericFormatID & format) override;
   void AS_ModifySelection(double &start, double &end, bool done) override;

   // SpectralSelectionBarListener callback methods
   double SSBL_GetRate() const override;
   NumericFormatID SSBL_GetFrequencySelectionFormatName() override;
   void SSBL_SetFrequencySelectionFormatName(
      const NumericFormatID & formatName) override;
   NumericFormatID SSBL_GetBandwidthSelectionFormatName() override;
   void SSBL_SetBandwidthSelectionFormatName(
      const NumericFormatID & formatName) override;
   void SSBL_ModifySpectralSelection(
      double &bottom, double &top, bool done) override;

private:
   void SnapSelection();

   AudacityProject &mProject;

   Observer::Subscription mSnappingChangedSubscription;
   Observer::Subscription mTimeSignatureChangedSubscription;
   Observer::Subscription mProjectRateChangedSubscription;
};

#endif
