/**********************************************************************

Audacity: A Digital Audio Editor

TimeTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TIME_TRACK_CONTROLS__
#define __AUDACITY_TIME_TRACK_CONTROLS__

#include "../../ui/CommonTrackControls.h" // to inherit

class TimeTrackControls final : public CommonTrackControls
{
   TimeTrackControls(const TimeTrackControls&) = delete;
   TimeTrackControls &operator=(const TimeTrackControls&) = delete;

public:
   explicit
   TimeTrackControls( std::shared_ptr<Track> pTrack )
      : CommonTrackControls( pTrack ) {}
   ~TimeTrackControls();

   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &state,
       const AudacityProject *pProject) override;

   PopupMenuTable *GetMenuExtension(Track *pTrack) override;

   static const int kRangeMin {10};
   static const int kRangeMax {1000};
};

#include "../../../widgets/PopupMenuTable.h"

class TimeTrackMenuTable : public PopupMenuTable
{
   TimeTrackMenuTable()
      : PopupMenuTable{ "TimeTrack" }
   {}

   DECLARE_POPUP_MENU(TimeTrackMenuTable);

public:
   static TimeTrackMenuTable &Instance();

protected:
   void InitUserData(void *pUserData) override;

private:
   CommonTrackControls::InitMenuData *mpData{};

   void OnSetTimeTrackRange(wxCommandEvent & /*event*/);
   void OnTimeTrackLin(wxCommandEvent & /*event*/);
   void OnTimeTrackLog(wxCommandEvent & /*event*/);
   void OnTimeTrackLogInt(wxCommandEvent & /*event*/);
};

#endif
