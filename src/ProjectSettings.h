/**********************************************************************

Audacity: A Digital Audio Editor

ProjectSettings.h

Paul Licameli split from AudacityProject.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_SETTINGS__
#define __AUDACITY_PROJECT_SETTINGS__

#include <atomic>
#include <wx/event.h> // to declare custom event type

#include "ClientData.h" // to inherit
#include "Prefs.h" // to inherit
#include "audacity/Types.h"

class AudacityProject;

// Sent to the project when certain settings change
// See enum EventCode below for values of GetInt() identifying changed setting
// The previous value of that setting can also be found using GetExtraLong()
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
   EVT_PROJECT_SETTINGS_CHANGE, wxCommandEvent);

enum
{
   SNAP_OFF,
   SNAP_NEAREST,
   SNAP_PRIOR
};

namespace ToolCodes {
enum {
   // The buttons that are in the Tools toolbar must be in correspondence
   // with the first few
   selectTool,
   envelopeTool,
   drawTool,
   multiTool,

#ifdef EXPERIMENTAL_BRUSH_TOOL
   brushTool,
#endif

   numTools,
   firstTool = selectTool,
};
}

///\brief Holds various per-project settings values,
/// and sends events to the project when certain values change
class AUDACITY_DLL_API ProjectSettings final
   : public ClientData::Base
   , private PrefsListener
{
public:
   static ProjectSettings &Get( AudacityProject &project );
   static const ProjectSettings &Get( const AudacityProject &project );
   
   // Values retrievable from GetInt() of the event for settings change
   enum EventCode : int {
      ChangedSyncLock,
      ChangedTool,
   };

   explicit ProjectSettings( AudacityProject &project );
   ProjectSettings( const ProjectSettings & ) PROHIBITED;
   ProjectSettings &operator=( const ProjectSettings & ) PROHIBITED;


   bool GetTracksFitVerticallyZoomed() const { return mTracksFitVerticallyZoomed; } //lda
   void SetTracksFitVerticallyZoomed(bool flag) { mTracksFitVerticallyZoomed = flag; } //lda

   bool GetShowId3Dialog() const { return mShowId3Dialog; } //lda
   void SetShowId3Dialog(bool flag) { mShowId3Dialog = flag; } //lda

   bool IsSyncLocked() const;
   void SetSyncLock(bool flag);
   
   // Snap To

   void SetSnapTo(int snap);
   int GetSnapTo() const;

   // Current tool

   void SetTool(int tool);
   int GetTool() const { return mCurrentTool; }

   // Current brush radius
   void SetBrushRadius(int brushRadius) { mCurrentBrushRadius = brushRadius; }
   int GetBrushRadius() const { return mCurrentBrushRadius; }

   void SetSmartSelection(bool isSelected) { mbSmartSelection = isSelected; }
   bool IsSmartSelection() const { return mbSmartSelection; }

   void SetOvertones(bool isSelected) { mbOvertones = isSelected; }
   bool IsOvertones() const { return mbOvertones; }

   bool IsSoloSimple() const { return mSoloPref == wxT("Simple"); }
   bool IsSoloNone() const { return mSoloPref == wxT("None"); }

   bool EmptyCanBeDirty() const { return mEmptyCanBeDirty; }

   bool GetShowSplashScreen() const { return mShowSplashScreen; }

private:
   void UpdatePrefs() override;

   AudacityProject &mProject;

   wxString mSoloPref;

   int mSnapTo;

   int mCurrentTool;
   int mCurrentBrushRadius;
   int mCurrentBrushHop;
   bool mbSmartSelection { false };
   bool mbOvertones { false };
   
   bool mTracksFitVerticallyZoomed{ false };  //lda
   bool mShowId3Dialog{ true }; //lda
   bool mIsSyncLocked{ false };
   bool mEmptyCanBeDirty;
   bool mShowSplashScreen;
};

#endif
