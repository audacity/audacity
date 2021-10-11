/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 SelectUtilities.h
 
 Paul Licameli split from SelectMenus.h
 
 **********************************************************************/

#ifndef __AUDACITY_SELECT_UTILITIES__
#define __AUDACITY_SELECT_UTILITIES__

class AudacityProject;
class Track;

/// Namespace for functions for Select menu
namespace SelectUtilities {

AUDACITY_DLL_API void DoSelectTimeAndTracks(
   AudacityProject &project, bool bAllTime, bool bAllTracks);
AUDACITY_DLL_API void SelectAllIfNone( AudacityProject &project );
AUDACITY_DLL_API bool SelectAllIfNoneAndAllowed( AudacityProject &project );
AUDACITY_DLL_API void SelectNone( AudacityProject &project );
AUDACITY_DLL_API void DoListSelection(
   AudacityProject &project, Track *t,
   bool shift, bool ctrl, bool modifyState );
AUDACITY_DLL_API void DoSelectAll( AudacityProject &project );
AUDACITY_DLL_API void DoSelectAllAudio( AudacityProject &project );
AUDACITY_DLL_API void DoSelectSomething( AudacityProject &project );

AUDACITY_DLL_API void LockPlayRegion(AudacityProject &project);
AUDACITY_DLL_API void UnlockPlayRegion(AudacityProject &project);

}

#endif
