/**********************************************************************
 
 Sneedacity: A Digital Audio Editor
 
 SelectUtilities.h
 
 Paul Licameli split from SelectMenus.h
 
 **********************************************************************/

#ifndef __SNEEDACITY_SELECT_UTILITIES__
#define __SNEEDACITY_SELECT_UTILITIES__

class SneedacityProject;
class Track;

/// Namespace for functions for Select menu
namespace SelectUtilities {

SNEEDACITY_DLL_API void DoSelectTimeAndTracks(
   SneedacityProject &project, bool bAllTime, bool bAllTracks);
SNEEDACITY_DLL_API void SelectAllIfNone( SneedacityProject &project );
SNEEDACITY_DLL_API bool SelectAllIfNoneAndAllowed( SneedacityProject &project );
SNEEDACITY_DLL_API void SelectNone( SneedacityProject &project );
SNEEDACITY_DLL_API void DoListSelection(
   SneedacityProject &project, Track *t,
   bool shift, bool ctrl, bool modifyState );
SNEEDACITY_DLL_API void DoSelectAll( SneedacityProject &project );
SNEEDACITY_DLL_API void DoSelectAllAudio( SneedacityProject &project );
SNEEDACITY_DLL_API void DoSelectSomething( SneedacityProject &project );

}

#endif
