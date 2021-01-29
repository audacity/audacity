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

void DoSelectTimeAndTracks(
   AudacityProject &project, bool bAllTime, bool bAllTracks);
void SelectAllIfNone( AudacityProject &project );
bool SelectAllIfNoneAndAllowed( AudacityProject &project );
void SelectNone( AudacityProject &project );
void DoListSelection(
   AudacityProject &project, Track *t,
   bool shift, bool ctrl, bool modifyState );
void DoSelectAll( AudacityProject &project );
void DoSelectAllAudio( AudacityProject &project );
void DoSelectSomething( AudacityProject &project );

}

#endif
