/**********************************************************************

  Audacity: A Digital Audio Editor

  DevicePrefs.h

  Joshua Haberman
  James Crook

**********************************************************************/

#ifndef __AUDACITY_DEVICE_PREFS__
#define __AUDACITY_DEVICE_PREFS__

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/string.h>
#include <wx/window.h>
#include <wx/dynarray.h>

#include "../ShuttleGui.h"

#include "PrefsPanel.h"

class DevicePrefs:public PrefsPanel
{
 public:
   DevicePrefs(wxWindow * parent);
   virtual ~DevicePrefs();
   virtual bool Apply();

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S);
   void GetNamesAndLabels();

   void OnHost(wxCommandEvent & e);
   void OnDevice(wxCommandEvent & e);

   /* @return The default playback device name for the selected HostAPI
    *
    * Created so we can set a default that respects the user's choice of API,
    * unlike Pa_GetDefaultOutputDevice() which always returns the default
    * device in the default API.
    * @param index Which HostAPI in the lists mHostNames / mHostIndexes / 
    * mHostLabels the user has selected.
    */
   wxString GetDefaultPlayDevice(int index);
   wxString GetDefaultRecordDevice(int index);

   wxArrayString mHostNames;
   wxArrayString mHostLabels;

   wxString mPlayDevice;
   wxString mRecordDevice;
   long mRecordChannels;

   wxChoice *mHost;
   wxChoice *mPlay;
   wxChoice *mRecord;
   wxChoice *mChannels;

   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: df22b108-e989-4ec4-a8b6-dddbcc7be6a7
