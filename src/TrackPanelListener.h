/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanelListener.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL_LISTENER__
#define __AUDACITY_TRACK_PANEL_LISTENER__

class ToolsToolBar;
class ControlToolBar;

class AUDACITY_DLL_API TrackPanelListener {

 public:
   TrackPanelListener(){};
   virtual ~TrackPanelListener(){};

   virtual void TP_DisplaySelection() = 0;
   virtual void TP_DisplayStatusMessage(wxString msg) = 0;

   virtual int TP_GetCurrentTool() = 0;
   virtual ToolsToolBar * TP_GetToolsToolBar() = 0;
   virtual ControlToolBar * TP_GetControlToolBar() = 0;

   virtual void TP_OnPlayKey() = 0;
   virtual void TP_PushState(wxString shortDesc, wxString longDesc,
                            int flags = PUSH_AUTOSAVE) = 0;
   virtual void TP_ModifyState(bool bWantsAutoSave) = 0;    // if true, writes auto-save file. Should set only if you really want the state change restored after
                                                            // a crash, as it can take many seconds for large (eg. 10 track-hours) projects
   virtual void TP_RedrawScrollbars() = 0;
   virtual void TP_ScrollLeft() = 0;
   virtual void TP_ScrollRight() = 0;
   virtual void TP_ScrollWindow(double scrollto) = 0;
   virtual void TP_ScrollUpDown(int delta) = 0;
   virtual void TP_HandleResize() = 0;
};

#endif
