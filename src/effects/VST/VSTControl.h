/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControl.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_AUCONTROL_H
#define AUDACITY_AUCONTROL_H

#if !defined(_LP64)
#include <Carbon/Carbon.h>
#endif

#include <wx/osx/private.h>
#include <wx/control.h>

//#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AudioComponent.h>
//#include <AudioUnit/AudioUnitProperties.h>

class VSTControlImpl : public wxWidgetCocoaImpl
{
public :
   VSTControlImpl(wxWindowMac *peer, NSView *view);
   ~VSTControlImpl();
};

class VSTControl : public wxControl
{
public:
   VSTControl();
   ~VSTControl();

   bool Create(wxWindow *parent, AudioComponent comp, AudioUnit unit, bool custom);
   void CreateCocoa();
   void CreateGeneric();
   void CocoaViewResized();

   void OnSize(wxSizeEvent & evt);

#if !defined(_LP64)
   void CreateCarbon();
   void CreateCarbonOverlay();
   void CarbonViewResized();
   static pascal OSStatus ControlEventHandlerCallback(EventHandlerCallRef handler,
                                                      EventRef event,
                                                      void *data);
#endif

private:
   AudioComponent mComponent;
   AudioUnit mUnit;

   NSView *mAUView;
   NSView *mView;

   wxSize mLastMin;
   bool mSettingSize;

#if !defined(_LP64)
   AudioComponentInstance mInstance;
   WindowRef mWindowRef;
   HIViewRef mHIView;
#endif

   DECLARE_EVENT_TABLE();
};

#endif
