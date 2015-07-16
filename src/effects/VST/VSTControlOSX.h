/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlOSX.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_VSTCONTROLOSX_H
#define AUDACITY_VSTCONTROLOSX_H

#if !defined(_LP64)
#include <Carbon/Carbon.h>
#endif

#include <wx/osx/private.h>

#include <wx/control.h>

#include "aeffectx.h"

class VSTControlImpl : public wxWidgetCocoaImpl
{
public :
   VSTControlImpl(wxWindowMac *peer, NSView *view);
   ~VSTControlImpl();
};

class VSTControl : public VSTControlBase
{
public:
   VSTControl();
   ~VSTControl();

   bool Create(wxWindow *parent, VSTEffectLink *link);

   void CreateCocoa();
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
   NSView *mVSTView;
   NSView *mView;

   wxSize mLastMin;
   bool mSettingSize;

#if !defined(_LP64)

   WindowRef mWindowRef;
   WindowRef mPreviousRef;
   HIViewRef mHIView;

#endif

   DECLARE_EVENT_TABLE();
};

#endif
