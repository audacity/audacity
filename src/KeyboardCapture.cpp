/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 KeyboardCapture.cpp
 
 Paul Licameli split this from Project.cpp
 
 **********************************************************************/

#include "KeyboardCapture.h"

#if defined(__WXMAC__)
#include <wx/textctrl.h>
#include <AppKit/AppKit.h>
#include <wx/osx/core/private.h>
#include <wx/osx/cocoa/private.h>
#elif defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#include <wx/app.h>
#include <wx/button.h>
#include <wx/eventfilter.h>
#include <wx/toplevel.h>
#include <wx/weakref.h>
#include <wx/window.h>

#include "AudacityException.h"

////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////
DEFINE_EVENT_TYPE(EVT_CAPTURE_KEY);

namespace {

wxWindowRef &sHandler()
{
   static wxWindowRef theHandler;
   return theHandler;
}
KeyboardCapture::FilterFunction &sPreFilter()
{
   static KeyboardCapture::FilterFunction theFilter;
   return theFilter;
}
KeyboardCapture::FilterFunction &sPostFilter()
{
   static KeyboardCapture::FilterFunction theFilter;
   return theFilter;
}

}

namespace KeyboardCapture
{

// Keyboard capture
bool IsHandler(const wxWindow *handler)
{
   return GetHandler() == handler;
}

wxWindow *GetHandler()
{
   return sHandler();
}

void Capture(wxWindow *handler)
{
   sHandler() = handler;
}

void Release(wxWindow *handler)
{
//   wxASSERT(sHandler() == handler);
   sHandler() = nullptr;
}

FilterFunction SetPreFilter( const FilterFunction &function )
{
   auto result = sPreFilter();
   sPreFilter() = function;
   return result;
}

FilterFunction SetPostFilter( const FilterFunction &function )
{
   auto result = sPostFilter();
   sPostFilter() = function;
   return result;
}

void OnFocus( wxWindow &window, wxFocusEvent &event )
{
   if (event.GetEventType() == wxEVT_KILL_FOCUS)
      KeyboardCapture::Release( &window );
   else
      KeyboardCapture::Capture( &window );

   window.Refresh( false );
   event.Skip();
}

}

// Shared by all projects
static class EventMonitor final : public wxEventFilter
{
public:
   EventMonitor()
   :  wxEventFilter()
   {
#if defined(__WXMAC__)
      // In wx3, the menu accelerators take precedence over key event processing
      // so we won't get wxEVT_CHAR_HOOK events for combinations assigned to menus.
      // Since we only support OS X 10.6 or greater, we can use an event monitor
      // to capture the key event before it gets to the normal wx3 processing.

      // The documentation for addLocalMonitorForEventsMatchingMask implies that
      // NSKeyUpMask can't be used in 10.6, but testing shows that it can.
      NSEventMask mask = NSKeyDownMask | NSKeyUpMask;

      mHandler =
      [
         NSEvent addLocalMonitorForEventsMatchingMask:mask handler:^NSEvent *(NSEvent *event)
         {
            WXWidget widget = (WXWidget) [ [event window] firstResponder];
            if (widget)
            {
               wxWidgetCocoaImpl *impl = (wxWidgetCocoaImpl *)
                  wxWidgetImpl::FindFromWXWidget(widget);
               if (impl)
               {
                  mEvent = event;

                  wxKeyEvent wxevent([event type] == NSKeyDown ? wxEVT_KEY_DOWN : wxEVT_KEY_UP);
                  impl->SetupKeyEvent(wxevent, event);

                  NSEvent *result;
                  if ([event type] == NSKeyDown)
                  {
                     wxKeyEvent eventHook(wxEVT_CHAR_HOOK, wxevent);
                     result = FilterEvent(eventHook) == Event_Processed ? nil : event;
                  }
                  else
                  {
                     result = FilterEvent(wxevent) == Event_Processed ? nil : event;
                  }

                  mEvent = nullptr;
                  return result;
               }
            }

            return event;
         }
      ];

      // Bug1252: must also install this filter with wxWidgets, else
      // we don't intercept command keys when focus is in a combo box.
      wxEvtHandler::AddFilter(this);
#else

      wxEvtHandler::AddFilter(this);

#endif
   }

   ~EventMonitor() override
   {
#if defined(__WXMAC__)
      wxEvtHandler::RemoveFilter(this);
      [NSEvent removeMonitor:mHandler];
#else
      wxEvtHandler::RemoveFilter(this);
#endif
   }

   int FilterEvent(wxEvent& event) override
   {
      // Unguarded exception propagation may crash the program, at least
      // on Mac while in the objective-C closure above
      return GuardedCall< int > ( [&] {
         // Quickly bail if this isn't something we want.
         wxEventType type = event.GetEventType();
         if (type != wxEVT_CHAR_HOOK && type != wxEVT_KEY_UP)
         {
            return Event_Skip;
         }

         wxKeyEvent key = static_cast<wxKeyEvent &>( event );

         if ( !( sPreFilter() && sPreFilter()( key ) ) )
            return Event_Skip;

#ifdef __WXMAC__
         // Bugs 1329 and 2107 (Mac only)
         // wxButton::SetDefault() alone doesn't cause correct event routing
         // of key-down to the button when a text entry or combo has focus,
         // but we can intercept wxEVT_CHAR_HOOK here and do it
         if ( type == wxEVT_CHAR_HOOK &&
            key.GetKeyCode() == WXK_RETURN ) {
            const auto focus = wxWindow::FindFocus();
            // Bug 2267 (Mac only): don't apply fix for 2107 when a text entry
            // needs to allow multiple line input
            const auto text = dynamic_cast<wxTextCtrl*>(focus);
            if ( !(text && text->IsMultiLine()) ) {
               if (auto top =
                  dynamic_cast< wxTopLevelWindow* >(
                     wxGetTopLevelParent( focus ) ) ) {
                  if ( auto button =
                     dynamic_cast<wxButton*>( top->GetDefaultItem() ) ) {
                     wxCommandEvent newEvent{ wxEVT_BUTTON, button->GetId() };
                     button->GetEventHandler()->AddPendingEvent( newEvent );
                     return Event_Processed;
                  }
               }
            }
         }
#endif

         // Make a copy of the event and (possibly) make it look like a key down
         // event.
         if (type == wxEVT_CHAR_HOOK)
         {
            key.SetEventType(wxEVT_KEY_DOWN);
         }

         // Give the capture handler first dibs at the event.
         wxWindow *handler = KeyboardCapture::GetHandler();
         if (handler && HandleCapture(handler, key))
         {
            return Event_Processed;
         }

         if ( sPostFilter() && sPostFilter()( key ) )
            return Event_Processed;

         // Give it back to WX for normal processing.
         return Event_Skip;
      },
      // Immediate handler invokes the same high level catch-all as for
      // unhandled exceptions, which will also do its own delayed handling
      [](AudacityException *pEx){
         if (pEx)
            wxTheApp->OnExceptionInMainLoop();
         else
            throw;
         return Event_Processed;
      },
      // So don't duplicate delayed handling:
      [](auto){}
      );
   }

private:

   // Returns true if the event was captured and processed
   bool HandleCapture(wxWindow *target, const wxKeyEvent & event)
   {
      if (wxGetTopLevelParent(target) != wxGetTopLevelParent(wxWindow::FindFocus()))
      {
         return false;
      }
      wxEvtHandler *handler = target->GetEventHandler();

      // We make a copy of the event because the capture handler may modify it.
      wxKeyEvent temp = event;

#if defined(__WXGTK__)
      // wxGTK uses the control and alt modifiers to represent ALTGR,
      // so remove it as it might confuse the capture handlers.
      if (temp.GetModifiers() == (wxMOD_CONTROL | wxMOD_ALT))
      {
         temp.SetControlDown(false);
         temp.SetAltDown(false);
      }
#endif

      // Ask the capture handler if the key down/up event is something it
      // might be interested in handling.
      wxCommandEvent e(EVT_CAPTURE_KEY);
      e.SetEventObject(&temp);
      e.StopPropagation();
      if (!handler->ProcessEvent(e))
      {
         return false;
      }

      // Now, let the handler process the normal key event.
      bool keyDown = temp.GetEventType() == wxEVT_KEY_DOWN;
      temp.WasProcessed();
      temp.StopPropagation();
      wxEventProcessInHandlerOnly onlyDown(temp, handler);
      bool processed = handler->ProcessEvent(temp);

      // Don't go any further if the capture handler didn't process
      // the key down event.
      if (!processed && keyDown)
      {
         return false;
      }

      // At this point the capture handler has either processed a key down event
      // or we're dealing with a key up event.
      //
      // So, only generate the char events for key down events.
      if (keyDown)
      {
         wxString chars = GetUnicodeString(temp);
         for (size_t i = 0, cnt = chars.length(); i < cnt; i++)
         {
            temp = event;
            temp.SetEventType(wxEVT_CHAR);
            temp.WasProcessed();
            temp.StopPropagation();
            temp.m_uniChar = chars[i];
            wxEventProcessInHandlerOnly onlyChar(temp, handler);
            handler->ProcessEvent(temp);
         }
      }

      // We get here for processed key down events or for key up events, whether
      // processed or not.
      return true;
   }

   // Convert the key down event to a unicode string.
   wxString GetUnicodeString(const wxKeyEvent & event)
   {
      wxString chars;

#if defined(__WXMSW__)

      BYTE ks[256];
      GetKeyboardState(ks);
      WCHAR ucode[256];
      int res = ToUnicode(event.GetRawKeyCode(), 0, ks, ucode, 256, 0);
      if (res >= 1)
      {
         chars.Append(ucode, res);
      }

#elif defined(__WXGTK__)

      chars.Append((wxChar) gdk_keyval_to_unicode(event.GetRawKeyCode()));

#elif defined(__WXMAC__)

      if (!mEvent) {
         // TODO:  we got here without getting the NSEvent pointer,
         // as in the combo box case of bug 1252.  We can't compute it!
         // This makes a difference only when there is a capture handler.
         // It's never the case yet that there is one.

         // Return just a one-character string.
         return event.GetUnicodeKey();
      }

      NSString *c = [mEvent charactersIgnoringModifiers];
      if ([c length] == 1)
      {
         unichar codepoint = [c characterAtIndex:0];
         if ((codepoint >= 0xF700 && codepoint <= 0xF8FF) || codepoint == 0x7F)
         {
            return chars;
         }
      }

      c = [mEvent characters];
      chars = [c UTF8String];

      TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardInputSource();
      CFDataRef uchr = (CFDataRef)TISGetInputSourceProperty(currentKeyboard, kTISPropertyUnicodeKeyLayoutData);
      CFRelease(currentKeyboard);
      if (uchr == NULL)
      {
         return chars;
      }

      const UCKeyboardLayout *keyboardLayout = (const UCKeyboardLayout*)CFDataGetBytePtr(uchr);
      if (keyboardLayout == NULL)
      {
         return chars;
      }

      const UniCharCount maxStringLength = 255;
      UniCharCount actualStringLength = 0;
      UniChar unicodeString[maxStringLength];
      UInt32 nsflags = [mEvent modifierFlags];
      UInt16 modifiers = (nsflags & NSAlphaShiftKeyMask ? alphaLock : 0) |
                         (nsflags & NSShiftKeyMask ? shiftKey : 0) |
                         (nsflags & NSControlKeyMask ? controlKey : 0) |
                         (nsflags & NSAlternateKeyMask ? optionKey : 0) |
                         (nsflags & NSCommandKeyMask ? cmdKey : 0);

      OSStatus status = UCKeyTranslate(keyboardLayout,
                                       [mEvent keyCode],
                                       kUCKeyActionDown,
                                       (modifiers >> 8) & 0xff,
                                       LMGetKbdType(),
                                       0,
                                       &mDeadKeyState,
                                       maxStringLength,
                                       &actualStringLength,
                                       unicodeString);

      if (status != noErr)
      {
         return chars;
      }

      chars = [ [NSString stringWithCharacters:unicodeString
                                       length:(NSInteger)actualStringLength] UTF8String];

#endif

      return chars;
   }

private:

#if defined(__WXMAC__)
   id mHandler;
   NSEvent *mEvent {};
   UInt32 mDeadKeyState;
#endif

} monitor;
