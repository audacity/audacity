/**********************************************************************

  Audacity: A Digital Audio Editor

  Keyboard.cpp

  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#include "../Audacity.h"
#include "Keyboard.h"

#include <wx/event.h>

NormalizedKeyString::NormalizedKeyString( const wxString & key )
   : NormalizedKeyStringBase( key )
{
#if defined(__WXMAC__)
   wxString newkey;
   wxString temp = key;

   // PRL:  This is needed to parse older preference files.
   temp.Replace(wxT("XCtrl+"), wxT("Control+"));

   // PRL:  RawCtrl is the proper replacement for Control, when formatting
   // wxMenuItem, so that wxWidgets shows ^ in the menu.  It is written into
   // NEW preference files (2.2.0 and later).
   temp.Replace(wxT("RawCtrl+"), wxT("Control+"));
   temp.Replace(wxT("Ctrl+"), wxT("Command+"));

   if (temp.Contains(wxT("Control+"))) {
      newkey += wxT("RawCtrl+");
   }

   if (temp.Contains(wxT("Alt+")) || temp.Contains(wxT("Option+"))) {
      newkey += wxT("Alt+");
   }

   if (temp.Contains(wxT("Shift+"))) {
      newkey += wxT("Shift+");
   }

   if (temp.Contains(wxT("Command+"))) {
      newkey += wxT("Ctrl+");
   }

   (NormalizedKeyStringBase&)*this =
      newkey + temp.AfterLast(wxT('+'));
#else
   (NormalizedKeyStringBase&)*this = key;
#endif
}

wxString NormalizedKeyString::Display(bool usesSpecialChars) const
{
   (void)usesSpecialChars;//compiler food
   // using GET to manipulate key string as needed for macOS differences
   // in displaying of it
   auto newkey = this->GET();
#if defined(__WXMAC__)

   if (!usesSpecialChars) {
      // Compose user-visible keystroke names, all ASCII
      newkey.Replace(wxT("RawCtrl+"), wxT("Control+"));
      newkey.Replace(wxT("Alt+"), wxT("Option+"));
      newkey.Replace(wxT("Ctrl+"), wxT("Command+"));
   }
   else {
      // Compose user-visible keystroke names, with special characters
      newkey.Replace(wxT("Shift+"), wxT("\u21e7"));
      newkey.Replace(wxT("RawCtrl+"), '^');
      newkey.Replace(wxT("Alt+"), wxT("\u2325"));
      newkey.Replace(wxT("Ctrl+"), wxT("\u2318"));
   }

#endif

   return newkey;
}

NormalizedKeyString KeyEventToKeyString(const wxKeyEvent & event)
{
   wxString newStr;

   long key = event.GetKeyCode();

   if (event.ControlDown())
      newStr += wxT("Ctrl+");

   if (event.AltDown())
      newStr += wxT("Alt+");

   if (event.ShiftDown())
      newStr += wxT("Shift+");

#if defined(__WXMAC__)
   if (event.RawControlDown())
      newStr += wxT("RawCtrl+");
#endif

   if (event.RawControlDown() && key >= 1 && key <= 26)
      newStr += (wxChar)(64 + key);
   else if (key >= 33 && key <= 255 && key != 127)
      newStr += (wxChar)key;
   else
   {
      switch(key)
      {
      case WXK_BACK:
         newStr += wxT("Backspace");
         break;
      case WXK_DELETE:
         newStr += wxT("Delete");
         break;
      case WXK_SPACE:
         newStr += wxT("Space");
         break;
      case WXK_TAB:
         newStr += wxT("Tab");
         break;
      case WXK_RETURN:
         newStr += wxT("Return");
         break;
      case WXK_PAGEUP:
         newStr += wxT("PageUp");
         break;
      case WXK_PAGEDOWN:
         newStr += wxT("PageDown");
         break;
      case WXK_END:
         newStr += wxT("End");
         break;
      case WXK_HOME:
         newStr += wxT("Home");
         break;
      case WXK_LEFT:
         newStr += wxT("Left");
         break;
      case WXK_UP:
         newStr += wxT("Up");
         break;
      case WXK_RIGHT:
         newStr += wxT("Right");
         break;
      case WXK_DOWN:
         newStr += wxT("Down");
         break;
      case WXK_ESCAPE:
         newStr += wxT("Escape");
         break;
      case WXK_INSERT:
         newStr += wxT("Insert");
         break;
      case WXK_NUMPAD0:
         newStr += wxT("NUMPAD0");
         break;
      case WXK_NUMPAD1:
         newStr += wxT("NUMPAD1");
         break;
      case WXK_NUMPAD2:
         newStr += wxT("NUMPAD2");
         break;
      case WXK_NUMPAD3:
         newStr += wxT("NUMPAD3");
         break;
      case WXK_NUMPAD4:
         newStr += wxT("NUMPAD4");
         break;
      case WXK_NUMPAD5:
         newStr += wxT("NUMPAD5");
         break;
      case WXK_NUMPAD6:
         newStr += wxT("NUMPAD6");
         break;
      case WXK_NUMPAD7:
         newStr += wxT("NUMPAD7");
         break;
      case WXK_NUMPAD8:
         newStr += wxT("NUMPAD8");
         break;
      case WXK_NUMPAD9:
         newStr += wxT("NUMPAD9");
         break;
      case WXK_MULTIPLY:
         newStr += wxT("*");
         break;
      case WXK_ADD:
         newStr += wxT("+");
         break;
      case WXK_SUBTRACT:
         newStr += wxT("-");
         break;
      case WXK_DECIMAL:
         newStr += wxT(".");
         break;
      case WXK_DIVIDE:
         newStr += wxT("/");
         break;
      case WXK_F1:
         newStr += wxT("F1");
         break;
      case WXK_F2:
         newStr += wxT("F2");
         break;
      case WXK_F3:
         newStr += wxT("F3");
         break;
      case WXK_F4:
         newStr += wxT("F4");
         break;
      case WXK_F5:
         newStr += wxT("F5");
         break;
      case WXK_F6:
         newStr += wxT("F6");
         break;
      case WXK_F7:
         newStr += wxT("F7");
         break;
      case WXK_F8:
         newStr += wxT("F8");
         break;
      case WXK_F9:
         newStr += wxT("F9");
         break;
      case WXK_F10:
         newStr += wxT("F10");
         break;
      case WXK_F11:
         newStr += wxT("F11");
         break;
      case WXK_F12:
         newStr += wxT("F12");
         break;
      case WXK_F13:
         newStr += wxT("F13");
         break;
      case WXK_F14:
         newStr += wxT("F14");
         break;
      case WXK_F15:
         newStr += wxT("F15");
         break;
      case WXK_F16:
         newStr += wxT("F16");
         break;
      case WXK_F17:
         newStr += wxT("F17");
         break;
      case WXK_F18:
         newStr += wxT("F18");
         break;
      case WXK_F19:
         newStr += wxT("F19");
         break;
      case WXK_F20:
         newStr += wxT("F20");
         break;
      case WXK_F21:
         newStr += wxT("F21");
         break;
      case WXK_F22:
         newStr += wxT("F22");
         break;
      case WXK_F23:
         newStr += wxT("F23");
         break;
      case WXK_F24:
         newStr += wxT("F24");
         break;
      case WXK_NUMPAD_ENTER:
         newStr += wxT("NUMPAD_ENTER");
         break;
      case WXK_NUMPAD_F1:
         newStr += wxT("NUMPAD_F1");
         break;
      case WXK_NUMPAD_F2:
         newStr += wxT("NUMPAD_F2");
         break;
      case WXK_NUMPAD_F3:
         newStr += wxT("NUMPAD_F3");
         break;
      case WXK_NUMPAD_F4:
         newStr += wxT("NUMPAD_F4");
         break;
      case WXK_NUMPAD_HOME:
         newStr += wxT("NUMPAD_HOME");
         break;
      case WXK_NUMPAD_LEFT:
         newStr += wxT("NUMPAD_LEFT");
         break;
      case WXK_NUMPAD_UP:
         newStr += wxT("NUMPAD_UP");
         break;
      case WXK_NUMPAD_RIGHT:
         newStr += wxT("NUMPAD_RIGHT");
         break;
      case WXK_NUMPAD_DOWN:
         newStr += wxT("NUMPAD_DOWN");
         break;
      case WXK_NUMPAD_PAGEUP:
         newStr += wxT("NUMPAD_PAGEUP");
         break;
      case WXK_NUMPAD_PAGEDOWN:
         newStr += wxT("NUMPAD_PAGEDOWN");
         break;
      case WXK_NUMPAD_END:
         newStr += wxT("NUMPAD_END");
         break;
      case WXK_NUMPAD_BEGIN:
         newStr += wxT("NUMPAD_HOME");
         break;
      case WXK_NUMPAD_INSERT:
         newStr += wxT("NUMPAD_INSERT");
         break;
      case WXK_NUMPAD_DELETE:
         newStr += wxT("NUMPAD_DELETE");
         break;
      case WXK_NUMPAD_EQUAL:
         newStr += wxT("NUMPAD_EQUAL");
         break;
      case WXK_NUMPAD_MULTIPLY:
         newStr += wxT("NUMPAD_MULTIPLY");
         break;
      case WXK_NUMPAD_ADD:
         newStr += wxT("NUMPAD_ADD");
         break;
      case WXK_NUMPAD_SUBTRACT:
         newStr += wxT("NUMPAD_SUBTRACT");
         break;
      case WXK_NUMPAD_DECIMAL:
         newStr += wxT("NUMPAD_DECIMAL");
         break;
      case WXK_NUMPAD_DIVIDE:
         newStr += wxT("NUMPAD_DIVIDE");
         break;
      default:
         return {}; // Don't do anything if we don't recognize the key
      }
   }

   return NormalizedKeyString{ newStr };
}
