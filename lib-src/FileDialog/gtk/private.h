///////////////////////////////////////////////////////////////////////////////
// Name:        wx/gtk/private.h
// Purpose:     wxGTK private macros, functions &c
// Author:      Vadim Zeitlin
// Modified by: Leland Lucius
// Created:     12.03.02
// RCS-ID:      $Id: private.h,v 1.3 2008-05-24 02:57:39 llucius Exp $
// Copyright:   (c) 2002 Vadim Zeitlin <vadim@wxwidgets.org>
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
///////////////////////////////////////////////////////////////////////////////

#ifndef _WX_GTK_PRIVATE_H_
#define _WX_GTK_PRIVATE_H_

#include <gdk/gdk.h>
#include <gtk/gtk.h>

#include "wx/event.h"

// fail all version tests if the GTK+ version is so ancient that it doesn't
// even have GTK_CHECK_VERSION
#ifndef GTK_CHECK_VERSION
#define GTK_CHECK_VERSION(a, b, c) 0
#endif

#ifdef __WXGTK20__
#if wxUSE_UNICODE
#define wxGTK_CONV(s) wxConvUTF8.cWX2MB(s)
#define wxGTK_CONV_BACK(s) wxConvUTF8.cMB2WX(s)
#else
#define wxGTK_CONV(s) wxConvUTF8.cWC2MB( wxConvLocal.cWX2WC(s) )
#define wxGTK_CONV_BACK(s)  wxConvLocal.cWC2WX( (wxConvUTF8.cMB2WC( s ) ) )
#endif
#else
#define wxGTK_CONV(s) s.c_str()
#define wxGTK_CONV_BACK(s) s
#endif


// GTK+ 2.0 compatibility define is broken when used from C++ as it
// casts enum to int implicitly
#ifdef __WXGTK20__
#undef gtk_signal_disconnect_by_func
#define gtk_signal_disconnect_by_func(object,func,data) \
gtk_signal_compat_matched((object), (func), (data), \
(GSignalMatchType)(G_SIGNAL_MATCH_FUNC | \
G_SIGNAL_MATCH_DATA), 0)
#endif

// child is not a member of GTK_BUTTON() any more in GTK+ 2.0
#ifdef __WXGTK20__
#define BUTTON_CHILD(w) GTK_BIN((w))->child
#else
#define BUTTON_CHILD(w) GTK_BUTTON((w))->child
#endif

// event_window has disappeared from GtkToggleButton in GTK+ 2.0
#ifdef __WXGTK20__
#define TOGGLE_BUTTON_EVENT_WIN(w) GTK_BUTTON((w))->event_window
#else
#define TOGGLE_BUTTON_EVENT_WIN(w) GTK_TOGGLE_BUTTON((w))->event_window
#endif

// gtk_editable_{copy|cut|paste}_clipboard() had an extra argument under
// previous GTK+ versions but no more
#if defined(__WXGTK20__) || (GTK_MINOR_VERSION > 0)
#define DUMMY_CLIPBOARD_ARG
#else
#define DUMMY_CLIPBOARD_ARG  ,0
#endif

// _GtkEditable is now private
#ifdef __WXGTK20__
#define GET_EDITABLE_POS(w) gtk_editable_get_position(GTK_EDITABLE(w))
#define SET_EDITABLE_POS(w, pos) \
gtk_editable_set_position(GTK_EDITABLE(w), (pos))
#else
#define GET_EDITABLE_POS(w) GTK_EDITABLE((w))->current_pos
#define SET_EDITABLE_POS(w, pos) \
GTK_EDITABLE((w))->current_pos = (pos)
#endif

// this GtkNotebook struct field has been renamed
#ifdef __WXGTK20__
#define NOTEBOOK_PANEL(nb)  GTK_NOTEBOOK(nb)->event_window
#else
#define NOTEBOOK_PANEL(nb)  GTK_NOTEBOOK(nb)->panel
#endif

#ifdef __WXGTK20__
#define SCROLLBAR_CBACK_ARG
#define GET_SCROLL_TYPE(w)   GTK_SCROLL_JUMP
#else
#define SCROLLBAR_CBACK_ARG
#define GET_SCROLL_TYPE(w)   GTK_RANGE((w))->scroll_type
#endif

// translate a GTK+ scroll type to a wxEventType
inline wxEventType GtkScrollTypeToWx(guint scrollType)
{
   wxEventType command;
   switch ( scrollType )
   {
      case GTK_SCROLL_STEP_BACKWARD:
         command = wxEVT_SCROLL_LINEUP;
         break;
         
      case GTK_SCROLL_STEP_FORWARD:
         command = wxEVT_SCROLL_LINEDOWN;
         break;
         
      case GTK_SCROLL_PAGE_BACKWARD:
         command = wxEVT_SCROLL_PAGEUP;
         break;
         
      case GTK_SCROLL_PAGE_FORWARD:
         command = wxEVT_SCROLL_PAGEDOWN;
         break;
         
      default:
         command = wxEVT_SCROLL_THUMBTRACK;
   }
   
   return command;
}

inline wxEventType GtkScrollWinTypeToWx(guint scrollType)
{
   // GtkScrollTypeToWx() returns SCROLL_XXX, not SCROLLWIN_XXX as we need
   return GtkScrollTypeToWx(scrollType) +
   wxEVT_SCROLLWIN_TOP - wxEVT_SCROLL_TOP;
}

// Needed for implementing e.g. combobox on wxGTK within a modal dialog.
void wxAddGrab(wxWindow* window);
void wxRemoveGrab(wxWindow* window);

#ifdef __WXGTK20__
// Escapes string so that it is valid Pango markup XML string:
wxString wxEscapeStringForPangoMarkup(const wxString& str);
#endif

// The declaration for gtk_icon_size_lookup was accidentally ifdefed out in
// GTK+ 2.1.0 which Sun seem to have shipped with some versions of JDS
// for Solaris 9 x86.
#ifdef NEED_GTK_ICON_SIZE_LOOKUP
extern "C" gboolean gtk_icon_size_lookup  (GtkIconSize  size,
                                           gint         *width,
                                           gint         *height);
#endif

#endif // _WX_GTK_PRIVATE_H_

