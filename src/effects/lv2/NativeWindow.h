/**********************************************************************

  Audacity: A Digital Audio Editor

  NativeWindow.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

  NOTE: Mostly copied from wxWidgets 3.1.1

*********************************************************************/

#ifndef NATIVEWINDOW_H
#define NATIVEWINDOW_H

#if defined(__WXMSW__)
#include <wx/msw/private.h>
#elif defined(__WXMAC__)
#include <wx/osx/core/private.h>
#include <wx/osx/cocoa/private.h>
#elif defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

class NativeWindow : public wxWindow
{
public:
   NativeWindow()
   {
   }

#if defined(__WXMSW__)

   virtual ~NativeWindow()
   {
      UnsubclassWin();
   }

   bool Create(wxWindow* parent, WXWidget hwnd)
   {
       const wxRect r = wxRectFromRECT(wxGetWindowRect((HWND)hwnd));

       // Skip wxWindow::Create() which would try to create a new HWND, we don't
       // want this as we already have one.
       if (!CreateBase(parent,
                       wxID_ANY,
                       r.GetPosition(),
                       r.GetSize(),
                       0,
                       wxDefaultValidator,
                       wxS("nativewindow")))
      {
         return false;
      }

      parent->AddChild(this);

      SubclassWin(hwnd);

      
      InheritAttributes();

      return true;
   }

#elif defined(__WXMAC__)

   virtual ~NativeWindow()
   {
      GetPeer()->RemoveFromParent();
      SetPeer( nullptr );
   }

   bool Create(wxWindow* parent, WXWidget view)
   {
      DontCreatePeer();

      if (!wxWindow::Create(parent, wxID_ANY))
      {
         return false;
      }

      SetPeer(new wxWidgetCocoaImpl(this, view));

      return true;
   }

#elif defined(__WXGTK__)

   virtual ~NativeWindow()
   {
   }

   bool Create(wxWindow* parent, WXWidget widget)
   {
      if (!CreateBase(parent, wxID_ANY))
      {
         return false;
      }

      m_widget = widget;
      g_object_ref(m_widget);

      parent->DoAddChild(this);

      PostCreation();

      // Ensure that the best (and minimal) size is set to fully display the
      // widget.
      GtkRequisition req;
      gtk_widget_size_request(widget, &req);
      SetInitialSize(wxSize(req.width, req.height));

      return true;
   }
#endif
};

#endif

