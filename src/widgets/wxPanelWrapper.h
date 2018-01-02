//
//  wxPanelWrapper.h
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#ifndef __AUDACITY_WXPANEL_WRAPPER__
#define __AUDACITY_WXPANEL_WRAPPER__

#include "../MemoryX.h"
#include <wx/panel.h>
#include <wx/dialog.h>

void wxTabTraversalWrapperCharHook(wxKeyEvent &event);

template <typename Base>
class wxTabTraversalWrapper : public Base
{
public:
   template <typename... Args>
   explicit wxTabTraversalWrapper(Args&&... args)
   : Base( std::forward<Args>(args)... )
   {
      this->Bind(wxEVT_CHAR_HOOK, wxTabTraversalWrapperCharHook);
   }

   wxTabTraversalWrapper(const wxTabTraversalWrapper&) = delete;
   wxTabTraversalWrapper(wxTabTraversalWrapper&&) = delete;
   wxTabTraversalWrapper& operator=(const wxTabTraversalWrapper&) = delete;
   wxTabTraversalWrapper& operator=(wxTabTraversalWrapper&&) = delete;

   ~wxTabTraversalWrapper()
   {
      this->Unbind(wxEVT_CHAR_HOOK, wxTabTraversalWrapperCharHook);
   }
};

class wxPanelWrapper : public wxTabTraversalWrapper<wxPanel>
{
public:
   // Constructors
   wxPanelWrapper() = default;

   explicit wxPanelWrapper(
         wxWindow *parent,
         wxWindowID winid = wxID_ANY,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         long style = wxTAB_TRAVERSAL | wxNO_BORDER,
         // Important:  default window name localizes!
         const wxString& name = _("Panel"))
   : wxTabTraversalWrapper<wxPanel> { parent, winid, pos, size, style, name }
   {}

    // Pseudo ctor
    bool Create(
         wxWindow *parent,
         wxWindowID winid = wxID_ANY,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize,
         long style = wxTAB_TRAVERSAL | wxNO_BORDER,
         // Important:  default window name localizes!
         const wxString& name = _("Panel"))
   {
      return wxTabTraversalWrapper<wxPanel>::Create(
         parent, winid, pos, size, style, name
      );
   }
};

class wxDialogWrapper : public wxTabTraversalWrapper<wxDialog>
{
public:
   // Constructors
   wxDialogWrapper() = default;

   // Constructor with no modal flag - the new convention.
   explicit wxDialogWrapper(
      wxWindow *parent, wxWindowID id,
      const wxString& title,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      long style = wxDEFAULT_DIALOG_STYLE,
      // Important:  default window name localizes!
      const wxString& name = _("Dialog"))
   : wxTabTraversalWrapper<wxDialog>
      { parent, id, title, pos, size, style, name }
   {}

   // Pseudo ctor
   bool Create(
      wxWindow *parent, wxWindowID id,
      const wxString& title,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      long style = wxDEFAULT_DIALOG_STYLE,
      // Important:  default window name localizes!
      const wxString& name = _("Dialog"))
   {
      return wxTabTraversalWrapper<wxDialog>::Create(
         parent, id, title, pos, size, style, name
      );
   }
};

#include <wx/dirdlg.h>

class wxDirDialogWrapper : public wxTabTraversalWrapper<wxDirDialog>
{
public:
   wxDirDialogWrapper() = default;

   // Constructor with no modal flag - the new convention.
   explicit wxDirDialogWrapper(
      wxWindow *parent,
      const wxString& message = _("Select a directory"),
      const wxString& defaultPath = wxT(""),
      long style = wxDD_DEFAULT_STYLE,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      // Important:  default window name localizes!
      const wxString& name = _("Directory Dialog"))
   : wxTabTraversalWrapper<wxDirDialog>
      { parent, message, defaultPath, style, pos, size, name }
   {}

   void Create(
      wxWindow *parent,
      const wxString& message = _("Select a directory"),
      const wxString& defaultPath = wxT(""),
      long style = wxDD_DEFAULT_STYLE,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& size = wxDefaultSize,
      // Important:  default window name localizes!
      const wxString& name = _("Directory Dialog"))
   {
      wxTabTraversalWrapper<wxDirDialog>::Create(
         parent, message, defaultPath, style, pos, size, name
      );
   }
};

#include "../lib-src/FileDialog/FileDialog.h"

class FileDialogWrapper : public wxTabTraversalWrapper<FileDialog>
{
public:
   FileDialogWrapper() = default;

   // Constructor with no modal flag - the new convention.
   explicit FileDialogWrapper(
      wxWindow *parent,
      const wxString& message = _("Select a file"),
      const wxString& defaultDir = wxEmptyString,
      const wxString& defaultFile = wxEmptyString,
      const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
      long style = wxFD_DEFAULT_STYLE,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& sz = wxDefaultSize,
      // Important:  default window name localizes!
      const wxString& name = _("File Dialog"))
   : wxTabTraversalWrapper<FileDialog>
   { parent, message, defaultDir, defaultFile, wildCard, style, pos, sz, name }
   {}

   void Create(
      wxWindow *parent,
      const wxString& message = _("Select a file"),
      const wxString& defaultDir = wxEmptyString,
      const wxString& defaultFile = wxEmptyString,
      const wxString& wildCard = wxFileSelectorDefaultWildcardStr,
      long style = wxFD_DEFAULT_STYLE,
      const wxPoint& pos = wxDefaultPosition,
      const wxSize& sz = wxDefaultSize,
      // Important:  default window name localizes!
      const wxString& name = _("File Dialog"))
   {
      wxTabTraversalWrapper<FileDialog>::Create(
         parent, message, defaultDir, defaultFile, wildCard, style, pos, sz, name
      );
   }
};

#endif
