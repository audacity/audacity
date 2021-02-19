//
// Copied from wxWidgets 3.0.2 and modified for Audacity
//
/////////////////////////////////////////////////////////////////////////////
// Name:        src/msw/filedlg.cpp
// Purpose:     wxFileDialog
// Author:      Julian Smart
// Modified by: Leland Lucius
// Created:     01/02/97
// Copyright:   (c) Julian Smart
// Licence:     wxWindows licence
/////////////////////////////////////////////////////////////////////////////

// ============================================================================
// declarations
// ============================================================================

// ----------------------------------------------------------------------------
// headers
// ----------------------------------------------------------------------------

// For compilers that support precompilation, includes "wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
    #include <wx/msw/wrapcdlg.h>
    #include <wx/msw/missing.h>
    #include <wx/utils.h>
    #include <wx/msgdlg.h>
    #include <wx/filefn.h>
    #include <wx/intl.h>
    #include <wx/log.h>
    #include <wx/app.h>
    #include <wx/math.h>
#endif

#include <stdlib.h>
#include <string.h>

#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/scopeguard.h>
#include <wx/sizer.h>
#include <wx/tokenzr.h>
#include <wx/modalhook.h>
#include <wx/filectrl.h>

#include "../FileDialog.h"

#include <shlobj.h>

// ----------------------------------------------------------------------------
// constants
// ----------------------------------------------------------------------------

#ifdef _WIN32
# define wxMAXPATH   65534
#else
# define wxMAXPATH   1024
#endif

# define wxMAXFILE   1024

# define wxMAXEXT    5

// ----------------------------------------------------------------------------
// globals
// ----------------------------------------------------------------------------

// standard dialog size
static wxRect gs_rectDialog(0, 0, 428, 266);

// ============================================================================
// implementation
// ============================================================================

IMPLEMENT_CLASS(FileDialog, wxFileDialogBase)

// ----------------------------------------------------------------------------

namespace
{

#if wxUSE_DYNLIB_CLASS

typedef BOOL (WINAPI *GetProcessUserModeExceptionPolicy_t)(LPDWORD);
typedef BOOL (WINAPI *SetProcessUserModeExceptionPolicy_t)(DWORD);

GetProcessUserModeExceptionPolicy_t gs_pfnGetProcessUserModeExceptionPolicy
    = (GetProcessUserModeExceptionPolicy_t) -1;

SetProcessUserModeExceptionPolicy_t gs_pfnSetProcessUserModeExceptionPolicy
    = (SetProcessUserModeExceptionPolicy_t) -1;

DWORD gs_oldExceptionPolicyFlags = 0;

bool gs_changedPolicy = false;

#endif // #if wxUSE_DYNLIB_CLASS

/*
Since Windows 7 by default (callback) exceptions aren't swallowed anymore
with native x64 applications. Exceptions can occur in a file dialog when
using the hook procedure in combination with third-party utilities.
Since Windows 7 SP1 the swallowing of exceptions can be enabled again
by using SetProcessUserModeExceptionPolicy.
*/
void ChangeExceptionPolicy()
{
#if wxUSE_DYNLIB_CLASS
    gs_changedPolicy = false;

    wxLoadedDLL dllKernel32(wxT("kernel32.dll"));

    if ( gs_pfnGetProcessUserModeExceptionPolicy
        == (GetProcessUserModeExceptionPolicy_t) -1)
    {
        wxDL_INIT_FUNC(gs_pfn, GetProcessUserModeExceptionPolicy, dllKernel32);
        wxDL_INIT_FUNC(gs_pfn, SetProcessUserModeExceptionPolicy, dllKernel32);
    }

    if ( !gs_pfnGetProcessUserModeExceptionPolicy
        || !gs_pfnSetProcessUserModeExceptionPolicy
        || !gs_pfnGetProcessUserModeExceptionPolicy(&gs_oldExceptionPolicyFlags) )
    {
        return;
    }

    if ( gs_pfnSetProcessUserModeExceptionPolicy(gs_oldExceptionPolicyFlags
        | 0x1 /* PROCESS_CALLBACK_FILTER_ENABLED */ ) )
    {
        gs_changedPolicy = true;
    }

#endif // wxUSE_DYNLIB_CLASS
}

void RestoreExceptionPolicy()
{
#if wxUSE_DYNLIB_CLASS
    if (gs_changedPolicy)
    {
        gs_changedPolicy = false;
        (void) gs_pfnSetProcessUserModeExceptionPolicy(gs_oldExceptionPolicyFlags);
    }
#endif // wxUSE_DYNLIB_CLASS
}

} // unnamed namespace

// ----------------------------------------------------------------------------
// hook function for moving the dialog
// ----------------------------------------------------------------------------

UINT_PTR APIENTRY FileDialog::ParentHook(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
   OPENFILENAME *pOfn = reinterpret_cast<OPENFILENAME *>(GetWindowLongPtr(hDlg, GWLP_USERDATA));
   return reinterpret_cast<FileDialog *>(pOfn->lCustData)->MSWParentHook(hDlg, iMsg, wParam, lParam, pOfn);
}

UINT_PTR FileDialog::MSWParentHook(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam, OPENFILENAME *pOfn)
{
   // Allow default handling to process first
   UINT_PTR ret = CallWindowProc(mParentProc, hDlg, iMsg, wParam, lParam);

   if (iMsg == WM_SIZE)
   {
      MSWOnSize(mParentDlg, pOfn);
   }

   if (iMsg == WM_GETMINMAXINFO)
   {
      MSWOnGetMinMaxInfo(mParentDlg, pOfn, reinterpret_cast<LPMINMAXINFO>(lParam));
   }

   return ret;
}

void FileDialog::MSWOnSize(HWND hDlg, LPOPENFILENAME pOfn)
{
   wxRect r;
   wxCopyRECTToRect(wxGetClientRect(hDlg), r);

   SetHWND(mChildDlg);

   SetWindowPos(mChildDlg,
                HWND_TOP,
                0,
                0,
                r.GetWidth(),
                r.GetHeight(),
                SWP_NOZORDER | SWP_NOMOVE);

   SetSize(r);

   if (mRoot)
   {
      mRoot->SetSize(r.GetWidth(), mRoot->GetSize().GetHeight());
   }

   SetHWND(NULL);
}

// Provide the minimum size of the dialog
//
// We've captured the full dialog size in MSWOnInitDone() below.  This will be returned
// as the minimum size.
//
// When the user tries to resize the dialog, for some unknown reason the common dialog control
// doesn't let the user resize it smaller than it was the last time the dialog was used.  This
// may be a problem in this code and/or may only be a concern under Windows 10.  Either way, we
// override the minimum size supplied by the common dialog control with our own size here.
void FileDialog::MSWOnGetMinMaxInfo(HWND hwnd, LPOPENFILENAME pOfn, LPMINMAXINFO pMmi)
{
   if (mMinSize.x > 0 && mMinSize.y > 0)
   {
      pMmi->ptMinTrackSize = mMinSize;
   }
}

UINT_PTR APIENTRY FileDialog::DialogHook(HWND hDlg, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
   OPENFILENAME *pOfn;

   if (iMsg == WM_INITDIALOG)
   {
      pOfn = reinterpret_cast<OPENFILENAME *>(lParam);
   }
   else
   {
      pOfn = reinterpret_cast<OPENFILENAME *>(GetWindowLongPtr(hDlg, GWLP_USERDATA));
   }

   return reinterpret_cast<FileDialog *>(pOfn->lCustData)->MSWDialogHook(hDlg, iMsg, wParam, lParam, pOfn);
}

UINT_PTR FileDialog::MSWDialogHook(HWND hDlg, UINT iMsg, WPARAM WXUNUSED(wParam), LPARAM lParam, OPENFILENAME *pOfn)
{
   switch (iMsg)
   {
      case WM_INITDIALOG:
         MSWOnInitDialog(hDlg, pOfn);
      break;

      case WM_DESTROY:
         MSWOnDestroy(hDlg, pOfn);
      break;

      case WM_NOTIFY:
      {
         NMHDR * const pNM = reinterpret_cast<NMHDR *>(lParam);
         if (pNM->code > CDN_LAST && pNM->code <= CDN_FIRST)
         {
            OFNOTIFY * const pNotifyCode = reinterpret_cast<OFNOTIFY *>(lParam);
            switch (pNotifyCode->hdr.code)
            {
               case CDN_INITDONE:
                  MSWOnInitDone(hDlg, pOfn);
               break;

               case CDN_FOLDERCHANGE:
                  MSWOnFolderChange(hDlg, pOfn);
               break;

               case CDN_SELCHANGE:
                  MSWOnSelChange(hDlg, pOfn);
               break;

               case CDN_TYPECHANGE:
                  MSWOnTypeChange(hDlg, pOfn);
               break;
            }
         }
      }
      break;
   }

   // do the default processing
   return 0;
}

void FileDialog::MSWOnInitDialog(HWND hDlg, LPOPENFILENAME pOfn)
{
   // Since we've specified the OFN_EXPLORER flag, the "real" dialog is the parent of this one
   mParentDlg = ::GetParent(hDlg);

   // Now we can initialize the disabler
   mDisabler.Init(this, mParentDlg);

   // This is the dialog were our controls will go
   mChildDlg = hDlg;

   // Store the OPENFILENAME pointer in each window
   SetWindowLongPtr(mParentDlg, GWLP_USERDATA, reinterpret_cast<LPARAM>(pOfn));
   SetWindowLongPtr(mChildDlg, GWLP_USERDATA, reinterpret_cast<LPARAM>(pOfn));

   // Subclass the parent dialog so we can receive WM_SIZE messages
   mParentProc = reinterpret_cast<WNDPROC>(SetWindowLongPtr(mParentDlg, GWLP_WNDPROC, reinterpret_cast<LPARAM>(&ParentHook)));

   // set HWND for wx
   SetHWND(mChildDlg);

   if (HasUserPaneCreator())
   {
      // Create the root window
      wxBoxSizer *verticalSizer = new wxBoxSizer(wxVERTICAL);
      mRoot = new wxPanel(this, wxID_ANY);
   
      wxPanel *userpane = new wxPanel(mRoot, wxID_ANY);
      CreateUserPane(userpane);

      wxBoxSizer *horizontalSizer = new wxBoxSizer(wxHORIZONTAL);
      horizontalSizer->Add(userpane, 1, wxEXPAND);
      verticalSizer->Add(horizontalSizer, 1, wxEXPAND);

      mRoot->SetSizer(verticalSizer);
      mRoot->Layout();
      mRoot->Fit();

      // This reserves space for the additional panel
      wxSize sz = mRoot->GetBestSize();
      SetWindowPos(mChildDlg,
                   HWND_TOP,
                   0,
                   0,
                   sz.GetWidth(),
                   sz.GetHeight(),
                   SWP_NOZORDER | SWP_NOMOVE);

   }

   SetHWND(NULL);
}

void FileDialog::MSWOnDestroy(HWND hDlg, LPOPENFILENAME WXUNUSED(pOfn))
{
   // Save final dialog position for next time
   wxCopyRECTToRect(wxGetWindowRect(mParentDlg), gs_rectDialog);

   // Must explicitly delete the root window.  Otherwise, wx will try to 
   // destroy it when the FileDialog is deleted.  But, the windows will
   // have already been deleted as a result of the OpenFile dialog being
   // destroyed.
   delete mRoot;
}

void FileDialog::MSWOnInitDone(HWND hDlg, LPOPENFILENAME pOfn)
{
   // set HWND so that our DoMoveWindow() works correctly
   SetHWND(mChildDlg);

   // capture the full initial size of the dialog to use as the minimum size
   RECT r;
   GetWindowRect(mParentDlg, &r);
   mMinSize = {r.right - r.left, r.bottom - r.top};

   if (m_centreDir)
   {
      // now we have the real dialog size, remember it
      RECT rect;
      GetWindowRect(mParentDlg, &rect);
      gs_rectDialog = wxRectFromRECT(rect);

      // and position the window correctly: notice that we must use the base
      // class version as our own doesn't do anything except setting flags
      wxFileDialogBase::DoCentre(m_centreDir);
   }
   else // need to just move it to the correct place
   {
      SetPosition(gs_rectDialog.GetPosition());
   }

   // Filter change event must be sent once initialized
   MSWOnTypeChange(hDlg, pOfn);

   // we shouldn't destroy this HWND
   SetHWND(NULL);
}

void FileDialog::MSWOnFolderChange(HWND hDlg, LPOPENFILENAME pOfn)
{
   FilterFiles(mParentDlg, true);

   wxChar path[wxMAXPATH];
   int result = CommDlg_OpenSave_GetFolderPath(::GetParent(hDlg), path, WXSIZEOF(path));
   if (result < 0 || result > WXSIZEOF(path))
   {
      return;
   }

   m_dir = path;

   wxFileCtrlEvent event(wxEVT_FILECTRL_FOLDERCHANGED, this, GetId());
   event.SetDirectory(m_dir);
   GetEventHandler()->ProcessEvent(event);
}

void FileDialog::MSWOnSelChange(HWND hDlg, LPOPENFILENAME pOfn)
{
   // set HWND for wx
   SetHWND(mChildDlg);

   // Get pointer to the ListView control
   HWND lv = ::GetDlgItem(::GetDlgItem(mParentDlg, lst2), 1);
   if (lv == NULL)
   {
      return;
   }

   wxChar path[wxMAXPATH];
   int result = CommDlg_OpenSave_GetFilePath(::GetParent(hDlg), path, WXSIZEOF(path));
   if (result < 0 || result > WXSIZEOF(path))
   {
      return;
   }

   m_path = path;
   m_fileName = wxFileNameFromPath(m_path);
   m_dir = wxPathOnly(m_path);

   m_fileNames.Clear();
   m_fileNames.Add(m_fileName);

   wxFileCtrlEvent event(wxEVT_FILECTRL_SELECTIONCHANGED, this, GetId());
   event.SetDirectory(m_dir);
   event.SetFiles(m_fileNames);
   GetEventHandler()->ProcessEvent(event);

   // we shouldn't destroy this HWND
   SetHWND(NULL);
}

void FileDialog::MSWOnTypeChange(HWND hDlg, LPOPENFILENAME pOfn)
{
   // set HWND for wx
   SetHWND(mChildDlg);

   ParseFilter(pOfn->nFilterIndex);
   FilterFiles(mParentDlg, true);

   m_filterIndex = pOfn->nFilterIndex - 1;

   wxFileCtrlEvent event(wxEVT_FILECTRL_FILTERCHANGED, this, GetId());
   event.SetFilterIndex(m_filterIndex);
   GetEventHandler()->ProcessEvent(event);

   // we shouldn't destroy this HWND
   SetHWND(NULL);
}

#define WM_GETISHELLBROWSER WM_USER + 7

void FileDialog::FilterFiles(HWND hwnd, bool refresh)
{
   IShellFolder *ishell = NULL;
   IShellBrowser *ishellbrowser = NULL;  // Does not have to be released
   IShellView *ishellview = NULL;
   IFolderView *ifolderview = NULL;
   LPMALLOC imalloc = NULL;
   HRESULT hr;
   
   // Get pointer to the ListView control
   HWND lv = ::GetDlgItem(::GetDlgItem(hwnd, lst2), 1);
   if (lv == NULL)
   {
      return;
   }
   
   // Get shell's memory allocation interface (must be Release()'d)
   hr = SHGetMalloc(&imalloc);
   if ((hr != NOERROR) || (imalloc == NULL))
   {
      wxASSERT((hr == NOERROR) && (imalloc != NULL));
      return;
   }

   // Get IShellBrowser interface for current dialog
   ishellbrowser = (IShellBrowser*)::SendMessage(hwnd, WM_GETISHELLBROWSER, 0, 0);
   if (ishellbrowser)
   {
      // Get IShellBrowser interface for returned browser
      if (ishellbrowser->QueryActiveShellView(&ishellview) == S_OK)
      {
         // Get the IFolderView interface...available on XP or greater
         ishellview->QueryInterface(IID_IFolderView, (void **)&ifolderview);
      }
   }

   // Init
   LVITEM lvi;
   wxZeroMemory(lvi);

   // Process all items
   int fltcnt = (int) m_Filters.GetCount();
   int itmcnt = ListView_GetItemCount(lv);
   for (int itm = 0; itm < itmcnt; itm++)
   {
      // Retrieve the file IDL
      lvi.iItem = itm;
      lvi.mask = LVIF_PARAM;
      if (ListView_GetItem(lv, &lvi) != TRUE)
      {
         wxASSERT(FALSE);
         break;
      }

      LPCITEMIDLIST fidl = (LPCITEMIDLIST) lvi.lParam;

      // On Vista, lParam no longer contains the pidl so retrieve it via the
      // IFolderView interface.  This interface is only available on XP or higher
      // so if that limitation isn't workable, use IShellView::GetItemObject() to
      // retrieve items.
      if (fidl == NULL && ifolderview != NULL)
      {
         ifolderview->Item(itm, (LPITEMIDLIST *) &fidl);
      }

      if (fidl == NULL)
      {
         wxASSERT(fidl != NULL);
         break;
      }

      // Retrieve the IShellFolder interface of the parent (must be Release()'d)
      if (ishell == NULL)
      {
         hr = SHBindToParent(fidl, IID_IShellFolder, (void **)&ishell, NULL);
         if (!SUCCEEDED(hr))
         {
            wxASSERT(SUCCEEDED(hr));
            break;
         }
      }
      
      // Get the attributes of the object
      DWORD attr = SFGAO_FOLDER | SFGAO_BROWSABLE;
      hr = ishell->GetAttributesOf(1, &fidl, &attr);
      if (!SUCCEEDED(hr))
      {
         wxASSERT(SUCCEEDED(hr));
         break;
      }
      
      // Allow all folders (things like zip files get filtered below)
      if ((attr & (SFGAO_FOLDER)) && !(attr & SFGAO_BROWSABLE))
      {
         continue;
      }
      
      // Retrieve the parsable name of the object (includes extension)
      STRRET str;
      hr = ishell->GetDisplayNameOf(fidl, SHGDN_INFOLDER | SHGDN_FORPARSING, &str);
      if (hr != NOERROR)
      {
         // For some objects, we get back an error of 80070057.  I'm assuming this
         // means there is no way to represent the name (like some sort of virtual name)
         // or I've not used the correct PIDL.  But, in either case, it "probably"
         // represents some sort of folder (at least in all cases I've seen), so we
         // simply allow it to display.
         continue;
      }
      
      // Convert result to wxString
      wxString filename;
      switch (str.uType)
      {
         case STRRET_WSTR:
            filename = str.pOleStr;
            imalloc->Free(str.pOleStr);
            break;
            
         case STRRET_OFFSET:
            filename = wxString(((char *)fidl) + str.uOffset, wxConvISO8859_1);
            break;
            
         case STRRET_CSTR:
            filename = wxString(str.cStr, wxConvISO8859_1);
            break;
      }
      
      // Convert the filename to lowercase (and remember to write filters in lowercase!)
      filename = filename.Lower();

      // Attempt to match it to all of our filters
      bool match = false;
      for (int flt = 0; flt < fltcnt; flt++)
      {
         if (wxMatchWild(m_Filters[flt], filename, false))
         {
            match = true;
            break;
         }
      }
      
      // Remove it from the display if it didn't match any of the filters.
      if (!match)
      {
         ListView_DeleteItem(lv, itm);
         itm--;
         itmcnt--;
      }
   }

   // On Vista and maybe XP, we seem to need to refresh the view after
   // changing the filters.  But, only refresh for type changes and not
   // selection changes since it causes additional selection change
   // events to occur.
   if (ishellview && refresh)
   {
      ishellview->Refresh();
   }

   // Release the interface
   if (ifolderview)
   {
      ifolderview->Release();
   }

   // Release the interface
   if (ishellview)
   {
      ishellview->Release();
   }

   // Release the interface
   if (ishell)
   {
      ishell->Release();
   }
   
   // Release the interface
   if (imalloc)
   {
      imalloc->Release();
   }
}

void FileDialog::ParseFilter(int index)
{
   m_Filters.Empty();
   
   wxStringTokenizer tokenWild(m_FilterGroups[index - 1], wxT(";"));
   
   while (tokenWild.HasMoreTokens())
   {
      wxString token = tokenWild.GetNextToken();
      if (m_Filters.Index(token, false) == wxNOT_FOUND)
      {
         m_Filters.Add(token);
      }
   }
}

// ----------------------------------------------------------------------------
// FileDialog
// ----------------------------------------------------------------------------

FileDialog::FileDialog()
:   FileDialogBase()
{
    Init();
}

FileDialog::FileDialog(wxWindow *parent,
                       const wxString& message,
                       const wxString& defaultDir,
                       const wxString& defaultFile,
                       const wxString& wildCard,
                       long style,
                       const wxPoint& pos,
                       const wxSize& sz,
                       const wxString& name)
:   FileDialogBase()
{
    Init();

    FileDialogBase::Create(parent,message,defaultDir,defaultFile,wildCard,style,pos,sz,name);
}

void FileDialog::Init()
{
   mRoot = NULL;
   mMinSize = {0, 0};

   // NB: all style checks are done by wxFileDialogBase::Create

   m_bMovedWindow = false;
   m_centreDir = 0;

   // Must set to zero, otherwise the wx routines won't size the window
   // the second time you call the file dialog, because it thinks it is
   // already at the requested size.. (when centering)
   gs_rectDialog.x =
   gs_rectDialog.y = 0;
}

void FileDialog::GetPaths(wxArrayString& paths) const
{
   paths.Empty();
   
   wxString dir(m_dir);
   if (m_dir.empty() || m_dir.Last() != wxT('\\'))
      dir += wxT('\\');
   
   size_t count = m_fileNames.GetCount();
   for (size_t n = 0; n < count; n++)
   {
      if (wxFileName(m_fileNames[n]).IsAbsolute())
         paths.Add(m_fileNames[n]);
      else
         paths.Add(dir + m_fileNames[n]);
   }
}

void FileDialog::GetFilenames(wxArrayString& files) const
{
   files = m_fileNames;
}

void FileDialog::SetFileExtension(const wxString& extension)
{
   if (mParentDlg)
   {
      wxChar path[wxMAXPATH];

      if (CommDlg_OpenSave_GetFilePath(mParentDlg, path, WXSIZEOF(path)))
      {
         wxFileName fn(path);
         fn.SetExt(extension);

         // Change the currently entered file name.
         CommDlg_OpenSave_SetControlText(mParentDlg, edt1, fn.GetFullName().t_str());

         // Make this the default extension as well. So if the user specifies a file
         // name without an extension, this one will be used instead of the first
         // extension in the filter list.
         CommDlg_OpenSave_SetDefExt(mParentDlg, fn.GetExt().t_str());
      }
   }
}

void FileDialog::DoGetPosition( int *x, int *y ) const
{
   if (x)
      *x = gs_rectDialog.x;
   if (y)
      *y = gs_rectDialog.y;
}

void FileDialog::DoGetSize(int *width, int *height) const
{
   if (width)
      *width = gs_rectDialog.width;
   if (height)
      *height = gs_rectDialog.height;
}

void FileDialog::DoMoveWindow(int x, int y, int WXUNUSED(width), int WXUNUSED(height))
{
   m_bMovedWindow = true;
   
   gs_rectDialog.x = x;
   gs_rectDialog.y = y;
   
   // our HWND is only set when we're called from MSWOnInitDone(), test if
   // this is the case
   HWND hwnd = GetHwnd();
   if (hwnd)
   {
      // size of the dialog can't be changed because the controls are not
      // laid out correctly then
      ::SetWindowPos(hwnd, HWND_TOP, x, y, 0, 0, SWP_NOZORDER | SWP_NOSIZE);
   }
   else // just remember that we were requested to move the window
   {
      m_bMovedWindow = true;

      // if Centre() had been called before, it shouldn't be taken into
      // account now
      m_centreDir = 0;
   }
}

void FileDialog::DoCentre(int dir)
{
   m_centreDir = dir;
   m_bMovedWindow = true;

   // it's unnecessary to do anything else at this stage as we'll redo it in
   // MSWOnInitDone() anyhow
}

// helper used below in ShowCommFileDialog(): style is used to determine
// whether to show the "Save file" dialog (if it contains wxFD_SAVE bit) or
// "Open file" one; returns true on success or false on failure in which case
// err is filled with the CDERR_XXX constant
static bool DoShowCommFileDialog(OPENFILENAME *of, long style, DWORD *err)
{
   if (style & wxFD_SAVE ? GetSaveFileName(of) : GetOpenFileName(of))
      return true;

   if (err)
   {
      *err = CommDlgExtendedError();
   }

   return false;
}

// We want to use OPENFILENAME struct version 5 (Windows 2000/XP) but we don't
// know if the OPENFILENAME declared in the currently used headers is a V5 or
// V4 (smaller) one so we try to manually extend the struct in case it is the
// old one.
//
// We don't do this on Windows CE nor under Win64, however, as there are no
// compilers with old headers for these architectures
#if defined(__WXWINCE__) || defined(__WIN64__)
typedef OPENFILENAME wxOPENFILENAME;

static const DWORD gs_ofStructSize = sizeof(OPENFILENAME);
#else // !__WXWINCE__ || __WIN64__
#define wxTRY_SMALLER_OPENFILENAME

struct wxOPENFILENAME : public OPENFILENAME
{
   // fields added in Windows 2000/XP comdlg32.dll version
   void *pVoid;
   DWORD dw1;
   DWORD dw2;
};

// hardcoded sizeof(OPENFILENAME) in the Platform SDK: we have to do it
// because sizeof(OPENFILENAME) in the headers we use when compiling the
// library could be less if _WIN32_WINNT is not >= 0x500
static const DWORD wxOPENFILENAME_V5_SIZE = 88;

// this is hardcoded sizeof(OPENFILENAME_NT4) from Platform SDK
static const DWORD wxOPENFILENAME_V4_SIZE = 76;

// always try the new one first
static DWORD gs_ofStructSize = wxOPENFILENAME_V5_SIZE;
#endif // __WXWINCE__ || __WIN64__/!...

static bool ShowCommFileDialog(OPENFILENAME *of, long style)
{
   DWORD errCode;
   bool success = DoShowCommFileDialog(of, style, &errCode);

#ifdef wxTRY_SMALLER_OPENFILENAME
   // the system might be too old to support the new version file dialog
   // boxes, try with the old size
   if (!success && errCode == CDERR_STRUCTSIZE &&
      of->lStructSize != wxOPENFILENAME_V4_SIZE)
   {
      of->lStructSize = wxOPENFILENAME_V4_SIZE;

      success = DoShowCommFileDialog(of, style, &errCode);

      if (success || !errCode)
      {
         // use this struct size for subsequent dialogs
         gs_ofStructSize = of->lStructSize;
      }
   }
#endif // wxTRY_SMALLER_OPENFILENAME

   if (!success && errCode == FNERR_INVALIDFILENAME && of->lpstrFile[0])
   {
      // this can happen if the default file name is invalid, try without it
      // now
      of->lpstrFile[0] = wxT('\0');
      success = DoShowCommFileDialog(of, style, &errCode);
   }

   if (!success)
   {
      // common dialog failed - why?
      if (errCode != 0)
      {
         wxLogError(wxT("File dialog failed with error code %0lx."), errCode);
      }
      //else: it was just cancelled

      return false;
   }

   return true;
}

int FileDialog::ShowModal()
{
   WX_HOOK_MODAL_DIALOG();

   HWND hWnd = 0;
   if (m_parent) hWnd = (HWND) m_parent->GetHWND();
   if (!hWnd && wxTheApp->GetTopWindow())
      hWnd = (HWND) wxTheApp->GetTopWindow()->GetHWND();
   
   static wxChar fileNameBuffer [ wxMAXPATH ];           // the file-name
   wxChar        titleBuffer    [ wxMAXFILE+1+wxMAXEXT ];  // the file-name, without path
   
   *fileNameBuffer = wxT('\0');
   *titleBuffer    = wxT('\0');
   
   // We always need EXPLORER and ENABLEHOOK to use our filtering code
   DWORD msw_flags = OFN_HIDEREADONLY | OFN_EXPLORER | OFN_ENABLEHOOK | OFN_ENABLESIZING | OFN_ENABLETEMPLATEHANDLE;

   if (HasFdFlag(wxFD_FILE_MUST_EXIST))
      msw_flags |= OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

   /*
      If the window has been moved the programmer is probably
      trying to center or position it.  Thus we set the callback
      or hook function so that we can actually adjust the position.
      Without moving or centering the dlg, it will just stay
      in the upper left of the frame, it does not center
      automatically.
   */
   if (m_bMovedWindow || HasExtraControlCreator()) // we need these flags.
   {
      ChangeExceptionPolicy();
      msw_flags |= OFN_EXPLORER|OFN_ENABLEHOOK;
#ifndef __WXWINCE__
      msw_flags |= OFN_ENABLESIZING;
#endif
   }

   wxON_BLOCK_EXIT0(RestoreExceptionPolicy);

   if (HasFdFlag(wxFD_MULTIPLE))
   {
      // OFN_EXPLORER must always be specified with OFN_ALLOWMULTISELECT
      msw_flags |= OFN_EXPLORER | OFN_ALLOWMULTISELECT;
   }

   // if wxCHANGE_DIR flag is not given we shouldn't change the CWD which the
   // standard dialog does by default (notice that under NT it does it anyhow, 
   // OFN_NOCHANGEDIR or not, see below)
   if (!HasFdFlag(wxFD_CHANGE_DIR))
   {
      msw_flags |= OFN_NOCHANGEDIR;
   }
   
   if (HasFdFlag(wxFD_OVERWRITE_PROMPT))
   {
      msw_flags |= OFN_OVERWRITEPROMPT;
   }
   
   // Define a dummy dialog box template
   GlobalPtr hgbl(256, GMEM_ZEROINIT);
   GlobalPtrLock hgblLock(hgbl);
   LPDLGTEMPLATE lpdt = static_cast<LPDLGTEMPLATE>((void *)hgblLock);
   lpdt->style = DS_CONTROL | WS_CHILD | WS_CLIPSIBLINGS;
   lpdt->cdit = 0;         // Number of controls
   lpdt->x = 0;
   lpdt->y = 0;
   lpdt->cx = 0;
   lpdt->cy = 0;

   OPENFILENAME of;
   wxZeroMemory(of);
   
   // Allow Places bar to show on supported platforms
   of.lStructSize       = sizeof(OPENFILENAME);
   of.hwndOwner         = hWnd;
   of.lpstrTitle        = m_message.t_str();
   of.lpstrFileTitle    = titleBuffer;
   of.nMaxFileTitle     = wxMAXFILE + 1 + wxMAXEXT;
   of.hInstance         = (HINSTANCE) lpdt;

   // Convert forward slashes to backslashes (file selector doesn't like
   // forward slashes) and also squeeze multiple consecutive slashes into one
   // as it doesn't like two backslashes in a row neither
   
   wxString  dir;
   size_t    i, len = m_dir.length();
   dir.reserve(len);
   for ( i = 0; i < len; i++ )
   {
      wxChar ch = m_dir[i];
      switch ( ch )
      {
         case wxT('/'):
            // convert to backslash
            ch = wxT('\\');
            
            // fall through
            
         case wxT('\\'):
            while ( i < len - 1 )
            {
               wxChar chNext = m_dir[i + 1];
               if ( chNext != wxT('\\') && chNext != wxT('/') )
                  break;
               
               // ignore the next one, unless it is at the start of a UNC path
               if (i > 0)
                  i++;
               else
                  break;
            }
            // fall through
            
            default:
            // normal char
            dir += ch;
      }
   }
   
   of.lpstrInitialDir   = dir.c_str();
   
   of.Flags             = msw_flags;
   of.lpfnHook          = DialogHook;
   of.lCustData         = (LPARAM) this;

   wxArrayString wildDescriptions;
   
   size_t items = wxParseCommonDialogsFilter(m_wildCard, wildDescriptions, m_FilterGroups);
   
   wxASSERT_MSG( items > 0 , wxT("empty wildcard list") );
   
   wxString filterBuffer;
   
   for (i = 0; i < items ; i++)
   {
      filterBuffer += wildDescriptions[i];
      filterBuffer += wxT("|");
      filterBuffer += m_FilterGroups[i];
      filterBuffer += wxT("|");
   }
   
   // Replace | with \0
   for (i = 0; i < filterBuffer.Len(); i++ )
   {
      if ( filterBuffer.GetChar(i) == wxT('|') )
      {
         filterBuffer[i] = wxT('\0');
      }
   }
   
   of.lpstrFilter  = (LPCTSTR)filterBuffer.c_str();
   of.nFilterIndex = m_filterIndex + 1;
   
   ParseFilter(of.nFilterIndex);
   
   //=== Setting defaultFileName >>=========================================
   
   wxStrlcpy(fileNameBuffer, m_fileName.c_str(), WXSIZEOF(fileNameBuffer));
   
   of.lpstrFile = fileNameBuffer;  // holds returned filename
   of.nMaxFile  = wxMAXPATH;
   
   // we must set the default extension because otherwise Windows would check
   // for the existing of a wrong file with wxOVERWRITE_PROMPT (i.e. if the
   // user types "foo" and the default extension is ".bar" we should force it
   // to check for "foo.bar" existence and not "foo")
   wxString defextBuffer; // we need it to be alive until GetSaveFileName()!
   if (HasFdFlag(wxFD_SAVE))
   {
      const wxChar* extension = filterBuffer;
      int maxFilter = (int)(of.nFilterIndex*2L) - 1;
      
      for( int i = 0; i < maxFilter; i++ )           // get extension
         extension = extension + wxStrlen( extension ) + 1;
      
      // use dummy name a to avoid assert in AppendExtension
      defextBuffer = AppendExtension(wxT("a"), extension);
      if (defextBuffer.StartsWith(wxT("a.")))
      {
         defextBuffer = defextBuffer.Mid(2); // remove "a."
         of.lpstrDefExt = defextBuffer.c_str();
      }
   }
   
   // store off before the standard windows dialog can possibly change it 
   const wxString cwdOrig = wxGetCwd(); 
   
   //== Execute FileDialog >>=================================================

   if (!ShowCommFileDialog(&of, m_windowStyle))
      return wxID_CANCEL;

   // GetOpenFileName will always change the current working directory on
   // (according to MSDN) "Windows NT 4.0/2000/XP" because the flag
   // OFN_NOCHANGEDIR has no effect.  If the user did not specify
   // wxFD_CHANGE_DIR let's restore the current working directory to what it
   // was before the dialog was shown.
   if (msw_flags & OFN_NOCHANGEDIR)
   {
      wxSetWorkingDirectory(cwdOrig);
   }

   m_fileNames.Empty();
      
   if ((HasFdFlag(wxFD_MULTIPLE)) &&
#if defined(OFN_EXPLORER)
         ( fileNameBuffer[of.nFileOffset-1] == wxT('\0') )
#else
         ( fileNameBuffer[of.nFileOffset-1] == wxT(' ') )
#endif // OFN_EXPLORER
         )
   {
#if defined(OFN_EXPLORER)
      m_dir = fileNameBuffer;
      i = of.nFileOffset;
      m_fileName = &fileNameBuffer[i];
      m_fileNames.Add(m_fileName);
      i += m_fileName.Len() + 1;
         
      while (fileNameBuffer[i] != wxT('\0'))
      {
         m_fileNames.Add(&fileNameBuffer[i]);
         i += wxStrlen(&fileNameBuffer[i]) + 1;
      }
#else
      wxStringTokenizer toke(fileNameBuffer, wxT(" \t\r\n"));
      m_dir = toke.GetNextToken();
      m_fileName = toke.GetNextToken();
      m_fileNames.Add(m_fileName);
         
      while (toke.HasMoreTokens())
         m_fileNames.Add(toke.GetNextToken());
#endif // OFN_EXPLORER
         
      wxString dir(m_dir);
      if ( m_dir.Last() != wxT('\\') )
         dir += wxT('\\');
         
      m_path = dir + m_fileName;
      m_filterIndex = (int)of.nFilterIndex - 1;
   }
   else
   {
      //=== Adding the correct extension >>=================================
      m_filterIndex = (int)of.nFilterIndex - 1;

      if ( !of.nFileExtension || fileNameBuffer[of.nFileExtension] == wxT('\0') )
      {
         // User has typed a filename without an extension:
         const wxChar* extension = filterBuffer;
         int   maxFilter = (int)(of.nFilterIndex*2L) - 1;
         
         for( int i = 0; i < maxFilter; i++ )           // get extension
            extension = extension + wxStrlen( extension ) + 1;
         
         m_fileName = AppendExtension(fileNameBuffer, extension);
         wxStrlcpy(fileNameBuffer, m_fileName.c_str(), WXSIZEOF(fileNameBuffer));
      }

      m_path = fileNameBuffer;
      m_fileName = wxFileNameFromPath(fileNameBuffer);
      m_fileNames.Add(m_fileName);
      m_dir = wxPathOnly(fileNameBuffer);
   }
   
   return wxID_OK;
}

FileDialog::Disabler::Disabler()
{
   mRoot = NULL;
   mHwnd = (HWND) INVALID_HANDLE_VALUE;
   mModalCount = 0;

   Register();
}

void FileDialog::Disabler::Init(wxWindow *root, HWND hwnd)
{
   mRoot = root;
   mHwnd = hwnd;
}

int FileDialog::Disabler::Enter(wxDialog *dialog)
{
   if (mHwnd != (HWND) INVALID_HANDLE_VALUE)
   {
      if (IsChild(dialog)) {
         ::EnableWindow(mHwnd, FALSE);
         mModalCount++;
      }
   }

   return wxID_NONE;
}

void FileDialog::Disabler::Exit(wxDialog *dialog)
{
   if (mHwnd != (HWND) INVALID_HANDLE_VALUE)
   {
      if (IsChild(dialog))
      {
         mModalCount--;
         if (mModalCount == 0)
         {
            ::EnableWindow(mHwnd, TRUE);
            ::SetWindowPos(mHwnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
         }
      }
   }
}

bool FileDialog::Disabler::IsChild(const wxDialog *dialog) const
{
   if (!dialog)
   {
      return false;
   }

   for (const wxWindow *w = dialog->GetParent(); w != NULL; w = w->GetParent())
   {
      if (w == mRoot)
      {
         return true;
      }
   }

   return false;
}
