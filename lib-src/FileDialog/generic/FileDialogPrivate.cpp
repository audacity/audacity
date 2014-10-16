//////////////////////////////////////////////////////////////////////////////
// Name:        filedlgg.cpp
// Purpose:     wxGenericFileDialog
// Author:      Robert Roebling
// Modified by: Leland Lucius
// Created:     12/12/98
// RCS-ID:      $Id: FileDialogPrivate.cpp,v 1.4 2009-09-29 00:28:07 msmeyer Exp $
// Copyright:   (c) Robert Roebling
// Licence:     wxWindows licence
//
// Modified for Audacity to support an additional button on Save dialogs
//
/////////////////////////////////////////////////////////////////////////////

// For compilers that support precompilation, includes "wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#include "../FileDialog.h"
#include "FileDialogPrivate.h"

#include "wx/checkbox.h"
#include "wx/textctrl.h"
#include "wx/choice.h"
#include "wx/checkbox.h"
#include "wx/stattext.h"
#include "wx/debug.h"
#include "wx/log.h"
#include "wx/intl.h"
#include "wx/msgdlg.h"
#include "wx/sizer.h"
#include "wx/bmpbuttn.h"
#include "wx/tokenzr.h"
#include "wx/config.h"
#include "wx/imaglist.h"
#include "wx/dir.h"
#include "wx/artprov.h"
#include "wx/settings.h"
#include "wx/filefn.h"
#include "wx/file.h"        // for wxS_IXXX constants only
#include "wx/filedlg.h"     // wxFD_OPEN, wxFD_SAVE...
#include "wx/generic/dirctrlg.h" // for wxFileIconsTable

#if wxUSE_TOOLTIPS
#include "wx/tooltip.h"
#endif

#ifndef __WXWINCE__
#include <sys/types.h>
#include <sys/stat.h>
#endif

#ifdef __UNIX__
#include <dirent.h>
#include <pwd.h>
#ifndef __VMS
# include <grp.h>
#endif
#endif

#ifdef __WINDOWS__
#include "wx/msw/wrapwin.h"
#include "wx/msw/mslu.h"
#endif

#ifdef __WATCOMC__
#include <direct.h>
#endif

#ifndef __WXWINCE__
#include <time.h>
#endif

#if defined(__UNIX__) || defined(__DOS__)
#include <unistd.h>
#endif

// ----------------------------------------------------------------------------
// private functions
// ----------------------------------------------------------------------------

static
int wxCALLBACK FileDataNameCompare( long data1, long data2, long data)
{
   FileData *fd1 = (FileData*)data1;
   FileData *fd2 = (FileData*)data2;
   if (fd1->GetFileName() == wxT("..")) return -data;
   if (fd2->GetFileName() == wxT("..")) return data;
   if (fd1->IsDir() && !fd2->IsDir()) return -data;
   if (fd2->IsDir() && !fd1->IsDir()) return data;
   return data*wxStrcmp( fd1->GetFileName(), fd2->GetFileName() );
}

static
int wxCALLBACK FileDataSizeCompare( long data1, long data2, long data)
{
   FileData *fd1 = (FileData*)data1;
   FileData *fd2 = (FileData*)data2;
   if (fd1->GetFileName() == wxT("..")) return -data;
   if (fd2->GetFileName() == wxT("..")) return data;
   if (fd1->IsDir() && !fd2->IsDir()) return -data;
   if (fd2->IsDir() && !fd1->IsDir()) return data;
   if (fd1->IsLink() && !fd2->IsLink()) return -data;
   if (fd2->IsLink() && !fd1->IsLink()) return data;
   return data*(fd1->GetSize() - fd2->GetSize());
}

static
int wxCALLBACK FileDataTypeCompare( long data1, long data2, long data)
{
   FileData *fd1 = (FileData*)data1;
   FileData *fd2 = (FileData*)data2;
   if (fd1->GetFileName() == wxT("..")) return -data;
   if (fd2->GetFileName() == wxT("..")) return data;
   if (fd1->IsDir() && !fd2->IsDir()) return -data;
   if (fd2->IsDir() && !fd1->IsDir()) return data;
   if (fd1->IsLink() && !fd2->IsLink()) return -data;
   if (fd2->IsLink() && !fd1->IsLink()) return data;
   return data*wxStrcmp( fd1->GetFileType(), fd2->GetFileType() );
}

static
int wxCALLBACK FileDataTimeCompare( long data1, long data2, long data)
{
   FileData *fd1 = (FileData*)data1;
   FileData *fd2 = (FileData*)data2;
   if (fd1->GetFileName() == wxT("..")) return -data;
   if (fd2->GetFileName() == wxT("..")) return data;
   if (fd1->IsDir() && !fd2->IsDir()) return -data;
   if (fd2->IsDir() && !fd1->IsDir()) return data;
   
   return fd1->GetDateTime().IsLaterThan(fd2->GetDateTime()) ? int(data) : -int(data);
}

#if defined(__WXWINCE__)
#define IsTopMostDir(dir) (dir == wxT("\\") || dir == wxT("/"))
#elif (defined(__DOS__) || defined(__WINDOWS__) || defined (__OS2__))
#define IsTopMostDir(dir)   (dir.empty())
#else
#define IsTopMostDir(dir)   (dir == wxT("/"))
#endif

// defined in src/generic/dirctrlg.cpp
extern size_t wxGetAvailableDrives(wxArrayString &paths, wxArrayString &names, wxArrayInt &icon_ids);

//-----------------------------------------------------------------------------
//  FileData
//-----------------------------------------------------------------------------

FileData::FileData( const wxString &filePath, const wxString &fileName, fileType type, int image_id )
{
   Init();
   m_fileName = fileName;
   m_filePath = filePath;
   m_type = type;
   m_image = image_id;
   
   ReadData();
}

void FileData::Init()
{
   m_size = 0;
   m_type = FileData::is_file;
   m_image = wxFileIconsTable::file;
}

void FileData::Copy( const FileData& fileData )
{
   m_fileName = fileData.GetFileName();
   m_filePath = fileData.GetFilePath();
   m_size = fileData.GetSize();
   m_dateTime = fileData.GetDateTime();
   m_permissions = fileData.GetPermissions();
   m_type = fileData.GetType();
   m_image = fileData.GetImageId();
}

void FileData::ReadData()
{
   if (IsDrive())
   {
      m_size = 0;
      return;
   }
   
#if defined(__DOS__) || (defined(__WINDOWS__) && !defined(__WXWINCE__)) || defined(__OS2__)
   // c:\.. is a drive don't stat it
   if ((m_fileName == wxT("..")) && (m_filePath.length() <= 5))
   {
      m_type = is_drive;
      m_size = 0;
      return;
   }
#endif // __DOS__ || __WINDOWS__
   
#ifdef __WXWINCE__
   
   // WinCE
   
   DWORD fileAttribs = GetFileAttributes(m_filePath.fn_str());
   m_type |= (fileAttribs & FILE_ATTRIBUTE_DIRECTORY) != 0 ? is_dir : 0;
   
   wxString p, f, ext;
   wxSplitPath(m_filePath, & p, & f, & ext);
   if (wxStricmp(ext, wxT("exe")) == 0)
      m_type |= is_exe;
   
   // Find out size
   m_size = 0;
   HANDLE fileHandle = CreateFile(m_filePath.fn_str(),
                                  GENERIC_READ,
                                  FILE_SHARE_READ,
                                  NULL,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL,
                                  NULL);
   
   if (fileHandle != INVALID_HANDLE_VALUE)
   { 
      m_size = GetFileSize(fileHandle, 0);
      CloseHandle(fileHandle);
   }
   
   m_dateTime = wxFileModificationTime(m_filePath);
   
#else
   
   // OTHER PLATFORMS
   
   wxStructStat buff;
   
#if defined(__UNIX__) && (!defined( __OS2__ ) && !defined(__VMS))
   lstat( m_filePath.fn_str(), &buff );
   m_type |= S_ISLNK( buff.st_mode ) != 0 ? is_link : 0;
#else // no lstat()
   // only translate to file charset if we don't go by our
   // wxStat implementation
#ifndef wxNEED_WX_UNISTD_H
   wxStat( m_filePath.fn_str() , &buff );
#else
   wxStat( m_filePath, &buff );
#endif
#endif
   
   m_type |= (buff.st_mode & S_IFDIR) != 0 ? is_dir : 0;
   m_type |= (buff.st_mode & wxS_IXUSR) != 0 ? is_exe : 0;
   m_size = (long)buff.st_size;
   
   m_dateTime = buff.st_mtime;
#endif
   // __WXWINCE__
   
#if defined(__UNIX__)
   m_permissions.Printf(wxT("%c%c%c%c%c%c%c%c%c"),
                        buff.st_mode & wxS_IRUSR ? wxT('r') : wxT('-'),
                        buff.st_mode & wxS_IWUSR ? wxT('w') : wxT('-'),
                        buff.st_mode & wxS_IXUSR ? wxT('x') : wxT('-'),
                        buff.st_mode & wxS_IRGRP ? wxT('r') : wxT('-'),
                        buff.st_mode & wxS_IWGRP ? wxT('w') : wxT('-'),
                        buff.st_mode & wxS_IXGRP ? wxT('x') : wxT('-'),
                        buff.st_mode & wxS_IROTH ? wxT('r') : wxT('-'),
                        buff.st_mode & wxS_IWOTH ? wxT('w') : wxT('-'),
                        buff.st_mode & wxS_IXOTH ? wxT('x') : wxT('-'));
#elif defined(__WIN32__)
   DWORD attribs = GetFileAttributes(m_filePath.fn_str());
   if (attribs != (DWORD)-1)
   {
      m_permissions.Printf(wxT("%c%c%c%c"),
                           attribs & FILE_ATTRIBUTE_ARCHIVE  ? wxT('A') : wxT(' '),
                           attribs & FILE_ATTRIBUTE_READONLY ? wxT('R') : wxT(' '),
                           attribs & FILE_ATTRIBUTE_HIDDEN   ? wxT('H') : wxT(' '),
                           attribs & FILE_ATTRIBUTE_SYSTEM   ? wxT('S') : wxT(' '));
   }
#endif
   
   // try to get a better icon
   if (m_image == wxFileIconsTable::file)
   {
      if (m_fileName.Find(wxT('.'), true) != wxNOT_FOUND)
      {
         m_image = wxTheFileIconsTable->GetIconID( m_fileName.AfterLast(wxT('.')));
      } else if (IsExe())
      {
         m_image = wxFileIconsTable::executable;
      }
   }
}

wxString FileData::GetFileType() const
{
   if (IsDir())
      return _("<DIR>");
   else if (IsLink())
      return _("<LINK>");
   else if (IsDrive())
      return _("<DRIVE>");
   else if (m_fileName.Find(wxT('.'), true) != wxNOT_FOUND)
      return m_fileName.AfterLast(wxT('.'));
   
   return wxEmptyString;
}

wxString FileData::GetModificationTime() const
{
   // want time as 01:02 so they line up nicely, no %r in WIN32
   return m_dateTime.FormatDate() + wxT(" ") + m_dateTime.Format(wxT("%I:%M:%S %p"));
}

wxString FileData::GetHint() const
{
   wxString s = m_filePath;
   s += wxT("  ");
   
   if (IsDir())
      s += _("<DIR>");
   else if (IsLink())
      s += _("<LINK>");
   else if (IsDrive())
      s += _("<DRIVE>");
   else // plain file
      s += wxString::Format( _("%ld bytes"), m_size );
   
   s += wxT(' ');
   
   if ( !IsDrive() )
   {
      s << GetModificationTime()
      << wxT("  ")
      << m_permissions;
   }
   
   return s;
};

wxString FileData::GetEntry( fileListFieldType num ) const
{
   wxString s;
   switch ( num )
   {
      case FileList_Name:
         s = m_fileName;
         break;
         
      case FileList_Size:
         if (!IsDir() && !IsLink() && !IsDrive())
            s.Printf(wxT("%ld"), m_size);
         break;
         
         case FileList_Type:
         s = GetFileType();
         break;
         
         case FileList_Time:
         if (!IsDrive())
            s = GetModificationTime();
         break;
         
#if defined(__UNIX__) || defined(__WIN32__)
         case FileList_Perm:
         s = m_permissions;
         break;
#endif // defined(__UNIX__) || defined(__WIN32__)
         
         default:
         wxFAIL_MSG( wxT("unexpected field in FileData::GetEntry()") );
   }
   
   return s;
}

void FileData::SetNewName( const wxString &filePath, const wxString &fileName )
{
   m_fileName = fileName;
   m_filePath = filePath;
}

void FileData::MakeItem( wxListItem &item )
{
   item.m_text = m_fileName;
   item.ClearAttributes();
   if (IsExe())
      item.SetTextColour(*wxRED);
   if (IsDir())
      item.SetTextColour(*wxBLUE);
   
   item.m_image = m_image;
   
   if (IsLink())
   {
      wxColour dg = wxTheColourDatabase->Find( wxT("MEDIUM GREY") );
      if ( dg.Ok() )
         item.SetTextColour(dg);
   }
   item.m_data = (long)this;
}

//-----------------------------------------------------------------------------
//  FileCtrl
//-----------------------------------------------------------------------------

static bool ignoreChanges = false;

IMPLEMENT_DYNAMIC_CLASS(FileCtrl,wxListCtrl)

BEGIN_EVENT_TABLE(FileCtrl,wxListCtrl)
EVT_LIST_DELETE_ITEM(wxID_ANY, FileCtrl::OnListDeleteItem)
EVT_LIST_DELETE_ALL_ITEMS(wxID_ANY, FileCtrl::OnListDeleteAllItems)
EVT_LIST_END_LABEL_EDIT(wxID_ANY, FileCtrl::OnListEndLabelEdit)
EVT_LIST_COL_CLICK(wxID_ANY, FileCtrl::OnListColClick)
END_EVENT_TABLE()


FileCtrl::FileCtrl()
{
   m_showHidden = false;
   m_sort_foward = 1;
   m_sort_field = FileData::FileList_Name;
}

FileCtrl::FileCtrl(wxWindow *win,
                   wxWindowID id,
                   const wxString& wild,
                   bool showHidden,
                   const wxPoint& pos,
                   const wxSize& size,
                   long style,
                   const wxValidator &validator,
                   const wxString &name)
: wxListCtrl(win, id, pos, size, style, validator, name),
m_wild(wild)
{
   wxImageList *imageList = wxTheFileIconsTable->GetSmallImageList();
   
   SetImageList( imageList, wxIMAGE_LIST_SMALL );
   
   m_showHidden = showHidden;
   
   m_sort_foward = 1;
   m_sort_field = FileData::FileList_Name;
   
   m_dirName = wxT("*");
   
   if (style & wxLC_REPORT)
      ChangeToReportMode();
}

void FileCtrl::ChangeToListMode()
{
   ClearAll();
   SetSingleStyle( wxLC_LIST );
   UpdateFiles();
}

void FileCtrl::ChangeToReportMode()
{
   ClearAll();
   SetSingleStyle( wxLC_REPORT );
   
   // do this since WIN32 does mm/dd/yy UNIX does mm/dd/yyyy
   // don't hardcode since mm/dd is dd/mm elsewhere
   int w, h;
   wxDateTime dt(22, wxDateTime::Dec, 2002, 22, 22, 22);
   wxString txt = dt.FormatDate() + wxT("22") + dt.Format(wxT("%I:%M:%S %p"));
   GetTextExtent(txt, &w, &h);
   
   InsertColumn( 0, _("Name"), wxLIST_FORMAT_LEFT, w );
   InsertColumn( 1, _("Size"), wxLIST_FORMAT_LEFT, w/2 );
   InsertColumn( 2, _("Type"), wxLIST_FORMAT_LEFT, w/2 );
   InsertColumn( 3, _("Modified"), wxLIST_FORMAT_LEFT, w );
#if defined(__UNIX__)
   GetTextExtent(wxT("Permissions 2"), &w, &h);
   InsertColumn( 4, _("Permissions"), wxLIST_FORMAT_LEFT, w );
#elif defined(__WIN32__)
   GetTextExtent(wxT("Attributes 2"), &w, &h);
   InsertColumn( 4, _("Attributes"), wxLIST_FORMAT_LEFT, w );
#endif
   
   UpdateFiles();
}

void FileCtrl::ChangeToSmallIconMode()
{
   ClearAll();
   SetSingleStyle( wxLC_SMALL_ICON );
   UpdateFiles();
}

void FileCtrl::ShowHidden( bool show )
{
   m_showHidden = show;
   UpdateFiles();
}

long FileCtrl::Add( FileData *fd, wxListItem &item )
{
   long ret = -1;
   item.m_mask = wxLIST_MASK_TEXT + wxLIST_MASK_DATA + wxLIST_MASK_IMAGE;
   fd->MakeItem( item );
   long my_style = GetWindowStyleFlag();
   if (my_style & wxLC_REPORT)
   {
      ret = InsertItem( item );
      for (int i = 1; i < FileData::FileList_Max; i++)
         SetItem( item.m_itemId, i, fd->GetEntry((FileData::fileListFieldType)i) );
   }
   else if ((my_style & wxLC_LIST) || (my_style & wxLC_SMALL_ICON))
   {
      ret = InsertItem( item );
   }
   return ret;
}

void FileCtrl::UpdateItem(const wxListItem &item)
{
   FileData *fd = (FileData*)GetItemData(item);
   wxCHECK_RET(fd, wxT("invalid filedata"));
   
   fd->ReadData();
   
   SetItemText(item, fd->GetFileName());
   SetItemImage(item, fd->GetImageId());
   
   if (GetWindowStyleFlag() & wxLC_REPORT)
   {
      for (int i = 1; i < FileData::FileList_Max; i++)
         SetItem( item.m_itemId, i, fd->GetEntry((FileData::fileListFieldType)i) );
   }
}

void FileCtrl::UpdateFiles()
{
   // don't do anything before ShowModal() call which sets m_dirName
   if ( m_dirName == wxT("*") )
      return;
   
   wxBusyCursor bcur; // this may take a while...
   
   DeleteAllItems();
   
   wxListItem item;
   item.m_itemId = 0;
   item.m_col = 0;
   
#if (defined(__WINDOWS__) || defined(__DOS__) || defined(__WXMAC__) || defined(__OS2__)) && !defined(__WXWINCE__)
   if ( IsTopMostDir(m_dirName) )
   {
      wxArrayString names, paths;
      wxArrayInt icons;
      size_t n, count = wxGetAvailableDrives(paths, names, icons);
      
      for (n=0; n<count; n++)
      {
         FileData *fd = new FileData(paths[n], names[n], FileData::is_drive, icons[n]);
         if (Add(fd, item) != -1)
            item.m_itemId++;
         else
            delete fd;
      }
   }
   else
#endif // defined(__DOS__) || defined(__WINDOWS__)
   {
      // Real directory...
      if ( !IsTopMostDir(m_dirName) && !m_dirName.empty() )
      {
         wxString p(wxPathOnly(m_dirName));
#if (defined(__UNIX__) || defined(__WXWINCE__)) && !defined(__OS2__)
         if (p.empty()) p = wxT("/");
#endif // __UNIX__
         FileData *fd = new FileData(p, wxT(".."), FileData::is_dir, wxFileIconsTable::folder);
         if (Add(fd, item) != -1)
            item.m_itemId++;
         else
            delete fd;
      }
      
      wxString dirname(m_dirName);
#if defined(__DOS__) || defined(__WINDOWS__) || defined(__OS2__)
      if (dirname.length() == 2 && dirname[1u] == wxT(':'))
         dirname << wxT('\\');
#endif // defined(__DOS__) || defined(__WINDOWS__) || defined(__OS2__)
      
      if (dirname.empty())
         dirname = wxFILE_SEP_PATH;
      
      wxLogNull logNull;
      wxDir dir(dirname);
      
      if ( dir.IsOpened() )
      {
         wxString dirPrefix(dirname);
         if (dirPrefix.Last() != wxFILE_SEP_PATH)
            dirPrefix += wxFILE_SEP_PATH;
         
         int hiddenFlag = m_showHidden ? wxDIR_HIDDEN : 0;
         
         bool cont;
         wxString f;
         
         // Get the directories first (not matched against wildcards):
         cont = dir.GetFirst(&f, wxEmptyString, wxDIR_DIRS | hiddenFlag);
         while (cont)
         {
            FileData *fd = new FileData(dirPrefix + f, f, FileData::is_dir, wxFileIconsTable::folder);
            if (Add(fd, item) != -1)
               item.m_itemId++;
            else
               delete fd;
            
            cont = dir.GetNext(&f);
         }
         
         // Tokenize the wildcard string, so we can handle more than 1
         // search pattern in a wildcard.
         wxStringTokenizer tokenWild(m_wild, wxT(";"));
         while ( tokenWild.HasMoreTokens() )
         {
            cont = dir.GetFirst(&f, tokenWild.GetNextToken(),
                                wxDIR_FILES | hiddenFlag);
            while (cont)
            {
               FileData *fd = new FileData(dirPrefix + f, f, FileData::is_file, wxFileIconsTable::file);
               if (Add(fd, item) != -1)
                  item.m_itemId++;
               else
                  delete fd;
               
               cont = dir.GetNext(&f);
            }
         }
      }
   }
   
   SortItems(m_sort_field, m_sort_foward);
}

void FileCtrl::SetWild( const wxString &wild )
{
   if (wild.Find(wxT('|')) != wxNOT_FOUND)
      return;
   
   m_wild = wild;
   UpdateFiles();
}

void FileCtrl::MakeDir()
{
   wxString new_name( _("NewName") );
   wxString path( m_dirName );
   path += wxFILE_SEP_PATH;
   path += new_name;
   if (wxFileExists(path))
   {
      // try NewName0, NewName1 etc.
      int i = 0;
      do {
         new_name = _("NewName");
         wxString num;
         num.Printf( wxT("%d"), i );
         new_name += num;
         
         path = m_dirName;
         path += wxFILE_SEP_PATH;
         path += new_name;
         i++;
      } while (wxFileExists(path));
   }
   
   wxLogNull log;
   if (!wxMkdir(path))
   {
      wxMessageDialog dialog(this, _("Operation not permitted."), _("Error"), wxOK | wxICON_ERROR );
      dialog.ShowModal();
      return;
   }
   
   FileData *fd = new FileData( path, new_name, FileData::is_dir, wxFileIconsTable::folder );
   wxListItem item;
   item.m_itemId = 0;
   item.m_col = 0;
   long id = Add( fd, item );
   
   if (id != -1)
   {
      SortItems(m_sort_field, m_sort_foward);
      id = FindItem( 0, (long)fd );
      EnsureVisible( id );
      EditLabel( id );
   }
   else
      delete fd;
}

void FileCtrl::GoToParentDir()
{
   if (!IsTopMostDir(m_dirName))
   {
      size_t len = m_dirName.Len();
      if (wxEndsWithPathSeparator(m_dirName))
         m_dirName.Remove( len-1, 1 );
      wxString fname( wxFileNameFromPath(m_dirName) );
      m_dirName = wxPathOnly( m_dirName );
#if defined(__DOS__) || defined(__WINDOWS__) || defined(__OS2__)
      if (!m_dirName.empty())
      {
         if (m_dirName.Last() == wxT('.'))
            m_dirName = wxEmptyString;
      }
#elif defined(__UNIX__)
      if (m_dirName.empty())
         m_dirName = wxT("/");
#endif
      UpdateFiles();
      long id = FindItem( 0, fname );
      if (id != wxNOT_FOUND)
      {
         ignoreChanges = true;
         SetItemState( id, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED );
         EnsureVisible( id );
         ignoreChanges = false;
      }
   }
}

void FileCtrl::GoToHomeDir()
{
   wxString s = wxGetUserHome( wxString() );
   GoToDir(s);
}

void FileCtrl::GoToDir( const wxString &dir )
{
   if (!wxDirExists(dir)) return;
   
   m_dirName = dir;
   UpdateFiles();
   
   ignoreChanges = true;
   SetItemState( 0, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED );
   ignoreChanges = false;
   
   EnsureVisible( 0 );
}

void FileCtrl::FreeItemData(wxListItem& item)
{
   if ( item.m_data )
   {
      FileData *fd = (FileData*)item.m_data;
      delete fd;
      
      item.m_data = 0;
   }
}

void FileCtrl::OnListDeleteItem( wxListEvent &event )
{
   FreeItemData(event.m_item);
}

void FileCtrl::OnListDeleteAllItems( wxListEvent & WXUNUSED(event) )
{
   FreeAllItemsData();
}

void FileCtrl::FreeAllItemsData()
{
   wxListItem item;
   item.m_mask = wxLIST_MASK_DATA;
   
   item.m_itemId = GetNextItem( -1, wxLIST_NEXT_ALL );
   while ( item.m_itemId != -1 )
   {
      GetItem( item );
      FreeItemData(item);
      item.m_itemId = GetNextItem( item.m_itemId, wxLIST_NEXT_ALL );
   }
}

void FileCtrl::OnListEndLabelEdit( wxListEvent &event )
{
   FileData *fd = (FileData*)event.m_item.m_data;
   wxASSERT( fd );
   
   if ((event.GetLabel().empty()) ||
       (event.GetLabel() == _(".")) ||
       (event.GetLabel() == _("..")) ||
       (event.GetLabel().First( wxFILE_SEP_PATH ) != wxNOT_FOUND))
   {
      wxMessageDialog dialog(this, _("Illegal directory name."), _("Error"), wxOK | wxICON_ERROR );
      dialog.ShowModal();
      event.Veto();
      return;
   }
   
   wxString new_name( wxPathOnly( fd->GetFilePath() ) );
   new_name += wxFILE_SEP_PATH;
   new_name += event.GetLabel();
   
   wxLogNull log;
   
   if (wxFileExists(new_name))
   {
      wxMessageDialog dialog(this, _("File name exists already."), _("Error"), wxOK | wxICON_ERROR );
      dialog.ShowModal();
      event.Veto();
   }
   
   if (wxRenameFile(fd->GetFilePath(),new_name))
   {
      fd->SetNewName( new_name, event.GetLabel() );
      
      ignoreChanges = true;
      SetItemState( event.GetItem(), wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED );
      ignoreChanges = false;
      
      UpdateItem( event.GetItem() );
      EnsureVisible( event.GetItem() );
   }
   else
   {
      wxMessageDialog dialog(this, _("Operation not permitted."), _("Error"), wxOK | wxICON_ERROR );
      dialog.ShowModal();
      event.Veto();
   }
}

void FileCtrl::OnListColClick( wxListEvent &event )
{
   int col = event.GetColumn();
   
   switch (col)
   {
      case FileData::FileList_Name :
      case FileData::FileList_Size :
      case FileData::FileList_Type :
      case FileData::FileList_Time : break;
      default : return;
   }
   
   if ((FileData::fileListFieldType)col == m_sort_field)
      m_sort_foward = !m_sort_foward;
   else
      m_sort_field = (FileData::fileListFieldType)col;
   
   SortItems(m_sort_field, m_sort_foward);
}

void FileCtrl::SortItems(FileData::fileListFieldType field, bool foward)
{
   m_sort_field = field;
   m_sort_foward = foward;
   long sort_dir = foward ? 1 : -1;
   
   switch (m_sort_field)
   {
      case FileData::FileList_Name :
      {
         wxListCtrl::SortItems((wxListCtrlCompare)FileDataNameCompare, sort_dir);
         break;
      }
      case FileData::FileList_Size :
      {
         wxListCtrl::SortItems((wxListCtrlCompare)FileDataSizeCompare, sort_dir);
         break;
      }
      case FileData::FileList_Type :
      {
         wxListCtrl::SortItems((wxListCtrlCompare)FileDataTypeCompare, sort_dir);
         break;
      }
      case FileData::FileList_Time :
      {
         wxListCtrl::SortItems((wxListCtrlCompare)FileDataTimeCompare, sort_dir);
         break;
      }
      default : break;
   }
}

FileCtrl::~FileCtrl()
{
   // Normally the data are freed via an EVT_LIST_DELETE_ALL_ITEMS event and
   // FileCtrl::OnListDeleteAllItems. But if the event is generated after
   // the destruction of the FileCtrl we need to free any data here:
   FreeAllItemsData();
}

//-----------------------------------------------------------------------------
// FileDialog
//-----------------------------------------------------------------------------

#define  ID_FILEDIALOG    9000
#define  ID_LIST_MODE     (ID_FILEDIALOG    )
#define  ID_REPORT_MODE   (ID_FILEDIALOG + 1)
#define  ID_UP_DIR        (ID_FILEDIALOG + 5)
#define  ID_PARENT_DIR    (ID_FILEDIALOG + 6)
#define  ID_NEW_DIR       (ID_FILEDIALOG + 7)
#define  ID_CHOICE        (ID_FILEDIALOG + 8)
#define  ID_TEXT          (ID_FILEDIALOG + 9)
#define  ID_LIST_CTRL     (ID_FILEDIALOG + 10)
#define  ID_CHECK         (ID_FILEDIALOG + 12)
#define  ID_EXTRABUTTON   (ID_FILEDIALOG + 13)

IMPLEMENT_DYNAMIC_CLASS(FILEDIALOG, wxFileDialogBase)

BEGIN_EVENT_TABLE(FILEDIALOG,wxDialog)
EVT_BUTTON(ID_LIST_MODE, FILEDIALOG::OnList)
EVT_BUTTON(ID_REPORT_MODE, FILEDIALOG::OnReport)
EVT_BUTTON(ID_UP_DIR, FILEDIALOG::OnUp)
EVT_BUTTON(ID_PARENT_DIR, FILEDIALOG::OnHome)
EVT_BUTTON(ID_NEW_DIR, FILEDIALOG::OnNew)
EVT_BUTTON(ID_EXTRABUTTON, FILEDIALOG::OnExtra)
EVT_BUTTON(wxID_OK, FILEDIALOG::OnListOk)
EVT_LIST_ITEM_SELECTED(ID_LIST_CTRL, FILEDIALOG::OnSelected)
EVT_LIST_ITEM_ACTIVATED(ID_LIST_CTRL, FILEDIALOG::OnActivated)
EVT_CHOICE(ID_CHOICE,FILEDIALOG::OnChoiceFilter)
EVT_TEXT_ENTER(ID_TEXT,FILEDIALOG::OnTextEnter)
EVT_TEXT(ID_TEXT,FILEDIALOG::OnTextChange)
EVT_CHECKBOX(ID_CHECK,FILEDIALOG::OnCheck)
END_EVENT_TABLE()

long FILEDIALOG::ms_lastViewStyle = wxLC_LIST;
bool FILEDIALOG::ms_lastShowHidden = false;

void FILEDIALOG::Init()
{
   m_bypassGenericImpl = false;
   
   m_choice = NULL;
   m_text   = NULL;
   m_list   = NULL;
   m_check  = NULL;
   m_static = NULL;
   m_upDirButton  = NULL;
   m_newDirButton = NULL;
}

FILEDIALOG::FILEDIALOG(wxWindow *parent,
                       const wxString& message,
                       const wxString& defaultDir,
                       const wxString& defaultFile,
                       const wxString& wildCard,
                       long  style,
                       const wxPoint& pos,
                       const wxSize& sz,
                       const wxString& name,
                       bool  bypassGenericImpl ) : wxFileDialogBase()
{
   Init();
   Create( parent, message, defaultDir, defaultFile, wildCard, style, pos, sz, name,bypassGenericImpl );
}

bool FILEDIALOG::Create( wxWindow *parent,
                        const wxString& message,
                        const wxString& defaultDir,
                        const wxString& defaultFile,
                        const wxString& wildCard,
                        long  style,
                        const wxPoint& pos,
                        const wxSize& sz,
                        const wxString& name,
                        bool  bypassGenericImpl )
{
   m_dialogStyle = style;
   m_bypassGenericImpl = bypassGenericImpl;
   
   if (!wxFileDialogBase::Create(parent, message, defaultDir, defaultFile,
                                 wildCard, style, pos, sz, name))
   {
      return false;
   }
   
   if (m_bypassGenericImpl)
      return true;
   
   if (!wxDialog::Create( parent, wxID_ANY, message, pos, wxDefaultSize,
                         wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER ))
   {
      return false;
   }
   
   ignoreChanges = true;
   
   if (wxConfig::Get(false))
   {
      wxConfig::Get()->Read(wxT("/wxWindows/wxFileDialog/ViewStyle"),
                            &ms_lastViewStyle);
      wxConfig::Get()->Read(wxT("/wxWindows/wxFileDialog/ShowHidden"),
                            &ms_lastShowHidden);
   }
   
   if (m_dialogStyle == 0)
      m_dialogStyle = wxFD_OPEN;
   if ((m_dialogStyle & wxFD_MULTIPLE ) && !(m_dialogStyle & wxFD_OPEN))
      m_dialogStyle |= wxFD_OPEN;
   
   if ((m_dir.empty()) || (m_dir == wxT(".")))
   {
      m_dir = wxGetCwd();
      if (m_dir.empty())
         m_dir = wxFILE_SEP_PATH;
   }
   
   size_t len = m_dir.Len();
   if ((len > 1) && (wxEndsWithPathSeparator(m_dir)))
      m_dir.Remove( len-1, 1 );
   
   m_path = m_dir;
   m_path += wxFILE_SEP_PATH;
   m_path += defaultFile;
   m_filterExtension = wxEmptyString;
   
   // layout
   
   bool is_pda = (wxSystemSettings::GetScreenType() <= wxSYS_SCREEN_PDA);
   
   wxBoxSizer *mainsizer = new wxBoxSizer( wxVERTICAL );
   
   wxBoxSizer *buttonsizer = new wxBoxSizer( wxHORIZONTAL );
   
   wxBitmapButton *but;
   
   but = new wxBitmapButton(this, ID_LIST_MODE,
                            wxArtProvider::GetBitmap(wxART_LIST_VIEW, wxART_BUTTON));
#if wxUSE_TOOLTIPS
   but->SetToolTip( _("View files as a list view") );
#endif
   buttonsizer->Add( but, 0, wxALL, 5 );
   
   but = new wxBitmapButton(this, ID_REPORT_MODE,
                            wxArtProvider::GetBitmap(wxART_REPORT_VIEW, wxART_BUTTON));
#if wxUSE_TOOLTIPS
   but->SetToolTip( _("View files as a detailed view") );
#endif
   buttonsizer->Add( but, 0, wxALL, 5 );
   
   buttonsizer->Add( 30, 5, 1 );
   
   m_upDirButton = new wxBitmapButton(this, ID_UP_DIR,
                                      wxArtProvider::GetBitmap(wxART_GO_DIR_UP, wxART_BUTTON));
#if wxUSE_TOOLTIPS
   m_upDirButton->SetToolTip( _("Go to parent directory") );
#endif
   buttonsizer->Add( m_upDirButton, 0, wxALL, 5 );
   
#ifndef __DOS__ // VS: Home directory is meaningless in MS-DOS...
   but = new wxBitmapButton(this, ID_PARENT_DIR,
                            wxArtProvider::GetBitmap(wxART_GO_HOME, wxART_BUTTON));
#if wxUSE_TOOLTIPS
   but->SetToolTip( _("Go to home directory") );
#endif
   buttonsizer->Add( but, 0, wxALL, 5);
   
   buttonsizer->Add( 20, 20 );
#endif //!__DOS__
   
   m_newDirButton = new wxBitmapButton(this, ID_NEW_DIR,
                                       wxArtProvider::GetBitmap(wxART_NEW_DIR, wxART_BUTTON));
#if wxUSE_TOOLTIPS
   m_newDirButton->SetToolTip( _("Create new directory") );
#endif
   buttonsizer->Add( m_newDirButton, 0, wxALL, 5 );
   
   if (is_pda)
      mainsizer->Add( buttonsizer, 0, wxALL | wxEXPAND, 0 );
   else
      mainsizer->Add( buttonsizer, 0, wxALL | wxEXPAND, 5 );
   
   wxBoxSizer *staticsizer = new wxBoxSizer( wxHORIZONTAL );
   if (!is_pda)
      staticsizer->Add( new wxStaticText( this, wxID_ANY, _("Current directory:") ), 0, wxRIGHT, 10 );
   m_static = new wxStaticText( this, wxID_ANY, m_dir );
   staticsizer->Add( m_static, 1 );
   mainsizer->Add( staticsizer, 0, wxEXPAND | wxLEFT|wxRIGHT|wxBOTTOM, 10 );
   
   long style2 = ms_lastViewStyle;
   if ( !(m_dialogStyle & wxFD_MULTIPLE) )
      style2 |= wxLC_SINGLE_SEL;
   
#ifdef __WXWINCE__
   style2 |= wxSIMPLE_BORDER;
#else
   style2 |= wxSUNKEN_BORDER;
#endif
   
   wxSize list_size(500,240);
   if (is_pda) list_size = wxSize(50,80);
   
   m_list = new FileCtrl( this, ID_LIST_CTRL,
                         wxEmptyString, ms_lastShowHidden,
                         wxDefaultPosition, list_size,
                         style2);
   
   if (is_pda)
   {
      // PDAs have a different screen layout
      mainsizer->Add( m_list, 1, wxEXPAND|wxSHRINK | wxLEFT|wxRIGHT, 5 );
      
      wxBoxSizer *textsizer = new wxBoxSizer( wxHORIZONTAL );
      m_text = new wxTextCtrl( this, ID_TEXT, m_fileName, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
      textsizer->Add( m_text, 1, wxCENTER | wxALL, 5 );
      mainsizer->Add( textsizer, 0, wxEXPAND );
      
      m_check = NULL;
      m_choice = new wxChoice( this, ID_CHOICE );
      textsizer->Add( m_choice, 1, wxCENTER|wxALL, 5 );
      
      buttonsizer = new wxBoxSizer( wxHORIZONTAL );
      buttonsizer->Add( new wxButton( this, wxID_OK ), 0, wxCENTER | wxALL, 5 );
      buttonsizer->Add( new wxButton( this, wxID_CANCEL ), 0, wxCENTER | wxALL, 5 );
      mainsizer->Add( buttonsizer, 0, wxALIGN_RIGHT );
   }
   else
   {
      mainsizer->Add( m_list, 1, wxEXPAND | wxLEFT|wxRIGHT, 10 );
      
      wxBoxSizer *textsizer = new wxBoxSizer( wxHORIZONTAL );
      m_text = new wxTextCtrl( this, ID_TEXT, m_fileName, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
      textsizer->Add( m_text, 1, wxCENTER | wxLEFT|wxRIGHT|wxTOP, 10 );
      textsizer->Add( new wxButton( this, wxID_OK ), 0, wxCENTER | wxLEFT|wxRIGHT|wxTOP, 10 );
      mainsizer->Add( textsizer, 0, wxEXPAND );
      
      m_choicesizer = new wxBoxSizer( wxHORIZONTAL );
      m_choice = new wxChoice( this, ID_CHOICE );
      m_choicesizer->Add( m_choice, 1, wxCENTER|wxALL, 10 );
      m_check = new wxCheckBox( this, ID_CHECK, _("Show hidden files") );
      m_check->SetValue( ms_lastShowHidden );
      m_choicesizer->Add( m_check, 0, wxCENTER|wxALL, 10 );
      m_choicesizer->Add( new wxButton( this, wxID_CANCEL ), 0, wxCENTER | wxALL, 10 );
      mainsizer->Add( m_choicesizer, 0, wxEXPAND );
   }
   
   SetWildcard(wildCard);
   
   SetAutoLayout( true );
   SetSizer( mainsizer );
   
   if (!is_pda)
   {
      mainsizer->Fit( this );
      mainsizer->SetSizeHints( this );
      
      Centre( wxBOTH );
   }
   
   m_text->SetFocus();
   
   ignoreChanges = false;
   
   return true;
}

FILEDIALOG::~FILEDIALOG()
{
   ignoreChanges = true;
   
   if (!m_bypassGenericImpl)
   {
      if (wxConfig::Get(false))
      {
         wxConfig::Get()->Write(wxT("/wxWindows/wxFileDialog/ViewStyle"),
                                ms_lastViewStyle);
         wxConfig::Get()->Write(wxT("/wxWindows/wxFileDialog/ShowHidden"),
                                ms_lastShowHidden);
      }
      
      const int count = m_choice->GetCount();
      for ( int i = 0; i < count; i++ )
      {
         delete (wxString *)m_choice->GetClientData(i);
      }
   }
}

int FILEDIALOG::ShowModal()
{
   if (!m_buttonlabel.IsEmpty())
   {
      wxButton *btn = new wxButton( this, ID_EXTRABUTTON, m_buttonlabel );
      m_choicesizer->Insert( 1, btn, 0, wxCENTER | wxALL, 5 );
   }
   
   ignoreChanges = true;
   
   m_list->GoToDir(m_dir);
   UpdateControls();
   m_text->SetValue(m_fileName);
   
   ignoreChanges = false;
   
   return wxDialog::ShowModal();
}

bool FILEDIALOG::Show( bool show )
{
   // Called by ShowModal, so don't repeate the update
#ifndef __WIN32__
   if (show)
   {
      m_list->GoToDir(m_dir);
      UpdateControls();
      m_text->SetValue(m_fileName);
   }
#endif
   
   return wxDialog::Show( show );
}

void FILEDIALOG::DoSetFilterIndex(int filterindex)
{
   wxString *str = (wxString*) m_choice->GetClientData( filterindex );
   m_list->SetWild( *str );
   m_filterIndex = filterindex;
   if ( str->Left(2) == wxT("*.") )
   {
      m_filterExtension = str->Mid(1);
      if (m_filterExtension == wxT(".*"))
         m_filterExtension.clear();
   }
   else
   {
      m_filterExtension.clear();
   }
}

void FILEDIALOG::SetWildcard(const wxString& wildCard)
{
   wxFileDialogBase::SetWildcard(wildCard);
   
   wxArrayString wildDescriptions, wildFilters;
   const size_t count = wxParseCommonDialogsFilter(m_wildCard,
                                                   wildDescriptions,
                                                   wildFilters);
   wxCHECK_RET( count, wxT("wxFILEDIALOG: bad wildcard string") );
   
   const size_t countOld = m_choice->GetCount();
   size_t n;
   for ( n = 0; n < countOld; n++ )
   {
      delete (wxString *)m_choice->GetClientData(n);
   }
   
   for ( n = 0; n < count; n++ )
   {
      m_choice->Append( wildDescriptions[n], new wxString( wildFilters[n] ) );
   }
   
   SetFilterIndex( 0 );
}

void FILEDIALOG::SetFilterIndex( int filterindex )
{
   m_choice->SetSelection( filterindex );
   
   DoSetFilterIndex(filterindex);
}

void FILEDIALOG::OnChoiceFilter( wxCommandEvent &event )
{
   DoSetFilterIndex((int)event.GetInt());
}

void FILEDIALOG::OnCheck( wxCommandEvent &event )
{
   m_list->ShowHidden( (ms_lastShowHidden = event.GetInt() != 0) );
}

void FILEDIALOG::OnActivated( wxListEvent &event )
{
   HandleAction( event.m_item.m_text );
}

void FILEDIALOG::OnTextEnter( wxCommandEvent &WXUNUSED(event) )
{
   wxCommandEvent cevent(wxEVT_COMMAND_BUTTON_CLICKED, wxID_OK);
   cevent.SetEventObject( this );
   GetEventHandler()->ProcessEvent( cevent );
}

void FILEDIALOG::OnTextChange( wxCommandEvent &WXUNUSED(event) )
{
   if (!ignoreChanges)
   {
      // Clear selections.  Otherwise when the user types in a value they may
      // not get the file whose name they typed.
      if (m_list->GetSelectedItemCount() > 0)
      {
         long item = m_list->GetNextItem(-1, wxLIST_NEXT_ALL,
                                         wxLIST_STATE_SELECTED);
         while ( item != -1 )
         {
            m_list->SetItemState(item,0, wxLIST_STATE_SELECTED);
            item = m_list->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
         }
      }
   }
}

void FILEDIALOG::OnSelected( wxListEvent &event )
{
   static bool inSelected = false;
   
   if (inSelected)
      return;
   
   inSelected = true;
   wxString filename( event.m_item.m_text );
   
#ifdef __WXWINCE__
   // No double-click on most WinCE devices, so do action immediately.
   HandleAction( filename );
#else
   if (filename == wxT("..")) return;
   
   wxString dir = m_list->GetDir();
   if (!IsTopMostDir(dir))
      dir += wxFILE_SEP_PATH;
   dir += filename;
   if (wxDirExists(dir)) return;
   
   ignoreChanges = true;
   m_text->SetValue( filename );
   ignoreChanges = false;
#endif
   inSelected = false;
}

void FILEDIALOG::HandleAction( const wxString &fn )
{
   if (ignoreChanges)
      return;
   
   wxString filename( fn );
   wxString dir = m_list->GetDir();
   if (filename.empty()) return;
   if (filename == wxT(".")) return;
   
   // "some/place/" means they want to chdir not try to load "place"
   bool want_dir = filename.Last() == wxFILE_SEP_PATH;
   if (want_dir)
      filename = filename.RemoveLast();
   
   if (filename == wxT(".."))
   {
      ignoreChanges = true;
      m_list->GoToParentDir();
      m_list->SetFocus();
      UpdateControls();
      ignoreChanges = false;
      return;
   }
   
#ifdef __UNIX__
   if (filename == wxT("~"))
   {
      ignoreChanges = true;
      m_list->GoToHomeDir();
      m_list->SetFocus();
      UpdateControls();
      ignoreChanges = false;
      return;
   }
   
   if (filename.BeforeFirst(wxT('/')) == wxT("~"))
   {
      filename = wxString(wxGetUserHome()) + filename.Remove(0, 1);
   }
#endif // __UNIX__
   
   if (!(m_dialogStyle & wxFD_SAVE))
   {
      if ((filename.Find(wxT('*')) != wxNOT_FOUND) ||
          (filename.Find(wxT('?')) != wxNOT_FOUND))
      {
         if (filename.Find(wxFILE_SEP_PATH) != wxNOT_FOUND)
         {
            wxMessageBox(_("Illegal file specification."), _("Error"), wxOK | wxICON_ERROR );
            return;
         }
         m_list->SetWild( filename );
         return;
      }
   }
   
   if (!IsTopMostDir(dir))
      dir += wxFILE_SEP_PATH;
   if (!wxIsAbsolutePath(filename))
   {
      dir += filename;
      filename = dir;
   }
   
   if (wxDirExists(filename))
   {
      ignoreChanges = true;
      m_list->GoToDir( filename );
      UpdateControls();
      ignoreChanges = false;
      return;
   }
   
   // they really wanted a dir, but it doesn't exist
   if (want_dir)
   {
      wxMessageBox(_("Directory doesn't exist."), _("Error"),
                   wxOK | wxICON_ERROR );
      return;
   }
   
   // append the default extension to the filename if it doesn't have any
   //
   // VZ: the logic of testing for !wxFileExists() only for the open file
   //     dialog is not entirely clear to me, why don't we allow saving to a
   //     file without extension as well?
   if ( !(m_dialogStyle & wxFD_OPEN) || !wxFileExists(filename) )
   {
      filename = AppendExtension(filename, m_filterExtension);
   }
   
   // check that the file [doesn't] exist if necessary
   if ( (m_dialogStyle & wxFD_SAVE) &&
       (m_dialogStyle & wxFD_OVERWRITE_PROMPT) &&
       wxFileExists( filename ) )
   {
      wxString msg;
      msg.Printf( _("File '%s' already exists, do you really want to overwrite it?"), filename.c_str() );
      
      if (wxMessageBox(msg, _("Confirm"), wxYES_NO) != wxYES)
         return;
   }
   else if ( (m_dialogStyle & wxFD_OPEN) &&
            (m_dialogStyle & wxFD_FILE_MUST_EXIST) &&
            !wxFileExists(filename) )
   {
      wxMessageBox(_("Please choose an existing file."), _("Error"),
                   wxOK | wxICON_ERROR );
   }
   
   SetPath( filename );
   
   // change to the directory where the user went if asked
   if ( m_dialogStyle & wxFD_CHANGE_DIR )
   {
      wxString cwd;
      wxSplitPath(filename, &cwd, NULL, NULL);
      
      if ( cwd != wxGetCwd() )
      {
         wxSetWorkingDirectory(cwd);
      }
   }
   
   if (Validate() && TransferDataFromWindow())
      EndModal(wxID_OK);
}

void FILEDIALOG::OnListOk( wxCommandEvent &WXUNUSED(event) )
{
   HandleAction( m_text->GetValue() );
}

void FILEDIALOG::OnList( wxCommandEvent &WXUNUSED(event) )
{
   ignoreChanges = true;
   m_list->ChangeToListMode();
   ms_lastViewStyle = wxLC_LIST;
   m_list->SetFocus();
   ignoreChanges = false;
}

void FILEDIALOG::OnReport( wxCommandEvent &WXUNUSED(event) )
{
   ignoreChanges = true;
   m_list->ChangeToReportMode();
   ms_lastViewStyle = wxLC_REPORT;
   m_list->SetFocus();
   ignoreChanges = false;
}

void FILEDIALOG::OnUp( wxCommandEvent &WXUNUSED(event) )
{
   ignoreChanges = true;
   m_list->GoToParentDir();
   m_list->SetFocus();
   UpdateControls();
   ignoreChanges = false;
}

void FILEDIALOG::OnHome( wxCommandEvent &WXUNUSED(event) )
{
   ignoreChanges = true;
   m_list->GoToHomeDir();
   m_list->SetFocus();
   UpdateControls();
   ignoreChanges = false;
}

void FILEDIALOG::OnNew( wxCommandEvent &WXUNUSED(event) )
{
   ignoreChanges = true;
   
   m_list->MakeDir();
   
   ignoreChanges = false;
}

void FILEDIALOG::OnExtra( wxCommandEvent &WXUNUSED(event) )
{
#if !defined(GENERIC_FILEDIALOG)
   ClickButton(m_choice->GetSelection());
#endif
}

void FILEDIALOG::SetPath( const wxString& path )
{
   // not only set the full path but also update filename and dir
   m_path = path;
   
#ifdef __WXWINCE__
   if (m_path.empty())
      m_path = wxFILE_SEP_PATH;
#endif
   
   if ( !path.empty() )
   {
      wxString ext;
      wxSplitPath(path, &m_dir, &m_fileName, &ext);
      if (!ext.empty())
      {
         m_fileName += wxT(".");
         m_fileName += ext;
      }
   }
}

void FILEDIALOG::GetPaths( wxArrayString& paths ) const
{
   paths.Empty();
   if (m_list->GetSelectedItemCount() == 0)
   {
      paths.Add( GetPath() );
      return;
   }
   
   paths.Alloc( m_list->GetSelectedItemCount() );
   
   wxString dir = m_list->GetDir();
#ifdef __UNIX__
   if (dir != wxT("/"))
#endif
#ifdef __WXWINCE__
      if (dir != wxT("/") && dir != wxT("\\"))
#endif
         dir += wxFILE_SEP_PATH;
   
   wxListItem item;
   item.m_mask = wxLIST_MASK_TEXT;
   
   item.m_itemId = m_list->GetNextItem( -1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED );
   while ( item.m_itemId != -1 )
   {
      m_list->GetItem( item );
      paths.Add( dir + item.m_text );
      item.m_itemId = m_list->GetNextItem( item.m_itemId,
                                          wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED );
   }
}

void FILEDIALOG::GetFilenames(wxArrayString& files) const
{
   files.Empty();
   if (m_list->GetSelectedItemCount() == 0)
   {
      files.Add( GetFilename() );
      return;
   }
   files.Alloc( m_list->GetSelectedItemCount() );
   
   wxListItem item;
   item.m_mask = wxLIST_MASK_TEXT;
   
   item.m_itemId = m_list->GetNextItem( -1, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED );
   while ( item.m_itemId != -1 )
   {
      m_list->GetItem( item );
      files.Add( item.m_text );
      item.m_itemId = m_list->GetNextItem( item.m_itemId,
                                          wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED );
   }
}

void FILEDIALOG::UpdateControls()
{
   wxString dir = m_list->GetDir();
   m_static->SetLabel(dir);
   
   bool enable = !IsTopMostDir(dir);
   m_upDirButton->Enable(enable);
   
#if defined(__DOS__) || defined(__WINDOWS__) || defined(__OS2__)
   m_newDirButton->Enable(enable);
#endif // defined(__DOS__) || defined(__WINDOWS__) || defined(__OS2__)
}
