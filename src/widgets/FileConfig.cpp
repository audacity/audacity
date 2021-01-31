/**********************************************************************

  Audacity: A Digital Audio Editor

  FileConfig.cpp

  Leland Lucius

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/bmpbuttn.h>
#include <wx/button.h>
#include <wx/filefn.h>
#include <wx/fileconf.h>
#include <wx/sizer.h>
#include <wx/wfstream.h>

#include "FileConfig.h"
#include "HelpSystem.h"
#include "wxPanelWrapper.h"
#include "../ShuttleGui.h"

#include "../../images/Help.xpm"

#if !defined(F_OK)
#define F_OK 0x00
#endif
#if !defined(W_OK)
#define W_OK 0x02
#endif
#if !defined(R_OK)
#define R_OK 0x04
#endif

FileConfig::FileConfig(const wxString& appName,
                       const wxString& vendorName,
                       const wxString& localFilename,
                       const wxString& globalFilename,
                       long style,
                       const wxMBConv& conv)
:  wxFileConfig(appName, vendorName, localFilename, globalFilename, style, conv),
   mConfigPath(localFilename),
   mDirty(false)
{
}

void FileConfig::Init()
{
   // Prevent wxFileConfig from attempting a Flush() during object deletion. This happens
   // because we don't use the wxFileConfig::Flush() method and so the wxFileConfig dirty
   // flag never gets reset. During deleting it is checked and a Flush() performed. This
   // can (and probably will) create bogus temporary files.
   DisableAutoSave();

   while (true)
   {
      bool canRead = false;
      bool canWrite = false;
      int fd;

      fd = wxOpen(mConfigPath, O_RDONLY, S_IREAD);
      if (fd != -1 || errno == ENOENT)
      {
         canRead = true;
         if (fd != -1)
         {
            wxClose(fd);
         }
      }

      fd = wxOpen(mConfigPath, O_WRONLY | O_CREAT, S_IWRITE);
      if (fd != -1)
      {
         canWrite = true;
         wxClose(fd);
      }

      if (canRead && canWrite)
      {
         break;
      }

      // If we can't read an existing config file, we must not allow the user to retry
      // since the wxFileConfig initialization will not have read it and the caller
      // will assume that the file didn't exist and possibly initialize it. This
      // could lead to wiping out the original contents.
      //
      // If the wxFileConfig class allowed us to call wxFileConfig::Init(), we wouldn't
      // have to do all this mess.
      Warn(canRead == true);
   }
}

FileConfig::~FileConfig()
{
   wxASSERT(mDirty == false);
}

bool FileConfig::Flush(bool WXUNUSED(bCurrentOnly))
{
   if (!mDirty)
   {
      return true;
   }

   while (true)
   {
      FilePath backup = mConfigPath + ".bkp";

      if (!wxFileExists(backup) || (wxRemove(backup) == 0))
      {
         if (!wxFileExists(mConfigPath) || (wxRename(mConfigPath, backup) == 0))
         {
            wxFileOutputStream stream(mConfigPath);
            if (stream.IsOk())
            {
               if (Save(stream))
               {
                  stream.Sync();
                  if (stream.IsOk() && stream.Close())
                  {
                     if (!wxFileExists(backup) || (wxRemove(backup) == 0))
                     {
                        mDirty = false;
                        return true;
                     }
                  }
               }
            }

            if (wxFileExists(backup))
            {
               wxRemove(mConfigPath);
               wxRename(backup, mConfigPath);
            }
         }
      }

      Warn();
   }

   return false;
}

bool FileConfig::DoWriteString(const wxString& key, const wxString& szValue)
{
   bool res = wxFileConfig::DoWriteString(key, szValue);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

bool FileConfig::DoWriteLong(const wxString& key, long lValue)
{
   bool res = wxFileConfig::DoWriteLong(key, lValue);
   if (res)
   {
      mDirty = true;
   }
   return res;
}

#if wxUSE_BASE64
bool FileConfig::DoWriteBinary(const wxString& key, const wxMemoryBuffer& buf)
{
   bool res = wxFileConfig::DoWriteBinary(key, buf);
   if (res)
   {
      mDirty = true;
   }
   return res;
}
#endif // wxUSE_BASE64

void FileConfig::Warn(bool canRetry)
{
   wxDialogWrapper dlg(nullptr, wxID_ANY, XO("Audacity Configuration Error"));

   ShuttleGui S(&dlg, eIsCreating);

   wxButton *retryButton;
   wxButton *quitButton;

   S.SetBorder(5);
   S.StartVerticalLay(wxEXPAND, 1);
   {
      S.SetBorder(15);
      S.StartHorizontalLay(wxALIGN_RIGHT, 0);
      {
         auto cause = canRetry
            ? XO("The following configuration file could not be written:")
            : XO("The following configuration file could not be read:");
            
         auto retryMsg = canRetry
            ? XO("You can attempt to correct the issue and then click \"Retry\" to coninue.\n\n")
            : XO("");

         S.AddFixedText(
            XO("%s:\n\n"
               "\t%s\n\n"
               "This could be caused by many reasons, but the most likely are that "
               "the disk is full or you do not have write permissions to the file. "
               "More information can be obtained by clicking the help button below.\n\n"
               "%s"
               "If you choose to \"Quit Audacity\", your project may be left in an unsaved "
               "state which will be recovered the next time you open it.")
            .Format(cause, mConfigPath, retryMsg),
            false,
            500);
      }
      S.EndHorizontalLay();

      S.SetBorder(5);
      S.StartHorizontalLay(wxALIGN_RIGHT, 0);
      {
         // Can't use themed bitmap since the theme manager might not be
         // initialized yet and it requires a configuration file.
         wxButton *b = S.Id(wxID_HELP).AddBitmapButton(wxBitmap(Help_xpm));
         b->SetToolTip( XO("Help").Translation() );
         b->SetLabel(XO("Help").Translation());       // for screen readers

         b = S.Id(wxID_CANCEL).AddButton(XXO("&Quit Audacity"));

         if (canRetry)
         {
            b = S.Id(wxID_OK).AddButton(XXO("&Retry"));
            dlg.SetAffirmativeId(wxID_OK);
         }

         b->SetDefault();
         b->SetFocus();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   dlg.Layout();
   dlg.GetSizer()->Fit(&dlg);
   dlg.SetMinSize(dlg.GetSize());
   dlg.Center();

   auto onButton = [&](wxCommandEvent &e)
   {
      dlg.EndModal(e.GetId());
   };

   dlg.Bind(wxEVT_BUTTON, onButton);

   switch (dlg.ShowModal())
   {
      case wxID_HELP:
         // Can't use the HelpSystem since the theme manager may not
         // yet be initialized and it requires a configuration file.
         OpenInDefaultBrowser("https://" + 
                              HelpSystem::HelpHostname +
                              HelpSystem::HelpServerHomeDir +
                              "Error:_Audacity_settings_file_unwritable");
      break;

      case wxID_CANCEL:
         // This REALLY needs to use an exception with decent cleanup and program exit
         wxExit();
#if defined(__WXGTK__)
         wxAbort();
#else
         exit(-1);
#endif
      break;
   }

   dlg.Unbind(wxEVT_BUTTON, onButton);
}
