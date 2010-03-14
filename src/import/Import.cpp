/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.cpp

  Dominic Mazzoni

*******************************************************************//**

\file Import.cpp

  This file contains a general function which will import almost
  any type of sampled audio file (i.e. anything except MIDI)
  and return the tracks that were imported.  This function just
  figures out which one to call; the actual importers are in
  ImportPCM, ImportMP3, ImportOGG, ImportRawData, ImportLOF, 
  ImportQT and ImportFLAC.

*//***************************************************************//**

\class Format
\brief Abstract base class used in importing a file.

It's defined in Import.h

*//***************************************************************//**

\class Importer
\brief Class which actulaly imports the auido, using functions defined 
in ImportPCM.cpp, ImportMP3.cpp, ImportOGG.cpp, ImportRawData.cpp, 
and ImportLOF.cpp.
 
*//******************************************************************/



#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/intl.h>
#include <wx/listimpl.cpp>
#include <wx/log.h>
#include <wx/sizer.h>         //for wxBoxSizer
#include "../ShuttleGui.h"
#include "../Audacity.h"

#include "Import.h"
#include "ImportPlugin.h"
#include "ImportPCM.h"
#include "ImportMP3.h"
#include "ImportOGG.h"
#include "ImportQT.h"
#include "ImportRaw.h"
#include "ImportLOF.h"
#include "ImportFLAC.h"
#include "ImportFFmpeg.h"
#include "ImportGStreamer.h"
#include "../Track.h"
#include "../Prefs.h"

WX_DEFINE_LIST(ImportPluginList);
WX_DEFINE_LIST(UnusableImportPluginList);
WX_DEFINE_LIST(FormatList);

Importer::Importer()
{
   mImportPluginList = new ImportPluginList;
   mUnusableImportPluginList = new UnusableImportPluginList;

   // build the list of import plugin and/or unusableImporters.
   // order is significant.  If none match, they will all be tried
   // in the order defined here.
   GetPCMImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetOGGImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetFLACImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetMP3ImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetLOFImportPlugin(mImportPluginList, mUnusableImportPluginList);

   #ifdef USE_QUICKTIME
   GetQTImportPlugin(mImportPluginList, mUnusableImportPluginList);
   #endif
   #if defined(USE_FFMPEG)
   GetFFmpegImportPlugin(mImportPluginList, mUnusableImportPluginList);
   #endif
   #if defined(USE_GSTREAMER)
   GetGStreamerImportPlugin(mImportPluginList, mUnusableImportPluginList);
   #endif

}

Importer::~Importer()
{
   mImportPluginList->DeleteContents(true);
   delete mImportPluginList;
   mUnusableImportPluginList->DeleteContents(true);//JKC
   delete mUnusableImportPluginList;
}

void Importer::GetSupportedImportFormats(FormatList *formatList)
{
   ImportPluginList::compatibility_iterator importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *importPlugin = importPluginNode->GetData();
      formatList->Append(new Format(importPlugin->GetPluginFormatDescription(),
                                    importPlugin->GetSupportedExtensions()));
      importPluginNode = importPluginNode->GetNext();
   }
}

// returns number of tracks imported
int Importer::Import(wxString fName,
                     TrackFactory *trackFactory,
                     Track *** tracks,
                     Tags *tags,
                     wxString &errorMessage)
{
   ImportFileHandle *inFile = NULL;
   int numTracks = 0;

   wxString extension = fName.AfterLast(wxT('.'));

   // This list is used to call plugins in correct order
   ImportPluginList importPlugins;
   ImportPluginList::compatibility_iterator importPluginNode;

   // This list is used to remember plugins that should have been compatible with the file.
   ImportPluginList compatiblePlugins;
   
   // If user explicitly selected a filter,
   // then we should try importing via corresponding plugin first
   wxString type = gPrefs->Read(wxT("/LastOpenType"),wxT(""));

   // First, add all explicitly compatible plugins
   importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      if (plugin->GetPluginFormatDescription().CompareTo(type) == 0)
      {
         // This plugin corresponds to user-selected filter, try it first.
         importPlugins.Insert(plugin);
      }
      else if (plugin->SupportsExtension(extension))
      {
         importPlugins.Append(plugin);
      }
      if (plugin->SupportsExtension(extension))
        compatiblePlugins.Append(plugin);
      importPluginNode = importPluginNode->GetNext();
   }

   // Next, add all other plugins
   importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      if (importPlugins.Find(plugin) == NULL)
      {
         // Skip MP3 import plugin. Opens some non-mp3 audio files (ac3 for example) as garbage.
         if (plugin->GetPluginFormatDescription().CompareTo( _("MP3 files") ) != 0)
         {
            importPlugins.Append(plugin);
         }        
      }
      importPluginNode = importPluginNode->GetNext();
   }

   importPluginNode = importPlugins.GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      // Try to open the file with this plugin (probe it)
      inFile = plugin->Open(fName);
      if ( (inFile != NULL) && (inFile->GetStreamCount() > 0) )
      {
         // File has more than one stream - display stream selector
         if (inFile->GetStreamCount() > 1)                                                  
         {
            ImportStreamDialog ImportDlg(inFile, NULL, -1, _("Select stream(s) to import"));

            if (ImportDlg.ShowModal() == wxID_CANCEL)
            {
               delete inFile;
               return 0;
            }
         }
         // One stream - import it by default
         else
            inFile->SetStreamUsage(0,TRUE);

         int res;
         
         res = inFile->Import(trackFactory, tracks, &numTracks, tags);

         delete inFile;

         if (res == eProgressSuccess || res == eProgressStopped)
         {
            // LOF ("list-of-files") has different semantics
            if (extension.IsSameAs(wxT("lof"), false))
               return 1;

            if (numTracks > 0) {
               // success!
               return numTracks;
            }
         }

         if (res == eProgressCancelled || res == eProgressFailed)
         {
            return 0;
         }

         // We could exit here since we had a match on the file extension,
         // but there may be another plug-in that can import the file and
         // that may recognize the extension, so we allow the loop to
         // continue.
      }
      importPluginNode = importPluginNode->GetNext();
   }

   // None of our plugins can handle this file.  It might be that
   // Audacity supports this format, but support was not compiled in.
   // If so, notify the user of this fact
   UnusableImportPluginList::compatibility_iterator unusableImporterNode
      = mUnusableImportPluginList->GetFirst();
   while(unusableImporterNode)
   {
      UnusableImportPlugin *unusableImportPlugin = unusableImporterNode->GetData();
      if( unusableImportPlugin->SupportsExtension(extension) )
      {
         errorMessage.Printf(_("This version of Audacity was not compiled with %s support."),
                             unusableImportPlugin->
                             GetPluginFormatDescription().c_str());
         return 0;
      }
      unusableImporterNode = unusableImporterNode->GetNext();
   }

   /* warnings for unsupported data types */

#ifdef USE_MIDI
   // MIDI files must be imported, not opened
   if ((extension.IsSameAs(wxT("midi"), false))||(extension.IsSameAs(wxT("mid"), false))) {
      errorMessage.Printf(_("\"%s\" \nis a MIDI file, not an audio file. \nAudacity cannot open this type of file for playing, but you can\nedit it by clicking File > Import > MIDI."), fName.c_str());
      return 0;
   }
#endif

   if (compatiblePlugins.GetCount() <= 0)
   {
      // if someone has sent us a .cda file, send them away
      if (extension.IsSameAs(wxT("cda"), false)) {
         /* i18n-hint: %s will be the filename */
         errorMessage.Printf(_("\"%s\" is an audio CD track. \nAudacity cannot open audio CDs directly. \nExtract (rip) the CD tracks to an audio format that \nAudacity can import, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
   
      // playlist type files
      if ((extension.IsSameAs(wxT("m3u"), false))||(extension.IsSameAs(wxT("ram"), false))||(extension.IsSameAs(wxT("pls"), false))) {
         errorMessage.Printf(_("\"%s\" is a playlist file. \nAudacity cannot open this file because it only contains links to other files. \nYou may be able to open it in a text editor and download the actual audio files."), fName.c_str());
         return 0;
      }
      //WMA files of various forms
      if ((extension.IsSameAs(wxT("wma"), false))||(extension.IsSameAs(wxT("asf"), false))) {
         errorMessage.Printf(_("\"%s\" is a Windows Media Audio file. \nAudacity cannot open this type of file due to patent restrictions. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
      //AAC files of various forms (probably not encrypted)
      if ((extension.IsSameAs(wxT("aac"), false))||(extension.IsSameAs(wxT("m4a"), false))||(extension.IsSameAs(wxT("m4r"), false))||(extension.IsSameAs(wxT("mp4"), false))) {
         errorMessage.Printf(_("\"%s\" is an Advanced Audio Coding file. \nAudacity cannot open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
      // encrypted itunes files
      if ((extension.IsSameAs(wxT("m4p"), false))) {
         errorMessage.Printf(_("\"%s\" is an encrypted audio file. \nThese typically are from an online music store. \nAudacity cannot open this type of file due to the encryption. \nTry recording the file into Audacity, or burn it to audio CD then \nextract the CD track to a supported audio format such as WAV or AIFF."), fName.c_str());
         return 0;
      }
      // Real Inc. files of various sorts
      if ((extension.IsSameAs(wxT("ra"), false))||(extension.IsSameAs(wxT("rm"), false))||(extension.IsSameAs(wxT("rpm"), false))) {
         errorMessage.Printf(_("\"%s\" is a RealPlayer media file. \nAudacity cannot open this proprietary format. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
   
      // Other notes-based formats
      if ((extension.IsSameAs(wxT("kar"), false))||(extension.IsSameAs(wxT("mod"), false))||(extension.IsSameAs(wxT("rmi"), false))) {
         errorMessage.Printf(_("\"%s\" is a notes-based file, not an audio file. \nAudacity cannot open this type of file. \nTry converting it to an audio file such as WAV or AIFF and \nthen import it, or record it into Audacity."), fName.c_str());
         return 0;
      }
   
      // MusePack files
      if ((extension.IsSameAs(wxT("mp+"), false))||(extension.IsSameAs(wxT("mpc"), false))||(extension.IsSameAs(wxT("mpp"), false))) {
         errorMessage.Printf(_("\"%s\" is a Musepack audio file. \nAudacity cannot open this type of file. \nIf you think it might be an mp3 file, rename it to end with \".mp3\" \nand try importing it again. Otherwise you need to convert it to a supported audio \nformat, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
   
      // WavPack files
      if ((extension.IsSameAs(wxT("wv"), false))||(extension.IsSameAs(wxT("wvc"), false))) {
         errorMessage.Printf(_("\"%s\" is a Wavpack audio file. \nAudacity cannot open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
   
      // AC3 files
      if ((extension.IsSameAs(wxT("ac3"), false))) {
         errorMessage.Printf(_("\"%s\" is a Dolby Digital audio file. \nAudacity cannot currently open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
   
      // Speex files
      if ((extension.IsSameAs(wxT("spx"), false))) {
         errorMessage.Printf(_("\"%s\" is an Ogg Speex audio file. \nAudacity cannot currently open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }
   
      // Video files of various forms
      if ((extension.IsSameAs(wxT("mpg"), false))||(extension.IsSameAs(wxT("mpeg"), false))||(extension.IsSameAs(wxT("avi"), false))||(extension.IsSameAs(wxT("wmv"), false))||(extension.IsSameAs(wxT("rv"), false))) {
         errorMessage.Printf(_("\"%s\" is a video file. \nAudacity cannot currently open this type of file. \nYou need to extract the audio to a supported format, such as WAV or AIFF."), fName.c_str());
         return 0;
      }

      // we were not able to recognize the file type
      errorMessage.Printf(_("Audacity did not recognize the type of the file '%s'.\nIf it is uncompressed, try importing it using \"Import Raw\"."),fName.c_str());
   }
   else
   {
      // We DO have a plugin for this file, but import failed.
      wxString pluglist = wxEmptyString;

      importPluginNode = compatiblePlugins.GetFirst();
      while(importPluginNode)
      {
         ImportPlugin *plugin = importPluginNode->GetData();
         if (pluglist == wxEmptyString)
           pluglist = plugin->GetPluginFormatDescription();
         else
           pluglist = pluglist + wxT(", ") + plugin->GetPluginFormatDescription();
         importPluginNode = importPluginNode->GetNext();
      }

      errorMessage.Printf(_("Audacity recognized the type of the file '%s'.\nImporters supposedly supporting such files are:\n%s,\nbut none of them understood this file format."),fName.c_str(), pluglist.c_str());
   }

   return 0;
}

//-------------------------------------------------------------------------
// ImportStreamDialog
//-------------------------------------------------------------------------

BEGIN_EVENT_TABLE( ImportStreamDialog,wxDialog )
   EVT_BUTTON( wxID_OK, ImportStreamDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, ImportStreamDialog::OnCancel )
END_EVENT_TABLE()

ImportStreamDialog::ImportStreamDialog( ImportFileHandle *_mFile, wxWindow *parent, wxWindowID id, const wxString &title,
                                       const wxPoint &position, const wxSize& size, long style ):
wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER )
{
   mFile = _mFile;
   scount = mFile->GetStreamCount();
   for (wxInt32 i = 0; i < scount; i++)
      mFile->SetStreamUsage(i,FALSE);

   wxBoxSizer *vertSizer = new wxBoxSizer( wxVERTICAL );
   wxArrayString *choices = mFile->GetStreamInfo();
   StreamList = new wxListBox(this, -1, wxDefaultPosition, wxDefaultSize, *choices , wxLB_EXTENDED | wxLB_ALWAYS_SB);

   vertSizer->Add( StreamList, 1, wxEXPAND | wxALIGN_LEFT | wxALL, 5 );

   vertSizer->Add( CreateStdButtonSizer(this, eCancelButton|eOkButton), 0, wxEXPAND );

   SetAutoLayout( true );

   SetSizer( vertSizer );

   vertSizer->Fit( this );

   SetSize( 400, 200 );
}

ImportStreamDialog::~ImportStreamDialog()
{

}

void ImportStreamDialog::OnOk(wxCommandEvent &event)
{
   wxArrayInt selitems;
   int sels = StreamList->GetSelections(selitems);
   for (wxInt32 i = 0; i < sels; i++)
      mFile->SetStreamUsage(selitems[i],TRUE);
   EndModal( wxID_OK );
}

void ImportStreamDialog::OnCancel(wxCommandEvent &event)
{
   EndModal( wxID_CANCEL );
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 702e6bd3-b26c-424f-9d6a-c88b565ea143

