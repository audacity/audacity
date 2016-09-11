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



#include "../Audacity.h"
#include "Import.h"

#include <algorithm>
#include "ImportPlugin.h"

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/sizer.h>         //for wxBoxSizer
#include <wx/arrimpl.cpp>
#include <wx/listimpl.cpp>
#include "../ShuttleGui.h"
#include "../Project.h"

#include "ImportPCM.h"
#include "ImportMP3.h"
#include "ImportOGG.h"
#include "ImportQT.h"
#include "ImportRaw.h"
#include "ImportLOF.h"
#include "ImportFLAC.h"
#include "ImportFFmpeg.h"
#include "ImportGStreamer.h"
#include "../Prefs.h"

// ============================================================================
//
// Return reference to singleton
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================
Importer Importer::mInstance;
Importer & Importer::Get()
{
   return mInstance;
}

Importer::Importer()
{
}

Importer::~Importer()
{
}

bool Importer::Initialize()
{
   ImportPluginList{}.swap(mImportPluginList);
   UnusableImportPluginList{}.swap(mUnusableImportPluginList);
   ExtImportItems{}.swap(mExtImportItems);

   // build the list of import plugin and/or unusableImporters.
   // order is significant.  If none match, they will all be tried
   // in the order defined here.
   GetPCMImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetOGGImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetFLACImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetMP3ImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetLOFImportPlugin(mImportPluginList, mUnusableImportPluginList);

   #if defined(USE_FFMPEG)
   GetFFmpegImportPlugin(mImportPluginList, mUnusableImportPluginList);
   #endif
   #ifdef USE_QUICKTIME
   GetQTImportPlugin(mImportPluginList, mUnusableImportPluginList);
   #endif
   #if defined(USE_GSTREAMER)
   GetGStreamerImportPlugin(mImportPluginList, mUnusableImportPluginList);
   #endif

   ReadImportItems();

   return true;
}

bool Importer::Terminate()
{
   WriteImportItems();
   ImportPluginList{}.swap( mImportPluginList );
   UnusableImportPluginList{}.swap( mUnusableImportPluginList );

   return true;
}

void Importer::GetSupportedImportFormats(FormatList *formatList)
{
   for(const auto &importPlugin : mImportPluginList)
   {
#ifdef __AUDACITY_OLD_STD__
      formatList->push_back(Format{importPlugin->GetPluginFormatDescription(),
                               importPlugin->GetSupportedExtensions()});
#else
      formatList->emplace_back(importPlugin->GetPluginFormatDescription(),
                               importPlugin->GetSupportedExtensions());
#endif
   }
}

void Importer::StringToList(wxString &str, wxString &delims, wxArrayString &list, wxStringTokenizerMode mod)
{
   wxStringTokenizer toker;

   for (toker.SetString(str, delims, mod);
      toker.HasMoreTokens(); list.Add (toker.GetNextToken()));
}

void Importer::ReadImportItems()
{
   int item_counter = 0;
   wxStringTokenizer toker;
   wxString item_name;
   wxString item_value;

   ExtImportItems{}.swap(mExtImportItems);
   /* Rule string format is:
    * extension1:extension2:extension3\mime_type1:mime_type2:mime_type3|filter1:filter2:filter3\unusedfilter1:unusedfilter2
    * backslashes are escaped and unescaped internally
    */
   for (item_counter = 0; true; item_counter++)
   {
      wxString condition, filters, used_filters, unused_filters = wxEmptyString, extensions, mime_types = wxEmptyString;
      item_name.Printf (wxT("/ExtImportItems/Item%d"), item_counter);
      /* Break at first non-existent item */
      if (!gPrefs->Read(item_name, &item_value))
        break;

      toker.SetString(item_value, wxT("|"), wxTOKEN_RET_EMPTY_ALL);
      /* Break at first broken item */
      if (toker.CountTokens() != 2)
        break;

      auto new_item = make_movable<ExtImportItem>();

      /* First token is the filtering condition, second - the filter list */
      condition = toker.GetNextToken();
      filters = toker.GetNextToken();

      /* Condition token consists of extension list and mime type list
       * mime type list can be omitted entirely (complete with '\' separator)*/
      toker.SetString(condition, wxT("\\"), wxTOKEN_RET_EMPTY_ALL);
      extensions = toker.GetNextToken();
      if (toker.HasMoreTokens())
        mime_types = toker.GetNextToken();

      wxString delims(wxT(":"));
      StringToList (extensions, delims, new_item->extensions);

      if (mime_types != wxEmptyString)
         StringToList (mime_types, delims, new_item->mime_types);

      /* Filter token consists of used and unused filter lists */
      toker.SetString(filters, wxT("\\"), wxTOKEN_RET_EMPTY_ALL);
      used_filters = toker.GetNextToken();
      if (toker.HasMoreTokens())
        unused_filters = toker.GetNextToken();

      StringToList (used_filters, delims, new_item->filters);

      if (unused_filters != wxEmptyString)
      {
         /* Filters are stored in one list, but the position at which
          * unused filters start is remembered
          */
         new_item->divider = new_item->filters.Count();
         StringToList (unused_filters, delims, new_item->filters);
      }
      else
         new_item->divider = -1;

      /* Find corresponding filter object for each filter ID */
      for (size_t i = 0; i < new_item->filters.Count(); i++)
      {
         bool found = false;
         for (const auto &importPlugin : mImportPluginList)
         {
            if (importPlugin->GetPluginStringID().Cmp(new_item->filters[i]) == 0)
            {
               new_item->filter_objects.Add (importPlugin.get());
               found = true;
               break;
            }
         }
         /* IDs that do not have corresponding filters, will be shown as-is */
         if (!found)
           new_item->filter_objects.Add (NULL);
      }
      /* Find all filter objects that are not present in the filter list */
      for (const auto &importPlugin : mImportPluginList)
      {
         bool found = false;
         for (size_t i = 0; i < new_item->filter_objects.Count(); i++)
         {
            if (importPlugin.get() == new_item->filter_objects[i])
            {
               found = true;
               break;
            }
         }
         /* Add these filters at the bottom of used filter list */
         if (!found)
         {
            int index = new_item->divider;
            if (new_item->divider < 0)
               index = new_item->filters.Count();
            new_item->filters.Insert(importPlugin->GetPluginStringID(),index);
            new_item->filter_objects.Insert (importPlugin.get(), index);
            if (new_item->divider >= 0)
               new_item->divider++;
         }
      }
      this->mExtImportItems.push_back( std::move(new_item) );
   }
}

void Importer::WriteImportItems()
{
   size_t i;
   wxString val, name;
   for (i = 0; i < this->mExtImportItems.size(); i++)
   {
      ExtImportItem *item = mExtImportItems[i].get();
      val.Clear();

      for (size_t j = 0; j < item->extensions.Count(); j++)
      {
         val.Append (item->extensions[j]);
         if (j < item->extensions.Count() - 1)
            val.Append (wxT(":"));
      }
      val.Append (wxT("\\"));
      for (size_t j = 0; j < item->mime_types.Count(); j++)
      {
         val.Append (item->mime_types[j]);
         if (j < item->mime_types.Count() - 1)
            val.Append (wxT(":"));
      }
      val.Append (wxT("|"));
      for (size_t j = 0; j < item->filters.Count() && ((int) j < item->divider || item->divider < 0); j++)
      {
         val.Append (item->filters[j]);
         if (j < item->filters.Count() - 1 && ((int) j < item->divider - 1 || item->divider < 0))
            val.Append (wxT(":"));
      }
      if (item->divider >= 0)
      {
         val.Append (wxT("\\"));
         for (size_t j = item->divider; j < item->filters.Count(); j++)
         {
            val.Append (item->filters[j]);
            if (j < item->filters.Count() - 1)
               val.Append (wxT(":"));
         }
      }
      name.Printf (wxT("/ExtImportItems/Item%d"), (int)i);
      gPrefs->Write (name, val);
      gPrefs->Flush();
   }
   /* If we used to have more items than we have now, DELETE the excess items.
   We just keep deleting items and incrementing until we find there aren't any
   more to DELETE.*/
   i = this->mExtImportItems.size();
   do {
     name.Printf (wxT("/ExtImportItems/Item%d"), (int)i);
     // No item to DELETE?  Then it's time to finish.
     if (!gPrefs->Read(name, &val))
        break;
     // Failure to DELETE probably means a read-only config file.
     // no point continuing.
     // TODO: Possibly report (once).
     if( !gPrefs->DeleteEntry (name, false))
        break;
     i++;
   } while( true );
}

movable_ptr<ExtImportItem> Importer::CreateDefaultImportItem()
{
   auto new_item = make_movable<ExtImportItem>();
   new_item->extensions.Add(wxT("*"));
   new_item->mime_types.Add(wxT("*"));

   for (const auto &importPlugin : mImportPluginList)
   {
      new_item->filters.Add (importPlugin->GetPluginStringID());
      new_item->filter_objects.Add (importPlugin.get());
   }
   new_item->divider = -1;
   return new_item;
}

// returns number of tracks imported
bool Importer::Import(const wxString &fName,
                     TrackFactory *trackFactory,
                     TrackHolders &tracks,
                     Tags *tags,
                     wxString &errorMessage)
{
   AudacityProject *pProj = GetActiveProject();
   pProj->mbBusyImporting = true;

   wxString extension = fName.AfterLast(wxT('.'));

   using ImportPluginPtrs = std::vector< ImportPlugin* >;

   // This list is used to call plugins in correct order
   ImportPluginPtrs importPlugins;

   // This list is used to remember plugins that should have been compatible with the file.
   ImportPluginPtrs compatiblePlugins;

   // If user explicitly selected a filter,
   // then we should try importing via corresponding plugin first
   wxString type = gPrefs->Read(wxT("/LastOpenType"),wxT(""));

   // Not implemented (yet?)
   wxString mime_type = wxT("*");

   // First, add user-selected filter
   bool usersSelectionOverrides;
   gPrefs->Read(wxT("/ExtendedImport/OverrideExtendedImportByOpenFileDialogChoice"), &usersSelectionOverrides, false);

   wxLogDebug(wxT("LastOpenType is %s"),type.c_str());
   wxLogDebug(wxT("OverrideExtendedImportByOpenFileDialogChoice is %i"),usersSelectionOverrides);

   if (usersSelectionOverrides)
   {
      for (const auto &plugin : mImportPluginList)
      {
         if (plugin->GetPluginFormatDescription().CompareTo(type) == 0)
         {
            // This plugin corresponds to user-selected filter, try it first.
            wxLogDebug(wxT("Inserting %s"),plugin->GetPluginStringID().c_str());
            importPlugins.insert(importPlugins.begin(), plugin.get());
         }
      }
   }

   wxLogMessage(wxT("File name is %s"),(const char *) fName.c_str());
   wxLogMessage(wxT("Mime type is %s"),(const char *) mime_type.Lower().c_str());

   for (const auto &uItem : mExtImportItems)
   {
      ExtImportItem *item = uItem.get();
      bool matches_ext = false, matches_mime = false;
      wxLogDebug(wxT("Testing extensions"));
      for (size_t j = 0; j < item->extensions.Count(); j++)
      {
         wxLogDebug(wxT("%s"), (const char *) item->extensions[j].Lower().c_str());
         if (wxMatchWild (item->extensions[j].Lower(),fName.Lower(), false))
         {
            wxLogDebug(wxT("Match!"));
            matches_ext = true;
            break;
         }
      }
      if (item->extensions.Count() == 0)
      {
         wxLogDebug(wxT("Match! (empty list)"));
         matches_ext = true;
      }
      if (matches_ext)
         wxLogDebug(wxT("Testing mime types"));
      else
         wxLogDebug(wxT("Not testing mime types"));
      for (size_t j = 0; matches_ext && j < item->mime_types.Count(); j++)
      {
         if (wxMatchWild (item->mime_types[j].Lower(),mime_type.Lower(), false))
         {
            wxLogDebug(wxT("Match!"));
            matches_mime = true;
            break;
         }
      }
      if (item->mime_types.Count() == 0)
      {
         wxLogDebug(wxT("Match! (empty list)"));
         matches_mime = true;
      }
      if (matches_ext && matches_mime)
      {
         wxLogDebug(wxT("Complete match!"));
         for (size_t j = 0; j < item->filter_objects.Count() && (item->divider < 0 || (int) j < item->divider); j++)
         {
            // the filter_object can be NULL if a suitable importer was not found
            // this happens when we recompile with --without-ffmpeg and there
            // is still ffmpeg in prefs from previous --with-ffmpeg builds
            if (!(item->filter_objects[j]))
               continue;
            wxLogDebug(wxT("Inserting %s"),item->filter_objects[j]->GetPluginStringID().c_str());
            importPlugins.push_back(item->filter_objects[j]);
         }
      }
   }

   // Add all plugins that support the extension

   // Here we rely on the fact that the first plugin in mImportPluginList is libsndfile.
   // We want to save this for later insertion ahead of libmad, if libmad supports the extension.
   // The order of plugins in mImportPluginList is determined by the Importer constructor alone and
   // is not changed by user selection overrides or any other mechanism, but we include an assert
   // in case subsequent code revisions to the constructor should break this assumption that
   // libsndfile is first.
   ImportPlugin *libsndfilePlugin = mImportPluginList.begin()->get();
   wxASSERT(libsndfilePlugin->GetPluginStringID().IsSameAs(wxT("libsndfile")));

   for (const auto &plugin : mImportPluginList)
   {
      // Make sure its not already in the list
      if (importPlugins.end() ==
          std::find(importPlugins.begin(), importPlugins.end(), plugin.get()))
      {
         if (plugin->SupportsExtension(extension))
         {
            // If libmad is accidentally fed a wav file which has been incorrectly
            // given an .mp3 extension then it can choke on the contents and crash.
            // To avoid this, put libsndfile ahead of libmad in the lists created for
            // mp3 files, or for any of the extensions supported by libmad.
            // A genuine .mp3 file will first fail an attempted import with libsndfile
            // but then get processed as desired by libmad.
            // But a wav file which bears an incorrect .mp3 extension will be successfully
            // processed by libsndfile and thus avoid being submitted to libmad.
            if (plugin->GetPluginStringID().IsSameAs(wxT("libmad")))
            {
               // Make sure libsndfile is not already in the list
               if (importPlugins.end() ==
                   std::find(importPlugins.begin(), importPlugins.end(), libsndfilePlugin))
               {
                  wxLogDebug(wxT("Appending %s"),libsndfilePlugin->GetPluginStringID().c_str());
                  importPlugins.push_back(libsndfilePlugin);
               }
            }
            wxLogDebug(wxT("Appending %s"),plugin->GetPluginStringID().c_str());
            importPlugins.push_back(plugin.get());
         }
      }
   }

   // Add remaining plugins, except for libmad, which should not be used as a fallback for anything.
   // Otherwise, if FFmpeg (libav) has not been installed, libmad will still be there near the
   // end of the preference list importPlugins, where it will claim success importing FFmpeg file
   // formats unsuitable for it, and produce distorted results.
   for (const auto &plugin : mImportPluginList)
   {
      if (!(plugin->GetPluginStringID().IsSameAs(wxT("libmad"))))
      {
         // Make sure its not already in the list
         if (importPlugins.end() ==
             std::find(importPlugins.begin(), importPlugins.end(), plugin.get()))
         {
            wxLogDebug(wxT("Appending %s"),plugin->GetPluginStringID().c_str());
            importPlugins.push_back(plugin.get());
         }
      }
   }

   // Try the import plugins, in the permuted sequences just determined
   for (const auto plugin : importPlugins)
   {
      // Try to open the file with this plugin (probe it)
      wxLogMessage(wxT("Opening with %s"),plugin->GetPluginStringID().c_str());
      auto inFile = plugin->Open(fName);
      if ( (inFile != NULL) && (inFile->GetStreamCount() > 0) )
      {
         wxLogMessage(wxT("Open(%s) succeeded"),(const char *) fName.c_str());
         // File has more than one stream - display stream selector
         if (inFile->GetStreamCount() > 1)
         {
            ImportStreamDialog ImportDlg(inFile.get(), NULL, -1, _("Select stream(s) to import"));

            if (ImportDlg.ShowModal() == wxID_CANCEL)
            {
               pProj->mbBusyImporting = false;
               return false;
            }
         }
         // One stream - import it by default
         else
            inFile->SetStreamUsage(0,TRUE);

         int res;

         res = inFile->Import(trackFactory, tracks, tags);

         if (res == eProgressSuccess || res == eProgressStopped)
         {
            // LOF ("list-of-files") has different semantics
            if (extension.IsSameAs(wxT("lof"), false))
            {
               pProj->mbBusyImporting = false;
               return true;
            }

            if (tracks.size() > 0)
            {
               // success!
               pProj->mbBusyImporting = false;
               return true;
            }
         }

         if (res == eProgressCancelled || res == eProgressFailed)
         {
            pProj->mbBusyImporting = false;
            return false;
         }

         // We could exit here since we had a match on the file extension,
         // but there may be another plug-in that can import the file and
         // that may recognize the extension, so we allow the loop to
         // continue.
      }
   }
   wxLogError(wxT("Importer::Import: Opening failed."));

   // None of our plugins can handle this file.  It might be that
   // Audacity supports this format, but support was not compiled in.
   // If so, notify the user of this fact
   for (const auto &unusableImportPlugin : mUnusableImportPluginList)
   {
      if( unusableImportPlugin->SupportsExtension(extension) )
      {
         errorMessage.Printf(_("This version of Audacity was not compiled with %s support."),
                             unusableImportPlugin->
                             GetPluginFormatDescription().c_str());
         pProj->mbBusyImporting = false;
         return false;
      }
   }

   /* warnings for unsupported data types */

#ifdef USE_MIDI
   // MIDI files must be imported, not opened
   if ((extension.IsSameAs(wxT("midi"), false))||(extension.IsSameAs(wxT("mid"), false))) {
      errorMessage.Printf(_("\"%s\" \nis a MIDI file, not an audio file. \nAudacity cannot open this type of file for playing, but you can\nedit it by clicking File > Import > MIDI."), fName.c_str());
      pProj->mbBusyImporting = false;
      return false;
   }
#endif

   if (compatiblePlugins.empty())
   {
      // if someone has sent us a .cda file, send them away
      if (extension.IsSameAs(wxT("cda"), false)) {
         /* i18n-hint: %s will be the filename */
         errorMessage.Printf(_("\"%s\" is an audio CD track. \nAudacity cannot open audio CDs directly. \nExtract (rip) the CD tracks to an audio format that \nAudacity can import, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // playlist type files
      if ((extension.IsSameAs(wxT("m3u"), false))||(extension.IsSameAs(wxT("ram"), false))||(extension.IsSameAs(wxT("pls"), false))) {
         errorMessage.Printf(_("\"%s\" is a playlist file. \nAudacity cannot open this file because it only contains links to other files. \nYou may be able to open it in a text editor and download the actual audio files."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }
      //WMA files of various forms
      if ((extension.IsSameAs(wxT("wma"), false))||(extension.IsSameAs(wxT("asf"), false))) {
         errorMessage.Printf(_("\"%s\" is a Windows Media Audio file. \nAudacity cannot open this type of file due to patent restrictions. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }
      //AAC files of various forms (probably not encrypted)
      if ((extension.IsSameAs(wxT("aac"), false))||(extension.IsSameAs(wxT("m4a"), false))||(extension.IsSameAs(wxT("m4r"), false))||(extension.IsSameAs(wxT("mp4"), false))) {
         errorMessage.Printf(_("\"%s\" is an Advanced Audio Coding file. \nAudacity cannot open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }
      // encrypted itunes files
      if ((extension.IsSameAs(wxT("m4p"), false))) {
         errorMessage.Printf(_("\"%s\" is an encrypted audio file. \nThese typically are from an online music store. \nAudacity cannot open this type of file due to the encryption. \nTry recording the file into Audacity, or burn it to audio CD then \nextract the CD track to a supported audio format such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }
      // Real Inc. files of various sorts
      if ((extension.IsSameAs(wxT("ra"), false))||(extension.IsSameAs(wxT("rm"), false))||(extension.IsSameAs(wxT("rpm"), false))) {
         errorMessage.Printf(_("\"%s\" is a RealPlayer media file. \nAudacity cannot open this proprietary format. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // Other notes-based formats
      if ((extension.IsSameAs(wxT("kar"), false))||(extension.IsSameAs(wxT("mod"), false))||(extension.IsSameAs(wxT("rmi"), false))) {
         errorMessage.Printf(_("\"%s\" is a notes-based file, not an audio file. \nAudacity cannot open this type of file. \nTry converting it to an audio file such as WAV or AIFF and \nthen import it, or record it into Audacity."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // MusePack files
      if ((extension.IsSameAs(wxT("mp+"), false))||(extension.IsSameAs(wxT("mpc"), false))||(extension.IsSameAs(wxT("mpp"), false))) {
         errorMessage.Printf(_("\"%s\" is a Musepack audio file. \nAudacity cannot open this type of file. \nIf you think it might be an mp3 file, rename it to end with \".mp3\" \nand try importing it again. Otherwise you need to convert it to a supported audio \nformat, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // WavPack files
      if ((extension.IsSameAs(wxT("wv"), false))||(extension.IsSameAs(wxT("wvc"), false))) {
         errorMessage.Printf(_("\"%s\" is a Wavpack audio file. \nAudacity cannot open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // AC3 files
      if ((extension.IsSameAs(wxT("ac3"), false))) {
         errorMessage.Printf(_("\"%s\" is a Dolby Digital audio file. \nAudacity cannot currently open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // Speex files
      if ((extension.IsSameAs(wxT("spx"), false))) {
         errorMessage.Printf(_("\"%s\" is an Ogg Speex audio file. \nAudacity cannot currently open this type of file. \nYou need to convert it to a supported audio format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // Video files of various forms
      if ((extension.IsSameAs(wxT("mpg"), false))||(extension.IsSameAs(wxT("mpeg"), false))||(extension.IsSameAs(wxT("avi"), false))||(extension.IsSameAs(wxT("wmv"), false))||(extension.IsSameAs(wxT("rv"), false))) {
         errorMessage.Printf(_("\"%s\" is a video file. \nAudacity cannot currently open this type of file. \nYou need to extract the audio to a supported format, such as WAV or AIFF."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // Audacity project
      if (extension.IsSameAs(wxT("aup"), false)) {
         errorMessage.Printf(_("\"%s\" is an Audacity Project file. \nUse the 'File > Open' command to open Audacity Projects."), fName.c_str());
         pProj->mbBusyImporting = false;
         return false;
      }

      // we were not able to recognize the file type
      errorMessage.Printf(_("Audacity did not recognize the type of the file '%s'.\nIf it is uncompressed, try importing it using \"Import Raw\"."),fName.c_str());
   }
   else
   {
      // We DO have a plugin for this file, but import failed.
      wxString pluglist = wxEmptyString;

      for (const auto &plugin : compatiblePlugins)
      {
         if (pluglist == wxEmptyString)
           pluglist = plugin->GetPluginFormatDescription();
         else
           pluglist = pluglist + wxT(", ") + plugin->GetPluginFormatDescription();
      }

      errorMessage.Printf(_("Audacity recognized the type of the file '%s'.\nImporters supposedly supporting such files are:\n%s,\nbut none of them understood this file format."),fName.c_str(), pluglist.c_str());
   }

   pProj->mbBusyImporting = false;
   return false;
}

//-------------------------------------------------------------------------
// ImportStreamDialog
//-------------------------------------------------------------------------

BEGIN_EVENT_TABLE( ImportStreamDialog, wxDialogWrapper )
   EVT_BUTTON( wxID_OK, ImportStreamDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, ImportStreamDialog::OnCancel )
END_EVENT_TABLE()

ImportStreamDialog::ImportStreamDialog( ImportFileHandle *_mFile, wxWindow *parent, wxWindowID id, const wxString &title,
                                       const wxPoint &position, const wxSize& size, long style ):
wxDialogWrapper( parent, id, title, position, size, style | wxRESIZE_BORDER )
{
   SetName(GetTitle());

   mFile = _mFile;
   scount = mFile->GetStreamCount();
   for (wxInt32 i = 0; i < scount; i++)
      mFile->SetStreamUsage(i, FALSE);

   wxBoxSizer *vertSizer;
   {
      auto uVertSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      vertSizer = uVertSizer.get();

      auto choices = mFile->GetStreamInfo();
      StreamList = safenew wxListBox(this, -1, wxDefaultPosition, wxDefaultSize, choices, wxLB_EXTENDED | wxLB_ALWAYS_SB);

      vertSizer->Add(StreamList, 1, wxEXPAND | wxALIGN_LEFT | wxALL, 5);

      vertSizer->Add(CreateStdButtonSizer(this, eCancelButton | eOkButton).release(), 0, wxEXPAND);

      SetAutoLayout(true);

      SetSizer(uVertSizer.release());
   }

   vertSizer->Fit( this );

   SetSize( 400, 200 );
}

ImportStreamDialog::~ImportStreamDialog()
{

}

void ImportStreamDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   wxArrayInt selitems;
   int sels = StreamList->GetSelections(selitems);
   for (wxInt32 i = 0; i < sels; i++)
      mFile->SetStreamUsage(selitems[i],TRUE);
   EndModal( wxID_OK );
}

void ImportStreamDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal( wxID_CANCEL );
}

