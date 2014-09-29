/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPlugin.h

  Joshua Haberman
  Leland Lucius

*******************************************************************//**

\file ImportPlugin.h
\brief
  The interface that all file import "plugins" (if you want to call
  them that) must implement.  Defines ImportFileHandle, ImportPlugin,
  UnusableImportPlugin, ImportPluginList and UnusableImportPluginList.

  Since this is part of libaudacity, it must not use any GUI parts
  of wxWidgets.

*//****************************************************************//**

\class ImportFileHandle
\brief Base class for FlacImportFileHandle, LOFImportFileHandle,
MP3ImportFileHandle, OggImportFileHandle and PCMImportFileHandle.
Gives API for sound file import.

*//****************************************************************//**

\class ImportPlugin
\brief Base class for FlacImportPlugin, LOFImportPlugin,
MP3ImportPlugin, OggImportPlugin and PCMImportPlugin.
Gives API for sound file import.

*//****************************************************************//**

\class UnusableImportPlugin
\brief Used in place of a real plug in for plug ins that have not
been compiled or are not available in this version of Audacity.  Has
enough information to identify the file extensions that would be used,
but little else.

*//****************************************************************//**

\class ImportPluginList
\brief An ImportPlugin list.

*//****************************************************************//**

\class UnusableImportPluginList
\brief An UnusableImportPlugin list.

*//*******************************************************************/

#ifndef __AUDACITY_IMPORTER__
#define __AUDACITY_IMPORTER__

#include <wx/arrstr.h>
#include <wx/filename.h>
#include <wx/string.h>
#include <wx/list.h>

#include "../widgets/ProgressDialog.h"

class TrackFactory;
class Track;
class Tags;

class ImportFileHandle;

class ImportPlugin
{
public:

   // Get unique string ID of this plugin, usually it corresponds
   // to the underlying library, i.e. "libsndfile", "libflac", "libav"
   // These MUST NOT change across Audacity versions (but new IDs can
   // be added).
   virtual wxString GetPluginStringID() = 0;

   // Get a description of the file type this importer can import.
   // Examples: "Ogg Vorbis", "MP3", "Uncompressed PCM"
   virtual wxString GetPluginFormatDescription() = 0;

   // Get a list of extensions this plugin expects to be able to
   // import.  If a filename matches any of these extensions,
   // this importer will get first dibs on importing it.
   virtual wxArrayString GetSupportedExtensions()
   {
      return mExtensions;
   }

   bool SupportsExtension(wxString extension)
   {
      // Case-insensitive check if extension is supported
      return mExtensions.Index(extension, false) != wxNOT_FOUND;
   }

   // Open the given file, returning true if it is in a recognized
   // format, false otherwise.  This puts the importer into the open
   // state.
   virtual ImportFileHandle *Open(wxString Filename) = 0;

   virtual ~ImportPlugin() { }

protected:

   ImportPlugin(wxArrayString supportedExtensions):
      mExtensions(supportedExtensions)
   {
   }

   wxArrayString mExtensions;
};


class ImportFileHandle
{
public:
   ImportFileHandle(const wxString & filename)
   :  mFilename(filename),
   mProgress(NULL)
   {
   }

   virtual ~ImportFileHandle()
   {
      if (mProgress != NULL)
      {
         delete mProgress;
         mProgress = NULL;
      }
   }

   // The importer should call this to create the progress dialog and
   // identify the filename being imported.
   void CreateProgress()
   {
      wxFileName f(mFilename);
      wxString title;

      title.Printf(_("Importing %s"), GetFileDescription().c_str());
      mProgress = new ProgressDialog(title,
                                     f.GetFullName());
   }

   // This is similar to GetImporterDescription, but if possible the
   // importer will return a more specific description of the
   // specific file that is open.
   virtual wxString GetFileDescription() = 0;

   // Return an estimate of how many bytes the file will occupy once
   // imported
   virtual int GetFileUncompressedBytes() = 0;

   // do the actual import, creating whatever tracks are necessary with
   // the TrackFactory and calling the progress callback every iteration
   // through the importing loop
   virtual int Import(TrackFactory *trackFactory, Track ***outTracks,
                      int *outNumTracks, Tags *tags) = 0;

   // Return number of elements in stream list
   virtual wxInt32 GetStreamCount() = 0;

   // Return stream descriptions list
   virtual wxArrayString *GetStreamInfo() = 0;

   // Set stream "import/don't import" flag
   virtual void SetStreamUsage(wxInt32 StreamID, bool Use) = 0;

protected:
   wxString mFilename;
   ProgressDialog *mProgress;
};



class UnusableImportPlugin
{
public:
   UnusableImportPlugin(wxString formatName, wxArrayString extensions):
      mFormatName(formatName),
      mExtensions(extensions)
   {
   }

   wxString GetPluginFormatDescription()
   {
      return mFormatName;
   }

   bool SupportsExtension(wxString extension)
   {
      return mExtensions.Index(extension, false) != wxNOT_FOUND;
   }

private:
   wxString mFormatName;
   wxArrayString mExtensions;
};

WX_DECLARE_LIST(ImportPlugin, ImportPluginList);
WX_DECLARE_LIST(UnusableImportPlugin, UnusableImportPluginList);

#endif
