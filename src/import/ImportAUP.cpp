/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file ImportAUP.cpp
  @brief Upgrading project file formats from before version 3

*//****************************************************************//**

\class AUPImportFileHandle
\brief An ImportFileHandle for AUP files (pre-AUP3)

*//****************************************************************//**

\class AUPImportPlugin
\brief An ImportPlugin for AUP files (pre-AUP3)

*//*******************************************************************/



#include "Import.h"
#include "ImportPlugin.h"

#include "Envelope.h"
#include "FileFormats.h"
#include "FileNames.h"
#include "LabelTrack.h"
#if defined(USE_MIDI)
#include "NoteTrack.h"
#endif
#include "Prefs.h"
#include "Project.h"
#include "ProjectFileManager.h"
#include "ProjectHistory.h"
#include "ProjectSelectionManager.h"
#include "ProjectSettings.h"
#include "ProjectWindows.h"
#include "Sequence.h"
#include "Tags.h"
#include "TimeTrack.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "toolbars/SelectionBar.h"
#include "AudacityMessageBox.h"
#include "NumericTextCtrl.h"
#include "ProgressDialog.h"
#include "XMLFileReader.h"
#include "wxFileNameWrapper.h"

#include <map>

#define DESC XO("AUP project files (*.aup)")

static const auto exts = {wxT("aup")};

#include <wx/dir.h>
#include <wx/ffile.h>
#include <wx/file.h>
#include <wx/frame.h>
#include <wx/log.h>
#include <wx/string.h>

class AUPImportFileHandle;
using ImportHandle = std::unique_ptr<ImportFileHandle>;

using NewChannelGroup = std::vector<std::shared_ptr<WaveTrack>>;

class AUPImportPlugin final : public ImportPlugin
{
public:
   AUPImportPlugin();
   ~AUPImportPlugin();

   wxString GetPluginStringID() override;
   
   TranslatableString GetPluginFormatDescription() override;

   ImportHandle Open(const FilePath &fileName,
                     AudacityProject *project) override;
};

class AUPImportFileHandle final : public ImportFileHandle,
                                  public XMLTagHandler
{
public:
   AUPImportFileHandle(const FilePath &name,
                       AudacityProject *project);
   ~AUPImportFileHandle();

   TranslatableString GetFileDescription() override;

   ByteCount GetFileUncompressedBytes() override;

   ProgressResult Import(WaveTrackFactory *trackFactory,
                         TrackHolders &outTracks,
                         Tags *tags) override;

   wxInt32 GetStreamCount() override;

   const TranslatableStrings &GetStreamInfo() override;

   void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)) override;

   bool Open();

private:
   struct node
   {
      wxString parent;
      wxString tag;
      XMLTagHandler *handler;
   };
   using stack = std::vector<struct node>;

   bool HandleXMLTag(const std::string_view& tag, const AttributesList &attrs) override;
   void HandleXMLEndTag(const std::string_view& tag) override;
   XMLTagHandler *HandleXMLChild(const std::string_view& tag) override;

   bool HandleProject(XMLTagHandler *&handle);
   bool HandleLabelTrack(XMLTagHandler *&handle);
   bool HandleNoteTrack(XMLTagHandler *&handle);
   bool HandleTimeTrack(XMLTagHandler *&handle);
   bool HandleWaveTrack(XMLTagHandler *&handle);
   bool HandleTags(XMLTagHandler *&handle);
   bool HandleTag(XMLTagHandler *&handle);
   bool HandleLabel(XMLTagHandler *&handle);
   bool HandleWaveClip(XMLTagHandler *&handle);
   bool HandleSequence(XMLTagHandler *&handle);
   bool HandleWaveBlock(XMLTagHandler *&handle);
   bool HandleEnvelope(XMLTagHandler *&handle);
   bool HandleControlPoint(XMLTagHandler *&handle);
   bool HandleSimpleBlockFile(XMLTagHandler *&handle);
   bool HandleSilentBlockFile(XMLTagHandler *&handle);
   bool HandlePCMAliasBlockFile(XMLTagHandler *&handle);
   bool HandleImport(XMLTagHandler *&handle);

   // Called in first pass to collect information about blocks
   void AddFile(sampleCount len,
                sampleFormat format,
                const FilePath &blockFilename = wxEmptyString,
                const FilePath &audioFilename = wxEmptyString,
                sampleCount origin = 0,
                int channel = 0);

   // These two use the collected file information in a second pass
   bool AddSilence(sampleCount len);
   bool AddSamples(const FilePath &blockFilename,
                   const FilePath &audioFilename,
                   sampleCount len,
                   sampleFormat format,
                   sampleCount origin = 0,
                   int channel = 0);

   bool SetError(const TranslatableString &msg);
   bool SetWarning(const TranslatableString &msg);

private:
   AudacityProject &mProject;
   Tags *mTags;

   // project tag values that will be set in the actual project if the
   // import is successful
   #define field(n, t) bool have##n; t n
   struct
   {
      field(vpos, int);
      field(h, double);
      field(zoom, double);
      field(sel0, double);
      field(sel1, double);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      field(selLow, double) = SelectedRegion::UndefinedFrequency;
      field(selHigh, double) = SelectedRegion::UndefinedFrequency;
#endif
      field(rate, double);
      field(snapto, bool);
      field(selectionformat, wxString);
      field(audiotimeformat, wxString);
      field(frequencyformat, wxString);
      field(bandwidthformat, wxString);
   } mProjectAttrs;
   #undef field

   typedef struct
   {
      WaveTrack *track;
      WaveClip *clip;
      FilePath blockFile;
      FilePath audioFile;
      sampleCount len;
      sampleFormat format;
      sampleCount origin;
      int channel;
   } fileinfo;
   std::vector<fileinfo> mFiles;
   sampleCount mTotalSamples;

   sampleFormat mFormat;
   unsigned long mNumChannels;

   stack mHandlers;
   std::string mParentTag;
   std::string mCurrentTag;
   AttributesList mAttrs;

   wxFileName mProjDir;
   using BlockFileMap =
      std::map<wxString, std::pair<FilePath, std::shared_ptr<SampleBlock>>>;
   BlockFileMap mFileMap;

   WaveTrack *mWaveTrack;
   WaveClip *mClip;
   std::vector<WaveClip *> mClips;

   ProgressResult mUpdateResult;
   TranslatableString mErrorMsg;
};

namespace
{
// RHS is expected to be lowercase
bool CaseInsensitiveEquals(
   const std::string_view& lhs, const std::string_view& rhsLower)
{
   if (lhs.length() != rhsLower.length())
      return false;

   for (size_t i = 0; i < lhs.length(); ++i)
      if (std::tolower(lhs[i]) != rhsLower[i])
         return false;

   return true;
}
} // namespace

AUPImportPlugin::AUPImportPlugin()
:  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
{
   static_assert(
      sizeof(long long) >= sizeof(uint64_t) &&
      sizeof(long) >= sizeof(uint32_t),
      "Assumptions about sizes in XMLValueChecker calls are invalid!");
}

AUPImportPlugin::~AUPImportPlugin()
{
}

wxString AUPImportPlugin::GetPluginStringID()
{
   return wxT("legacyaup");
}
   
TranslatableString AUPImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportHandle AUPImportPlugin::Open(const FilePath &fileName,
                                   AudacityProject *project)
{
   auto handle = std::make_unique<AUPImportFileHandle>(fileName, project);

   if (!handle->Open())
   {
      // Error or not something that we recognize
      return nullptr;
   }

   return handle;
}

static Importer::RegisteredImportPlugin registered
{
   "AUP", std::make_unique<AUPImportPlugin>()
};

AUPImportFileHandle::AUPImportFileHandle(const FilePath &fileName,
                                         AudacityProject *project)
:  ImportFileHandle(fileName),
   mProject(*project)
{
}

AUPImportFileHandle::~AUPImportFileHandle()
{
}

TranslatableString AUPImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto AUPImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // TODO: Get Uncompressed byte count.
   return 0;
}

ProgressResult AUPImportFileHandle::Import(WaveTrackFactory *WXUNUSED(trackFactory),
                                           TrackHolders &WXUNUSED(outTracks),
                                           Tags *tags)
{
   auto &history = ProjectHistory::Get(mProject);
   auto &tracks = TrackList::Get(mProject);
   auto &viewInfo = ViewInfo::Get(mProject);
   auto &settings = ProjectSettings::Get(mProject);
   auto &selman = ProjectSelectionManager::Get(mProject);

   auto oldNumTracks = tracks.size();
   auto cleanup = finally([this, &tracks, oldNumTracks]{
      if (mUpdateResult != ProgressResult::Success) {
         // Revoke additions of tracks
         while (oldNumTracks < tracks.size()) {
            Track *lastTrack = *tracks.Any().rbegin();
            tracks.Remove(lastTrack);
         }
      }
   });

   bool isDirty = history.GetDirty() || !tracks.empty();

   mTotalSamples = 0;

   mTags = tags;

   CreateProgress();

   mUpdateResult = ProgressResult::Success;

   XMLFileReader xmlFile;

   bool success = xmlFile.Parse(this, mFilename);
   if (!success)
   {
      AudacityMessageBox(
         XO("Couldn't import the project:\n\n%s").Format(xmlFile.GetErrorStr()),
         XO("Import Project"),
         wxOK | wxCENTRE,
         &GetProjectFrame(mProject));

      return ProgressResult::Failed;
   }

   if (!mErrorMsg.empty())
   {
      // Error or warning
      AudacityMessageBox(
         mErrorMsg,
         XO("Import Project"),
         wxOK | wxCENTRE,
         &GetProjectFrame(mProject));

      if (mUpdateResult == ProgressResult::Failed)
      {
         // Error
         return ProgressResult::Failed;
      }
   }

   // If mUpdateResult had been changed, we would have returned already
   wxASSERT( mUpdateResult == ProgressResult::Success );

   sampleCount processed = 0;
   for (auto fi : mFiles)
   {
      mUpdateResult = mProgress->Update(processed.as_long_long(), mTotalSamples.as_long_long());
      if (mUpdateResult != ProgressResult::Success)
      {
         return mUpdateResult;
      }

      mClip = fi.clip;
      mWaveTrack = fi.track;

      if (fi.blockFile.empty())
      {
         AddSilence(fi.len);
      }
      else
      {
         AddSamples(fi.blockFile, fi.audioFile,
                    fi.len, fi.format, fi.origin, fi.channel);
      }

      processed += fi.len;
   }

   for (auto pClip : mClips)
      pClip->UpdateEnvelopeTrackLen();

   wxASSERT( mUpdateResult == ProgressResult::Success );

   // If the active project is "dirty", then bypass the below updates as we don't
   // want to going changing things the user may have already set up.
   if (isDirty)
   {
      return mUpdateResult;
   }

   if (mProjectAttrs.haverate)
   {
      auto &bar = SelectionBar::Get(mProject);
      bar.SetRate(mProjectAttrs.rate);
   }

   if (mProjectAttrs.havesnapto)
   {
      selman.AS_SetSnapTo(mProjectAttrs.snapto ? SNAP_NEAREST : SNAP_OFF);
   }

   if (mProjectAttrs.haveselectionformat)
   {
      selman.AS_SetSelectionFormat(NumericConverter::LookupFormat(NumericConverter::TIME, mProjectAttrs.selectionformat));
   }

   if (mProjectAttrs.haveaudiotimeformat)
   {
      selman.TT_SetAudioTimeFormat(NumericConverter::LookupFormat(NumericConverter::TIME, mProjectAttrs.audiotimeformat));
   }

   if (mProjectAttrs.havefrequencyformat)
   {
      selman.SSBL_SetFrequencySelectionFormatName(NumericConverter::LookupFormat(NumericConverter::TIME, mProjectAttrs.frequencyformat));
   }

   if (mProjectAttrs.havebandwidthformat)
   {
      selman.SSBL_SetBandwidthSelectionFormatName(NumericConverter::LookupFormat(NumericConverter::TIME, mProjectAttrs.bandwidthformat));
   }

   // PRL: It seems this must happen after SetSnapTo
   if (mProjectAttrs.havevpos)
   {
      viewInfo.vpos = mProjectAttrs.vpos;
   }

   if (mProjectAttrs.haveh)
   {
      viewInfo.h = mProjectAttrs.h;
   }

   if (mProjectAttrs.havezoom)
   {
      viewInfo.SetZoom(mProjectAttrs.zoom);
   }

   if (mProjectAttrs.havesel0)
   {
      viewInfo.selectedRegion.setT0(mProjectAttrs.sel0);
   }

   if (mProjectAttrs.havesel1)
   {
      viewInfo.selectedRegion.setT1(mProjectAttrs.sel1);
   }

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   if (mProjectAttrs.haveselLow)
   {
      viewInfo.selectedRegion.setF0(mProjectAttrs.selLow);
   }

   if (mProjectAttrs.haveselHigh)
   {
      viewInfo.selectedRegion.setF1(mProjectAttrs.selHigh);
   }
#endif

   return mUpdateResult;
}

wxInt32 AUPImportFileHandle::GetStreamCount()
{
   return 1;
}

const TranslatableStrings &AUPImportFileHandle::GetStreamInfo()
{
   static TranslatableStrings empty;
   return empty;
}

void AUPImportFileHandle::SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use))
{
}

bool AUPImportFileHandle::Open()
{
   wxFFile ff(mFilename, wxT("rb"));
   if (ff.IsOpened())
   {
      char buf[256];

      int numRead = ff.Read(buf, sizeof(buf));
      
      ff.Close();

      buf[sizeof(buf) - 1] = '\0';

      if (!wxStrncmp(buf, wxT("AudacityProject"), 15))
      {
         AudacityMessageBox(
            XO("This project was saved by Audacity version 1.0 or earlier. The format has\n"
               "changed and this version of Audacity is unable to import the project.\n\n"
               "Use a version of Audacity prior to v3.0.0 to upgrade the project and then\n"
               "you may import it with this version of Audacity."),
            XO("Import Project"),
            wxOK | wxCENTRE,
            &GetProjectFrame(mProject));

         return false;
      }

      if (wxStrncmp(buf, "<?xml", 5) == 0 &&
          (wxStrstr(buf, "<audacityproject") ||
           wxStrstr(buf, "<project") ))
      {
         return true;
      }
   }

   return false;
}

XMLTagHandler *AUPImportFileHandle::HandleXMLChild(const std::string_view& tag)
{
   return this;
}

void AUPImportFileHandle::HandleXMLEndTag(const std::string_view& tag)
{
   if (mUpdateResult != ProgressResult::Success)
   {
      return;
   }

   struct node node = mHandlers.back();

   if (tag == "waveclip")
   {
      mClip = nullptr;
   }

   if (node.handler)
   {
      node.handler->HandleXMLEndTag(tag);
   }
   
   mHandlers.pop_back();

   if (mHandlers.size())
   {
      node = mHandlers.back();
      mParentTag = node.parent;
      mCurrentTag = node.tag;
   }
}

bool AUPImportFileHandle::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (mUpdateResult != ProgressResult::Success)
   {
      return false;
   }

   mParentTag = mCurrentTag;
   mCurrentTag = std::string(tag);
   mAttrs = attrs;

   XMLTagHandler *handler = nullptr;
   bool success = false;

   if (mCurrentTag == "project" ||
       mCurrentTag == "audacityproject")
   {
      success = HandleProject(handler);
   }
   else if (mCurrentTag == "labeltrack")
   {
      success = HandleLabelTrack(handler);
   }
   else if (mCurrentTag == "notetrack")
   {
      success = HandleNoteTrack(handler);
   }
   else if (mCurrentTag == "timetrack")
   {
      success = HandleTimeTrack(handler);
   }
   else if (mCurrentTag == "wavetrack")
   {
      success = HandleWaveTrack(handler);
   }
   else if (mCurrentTag == "tags")
   {
      success = HandleTags(handler);
   }
   else if (mCurrentTag == "tag")
   {
      success = HandleTag(handler);
   }
   else if (mCurrentTag == "label")
   {
      success = HandleLabel(handler);
   }
   else if (mCurrentTag == "waveclip")
   {
      success = HandleWaveClip(handler);
   }
   else if (mCurrentTag == "sequence")
   {
      success = HandleSequence(handler);
   }
   else if (mCurrentTag == "waveblock")
   {
      success = HandleWaveBlock(handler);
   }
   else if (mCurrentTag == "envelope")
   {
      success = HandleEnvelope(handler);
   }
   else if (mCurrentTag == "controlpoint")
   {
      success = HandleControlPoint(handler);
   }
   else if (mCurrentTag == "simpleblockfile")
   {
      success = HandleSimpleBlockFile(handler);
   }
   else if (mCurrentTag == "silentblockfile")
   {
      success = HandleSilentBlockFile(handler);
   }
   else if (mCurrentTag == "pcmaliasblockfile")
   {
      success = HandlePCMAliasBlockFile(handler);
   }
   else if (mCurrentTag == "import")
   {
      success = HandleImport(handler);
   }

   if (!success || (handler && !handler->HandleXMLTag(tag, attrs)))
   {
      return SetError(XO("Internal error in importer...tag not recognized"));
   }

   mHandlers.push_back({mParentTag, mCurrentTag, handler});

   return true;
}

bool AUPImportFileHandle::HandleProject(XMLTagHandler *&handler)
{
   auto &fileMan = ProjectFileManager::Get(mProject);
   auto &window = GetProjectFrame(mProject);

   int requiredTags = 0;

   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      double dValue;

#define set(f, v) (mProjectAttrs.have ## f = true, mProjectAttrs.f = v)

      // ViewInfo
      if (attr == "vpos")
      {
         long lValue;
         if (!value.TryGet(lValue) || (lValue < 0))
         {
            return SetError(XO("Invalid project 'vpos' attribute."));
         }

         set(vpos, (int) lValue);
      }
      else if (attr == "h")
      {
         if (!value.TryGet(dValue))
         {
            return SetError(XO("Invalid project 'h' attribute."));
         }

         set(h, dValue);
      }
      else if (attr == "zoom")
      {
         if (!value.TryGet(dValue) || (dValue < 0.0))
         {
            return SetError(XO("Invalid project 'zoom' attribute."));
         }

         set(zoom, dValue);
      }
      // Viewinfo.SelectedRegion
      else if (attr == "sel0")
      {
         if (!value.TryGet(dValue))
         {
            return SetError(XO("Invalid project 'sel0' attribute."));
         }

         set(sel0, dValue);
      }
      else if (attr == "sel1")
      {
         if (!value.TryGet(dValue))
         {
            return SetError(XO("Invalid project 'sel1' attribute."));
         }

         set(sel1, dValue);
      }
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
      else if (attr == "selLow")
      {
         if (!value.TryGet(dValue) || (dValue < 0.0))
         {
            return SetError(XO("Invalid project 'selLow' attribute."));
         }

         set(selLow, dValue);
      }
      else if (attr == "selHigh")
      {
         if (!value.TryGet(dValue) || (dValue < 0.0))
         {
            return SetError(XO("Invalid project 'selHigh' attribute."));
         }

         set(selHigh, dValue);
      }
#endif
      else if (attr == "version")
      {
         requiredTags++;
      }

      else if (attr == "audacityversion")
      {
         requiredTags++;
      }
      else if (attr == "projname")
      {
         requiredTags++;

         mProjDir = mFilename;
         wxString altname = mProjDir.GetName() + wxT("_data");
         mProjDir.SetFullName(wxEmptyString);

         wxString projName = value.ToWString();
         bool found = false;

         // First try to load the data files based on the _data dir given in the .aup file
         if (!projName.empty())
         {
            mProjDir.AppendDir(projName);
            if (!mProjDir.DirExists())
            {
               mProjDir.RemoveLastDir();
               projName.clear();
            }
         }

         // If that fails then try to use the filename of the .aup as the base directory
         // This is because unzipped projects e.g. those that get transferred between mac-pc
         // may have encoding issues and end up expanding the wrong filenames for certain
         // international characters (such as capital 'A' with an umlaut.)
         if (projName.empty())
         {
            projName = altname;
            mProjDir.AppendDir(projName);
            if (!mProjDir.DirExists())
            {
               projName.clear();
            }
         }

         // No luck...complain and bail
         if (projName.empty())
         {
            AudacityMessageBox(
               XO("Couldn't find the project data folder: \"%s\"").Format(value.ToWString()),
               XO("Error Opening Project"),
               wxOK | wxCENTRE,
               &window);

            return false;
         }

         // Collect and hash the file names within the project directory
         wxArrayString files;
         size_t cnt = wxDir::GetAllFiles(mProjDir.GetFullPath(),
                                         &files,
                                         "*.*");

         for (const auto &fn : files)
         {
            mFileMap[wxFileNameFromPath(fn)] = {fn, {}};
         }
      }
      else if (attr == "rate")
      {
         if (!value.TryGet(dValue) || (dValue < 0.0))
         {
            return SetError(XO("Invalid project 'selLow' attribute."));
         }

         set(rate, dValue);
      }

      else if (attr == "snapto")
      {
         set(snapto, (value.ToWString() == "on" ? true : false));
      }

      else if (attr == "selectionformat")
      {
         set(selectionformat, value.ToWString());
      }

      else if (attr == "frequencyformat")
      {
         set(frequencyformat, value.ToWString());
      }

      else if (attr == "bandwidthformat")
      {
         set(bandwidthformat, value.ToWString());
      }
#undef set
   }

   if (requiredTags < 3)
   {
      return false;
   }

   // Do not set the handler - already handled

   return true;
}

bool AUPImportFileHandle::HandleLabelTrack(XMLTagHandler *&handler)
{
   handler = TrackList::Get(mProject).Add(std::make_shared<LabelTrack>());

   return true;
}

bool AUPImportFileHandle::HandleNoteTrack(XMLTagHandler *&handler)
{
#if defined(USE_MIDI)
   handler = TrackList::Get(mProject).Add(std::make_shared<NoteTrack>());

   return true;
#else
   AudacityMessageBox(
      XO("MIDI tracks found in project file, but this build of Audacity does not include MIDI support, bypassing track."),
      XO("Project Import"),
      wxOK | wxICON_EXCLAMATION | wxCENTRE,
      &GetProjectFrame(mProject));

   return false;
#endif
}

bool AUPImportFileHandle::HandleTimeTrack(XMLTagHandler *&handler)
{
   auto &tracks = TrackList::Get(mProject);

   // Bypass this timetrack if the project already has one
   // (See HandleTimeEnvelope and HandleControlPoint also)
   if (*tracks.Any<TimeTrack>().begin())
   {
      AudacityMessageBox(
         XO("The active project already has a time track and one was encountered in the project being imported, bypassing imported time track."),
         XO("Project Import"),
         wxOK | wxICON_EXCLAMATION | wxCENTRE,
         &GetProjectFrame(mProject));

      return true;
   }

   auto &viewInfo = ViewInfo::Get( mProject );
   handler =
      TrackList::Get(mProject).Add(std::make_shared<TimeTrack>(&viewInfo));

   return true;
}

bool AUPImportFileHandle::HandleWaveTrack(XMLTagHandler *&handler)
{
   auto &trackFactory = WaveTrackFactory::Get(mProject);
   handler = mWaveTrack =
      TrackList::Get(mProject).Add(trackFactory.Create());

   // No active clip.  In early versions of Audacity, there was a single
   // implied clip so we'll create a clip when the first "sequence" is
   // found.
   mClip = nullptr;

   return true;
}

bool AUPImportFileHandle::HandleTags(XMLTagHandler *&handler)
{
   wxString n;
   wxString v;

   // Support for legacy tags
   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      if (attr == "id3v2")
      {
         continue;
      }
      else if (attr == "track")
      {
         n = wxT("TRACKNUMBER");
      }
      else
      {
         n = std::string(attr);
         n.MakeUpper();
      }

      v = value.ToWString();

      if (!v.empty())
         mTags->SetTag(n, value.ToWString());
   }

   // Do not set the handler - already handled

   return true;
}

bool AUPImportFileHandle::HandleTag(XMLTagHandler *&handler)
{
   if (mParentTag != "tags")
   {
      return false;
   }

   wxString n, v;

   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      if (attr == "name")
      {
         n = value.ToWString();
      }
      else if (attr == "value")
      {
         v = value.ToWString();
      }
   }

   if (n == wxT("id3v2"))
   {
      // LLL:  This is obsolete, but it must be handled and ignored.
   }
   else
   {
      mTags->SetTag(n, v);
   }

   // Do not set the handler - already handled

   return true;
}

bool AUPImportFileHandle::HandleLabel(XMLTagHandler *&handler)
{
   if (mParentTag != "labeltrack")
   {
      return false;
   }

   // The parent handler also handles this tag
   handler = mHandlers.back().handler;

   return true;
}

bool AUPImportFileHandle::HandleWaveClip(XMLTagHandler *&handler)
{
   struct node node = mHandlers.back();

   if (mParentTag == "wavetrack")
   {
      WaveTrack *wavetrack = static_cast<WaveTrack *>(node.handler);

      handler = wavetrack->CreateClip();
   }
   else if (mParentTag == "waveclip")
   {
      // Nested wave clips are cut lines
      WaveClip *waveclip = static_cast<WaveClip *>(node.handler);

      handler = waveclip->HandleXMLChild(mCurrentTag);
   }

   mClip = static_cast<WaveClip *>(handler);
   mClips.push_back(mClip);

   return true;
}

bool AUPImportFileHandle::HandleEnvelope(XMLTagHandler *&handler)
{
   struct node node = mHandlers.back();

   if (mParentTag == "timetrack")
   {
      // If an imported timetrack was bypassed, then we want to bypass the
      // envelope as well.  (See HandleTimeTrack and HandleControlPoint)
      if (node.handler)
      {
         TimeTrack *timetrack = static_cast<TimeTrack *>(node.handler);

         handler = timetrack->GetEnvelope();
      }
   }
   // Earlier versions of Audacity had a single implied waveclip, so for
   // these versions, we get or create the only clip in the track.
   else if (mParentTag == "wavetrack")
   {
      handler = mWaveTrack->RightmostOrNewClip()->GetEnvelope();
   }
   // Nested wave clips are cut lines
   else if (mParentTag == "waveclip")
   {
      WaveClip *waveclip = static_cast<WaveClip *>(node.handler);

      handler = waveclip->GetEnvelope();
   }

   return true;
}

bool AUPImportFileHandle::HandleControlPoint(XMLTagHandler *&handler)
{
   struct node node = mHandlers.back();

   if (mParentTag == "envelope")
   {
      // If an imported timetrack was bypassed, then we want to bypass the
      // control points as well.  (See HandleTimeTrack and HandleEnvelope)
      if (node.handler)
      {
         Envelope *envelope = static_cast<Envelope *>(node.handler);

         handler = envelope->HandleXMLChild(mCurrentTag);
      }
   }

   return true;
}

bool AUPImportFileHandle::HandleSequence(XMLTagHandler *&handler)
{
   struct node node = mHandlers.back();

   WaveClip *waveclip = static_cast<WaveClip *>(node.handler);

   // Earlier versions of Audacity had a single implied waveclip, so for
   // these versions, we get or create the only clip in the track.
   if (mParentTag == "wavetrack")
   {
      XMLTagHandler *dummy;
      HandleWaveClip(dummy);
      waveclip = mClip;
   }

   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      if (attr == "maxsamples")
      {
         // This attribute is a sample count, so can be 64bit
         long long llvalue;
         if (!value.TryGet(llvalue) || (llvalue < 0))
         {
            return SetError(XO("Invalid sequence 'maxsamples' attribute."));
         }

         // Dominic, 12/10/2006:
         //    Let's check that maxsamples is >= 1024 and <= 64 * 1024 * 1024
         //    - that's a pretty wide range of reasonable values.
         if ((llvalue < 1024) || (llvalue > 64 * 1024 * 1024))
         {
            return SetError(XO("Invalid sequence 'maxsamples' attribute."));
         }
      }
      else if (attr == "sampleformat")
      {
         // This attribute is a sample format, normal int
         long fValue;
         if (!value.TryGet(fValue) || (fValue < 0) || !Sequence::IsValidSampleFormat(fValue))
         {
            return SetError(XO("Invalid sequence 'sampleformat' attribute."));
         }

         mFormat = (sampleFormat) fValue;
         waveclip->GetSequence()->ConvertToSampleFormat( mFormat );
      }
      else if (attr == "numsamples")
      {
         // This attribute is a sample count, so can be 64bit
         long long llvalue;
         if (!value.TryGet(llvalue) || (llvalue < 0))
         {
            return SetError(XO("Invalid sequence 'numsamples' attribute."));
         }
      }
   }

   // Do not set the handler - already handled

   return true;
}

bool AUPImportFileHandle::HandleWaveBlock(XMLTagHandler *&handler)
{
   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      if (attr == "start")
      {
         // making sure that values > 2^31 are OK because long clips will need them.
         long long llvalue;
         if (!value.TryGet(llvalue) || (llvalue < 0))
         {
            return SetError(XO("Unable to parse the waveblock 'start' attribute"));
         }
      }
   }

   // Do not set the handler - already handled

   return true;
}

bool AUPImportFileHandle::HandleSimpleBlockFile(XMLTagHandler *&handler)
{
   FilePath filename;
   size_t len = 0;

   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      // Can't use XMLValueChecker::IsGoodFileName here, but do part of its test.
      if (CaseInsensitiveEquals(attr, "filename"))
      {
         const wxString strValue = value.ToWString();

         if (XMLValueChecker::IsGoodFileString(strValue))
         {
            if (mFileMap.find(strValue) != mFileMap.end())
            {
               filename = mFileMap[strValue].first;
            }
            else
            {
               SetWarning(XO("Missing project file %s\n\nInserting silence instead.")
                  .Format(strValue));
            }
         }
      }
      else if (attr == "len")
      {
         long lValue;
         if (!value.TryGet(lValue) || (lValue <= 0))
         {
            return SetError(XO("Missing or invalid simpleblockfile 'len' attribute."));
         }

         len = lValue;
      }
   }

   // Do not set the handler - already handled

   AddFile(len, mFormat, filename, filename);

   return true;
}

bool AUPImportFileHandle::HandleSilentBlockFile(XMLTagHandler *&handler)
{
   FilePath filename;
   size_t len = 0;

   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      if (attr == "len")
      {
         long lValue;
         if (!value.TryGet(lValue) || !(lValue > 0))
         {
            return SetError(XO("Missing or invalid silentblockfile 'len' attribute."));
         }

         len = lValue;
      }
   }

   // Do not set the handler - already handled

   AddFile(len, mFormat);

   return true;
}

bool AUPImportFileHandle::HandlePCMAliasBlockFile(XMLTagHandler *&handler)
{
   wxString summaryFilename;
   wxFileName filename;
   sampleCount start = 0;
   size_t len = 0;
   int channel = 0;
   wxString name;

   for (auto pair : mAttrs)
   {
      auto attr = pair.first;
      auto value = pair.second;

      if (CaseInsensitiveEquals(attr, "aliasfile"))
      {
         const wxString strValue = value.ToWString();

         if (XMLValueChecker::IsGoodPathName(strValue))
         {
            filename.Assign(strValue);
         }
         else if (XMLValueChecker::IsGoodFileName(strValue, mProjDir.GetPath()))
         {
            // Allow fallback of looking for the file name, located in the data directory.
            filename.Assign(mProjDir.GetPath(), strValue);
         }
         else if (XMLValueChecker::IsGoodPathString(strValue))
         {
            // If the aliased file is missing, we failed XMLValueChecker::IsGoodPathName()
            // and XMLValueChecker::IsGoodFileName, because both do existence tests.
            SetWarning(XO("Missing alias file %s\n\nInserting silence instead.")
               .Format(strValue));
         }
      }
      else if (CaseInsensitiveEquals(attr, "summaryfile"))
      {
         summaryFilename = value.ToWString();
      }
      else if (CaseInsensitiveEquals(attr, "aliasstart"))
      {
         long long llValue;
         if (!value.TryGet(llValue) || (llValue < 0))
         {
            return SetError(XO("Missing or invalid pcmaliasblockfile 'aliasstart' attribute."));
         }

         start = llValue;
      }
      else if (CaseInsensitiveEquals(attr, "aliaslen"))
      {
         long lValue;
         if (!value.TryGet(lValue) || (lValue <= 0))
         {
            return SetError(XO("Missing or invalid pcmaliasblockfile 'aliaslen' attribute."));
         }

         len = lValue;
      }
      else if (CaseInsensitiveEquals(attr, "aliaschannel"))
      {
         long lValue;
         if (!value.TryGet(lValue) || (lValue < 0))
         {
            return SetError(XO("Missing or invalid pcmaliasblockfile 'aliaslen' attribute."));
         }

         channel = lValue;
      }
   }

   // Do not set the handler - already handled

   if (filename.IsOk())
      AddFile(len, mFormat,
              summaryFilename, filename.GetFullPath(),
              start, channel);
   else
      AddFile(len, mFormat); // will add silence instead

   return true;
}

bool AUPImportFileHandle::HandleImport(XMLTagHandler *&handler)
{
   // Adapted from ImportXMLTagHandler::HandleXMLTag as in version 2.4.2
   if (mAttrs.empty() || mAttrs.front().first != "filename")
      return false;

   wxString strAttr = mAttrs.front().second.ToWString();

   if (!XMLValueChecker::IsGoodPathName(strAttr))
   {
      // Maybe strAttr is just a fileName, not the full path. Try the project data directory.
      wxFileNameWrapper fileName0{ mFilename };
      fileName0.SetExt({});
      wxFileNameWrapper fileName{
         fileName0.GetFullPath() + wxT("_data"), strAttr };
      if (XMLValueChecker::IsGoodFileName(strAttr, fileName.GetPath(wxPATH_GET_VOLUME)))
         strAttr = fileName.GetFullPath();
      else
      {
         wxLogWarning(wxT("Could not import file: %s"), strAttr);
         return false;
      }
   }

   auto &tracks = TrackList::Get(mProject);
   auto oldNumTracks = tracks.size();
   Track *pLast = nullptr;
   if (oldNumTracks > 0)
      pLast = *tracks.Any().rbegin();

   // Guard this call so that C++ exceptions don't propagate through
   // the expat library
   GuardedCall(
      [&] {
         ProjectFileManager::Get( mProject ).Import(strAttr, false); },
      [&] (AudacityException*) {}
   );

   if (oldNumTracks == tracks.size())
      return false;

   // Handle other attributes, now that we have the tracks.
   // Apply them to all new wave tracks.
   bool bSuccess = true;

   auto range = tracks.Any();
   if (pLast) {
      range = range.StartingWith(pLast);
      ++range.first;
   }

   mAttrs.erase(mAttrs.begin());

   for (auto pTrack: range.Filter<WaveTrack>())
   {
      // Most of the "import" tag attributes are the same as for "wavetrack" tags,
      // so apply them via WaveTrack::HandleXMLTag().
      bSuccess = pTrack->HandleXMLTag("wavetrack", mAttrs);

      // "offset" tag is ignored in WaveTrack::HandleXMLTag except for legacy projects,
      // so handle it here.
      double dblValue;

      for (auto pair : mAttrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "offset" && value.TryGet(dblValue))
            pTrack->SetOffset(dblValue);
      }
   }
   return bSuccess;
}

void AUPImportFileHandle::AddFile(sampleCount len,
                                  sampleFormat format,
                                  const FilePath &blockFilename /* = wxEmptyString */,
                                  const FilePath &audioFilename /* = wxEmptyString */,
                                  sampleCount origin /* = 0 */,
                                  int channel /* = 0 */)
{
   fileinfo fi = {};
   fi.track = mWaveTrack;
   fi.clip = mClip;
   fi.blockFile = blockFilename;
   fi.audioFile = audioFilename;
   fi.len = len;
   fi.format = format,
   fi.origin = origin,
   fi.channel = channel;

   mFiles.push_back(fi);

   mTotalSamples += len;
}

bool AUPImportFileHandle::AddSilence(sampleCount len)
{
   wxASSERT(mClip || mWaveTrack);

   if (mClip)
   {
      mClip->InsertSilence(mClip->GetPlayEndTime(), mWaveTrack->LongSamplesToTime(len));
   }
   else if (mWaveTrack)
   {
      mWaveTrack->InsertSilence(mWaveTrack->GetEndTime(), mWaveTrack->LongSamplesToTime(len));
   }

   return true;
}

// All errors that occur here will simply insert silence and allow the
// import to continue.
bool AUPImportFileHandle::AddSamples(const FilePath &blockFilename,
                                     const FilePath &audioFilename,
                                     sampleCount len,
                                     sampleFormat format,
                                     sampleCount origin /* = 0 */,
                                     int channel /* = 0 */)
{
   auto pClip = mClip ? mClip : mWaveTrack->RightmostOrNewClip();
   auto &pBlock = mFileMap[wxFileNameFromPath(blockFilename)].second;
   if (pBlock) {
      // Replicate the sharing of blocks
      pClip->AppendSharedBlock( pBlock );
      return true;
   }

   // Third party library has its own type alias, check it before
   // adding origin + size_t
   static_assert(sizeof(sampleCount::type) <= sizeof(sf_count_t),
                 "Type sf_count_t is too narrow to hold a sampleCount");

   SF_INFO info;
   memset(&info, 0, sizeof(info));

   wxFile f; // will be closed when it goes out of scope
   SNDFILE *sf = nullptr;
   bool success = false;

#ifndef UNCAUGHT_EXCEPTIONS_UNAVAILABLE
   const auto uncaughtExceptionsCount = std::uncaught_exceptions();
#endif  
   
   auto cleanup = finally([&]
   {
      // Do this before any throwing might happen
      if (sf)
      {
         SFCall<int>(sf_close, sf);
      }

      if (!success)
      {
         SetWarning(XO("Error while processing %s\n\nInserting silence.").Format(audioFilename));

         // If we are unwinding for an exception, don't do another
         // potentially throwing operation
#ifdef UNCAUGHT_EXCEPTIONS_UNAVAILABLE
         if (!std::uncaught_exception())
#else
         if (uncaughtExceptionsCount == std::uncaught_exceptions())
#endif
            // If this does throw, let that propagate, don't guard the call
            AddSilence(len);
      }
   });

   if (!f.Open(audioFilename))
   {
      SetWarning(XO("Failed to open %s").Format(audioFilename));

      return true;
   }

   // Even though there is an sf_open() that takes a filename, use the one that
   // takes a file descriptor since wxWidgets can open a file with a Unicode name and
   // libsndfile can't (under Windows).
   sf = SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_READ, &info, FALSE);
   if (!sf)
   {
      SetWarning(XO("Failed to open %s").Format(audioFilename));

      return true;
   }

   if (origin > 0)
   {
      if (SFCall<sf_count_t>(sf_seek, sf, origin.as_long_long(), SEEK_SET) < 0)
      {
         SetWarning(XO("Failed to seek to position %lld in %s")
            .Format(origin.as_long_long(), audioFilename));

         return true;
      }
   }

   sf_count_t cnt = len.as_size_t();
   int channels = info.channels;

   wxASSERT(channels >= 1);
   wxASSERT(channel < channels);

   SampleBuffer buffer(cnt, format);
   samplePtr bufptr = buffer.ptr();

   size_t framesRead = 0;
   
   // These cases preserve the logic formerly in BlockFile.cpp,
   // which was deleted at commit 98d1468.
   if (channels == 1 && format == int16Sample && sf_subtype_is_integer(info.format))
   {
      // If both the src and dest formats are integer formats,
      // read integers directly from the file, conversions not needed
      framesRead = SFCall<sf_count_t>(sf_readf_short, sf, (short *) bufptr, cnt);
   }
   else if (channels == 1 && format == int24Sample && sf_subtype_is_integer(info.format))
   {
      framesRead = SFCall<sf_count_t>(sf_readf_int, sf, (int *) bufptr, cnt);
      if (framesRead != cnt)
      {
         SetWarning(XO("Unable to read %lld samples from %s")
            .Format(cnt, audioFilename));

         return true;
      }

      // libsndfile gave us the 3 byte sample in the 3 most
      // significant bytes -- we want it in the 3 least
      // significant bytes.
      int *intPtr = (int *) bufptr;
      for (size_t i = 0; i < framesRead; i++)
      {
         intPtr[i] = intPtr[i] >> 8;
      }
   }
   else if (format == int16Sample && !sf_subtype_more_than_16_bits(info.format))
   {
      // Special case: if the file is in 16-bit (or less) format,
      // and the calling method wants 16-bit data, go ahead and
      // read 16-bit data directly.  This is a pretty common
      // case, as most audio files are 16-bit.
      SampleBuffer temp(cnt * channels, int16Sample);
      short *tmpptr = (short *) temp.ptr();

      framesRead = SFCall<sf_count_t>(sf_readf_short, sf, tmpptr, cnt);
      if (framesRead != cnt)
      {
         SetWarning(XO("Unable to read %lld samples from %s")
            .Format(cnt, audioFilename));

         return true;
      }

      for (size_t i = 0; i < framesRead; i++)
      {
         ((short *)bufptr)[i] = tmpptr[(channels * i) + channel];
      }
   }
   else
   {
      /*
       Therefore none of the three cases above:
      !(channels == 1 && format == int16Sample && sf_subtype_is_integer(info.format))
       &&
      !(channels == 1 && format == int24Sample && sf_subtype_is_integer(info.format))
       &&
      !(format == int16Sample && !sf_subtype_more_than_16_bits(info.format))

       So format is not 16 bits with wider file format (third conjunct),
       but still maybe it is 24 bits with float file format (second conjunct).
       */

      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      SampleBuffer tmpbuf(cnt * channels, floatSample);
      float *tmpptr = (float *) tmpbuf.ptr();

      framesRead = SFCall<sf_count_t>(sf_readf_float, sf, tmpptr, cnt);
      if (framesRead != cnt)
      {
         SetWarning(XO("Unable to read %lld samples from %s")
            .Format(cnt, audioFilename));

         return true;
      }

      /*
       Dithering will happen in CopySamples if format is 24 bits.
       Should that be done?

       Either the file is an ordinary simple block file -- and presumably the
       track was saved specifying a matching format, so format is float and
       there is no dithering.

       Or else this is the very unusual case of an .auf file, importing PCM data
       on demand.  The destination format is narrower, requiring dither, only
       if the user also specified a narrow format for the track.  In such a
       case, dithering is right.
       */
      CopySamples((samplePtr)(tmpptr + channel),
                  floatSample,
                  bufptr,
                  format,
                  framesRead,
                  gHighQualityDither /* high quality by default */,
                  channels /* source stride */);
   }

   wxASSERT(mClip || mWaveTrack);

   // Add the samples to the clip/track
   if (pClip)
   {
      pBlock = pClip->AppendNewBlock(bufptr, format, cnt);
   }

   // Let the finally block know everything is good
   success = true;

   return true;
}

bool AUPImportFileHandle::SetError(const TranslatableString &msg)
{
   wxLogError(msg.Debug());

   if (mErrorMsg.empty() || mUpdateResult == ProgressResult::Success)
   {
      mErrorMsg = msg;
   }

   // The only place where mUpdateResult is set during XML handling callbacks
   mUpdateResult = ProgressResult::Failed;

   return false;
}

bool AUPImportFileHandle::SetWarning(const TranslatableString &msg)
{
   wxLogWarning(msg.Debug());

   if (mErrorMsg.empty())
   {
      mErrorMsg = msg;
   }

   return false;
}
