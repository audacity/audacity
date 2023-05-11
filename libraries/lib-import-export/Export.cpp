/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//*****************************************************************/

#include "Export.h"

#include "BasicUI.h"
#include "Mix.h"
#include "Project.h"
#include "WaveTrack.h"
#include "wxFileNameWrapper.h"

#include "ExportUtils.h"
#include "ExportProgressListener.h"

namespace {
   const auto PathStart = L"Exporters";

   using ExportPluginFactories = std::vector< Exporter::ExportPluginFactory >;
   ExportPluginFactories &sFactories()
   {
      static ExportPluginFactories theList;
      return theList;
   }

   class ExportProgressResultProxy final : public ExportProgressListener
   {
      ExportProgressListener& mListener;
      ExportResult mResult{ExportResult::Error};
   public:
      ExportProgressResultProxy(ExportProgressListener& listener)
         : mListener(listener)
      {
         
      }
      
      void OnExportProgress(double value) override
      {
         mListener.OnExportProgress(value);
      }
      void OnExportResult(ExportResult result) override
      {
         mListener.OnExportResult(result);
         mResult = result;
      }
      
      ExportResult GetResult() const noexcept
      {
         return mResult;
      }
   };
}

Registry::GroupItemBase &Exporter::ExporterItem::Registry()
{
   static Registry::GroupItem<Registry::DefaultTraits> registry{ PathStart };
   return registry;
}

Exporter::ExporterItem::ExporterItem(
   const Identifier &id, const Exporter::ExportPluginFactory &factory )
   : SingleItem{ id }
   , mFactory{ factory }
{}

Exporter::RegisteredExportPlugin::RegisteredExportPlugin(
   const Identifier &id,
   const ExportPluginFactory &factory,
   const Registry::Placement &placement )
   : RegisteredItem{
      factory ? std::make_unique< ExporterItem >( id, factory ) : nullptr,
      placement
   }
{
}

Exporter::Exporter( AudacityProject &project )
: mProject{ &project }
{
   using namespace Registry;
   static OrderingPreferenceInitializer init{
      PathStart,
      { {wxT(""), wxT("PCM,MP3,OGG,FLAC,MP2,CommandLine,FFmpeg") } },
   };

   // build the list of export plugins.
   for ( const auto &factory : sFactories() )
      mPlugins.emplace_back( factory() );

   struct MyVisitor final : Visitor {
      MyVisitor()
      {
         // visit the registry to collect the plug-ins properly
         // sorted
         GroupItem<Registry::DefaultTraits> top{ PathStart };
         Registry::Visit( *this, &top, &ExporterItem::Registry() );
      }

      void Visit( SingleItem &item, const Path &path ) override
      {
         mPlugins.emplace_back(
            static_cast<ExporterItem&>( item ).mFactory() );
      }

      ExportPluginArray mPlugins;
   } visitor;

   mPlugins.swap( visitor.mPlugins );
}

Exporter::~Exporter()
{
}

const ExportPluginArray &Exporter::GetPlugins()
{
   return mPlugins;
}

ExportPlugin* Exporter::GetPlugin()
{
   return mPlugins[mFormat].get();
}

ExportPlugin* Exporter::GetPlugin(int pluginIndex)
{
   if(pluginIndex >= 0 && pluginIndex < mPlugins.size())
      return mPlugins[pluginIndex].get();
   return nullptr;
}

ExportPlugin* Exporter::FindPluginByType(const FileExtension &type)
{
   for (const auto& plugin : mPlugins)
   {
      for (int j = 0; j < plugin->GetFormatCount(); j++)
      {
         auto formatInfo = plugin->GetFormatInfo(j);
         if (formatInfo.mFormat.IsSameAs(type, false))
            return plugin.get();
      }
   }
   return nullptr;
}

void Exporter::Configure(const wxFileName &filename,
                         int pluginIndex,
                         int formatIndex,
                         const ExportPlugin::Parameters& parameters)
{
   mFilename = filename;
   mActualName = filename;
   mFormat = pluginIndex;
   mSubFormat = formatIndex;
   mParameters = parameters;
}

bool Exporter::SetExportRange(double t0, double t1, bool selectedOnly, bool skipSilenceAtBeginning)
{
   double earliestBegin = t0;
   double latestEnd = t1;
   int numSelected = 0;

   auto &tracks = TrackList::Get(*mProject);

   const auto range = ExportUtils::FindExportWaveTracks(tracks, selectedOnly);

   for (auto tr : range) {
      numSelected++;

      if (tr->GetOffset() < earliestBegin) {
         earliestBegin = tr->GetOffset();
      }

      if (tr->GetEndTime() > latestEnd) {
         latestEnd = tr->GetEndTime();
      }
   }

   if (numSelected == 0) {
      return false;
   }

   mT0 = t0;
   mT1 = t1;
   mSelectedOnly = selectedOnly;
   mNumSelected = numSelected;
   mMono = std::all_of(range.begin(), range.end(), [](const WaveTrack *pTrack){
      return IsMono(*pTrack) && pTrack->GetPan() == 0.0;
   });

   // The skipping of silent space could be cleverer and take 
   // into account clips.
   // As implemented now, it can only skip initial silent space that 
   // has no clip before it, and terminal silent space that has no clip 
   // after it.
   if (t0 < earliestBegin){
      // Bug 1904 
      // Previously we always skipped initial silent space.
      // Now skipping it is an opt-in option.
      if (skipSilenceAtBeginning)
         mT0 = earliestBegin;
   }

   // We still skip silent space at the end
   if (mT1 > latestEnd)
      mT1 = latestEnd;

   return true;
}

void Exporter::Process(ExportProgressListener& progressListener)
{
   // Ensure filename doesn't interfere with project files.
   FixFilename();

   ExportTracks(progressListener, mParameters);

   // Get rid of mixerspec
   mMixerSpec.reset();
}

void Exporter::Process(ExportProgressListener& progressListener,
                       const ExportPlugin::Parameters& parameters,
                       unsigned numChannels,
                       const FileExtension &type, const wxString & filename,
                       bool selectedOnly, double t0, double t1)
{
   int i = -1;
   for (const auto& pPlugin : mPlugins)
   {
      ++i;
      for (int j = 0; j < pPlugin->GetFormatCount(); j++)
      {
         auto formatInfo = pPlugin->GetFormatInfo(j);
         if (formatInfo.mFormat.IsSameAs(type, false))
         {
            mChannels = numChannels;
            mSelectedOnly = selectedOnly;
            mT0 = t0;
            mT1 = t1;
            
            Configure(filename, i, j, parameters);
            FixFilename();
            ExportTracks(progressListener, parameters);
            return;
         }
      }
   }
   progressListener.OnExportResult(ExportProgressListener::ExportResult::Error);
}

//
// For safety, if the file already exists it stores the filename
// the user wants in actualName, and returns a temporary file name.
// The calling function should rename the file when it's successfully
// exported.
//
void Exporter::FixFilename()
{
   //
   // To be even safer, return a temporary file name based
   // on this one...
   //

   mActualName = mFilename;

   int suffix = 0;
   while (mFilename.FileExists()) {
      mFilename.SetName(mActualName.GetName() +
                        wxString::Format(wxT("%d"), suffix));
      suffix++;
   }
}

void Exporter::ExportTracks(ExportProgressListener& progressListener,
                            const ExportPlugin::Parameters& parameters)
{
   // Keep original in case of failure
   if (mActualName != mFilename) {
      ::wxRenameFile(mActualName.GetFullPath(), mFilename.GetFullPath());
   }

   ExportProgressResultProxy progressProxy(progressListener);

   auto cleanup = finally( [&] {
      const auto success = progressProxy.GetResult() == ExportProgressListener::ExportResult::Success
         || progressProxy.GetResult() == ExportProgressListener::ExportResult::Stopped;
      
      if (mActualName != mFilename) {
         // Remove backup
         if ( success )
            ::wxRemoveFile(mFilename.GetFullPath());
         else {
            // Restore original, if needed
            ::wxRemoveFile(mActualName.GetFullPath());
            ::wxRenameFile(mFilename.GetFullPath(), mActualName.GetFullPath());
         }
         // Restore filename
         mFilename = mActualName;
      }
      else {
         if ( ! success )
            // Remove any new, and only partially written, file.
            ::wxRemoveFile(mFilename.GetFullPath());
      }
   } );

   mPlugins[mFormat]->Export(mProject,
                             progressProxy,
                             parameters,
                             mMixerSpec ? mMixerSpec->GetNumChannels() : mChannels,
                             mActualName.GetFullPath(),
                             mSelectedOnly,
                             mT0,
                             mT1,
                             mMixerSpec.get(),
                             NULL,
                             mSubFormat);
}

int Exporter::GetAutoExportFormat() {
   return mFormat;
}

int Exporter::GetAutoExportSubFormat() {
   return mSubFormat;
}

wxFileName Exporter::GetAutoExportFileName() {
   return mFilename;
}

ExportPlugin::Parameters Exporter::GetAutoExportParameters()
{
   return mParameters;
}


MixerOptions::Downmix* Exporter::CreateMixerSpec()
{
   constexpr auto MaxExportChannels = 32u;

   auto exportTracks = ExportUtils::FindExportWaveTracks(TrackList::Get( *mProject ), mSelectedOnly);
   mMixerSpec = std::make_unique<MixerOptions::Downmix>(exportTracks.size(),
                                                        std::clamp(mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels,
                                                                   // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
                                                                   // sometimes not slidable.  My suspicion is that a mixer may incorrectly
                                                                   // state the number of channels - so we assume there are always at least two.
                                                                   // The downside is that if someone is exporting to a mono device, the dialog
                                                                   // will allow them to output to two channels. Hmm.  We may need to revisit this.
                                                                   // STF (April 2016): AMR (narrowband) and MP3 may export 1 channel.
                                                                   1u,
                                                                   MaxExportChannels));
   return mMixerSpec.get();
}

Exporter::DownMixMode Exporter::SetUseStereoOrMonoOutput()
{
   // Clean up ... should never happen
   mMixerSpec.reset();
   
   const unsigned channels = mMono ? 1 : 2;
   mChannels =
      std::min(channels, mPlugins[mFormat]->GetFormatInfo(mSubFormat).mMaxChannels);
   
   wxString exportFormat = mPlugins[mFormat]->GetFormatInfo(mSubFormat).mFormat;
   
   if(mNumSelected > 1 || channels > mChannels)
   {
      //TODO: violates OCP
      if(exportFormat == wxT("CL") || exportFormat == wxT("FFMPEG"))
         return DownMixMode::FormatDefined;

      return mChannels == 1 ? DownMixMode::Mono : DownMixMode::Stereo;
   }
   return DownMixMode::None;
}

bool Exporter::CanMetaData() const
{
   return mPlugins[mFormat]->GetFormatInfo(mSubFormat).mCanMetaData;
}

TranslatableString AudacityExportCaptionStr()
{
   return XO("Warning");
}
TranslatableString AudacityExportMessageStr()
{
   return XO("Unable to export.\nError %s");
}


// This creates a generic export error dialog
// Untranslated ErrorCodes like "MP3:1882" are used since we don't yet have
// a good user facing error message.  They allow us to 
// distinguish where the error occurred, and we can update the landing
// page as we learn more about when (if ever) these errors actually happen.
// The number happens to at one time have been a line number, but all
// we need from them is that they be distinct.
void ShowExportErrorDialog(wxString ErrorCode,
   TranslatableString message,
   const TranslatableString& caption,
   bool allowReporting)
{
   using namespace BasicUI;
   ShowErrorDialog( {},
      caption,
      message.Format( ErrorCode ),
      "Error:_Unable_to_export", // URL.
      ErrorDialogOptions { allowReporting ? ErrorDialogType::ModalErrorReport : ErrorDialogType::ModalError });
}

void ShowDiskFullExportErrorDialog(const wxFileNameWrapper &fileName)
{
   BasicUI::ShowErrorDialog( {},
      XO("Warning"),
      FileException::WriteFailureMessage(fileName),
      "Error:_Disk_full_or_not_writable"
   );
}


