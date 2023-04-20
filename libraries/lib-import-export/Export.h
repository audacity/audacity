/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT__
#define __AUDACITY_EXPORT__

#include <functional>
#include <vector>
#include <wx/filename.h> // member variable
#include "Identifier.h"
#include "FileNames.h" // for FileTypes

#include "Registry.h"
#include "ExportPlugin.h"

class AudacityProject;
class WaveTrack;
class ExportPluginDelegate;
namespace MixerOptions{ class Downmix; }
using MixerSpec = MixerOptions::Downmix;
using WaveTrackConstArray = std::vector < std::shared_ptr < const WaveTrack > >;

using ExportPluginArray = std::vector < std::unique_ptr< ExportPlugin > > ;

class IMPORT_EXPORT_API Exporter final
{
   struct ExporterItem;
public:
   
   enum class DownMixMode
   {
      None,
      Mono,
      Stereo,
      FormatDefined
   };

   using ExportPluginFactory =
      std::function< std::unique_ptr< ExportPlugin >() >;

   // Objects of this type are statically constructed in files implementing
   // subclasses of ExportPlugin
   // Register factories, not plugin objects themselves, which allows them
   // to have some fresh state variables each time export begins again
   // and to compute translated strings for the current locale
   struct IMPORT_EXPORT_API RegisteredExportPlugin
      : public Registry::RegisteredItem<ExporterItem>
   {
      RegisteredExportPlugin(
         const Identifier &id, // an internal string naming the plug-in
         const ExportPluginFactory&,
         const Registry::Placement &placement = { wxEmptyString, {} } );
   };

   Exporter( AudacityProject &project );
   ~Exporter();

   void Configure(const wxFileName& filename, int pluginIndex, int formatIndex, const ExportPlugin::Parameters& parameters);
   
   bool SetExportRange(double t0, double t1, bool selectedOnly, bool skipSilenceAtBeginning = false);
   
   MixerOptions::Downmix* CreateMixerSpec();
   
   DownMixMode SetUseStereoOrMonoOutput();
   
   bool CanMetaData() const;
   
   ExportResult Process(ExportPluginDelegate& delegate);
   
   ExportResult Process(ExportPluginDelegate& delegate,
                const ExportPlugin::Parameters& parameters,
                unsigned numChannels,
                const FileExtension &type, const wxString & filename,
                bool selectedOnly, double t0, double t1);

   const ExportPluginArray &GetPlugins();
   
   int GetAutoExportFormat();
   int GetAutoExportSubFormat();
   wxFileName GetAutoExportFileName();
   ExportPlugin::Parameters GetAutoExportParameters();

private:
   struct IMPORT_EXPORT_API ExporterItem final : Registry::SingleItem {
      static Registry::GroupItem &Registry();
      ExporterItem(
         const Identifier &id, const Exporter::ExportPluginFactory &factory );
      Exporter::ExportPluginFactory mFactory;
   };

   void FixFilename();
   ExportResult ExportTracks(ExportPluginDelegate& delegate, const ExportPlugin::Parameters& parameters);

private:
   AudacityProject *mProject;
   std::unique_ptr<MixerSpec> mMixerSpec;

   ExportPluginArray mPlugins;

   wxFileName mFilename;
   wxFileName mActualName;
   ExportPlugin::Parameters mParameters;

   double mT0;
   double mT1;
   int mFormat{-1};
   int mSubFormat{-1};
   unsigned mNumLeft;
   unsigned mNumRight;
   unsigned mNumMono;
   unsigned mChannels;
   bool mSelectedOnly;
};


IMPORT_EXPORT_API TranslatableString AudacityExportCaptionStr();
IMPORT_EXPORT_API TranslatableString AudacityExportMessageStr();

/// We have many Export errors that are essentially anonymous
/// and are distinguished only by an error code number.
/// Rather than repeat the code, we have it just once.
IMPORT_EXPORT_API void ShowExportErrorDialog(wxString ErrorCode,
   TranslatableString message = AudacityExportMessageStr(),
   const TranslatableString& caption = AudacityExportCaptionStr(),
   bool allowReporting = true);

IMPORT_EXPORT_API
void ShowDiskFullExportErrorDialog(const wxFileNameWrapper &fileName);

#endif
