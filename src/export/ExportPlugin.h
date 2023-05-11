/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPlugin.h

  Dominic Mazzoni

**********************************************************************/
#pragma once

#include <memory>

#include <wx/string.h>

#include "wxArrayStringEx.h"
#include "Identifier.h"
#include "TranslatableString.h"

class ShuttleGui;

class wxFileName;
class wxString;
class wxFileNameWrapper;

class AudacityProject;
class Tags;
class ExportProgressListener;

namespace MixerOptions{ class Downmix; }


struct AUDACITY_DLL_API FormatInfo
{
   wxString mFormat;
   TranslatableString mDescription;
   FileExtensions mExtensions;
   unsigned mMaxChannels;
   bool mCanMetaData;
};

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
class AUDACITY_DLL_API ExportPlugin /* not final */
{
public:
   ExportPlugin();
   virtual ~ExportPlugin();

   virtual int GetFormatCount() const = 0;
   virtual FormatInfo GetFormatInfo(int index) const = 0;

   virtual void OptionsCreate(ShuttleGui &S, int format) = 0;
   
   ///\brief Can be used to retrieve description of current export status in human-readable form
   virtual TranslatableString GetStatusString() const = 0;
   
   ///\return Optional detailed problem description in case of export failure.
   virtual TranslatableString GetErrorString() const = 0;

   virtual bool CheckFileName(wxFileName &filename, int format = 0);

   /** \brief called to export audio into a file.
    *
    * @param progressListener  Used to report on export progress and result
    * @param selectedOnly Set to true if all tracks should be mixed, to false
    * if only the selected tracks should be mixed and exported.
    * @param metadata A Tags object that will over-ride the one in *project and
    * be used to tag the file that is exported.
    * @param subformat Control which of the multiple formats this exporter is
    * capable of exporting should be used. Used where a single export plug-in
    * handles a number of related formats, but they have separate
    * entries in the Format drop-down list box. For example, the options to
    * export to "Other PCM", "AIFF 16 Bit" and "WAV 16 Bit" are all the same
    * libsndfile export plug-in, but with subformat set to 0, 1, and 2
    * respectively.
    * @return ProgressResult::Failed or ProgressResult::Cancelled if export
    * fails to complete for any reason, in which case this function is
    * responsible for alerting the user.  Otherwise ProgressResult::Success or
    * ProgressResult::Stopped
    */
   virtual void Export(AudacityProject *project,
                       ExportProgressListener &progressListener,
                       unsigned channels,
                       const wxFileNameWrapper &fName,
                       bool selectedOnly,
                       double t0,
                       double t1,
                       MixerOptions::Downmix *mixerSpec = nullptr,
                       const Tags *metadata = nullptr,
                       int subformat = 0) = 0;
   
   virtual void Cancel() = 0;
   
   virtual void Stop() = 0;
};

class ExportPluginEx : public ExportPlugin
{
   TranslatableString mStatus;
   TranslatableString mError;
   bool mCancelled{false};
   bool mStopped{false};
public:
   TranslatableString GetErrorString() const override;
   TranslatableString GetStatusString() const override;
   void Cancel() override;
   void Stop() override;
   
protected:
   void ExportBegin();
   void ExportFinish(ExportProgressListener& progressListener);
   
   bool IsCancelled() const noexcept;
   bool IsStopped() const noexcept;
   
   void SetStatusString(const TranslatableString& status);
   void SetErrorString(const TranslatableString& error);
};
