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

namespace BasicUI
{
class ProgressDialog;
enum class ProgressResult : unsigned;
}

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
   using ProgressResult = BasicUI::ProgressResult;

   ExportPlugin();
   virtual ~ExportPlugin();

   virtual int GetFormatCount() const = 0;
   virtual FormatInfo GetFormatInfo(int index) const = 0;

   virtual void OptionsCreate(ShuttleGui &S, int format) = 0;

   virtual bool CheckFileName(wxFileName &filename, int format = 0);

   /** \brief called to export audio into a file.
    *
    * @param pDialog To be initialized with pointer to a NEW ProgressDialog if
    * it was null, otherwise gives an existing dialog to be reused
   *  (working around a problem in wxWidgets for Mac; see bug 1600)
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
   virtual ProgressResult Export(AudacityProject *project,
                       std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
                       unsigned channels,
                       const wxFileNameWrapper &fName,
                       bool selectedOnly,
                       double t0,
                       double t1,
                       MixerOptions::Downmix *mixerSpec = NULL,
                       const Tags *metadata = NULL,
                       int subformat = 0) = 0;
};

