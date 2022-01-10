/**********************************************************************

Audacity: A Digital Audio Editor

ExportWavPack.cpp

Subhradeep Chakraborty

This program is distributed under the GNU General Public License, version 2.
A copy of this license is included with this source.

Based on ExportOGG.cpp by:
Joshua Haberman

**********************************************************************/

#ifdef USE_WAVPACK

#include "Export.h"
#include "../ShuttleGui.h"
#include "../widgets/ProgressDialog.h"
#include "wxFileNameWrapper.h"
#include "../Tags.h"
#include "Mix.h"


#include <wavpack.h>


class ExportWavPackOptions final : public wxPanelWrapper
{
public:

   ExportWavPackOptions(wxWindow *parent, int format);
   virtual ~ExportWavPackOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

};

class ExportWavPack final : public ExportPlugin
{
public:
   
   ExportWavPack();

   void OptionsCreate(ShuttleGui &S, int foramt) override;

   ProgressResult Export(AudacityProject *project,
               std::unique_ptr<ProgressDialog> &pDialog,
               unsigned channels,
               const wxFileNameWrapper &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;
   

};

#endif
