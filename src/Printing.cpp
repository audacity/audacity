/**********************************************************************

  Audacity: A Digital Audio Editor

  Printing.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class AudacityPrintout
\brief Derived from wxPrintout, this class helps with printing.

*//*******************************************************************/


#include "Audacity.h"

#include <wx/defs.h>
#include <wx/dc.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/print.h>
#include <wx/printdlg.h>

#include "AColor.h"
#include "Track.h"
#include "TrackArtist.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "widgets/Ruler.h"

// Globals, so that we remember settings from session to session
wxPrintData *gPrintData = NULL;
wxPageSetupData *gPageSetupData = NULL;

class AudacityPrintout : public wxPrintout
{
 public:
   AudacityPrintout(wxString title,
                    TrackList *tracks):
      wxPrintout(title),
      mTracks(tracks)
   {
   }
   bool OnPrintPage(int page);
   bool HasPage(int page);
   bool OnBeginDocument(int startPage, int endPage);
   void GetPageInfo(int *minPage, int *maxPage,
                    int *selPageFrom, int *selPageTo);

 private:
   TrackList *mTracks;
};

bool AudacityPrintout::OnPrintPage(int page)
{
   wxDC *dc = GetDC();
   if (!dc)
      return false;

   int width, height;
   dc->GetSize(&width, &height);

   int rulerScreenHeight = 40;
   int screenTotalHeight = mTracks->GetHeight() + rulerScreenHeight;

   double scale = height / (double)screenTotalHeight;

   int rulerPageHeight = (int)(rulerScreenHeight * scale);
   Ruler ruler;
   ruler.SetBounds(0, 0, width, rulerPageHeight);
   ruler.SetOrientation(wxHORIZONTAL);
   ruler.SetRange(0.0, mTracks->GetEndTime());
   ruler.SetFormat(Ruler::TimeFormat);
   ruler.SetLabelEdges(true);
   ruler.Draw(*dc);

   TrackArtist artist;
   artist.SetBackgroundBrushes(*wxWHITE_BRUSH, *wxWHITE_BRUSH,
                               *wxWHITE_PEN, *wxWHITE_PEN);
   ViewInfo viewInfo;
   viewInfo.sel0 = viewInfo.sel1 = 0;
   viewInfo.vpos = 0;
   viewInfo.h = 0.0;
   viewInfo.screen = mTracks->GetEndTime() - viewInfo.h;
   viewInfo.total = viewInfo.screen;
   viewInfo.zoom = viewInfo.lastZoom = width / viewInfo.screen;
   int y = rulerPageHeight;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();
   while (n) {
      wxRect r;
      r.x = 0;
      r.y = y;
      r.width = width;
      r.height = (int)(n->GetHeight() * scale);

      artist.DrawTrack(n, *dc, r, &viewInfo, false, false, false, false);

      dc->SetPen(*wxBLACK_PEN);
      AColor::Line(*dc, 0, r.y, width, r.y);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(n)){
         y += r.height;
         r.x = 0;
         r.y = y;
         r.width = width;
         r.height = (int)(n->GetHeight(true) * scale);
         artist.DrawTrack(n, *dc, r, &viewInfo, false, false, false, false);
         dc->SetPen(*wxBLACK_PEN);
         AColor::Line(*dc, 0, r.y, width, r.y);
      }
#endif
      n = iter.Next();
      y += r.height;
   };

   return true;
}

bool AudacityPrintout::HasPage(int page)
{
   return (page==1);
}

bool AudacityPrintout::OnBeginDocument(int startPage, int endPage)
{
   return wxPrintout::OnBeginDocument(startPage, endPage);
}

void AudacityPrintout::GetPageInfo(int *minPage, int *maxPage,
                                   int *selPageFrom, int *selPageTo)
{
   *minPage = 1;
   *maxPage = 1;
   *selPageFrom = 1;
   *selPageTo = 1;
}

void HandlePageSetup(wxWindow *parent)
{
   if (gPageSetupData == NULL)
      gPageSetupData = new wxPageSetupDialogData();
   if (gPrintData == NULL)
      gPrintData = new wxPrintData();

   (*gPageSetupData) = *gPrintData;

   wxPageSetupDialog pageSetupDialog(parent, gPageSetupData);
   pageSetupDialog.ShowModal();

   (*gPrintData) = pageSetupDialog.GetPageSetupData().GetPrintData();
   (*gPageSetupData) = pageSetupDialog.GetPageSetupData();
}

void HandlePrint(wxWindow *parent, wxString name, TrackList *tracks)
{
   if (gPageSetupData == NULL)
      gPageSetupData = new wxPageSetupDialogData();
   if (gPrintData == NULL)
      gPrintData = new wxPrintData();

   wxPrintDialogData printDialogData(*gPrintData);

   wxPrinter printer(&printDialogData);
   AudacityPrintout printout(name, tracks);
   if (!printer.Print(parent, &printout, true)) {
      if (wxPrinter::GetLastError() == wxPRINTER_ERROR) {
         wxMessageBox(_("There was a problem printing."),
                      _("Print"), wxOK);
      }
      else {
         // Do nothing, the user cancelled...
      }
   }
   else {
      *gPrintData = printer.GetPrintDialogData().GetPrintData();
   }
}
