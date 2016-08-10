/**********************************************************************

  Audacity: A Digital Audio Editor

  Printing.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class AudacityPrintout
\brief Derived from wxPrintout, this class helps with printing.

*//*******************************************************************/


#include "Audacity.h"
#include "Printing.h"

#include <wx/defs.h>
#include <wx/dc.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/print.h>
#include <wx/printdlg.h>

#include "AColor.h"
#include "TrackArtist.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "widgets/Ruler.h"

#include "Experimental.h"

// Globals, so that we remember settings from session to session
wxPrintData &gPrintData()
{
   static wxPrintData theData;
   return theData;
}

class AudacityPrintout final : public wxPrintout
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

bool AudacityPrintout::OnPrintPage(int WXUNUSED(page))
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
   const double screenDuration = mTracks->GetEndTime();
   ZoomInfo zoomInfo(0.0, width / screenDuration);
   int y = rulerPageHeight;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();
   while (n) {
      wxRect r;
      r.x = 0;
      r.y = y;
      r.width = width;
      r.height = (int)(n->GetHeight() * scale);

      artist.DrawTrack(n, *dc, r, SelectedRegion(), zoomInfo, false, false, false, false);

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
   wxPageSetupData pageSetupData;

   wxPageSetupDialog pageSetupDialog(parent, &pageSetupData);
   pageSetupDialog.ShowModal();

   gPrintData() = pageSetupDialog.GetPageSetupData().GetPrintData();
}

void HandlePrint(wxWindow *parent, const wxString &name, TrackList *tracks)
{
   wxPrintDialogData printDialogData(gPrintData());

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
      gPrintData() = printer.GetPrintDialogData().GetPrintData();
   }
}
