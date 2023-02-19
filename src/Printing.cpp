/**********************************************************************

  Audacity: A Digital Audio Editor

  Printing.cpp

  Dominic Mazzoni

*******************************************************************//*!

\class AudacityPrintout
\brief Derived from wxPrintout, this class helps with printing.

*//*******************************************************************/


#include <wx/defs.h>
#include <wx/dc.h>
#include <wx/print.h>
#include <wx/printdlg.h>

#include "AColor.h"
#include "widgets/LinearUpdater.h"
#include "ProjectWindows.h"
#include "TrackArtist.h"
#include "ViewInfo.h"
#include "Track.h"
#include "widgets/Ruler.h"
#include "AudacityMessageBox.h"
#include "widgets/LinearUpdater.h"
#include "widgets/TimeFormat.h"

#include "TrackPanelDrawingContext.h"

#include "tracks/ui/TrackView.h"

#include "commands/CommandContext.h"
#include "commands/CommandManager.h"
#include "CommonCommandFlags.h"
#include "Project.h"
#include "TrackPanel.h"

namespace {
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
                    TrackList *tracks, TrackPanel &panel):
      wxPrintout(title),
        mPanel(panel)
      , mTracks(tracks)
   {
   }
   bool OnPrintPage(int page);
   bool HasPage(int page);
   bool OnBeginDocument(int startPage, int endPage);
   void GetPageInfo(int *minPage, int *maxPage,
                    int *selPageFrom, int *selPageTo);

 private:
   TrackPanel &mPanel;
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
   int screenTotalHeight =
      TrackView::GetTotalHeight( *mTracks ) + rulerScreenHeight;

   double scale = height / (double)screenTotalHeight;

   int rulerPageHeight = (int)(rulerScreenHeight * scale);
   Ruler ruler{ LinearUpdater::Instance(), TimeFormat::Instance() };
   ruler.SetBounds(0, 0, width, rulerPageHeight);
   ruler.SetOrientation(wxHORIZONTAL);
   ruler.SetRange(0.0, mTracks->GetEndTime());
   ruler.SetLabelEdges(true);
   ruler.Draw(*dc);

   TrackArtist artist( &mPanel );
   artist.SetBackgroundBrushes(*wxWHITE_BRUSH, *wxWHITE_BRUSH,
                               *wxWHITE_PEN, *wxWHITE_PEN);
   const double screenDuration = mTracks->GetEndTime();
   SelectedRegion region{};
   artist.pSelectedRegion = &region;
   ZoomInfo zoomInfo(0.0, width / screenDuration);
   artist.pZoomInfo = &zoomInfo;
   int y = rulerPageHeight;

   for (auto n : mTracks->Any()) {
      auto &view = TrackView::Get( *n );
      wxRect r;
      r.x = 0;
      r.y = 0;
      r.width = width;
      // Note that the views as printed might not have the same proportional
      // heights as displayed on the screen, because the fixed-sized separators
      // are counted in those heights but not printed
      auto trackHeight = (int)(view.GetHeight() * scale);
      r.height = trackHeight;

      const auto subViews = view.GetSubViews( r );
      if (subViews.empty())
         continue;
   
      auto iter = subViews.begin(), end = subViews.end(), next = iter;
      auto yy = iter->first;
      for ( ; iter != end; iter = next ) {
         ++next;
         auto nextY = ( next == end )
            ? trackHeight
            : next->first;
         r.y = y + yy;
         r.SetHeight( nextY - yy );
         yy = nextY;

         TrackPanelDrawingContext context{
            *dc, {}, {}, &artist
         };
         iter->second->Draw( context, r, TrackArtist::PassTracks );
      }

      dc->SetPen(*wxBLACK_PEN);
      AColor::Line(*dc, 0, y, width, y);

      y += trackHeight;
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

void HandlePrint(
   wxWindow *parent, const wxString &name, TrackList *tracks,
   TrackPanel &panel)
{
   wxPrintDialogData printDialogData(gPrintData());

   wxPrinter printer(&printDialogData);
   AudacityPrintout printout(name, tracks, panel);
   if (!printer.Print(parent, &printout, true)) {
      if (wxPrinter::GetLastError() == wxPRINTER_ERROR) {
         AudacityMessageBox(
            XO("There was a problem printing."),
            XO("Print"),
            wxOK);
      }
      else {
         // Do nothing, the user cancelled...
      }
   }
   else {
      gPrintData() = printer.GetPrintDialogData().GetPrintData();
   }
}

void OnPageSetup(const CommandContext &context)
{
   auto &project = context.project;
   auto &window = GetProjectFrame( project );
   HandlePageSetup(&window);
}

void OnPrint(const CommandContext &context)
{
   auto &project = context.project;
   auto name = project.GetProjectName();
   auto &tracks = TrackList::Get( project );
   auto &window = GetProjectFrame( project );
   HandlePrint(&window, name, &tracks, TrackPanel::Get( project ));
}

using namespace MenuTable;
BaseItemSharedPtr PrintingItems()
{
   static BaseItemSharedPtr items{
   Section( "Print",
      Command( wxT("PageSetup"), XXO("Pa&ge Setup..."), OnPageSetup,
         AudioIONotBusyFlag() | TracksExistFlag() ),
      /* i18n-hint: (verb) It's item on a menu. */
      Command( wxT("Print"), XXO("&Print..."), OnPrint,
         AudioIONotBusyFlag() | TracksExistFlag() )
   ) };
   return items;
}

AttachedItem sAttachment{ { "File", { OrderingHint::Before, "Exit" } },
   Shared( PrintingItems() )
};

}
