/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//**

\file Project.cpp
\brief Implements AudacityProject, DropTarget, and FileObject.F
Includes Menus.cpp.

*//****************************************************************//**

\class AudacityProject
\brief AudacityProject provides the main window, with tools and
tracks contained within it.

  In Audacity, the main window you work in is called a project.
  AudacityProjects can contain an arbitrary number of tracks of many
  different types, but if a project contains just one or two
  tracks then it can be saved in standard formats like WAV or AIFF.
  This window is the one that contains the menu bar (except on
  the Mac).

\attention The menu functions for AudacityProject, those for creating
the menu bars and acting on clicks, are found in file Menus.cpp

*//****************************************************************//**

\class DropTarget
\brief DropTarget, derived from wxFileDropTarget gives drag and drop
functionality for audio files.

*//****************************************************************//**

\class FileObject
\brief FileObject, derived from wxFileDataObject gives extended drag
and drop functionality for audio files.

*//****************************************************************//**

\class ViewInfo
\brief ViewInfo is used mainly to hold the zooming, selection and
scroll information.  It also has some status flags.

*//*******************************************************************/

#include "Audacity.h"
#include "Project.h"

#include <stdio.h>
#include <iostream>
#include <wx/wxprec.h>
#include <wx/apptrait.h>

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/dnd.h>
#include <wx/docview.h>
#include <wx/event.h>
#include <wx/ffile.h>
#include <wx/filedlg.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/notebook.h>
#include <wx/progdlg.h>
#include <wx/scrolbar.h>
#include <wx/sizer.h>
#include <wx/statusbr.h>
#include <wx/string.h>
#include <wx/textfile.h>
#include <wx/timer.h>
#include <wx/display.h>

#include <wx/arrimpl.cpp>       // this allows for creation of wxObjArray

#if defined(__WXMAC__)
#if !wxCHECK_VERSION(3, 0, 0)
#include <CoreServices/CoreServices.h>
#include <wx/mac/private.h>
#endif
#endif

#include "FreqWindow.h"
#include "effects/Contrast.h"
#include "AutoRecovery.h"
#include "AudacityApp.h"
#include "AColor.h"
#include "AudioIO.h"
#include "Dependencies.h"
#include "Diags.h"
#include "HistoryWindow.h"
#include "Lyrics.h"
#include "LyricsWindow.h"
#include "MixerBoard.h"
#include "Internat.h"
#include "import/Import.h"
#include "LabelTrack.h"
#include "Legacy.h"
#include "Mix.h"
#include "NoteTrack.h"
#include "Prefs.h"
#include "Sequence.h"
#include "Snap.h"
#include "Tags.h"
#include "TimeTrack.h"
#include "TrackPanel.h"
#include "WaveTrack.h"
#include "DirManager.h"
#include "effects/Effect.h"
#include "prefs/PrefsDialog.h"
#include "widgets/LinkingHtmlWindow.h"
#include "widgets/ASlider.h"
#include "widgets/ErrorDialog.h"
#include "widgets/Ruler.h"
#include "widgets/Warning.h"
#include "xml/XMLFileReader.h"
#include "PlatformCompatibility.h"
#include "Experimental.h"
#include "export/Export.h"
#include "FileNames.h"
#include "BlockFile.h"
#include "ondemand/ODManager.h"
#include "ondemand/ODTask.h"
#include "ondemand/ODComputeSummaryTask.h"
#ifdef EXPERIMENTAL_OD_FLAC
#include "ondemand/ODDecodeFlacTask.h"
#endif
#include "ModuleManager.h"

#include "Theme.h"
#include "AllThemeResources.h"

#include "FileDialog.h"

#include "UndoManager.h"

#include "toolbars/ToolManager.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/DeviceToolBar.h"
#include "toolbars/EditToolBar.h"
#include "toolbars/MeterToolBar.h"
#include "toolbars/MixerToolBar.h"
#include "toolbars/ScrubbingToolBar.h"
#include "toolbars/SelectionBar.h"
#include "toolbars/SpectralSelectionBar.h"
#include "toolbars/ToolsToolBar.h"
#include "toolbars/TranscriptionToolBar.h"

#include "tracks/ui/EditCursorOverlay.h"
#include "tracks/ui/PlayIndicatorOverlay.h"
#include "tracks/ui/Scrubbing.h"

#include "commands/ScriptCommandRelay.h"
#include "commands/CommandDirectory.h"
#include "commands/CommandTargets.h"
#include "commands/Command.h"
#include "commands/CommandType.h"

#include "../images/AudacityLogoAlpha.xpm"

std::unique_ptr<TrackList> AudacityProject::msClipboard
   { std::make_unique<TrackList>() };
double AudacityProject::msClipT0 = 0.0;
double AudacityProject::msClipT1 = 0.0;
AudacityProject *AudacityProject::msClipProject = NULL;
ODLock &AudacityProject::AllProjectDeleteMutex()
{
   static ODLock theMutex;
   return theMutex;
};

#if defined(__WXMAC__)
// const int sbarSpaceWidth = 15;
// const int sbarControlWidth = 16;
// const int sbarExtraLen = 1;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels
#elif defined(__WXMSW__)
const int sbarSpaceWidth = 16;
const int sbarControlWidth = 16;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels
#else // wxGTK, wxMOTIF, wxX11
const int sbarSpaceWidth = 15;
const int sbarControlWidth = 15;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs -- in pixels
#include "Theme.h"
#include "AllThemeResources.h"
#endif

int AudacityProject::mProjectCounter=0;// global counter.


////////////////////////////////////////////////////////////
/// Custom events
////////////////////////////////////////////////////////////
DEFINE_EVENT_TYPE(EVT_CAPTURE_KEY);

//
// This small template class resembles a try-finally block
//
// It sets var to val_entry in the constructor and
// var to val_exit in the destructor.
//
template <typename T>
class VarSetter
{
public:
   VarSetter(T* var, T val_entry, T val_exit)
   {
      mVar = var;
      mValExit = val_exit;
      *var = val_entry;
   }

   ~VarSetter()
   {
      *mVar = mValExit;
   }
private:
   T* mVar;
   T mValExit;
};

// This wrapper prevents the scrollbars from retaining focus after being
// used.  Otherwise, the only way back to the track panel is to click it
// and that causes your original location to be lost.
class ScrollBar final : public wxScrollBar
{
public:
   ScrollBar(wxWindow* parent, wxWindowID id, long style)
   :   wxScrollBar(parent, id, wxDefaultPosition, wxDefaultSize, style)
   {
   }

   void OnSetFocus(wxFocusEvent & e)
   {
      wxWindow *w = e.GetWindow();
      if (w != NULL) {
         w->SetFocus();
      }
   }

   void SetScrollbar(int position, int thumbSize,
                     int range, int pageSize,
                     bool refresh = true) override;

private:
   DECLARE_EVENT_TABLE()
};

void ScrollBar::SetScrollbar(int position, int thumbSize,
                             int range, int pageSize,
                             bool refresh)
{
   // Mitigate flashing of scrollbars by refreshing only when something really changes.

   // PRL:  This may have been made unnecessary by other fixes for flashing, see
   // commit ac05b190bee7dd0000bce56edb0e5e26185c972f

   auto changed =
      position != GetThumbPosition() ||
      thumbSize != GetThumbSize() ||
      range != GetRange() ||
      pageSize != GetPageSize();
   if (!changed)
      return;

   wxScrollBar::SetScrollbar(position, thumbSize, range, pageSize, refresh);
}

BEGIN_EVENT_TABLE(ScrollBar, wxScrollBar)
   EVT_SET_FOCUS(ScrollBar::OnSetFocus)
END_EVENT_TABLE()

/* Define Global Variables */
//The following global counts the number of documents that have been opened
//for the purpose of project placement (not to keep track of the number)
//It is only accurate modulo ten, and does not decrement when a project is
//closed.
static int gAudacityOffsetInc = 0;
static int gAudacityPosInc = 0;
//This is a pointer to the currently-active project.
static AudacityProject *gActiveProject;
//This array holds onto all of the projects currently open
AProjectArray gAudacityProjects;

/* Declare Static functions */
static void SetActiveProject(AudacityProject * project);

AUDACITY_DLL_API AudacityProject *GetActiveProject()
{
   return gActiveProject;
}

void SetActiveProject(AudacityProject * project)
{
   gActiveProject = project;
   wxTheApp->SetTopWindow(project);
}

#if wxUSE_DRAG_AND_DROP
class FileObject final : public wxFileDataObject
{
public:
   FileObject()
   {
   }

   bool IsSupportedFormat(const wxDataFormat & format, Direction WXUNUSED(dir = Get)) const
      // PRL:  This function does NOT override any inherited virtual!  What does it do?
   {
      if (format.GetType() == wxDF_FILENAME) {
         return true;
      }

#if defined(__WXMAC__)
#if !wxCHECK_VERSION(3, 0, 0)
      if (format.GetFormatId() == kDragPromisedFlavorFindFile) {
         return true;
      }
#endif
#endif

      return false;
   }
};

class DropTarget final : public wxFileDropTarget
{
public:
   DropTarget(AudacityProject *proj)
   {
      mProject = proj;

      // SetDataObject takes ownership
      SetDataObject(safenew FileObject());
   }

   ~DropTarget()
   {
   }

#if defined(__WXMAC__)
#if !wxCHECK_VERSION(3, 0, 0)
   bool GetData() override
   {
      bool foundSupported = false;
      bool firstFileAdded = false;
      OSErr result;

      UInt16 items = 0;
      CountDragItems((DragReference)m_currentDrag, &items);

      for (UInt16 index = 1; index <= items; index++) {

         DragItemRef theItem = 0;
         GetDragItemReferenceNumber((DragReference)m_currentDrag, index, &theItem);

         UInt16 flavors = 0;
         CountDragItemFlavors((DragReference)m_currentDrag, theItem , &flavors ) ;

         for (UInt16 flavor = 1 ;flavor <= flavors; flavor++) {

            FlavorType theType = 0;
            result = GetFlavorType((DragReference)m_currentDrag, theItem, flavor, &theType);
            if (theType != kDragPromisedFlavorFindFile && theType != kDragFlavorTypeHFS) {
               continue;
            }
            foundSupported = true;

            Size dataSize = 0;
            GetFlavorDataSize((DragReference)m_currentDrag, theItem, theType, &dataSize);

            Ptr theData = new char[dataSize];
            GetFlavorData((DragReference)m_currentDrag, theItem, theType, (void*) theData, &dataSize, 0L);

            wxString name;
            if (theType == kDragPromisedFlavorFindFile) {
               name = wxMacFSSpec2MacFilename((FSSpec *)theData);
            }
            else if (theType == kDragFlavorTypeHFS) {
               name = wxMacFSSpec2MacFilename(&((HFSFlavor *)theData)->fileSpec);
            }

            delete[] theData;

            if (!firstFileAdded) {
               // reset file list
               ((wxFileDataObject*)GetDataObject())->SetData(0, "");
               firstFileAdded = true;
            }

            ((wxFileDataObject*)GetDataObject())->AddFile(name);

            // We only want to process one flavor
            break;
         }
      }
      return foundSupported;
   }
#endif

   bool OnDrop(wxCoord x, wxCoord y) override
   {
      // bool foundSupported = false;
#if !wxCHECK_VERSION(3, 0, 0)
      bool firstFileAdded = false;
      OSErr result;

      UInt16 items = 0;
      CountDragItems((DragReference)m_currentDrag, &items);

      for (UInt16 index = 1; index <= items; index++) {

         DragItemRef theItem = 0;
         GetDragItemReferenceNumber((DragReference)m_currentDrag, index, &theItem);

         UInt16 flavors = 0;
         CountDragItemFlavors((DragReference)m_currentDrag, theItem , &flavors ) ;

         for (UInt16 flavor = 1 ;flavor <= flavors; flavor++) {

            FlavorType theType = 0;
            result = GetFlavorType((DragReference)m_currentDrag, theItem, flavor, &theType);
            if (theType != kDragPromisedFlavorFindFile && theType != kDragFlavorTypeHFS) {
               continue;
            }
            return true;
         }
      }
#endif
      return CurrentDragHasSupportedFormat();
   }

#endif

   bool OnDropFiles(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y), const wxArrayString& filenames) override
   {
      //sort by OD non OD.  load Non OD first so user can start editing asap.
      wxArrayString sortednames(filenames);

      ODManager::Pauser pauser;

      sortednames.Sort(CompareNoCaseFileName);
      for (unsigned int i = 0; i < sortednames.GetCount(); i++) {

         mProject->Import(sortednames[i]);
      }
      mProject->HandleResize(); // Adjust scrollers for NEW track sizes.

      return true;
   }

private:
   AudacityProject *mProject;
};

#endif


bool ImportXMLTagHandler::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("import")) || attrs==NULL || (*attrs)==NULL || wxStrcmp(*attrs++, wxT("filename")))
       return false;
   wxString strAttr = *attrs;
   if (!XMLValueChecker::IsGoodPathName(strAttr))
   {
      // Maybe strAttr is just a fileName, not the full path. Try the project data directory.
      wxFileName fileName(mProject->GetDirManager()->GetProjectDataDir(), strAttr);
      if (XMLValueChecker::IsGoodFileName(strAttr, fileName.GetPath(wxPATH_GET_VOLUME)))
         strAttr = fileName.GetFullPath();
      else
      {
         wxLogWarning(wxT("Could not import file: %s"), strAttr.c_str());
         return false;
      }
   }

   WaveTrackArray trackArray;
   mProject->Import(strAttr, &trackArray);
   if (trackArray.empty())
      return false;

   // Handle other attributes, now that we have the tracks.
   attrs++;
   const wxChar** pAttr;
   bool bSuccess = true;

   for (size_t i = 0; i < trackArray.size(); i++)
   {
      // Most of the "import" tag attributes are the same as for "wavetrack" tags,
      // so apply them via WaveTrack::HandleXMLTag().
      bSuccess = trackArray[i]->HandleXMLTag(wxT("wavetrack"), attrs);

      // "offset" tag is ignored in WaveTrack::HandleXMLTag except for legacy projects,
      // so handle it here.
      double dblValue;
      pAttr = attrs;
      while (*pAttr)
      {
         const wxChar *attr = *pAttr++;
         const wxChar *value = *pAttr++;
         const wxString strValue = value;
         if (!wxStrcmp(attr, wxT("offset")) &&
               XMLValueChecker::IsGoodString(strValue) &&
               Internat::CompatibleToDouble(strValue, &dblValue))
            trackArray[i]->SetOffset(dblValue);
      }
   }
   return bSuccess;
};

AudacityProject *CreateNewAudacityProject()
{
   bool bMaximized;
   wxRect wndRect;
   bool bIconized;
   GetNextWindowPlacement(&wndRect, &bMaximized, &bIconized);

   // Create and show a NEW project
   // Use a non-default deleter in the smart pointer!
   gAudacityProjects.push_back( AProjectHolder {
      safenew AudacityProject(
         nullptr, -1,
         wxDefaultPosition,
         wxSize(wndRect.width, wndRect.height)
      ),
      Destroyer< AudacityProject > {}
   } );
   const auto p = gAudacityProjects.back().get();

   // wxGTK3 seems to need to require creating the window using default position
   // and then manually positioning it.
   p->SetPosition(wndRect.GetPosition());

   if(bMaximized) {
      p->Maximize(true);
   }
   else if (bIconized) {
      // if the user close down and iconized state we could start back up and iconized state
      // p->Iconize(TRUE);
   }

   //Initialise the Listener
   gAudioIO->SetListener(p);

   //Set the NEW project as active:
   SetActiveProject(p);

   // Okay, GetActiveProject() is ready. Now we can get its CommandManager,
   // and add the shortcut keys to the tooltips.
   p->GetToolManager()->RegenerateTooltips();

   ModuleManager::Get().Dispatch(ProjectInitialized);

   p->Show(true);

   return p;
}

void RedrawAllProjects()
{
   size_t len = gAudacityProjects.size();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->RedrawProject();
}

void RefreshCursorForAllProjects()
{
   size_t len = gAudacityProjects.size();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->RefreshCursor();
}

AUDACITY_DLL_API void CloseAllProjects()
{
   size_t len = gAudacityProjects.size();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->Close();

   //Set the Offset and Position increments to 0
   gAudacityOffsetInc = 0;
   gAudacityPosInc = 0;
}

// BG: The default size and position of the first window
void GetDefaultWindowRect(wxRect *defRect)
{
   *defRect = wxGetClientDisplayRect();

   int width = 940;
   int height = 674;

   //These conditional values assist in improving placement and size
   //of NEW windows on different platforms.
#ifdef __WXGTK__
   height += 20;
#endif

#ifdef __WXMSW__
   height += 40;
#endif

#ifdef __WXMAC__
   height += 55;
#endif

   // Use screen size where it is smaller than the values we would like.
   // Otherwise use the values we would like, and centred.
   if (width < defRect->width)
   {
      defRect->x = (defRect->width - width)/2;
      defRect->width = width;
   }

   if (height < defRect->height)
   {
      defRect->y = (defRect->height - height)/2;
      // Bug 1119 workaround
      // Small adjustment for very small Mac screens.
      // If there is only a tiny space at the top
      // then instead of vertical centre, align to bottom.
      const int pixelsFormenu = 60;
      if( defRect->y < pixelsFormenu )
         defRect->y *=2;
      defRect->height = height;
   }
}

// true iff we have enough of the top bar to be able to reposition the window.
bool IsWindowAccessible(wxRect *requestedRect)
{
   wxDisplay display;
   wxRect targetTitleRect(requestedRect->GetLeftTop(), requestedRect->GetBottomRight());
   // Hackery to approximate a window top bar size from a window size.
   // and exclude the open/close and borders.
   targetTitleRect.x += 15;
   targetTitleRect.width -= 100;
   if (targetTitleRect.width <  165) targetTitleRect.width = 165;
   targetTitleRect.height = 15;
   int targetBottom = targetTitleRect.GetBottom();
   int targetRight = targetTitleRect.GetRight();
   // This looks like overkill to check each and every pixel in the ranges.
   // and decide that if any is visible on screen we are OK.
   for (int i =  targetTitleRect.GetLeft(); i < targetRight; i++) {
      for (int j = targetTitleRect.GetTop(); j < targetBottom; j++) {
         int monitor = display.GetFromPoint(wxPoint(i, j));
         if (monitor != wxNOT_FOUND) {
            return TRUE;
         }
      }
   }
   return FALSE;
}

// Returns the screen containing a rectangle, or -1 if none does.
int ScreenContaining( wxRect & r ){
   unsigned int n = wxDisplay::GetCount();
   for(unsigned int i = 0;i<n;i++){
      wxDisplay d(i);
      wxRect scr = d.GetClientArea();
      if( scr.Contains( r ) )
         return (int)i;
   }
   return -1;
}

// BG: Calculate where to place the next window (could be the first window)
// BG: Does not store X and Y in prefs. This is intentional.
//
// LL: This should NOT need to be this complicated...FIXME
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized)
{
   int inc = 25;

   wxRect defaultRect;
   GetDefaultWindowRect(&defaultRect);

   gPrefs->Read(wxT("/Window/Maximized"), pMaximized, false);
   gPrefs->Read(wxT("/Window/Iconized"), pIconized, false);

   wxRect windowRect;
   gPrefs->Read(wxT("/Window/X"), &windowRect.x, defaultRect.x);
   gPrefs->Read(wxT("/Window/Y"), &windowRect.y, defaultRect.y);
   gPrefs->Read(wxT("/Window/Width"), &windowRect.width, defaultRect.width);
   gPrefs->Read(wxT("/Window/Height"), &windowRect.height, defaultRect.height);

   wxRect normalRect;
   gPrefs->Read(wxT("/Window/Normal_X"), &normalRect.x, defaultRect.x);
   gPrefs->Read(wxT("/Window/Normal_Y"), &normalRect.y, defaultRect.y);
   gPrefs->Read(wxT("/Window/Normal_Width"), &normalRect.width, defaultRect.width);
   gPrefs->Read(wxT("/Window/Normal_Height"), &normalRect.height, defaultRect.height);

   // Workaround 2.1.1 and earlier bug on OSX...affects only normalRect, but let's just
   // validate for all rects and plats
   if (normalRect.width == 0 || normalRect.height == 0) {
      normalRect = defaultRect;
   }
   if (windowRect.width == 0 || windowRect.height == 0) {
      windowRect = defaultRect;
   }


   wxRect screenRect( wxGetClientDisplayRect());
#if defined(__WXMAC__)

   // On OSX, the top of the window should never be less than the menu height,
   // so something is amiss if it is
   if (normalRect.y < screenRect.y) {
      normalRect = defaultRect;
   }
   if (windowRect.y < screenRect.y) {
      windowRect = defaultRect;
   }
#endif

   // We don't mind being 32 pixels off the screen in any direction.
   // Make sure initial sizes (pretty much) fit within the display bounds
   // We used to trim the sizes which could result in ridiculously small windows.
   // contributing to bug 1243.
   // Now instead if the window significantly doesn't fit the screen, we use the default 
   // window instead, which we know does.
   if (ScreenContaining( wxRect(normalRect).Deflate( 32, 32 ))<0) {
      normalRect = defaultRect;
   }
   if (ScreenContaining( wxRect(windowRect).Deflate( 32, 32 ) )<0) {
      windowRect = defaultRect;
   }

   if (gAudacityProjects.empty()) {
      if (*pMaximized || *pIconized) {
         *nextRect = normalRect;
      }
      else {
         *nextRect = windowRect;
      }
      if (!IsWindowAccessible(nextRect)) {
         *nextRect = defaultRect;
      }
   }
   else {
      bool validWindowSize = false;
      AudacityProject * validProject = NULL;
      size_t numProjects = gAudacityProjects.size();
      for (int i = numProjects; i > 0 ; i--) {
         if (!gAudacityProjects[i-1]->IsIconized()) {
             validWindowSize = true;
             validProject = gAudacityProjects[i-1].get();
             break;
         }
      }
      if (validWindowSize) {
         *nextRect = validProject->GetRect();
         *pMaximized = validProject->IsMaximized();
         *pIconized = validProject->IsIconized();
      }
      else {
         *nextRect = normalRect;
      }

      //Placement depends on the increments
      nextRect->x += inc;
      nextRect->y += inc;
   }

   // defaultrect is a rectangle on the first screen.  It's the right fallback to 
   // use most of the time if things are not working out right with sizing.
   // windowRect is a saved rectangle size.
   // normalRect seems to be a substitute for windowRect when iconized or maximised.

   // Windows can say that we are off screen when actually we are not.
   // On Windows 10 I am seeing miscalculation by about 6 pixels.
   // To fix this we allow some sloppiness on the edge being counted as off screen.
   // This matters most when restoring very carefully sized windows that are maximised
   // in one dimension (height or width) but not both.
   const int edgeSlop = 10;

   // Next four lines are getting the rectangle for the screen that contains the
   // top left corner of nextRect (and defaulting to rect of screen 0 otherwise).
   wxPoint p = nextRect->GetLeftTop();
   int scr = std::max( 0, wxDisplay::GetFromPoint( p ));
   wxDisplay d( scr );
   screenRect = d.GetClientArea();

   // Now we (possibly) start trimming our rectangle down.
   // Have we hit the right side of the screen?
   wxPoint bottomRight = nextRect->GetBottomRight();
   if (bottomRight.x > (screenRect.GetRight()+edgeSlop)) {
      int newWidth = screenRect.GetWidth() - nextRect->GetLeft();
      if (newWidth < defaultRect.GetWidth()) {
         nextRect->x = windowRect.x;
         nextRect->y = windowRect.y;
         nextRect->width = windowRect.width;
      }
      else {
         nextRect->width = newWidth;
      }
   }

   // Have we hit the bottom of the screen?
   bottomRight = nextRect->GetBottomRight();
   if (bottomRight.y > (screenRect.GetBottom()+edgeSlop)) {
      nextRect->y -= inc;
      bottomRight = nextRect->GetBottomRight();
      if (bottomRight.y > (screenRect.GetBottom()+edgeSlop)) {
         nextRect->SetBottom(screenRect.GetBottom());
      }
   }

   // After all that we could have a window that does not have a visible
   // top bar.  [It is unlikely, but something might have gone wrong]
   // If so, use the safe fallback size.
   if (!IsWindowAccessible(nextRect)) {
      *nextRect = defaultRect;
   }
}

static wxString CreateUniqueName()
{
   static int count = 0;
   return wxDateTime::Now().Format(wxT("%Y-%m-%d %H-%M-%S")) +
          wxString::Format(wxT(" N-%i"), ++count);
}

enum {
   FirstID = 1000,

   // Window controls

   HSBarID,
   VSBarID,
   TrackPanelID
};


BEGIN_EVENT_TABLE(AudacityProject, wxFrame)
   EVT_MENU(wxID_ANY, AudacityProject::OnMenu)
   EVT_MOUSE_EVENTS(AudacityProject::OnMouseEvent)
   EVT_CLOSE(AudacityProject::OnCloseWindow)
   EVT_SIZE(AudacityProject::OnSize)
   EVT_SHOW(AudacityProject::OnShow)
   EVT_MOVE(AudacityProject::OnMove)
   EVT_ACTIVATE(AudacityProject::OnActivate)
   EVT_COMMAND_SCROLL_LINEUP(HSBarID, AudacityProject::OnScrollLeftButton)
   EVT_COMMAND_SCROLL_LINEDOWN(HSBarID, AudacityProject::OnScrollRightButton)
   EVT_COMMAND_SCROLL(HSBarID, AudacityProject::OnScroll)
   EVT_COMMAND_SCROLL(VSBarID, AudacityProject::OnScroll)
   EVT_TIMER(AudacityProjectTimerID, AudacityProject::OnTimer)
   // Fires for menu with ID #1...first menu defined
   EVT_UPDATE_UI(1, AudacityProject::OnUpdateUI)
   EVT_ICONIZE(AudacityProject::OnIconize)
   EVT_COMMAND(wxID_ANY, EVT_OPEN_AUDIO_FILE, AudacityProject::OnOpenAudioFile)
   EVT_COMMAND(wxID_ANY, EVT_TOOLBAR_UPDATED, AudacityProject::OnToolBarUpdate)
   //mchinen:multithreaded calls - may not be threadsafe with CommandEvent: may have to change.
   EVT_COMMAND(wxID_ANY, EVT_ODTASK_UPDATE, AudacityProject::OnODTaskUpdate)
   EVT_COMMAND(wxID_ANY, EVT_ODTASK_COMPLETE, AudacityProject::OnODTaskComplete)
END_EVENT_TABLE()

AudacityProject::AudacityProject(wxWindow * parent, wxWindowID id,
                                 const wxPoint & pos,
                                 const wxSize & size)
   : wxFrame(parent, id, _TS("Audacity"), pos, size),
     mRate((double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate())),
     mDefaultFormat((sampleFormat) gPrefs->
           Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample)),
     mSnapTo(gPrefs->Read(wxT("/SnapTo"), SNAP_OFF)),
     mSelectionFormat(gPrefs->Read(wxT("/SelectionFormat"), wxT(""))),
     mFrequencySelectionFormatName(gPrefs->Read(wxT("/FrequencySelectionFormatName"), wxT(""))),
     mBandwidthSelectionFormatName(gPrefs->Read(wxT("/BandwidthSelectionFormatName"), wxT(""))),
     mUndoManager(std::make_unique<UndoManager>()),
     mViewInfo(0.0, 1.0, ZoomInfo::GetDefaultZoom()),
     mbLoadedFromAup( false )
{
   // Note that the first field of the status bar is a dummy, and it's width is set
   // to zero latter in the code. This field is needed for wxWidgets 2.8.12 because
   // if you move to the menu bar, the first field of the menu bar is cleared, which
   // is undesirable behaviour.
   // In addition, the help strings of menu items are by default sent to the first
   // field. Currently there are no such help strings, but it they were introduced, then
   // there would need to be an event handler to send them to the appropriate field.
   mStatusBar = CreateStatusBar(4);
   mProjectNo = mProjectCounter++; // Bug 322

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   // MM: DirManager is created dynamically, freed on demand via ref-counting
   // MM: We don't need to Ref() here because it start with refcount=1
   mDirManager = std::make_shared<DirManager>();

   mLastSavedTracks = NULL;

   // Register for tracklist updates
   mTracks->Connect(EVT_TRACKLIST_UPDATED,
                    wxCommandEventHandler(AudacityProject::OnTrackListUpdated),
                    NULL,
                    this);

   //
   // Initialize view info (shared with TrackPanel)
   //

   UpdatePrefs();

   mLockPlayRegion = false;

   // Make sure valgrind sees mIsSyncLocked is initialized, even
   // though we're about to set it from prefs.
   mIsSyncLocked = false;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &mIsSyncLocked, false);

   // LLL:  Read this!!!
   //
   // Until the time (and cpu) required to refresh the track panel is
   // reduced, leave the following window creations in the order specified.
   // This will place the refresh of the track panel last, allowing all
   // the others to get done quickly.
   //
   // Near as I can tell, this is only a problem under Windows.
   //


   // PRL:  this panel groups the top tool dock and the ruler into one
   // tab cycle.
   // Must create it with non-default width equal to the main window width,
   // or else the device toolbar doesn't make initial widths of the choice
   // controls correct.
   mTopPanel = safenew wxPanelWrapper {
      this, wxID_ANY, wxDefaultPosition,
      wxSize{ this->GetSize().GetWidth(), -1 }
   };
   mTopPanel->SetAutoLayout(true);

   //
   // Create the ToolDock
   //
   mToolManager = std::make_unique<ToolManager>( this, mTopPanel );
   GetSelectionBar()->SetListener(this);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   GetSpectralSelectionBar()->SetListener(this);
#endif
   mToolManager->LayoutToolBars();

   //
   // Create the horizontal ruler
   //
   mRuler = safenew AdornedRulerPanel( this, mTopPanel,
                                   wxID_ANY,
                                   wxDefaultPosition,
                                   wxSize( -1, AdornedRulerPanel::GetRulerHeight(false) ),
                                   &mViewInfo );

   //
   // Create the TrackPanel and the scrollbars
   //
   wxWindow    * pPage;

#ifdef EXPERIMENTAL_NOTEBOOK
   // We are using a notebook (tabbed panel), so we create the notebook and add pages.
   GuiFactory Factory;
   wxNotebook  * pNotebook;
   mMainPanel = Factory.AddPanel(
      this, wxPoint( left, top ), wxSize( width, height ) );
   pNotebook  = Factory.AddNotebook( mMainPanel );
   /* i18n-hint: This is an experimental feature where the main panel in
      Audacity is put on a notebook tab, and this is the name on that tab.
      Other tabs in that notebook may have instruments, patch panels etc.*/
   pPage = Factory.AddPage( pNotebook, _("Main Mix"));
#else
   // Not using a notebook, so we place the track panel inside another panel,
   // this keeps the notebook code and normal code consistant and also
   // paves the way for adding additional windows inside the track panel.
   mMainPanel = safenew wxPanelWrapper(this, -1,
      wxDefaultPosition,
      wxDefaultSize,
      wxNO_BORDER);
   mMainPanel->SetSizer( safenew wxBoxSizer(wxVERTICAL) );
   pPage = mMainPanel;
   // Set the colour here to the track panel background to avoid
   // flicker when Audacity starts up.
   // However, that leads to areas next to the horizontal scroller
   // being painted in background colour and not scroller background
   // colour, so suppress this for now.
   //pPage->SetBackgroundColour( theTheme.Colour( clrDark ));
#endif

   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      ubs->Add(mToolManager->GetTopDock(), 0, wxEXPAND | wxALIGN_TOP);
      ubs->Add(mRuler, 0, wxEXPAND);
      mTopPanel->SetSizer(ubs.release());
   }

   wxBoxSizer *bs;
   {
      auto ubs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      bs = ubs.get();
      bs->Add(mTopPanel, 0, wxEXPAND | wxALIGN_TOP);
      bs->Add(pPage, 1, wxEXPAND);
      bs->Add(mToolManager->GetBotDock(), 0, wxEXPAND);
      SetAutoLayout(true);
      SetSizer(ubs.release());
   }
   bs->Layout();

   // The right hand side translates to NEW TrackPanel(...) in normal
   // Audacity without additional DLLs.
   mTrackPanel = TrackPanel::FactoryFunction(pPage,
                                             TrackPanelID,
                                             wxDefaultPosition,
                                             wxDefaultSize,
                                             mTracks,
                                             &mViewInfo,
                                             this,
                                             mRuler);
   mTrackPanel->UpdatePrefs();

   mIndicatorOverlay = std::make_unique<PlayIndicatorOverlay>(this);

   mCursorOverlay = std::make_unique<EditCursorOverlay>(this);

#ifdef EXPERIMENTAL_SCRUBBING_BASIC
   mScrubOverlay = std::make_unique<ScrubbingOverlay>(this);
   mScrubber = std::make_unique<Scrubber>(this);
#endif

   // More order dependencies here...
   // This must follow construction of *mIndicatorOverlay, because it must
   // attach its timer event handler later (so that its handler is invoked
   // earlier)
   mPlaybackScroller = std::make_unique<PlaybackScroller>(this);

   // This must follow construction of *mPlaybackScroller,
   // because it must
   // attach its timer event handler later (so that its handler is invoked
   // earlier)
   this->Connect(EVT_TRACK_PANEL_TIMER,
      wxCommandEventHandler(ViewInfo::OnTimer),
      NULL,
      &mViewInfo);

   // Add the overlays, in the sequence in which they will be painted
   mTrackPanel->AddOverlay(mIndicatorOverlay.get());
   mTrackPanel->AddOverlay(mCursorOverlay.get());
#ifdef EXPERIMENTAL_SCRUBBING_BASIC
   mTrackPanel->AddOverlay(mScrubOverlay.get());
#endif

   CreateMenusAndCommands();

   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      creating the scrollbars after the TrackPanel, we resolve
   //      several focus problems.
   mHsbar = safenew ScrollBar(pPage, HSBarID, wxSB_HORIZONTAL);
   mVsbar = safenew ScrollBar(pPage, VSBarID, wxSB_VERTICAL);

   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      making the TrackPanel that first window, we resolve several
   //      keyboard focus problems.
   pPage->MoveBeforeInTabOrder(mTopPanel);

   bs = (wxBoxSizer *)pPage->GetSizer();

   {
      // Top horizontal grouping
      auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Track panel
      hs->Add(mTrackPanel, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP);

      {
         // Vertical grouping
         auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);

         // Vertical scroll bar
         vs->Add(mVsbar, 1, wxEXPAND | wxALIGN_TOP);
         hs->Add(vs.release(), 0, wxEXPAND | wxALIGN_TOP);
      }

      bs->Add(hs.release(), 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP);
   }

   {
      // Bottom horizontal grouping
      auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

      // Bottom scrollbar
      hs->Add(mTrackPanel->GetLeftOffset() - 1, 0);
      hs->Add(mHsbar, 1, wxALIGN_BOTTOM);
      hs->Add(mVsbar->GetSize().GetWidth(), 0);
      bs->Add(hs.release(), 0, wxEXPAND | wxALIGN_LEFT);
   }

   // Lay it out
   pPage->SetAutoLayout(true);
   pPage->Layout();

#ifdef EXPERIMENTAL_NOTEBOOK
   AddPages(this, Factory, pNotebook);
#endif

   mMainPanel->Layout();

   wxASSERT( mTrackPanel->GetProject()==this);

   // MM: Give track panel the focus to ensure keyboard commands work
   mTrackPanel->SetFocus();

   // Create tags object
   mTags = std::make_shared<Tags>();

   InitialState();
   FixScrollbars();
   mRuler->SetLeftOffset(mTrackPanel->GetLeftOffset());  // bevel on AdornedRuler

   //
   // Set the Icon
   //

   // loads either the XPM or the windows resource, depending on the platform
#if !defined(__WXMAC__) && !defined(__WXX11__)
   {
#if defined(__WXMSW__)
      wxIcon ic{ wxICON(AudacityLogo) };
#elif defined(__WXGTK__)
      wxIcon ic{wxICON(AudacityLogoAlpha)};
#else
      wxIcon ic{};
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
#endif
      SetIcon(ic);
   }
#endif
   mIconized = false;

   mTrackFactory.reset(safenew TrackFactory{ mDirManager, &mViewInfo });

   int widths[] = {0, GetControlToolBar()->WidthForStatusBar(mStatusBar), -1, 150};
   mStatusBar->SetStatusWidths(4, widths);
   wxString msg = wxString::Format(_("Welcome to Audacity version %s"),
                                   AUDACITY_VERSION_STRING);
   mStatusBar->SetStatusText(msg, mainStatusBarField);
   GetControlToolBar()->UpdateStatusBar(this);
   mLastStatusUpdateTime = ::wxGetUTCTime();

   mTimer = std::make_unique<wxTimer>(this, AudacityProjectTimerID);
   mTimer->Start(200);

#if wxUSE_DRAG_AND_DROP
   // We can import now, so become a drag target
//   SetDropTarget(safenew AudacityDropTarget(this));
//   mTrackPanel->SetDropTarget(safenew AudacityDropTarget(this));

   // SetDropTarget takes ownership
   mTrackPanel->SetDropTarget(safenew DropTarget(this));
#endif

   wxTheApp->Connect(EVT_AUDIOIO_CAPTURE,
                     wxCommandEventHandler(AudacityProject::OnCapture),
                     NULL,
                     this);

   //Initialize the last selection adjustment time.
   mLastSelectionAdjustment = ::wxGetLocalTimeMillis();

}

AudacityProject::~AudacityProject()
{
   // Tool manager gives us capture sometimes
   if(HasCapture())
      ReleaseMouse();

   if (wxGetApp().GetRecentFiles())
   {
      wxGetApp().GetRecentFiles()->RemoveMenu(mRecentFilesMenu);
   }

   if(mTrackPanel) {
#ifdef EXPERIMENTAL_SCRUBBING_BASIC
      mTrackPanel->RemoveOverlay(mScrubOverlay.get());
#endif
      mTrackPanel->RemoveOverlay(mCursorOverlay.get());
      mTrackPanel->RemoveOverlay(mIndicatorOverlay.get());
   }

   wxTheApp->Disconnect(EVT_AUDIOIO_CAPTURE,
                     wxCommandEventHandler(AudacityProject::OnCapture),
                     NULL,
                     this);
}

AudioIOStartStreamOptions AudacityProject::GetDefaultPlayOptions()
{
   AudioIOStartStreamOptions options { GetRate() };
   options.timeTrack = GetTracks()->GetTimeTrack();
   options.listener = this;
   return options;
}

void AudacityProject::UpdatePrefsVariables()
{
   gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &mShowId3Dialog, true);
   gPrefs->Read(wxT("/AudioFiles/NormalizeOnLoad"),&mNormalizeOnLoad, false);
   gPrefs->Read(wxT("/GUI/AutoScroll"), &mViewInfo.bUpdateTrackIndicator, true);
   gPrefs->Read(wxT("/GUI/EmptyCanBeDirty"), &mEmptyCanBeDirty, true );
   gPrefs->Read(wxT("/GUI/Help"), &mHelpPref, wxT("InBrowser") );
   gPrefs->Read(wxT("/GUI/SelectAllOnNone"), &mSelectAllOnNone, true);
   mStopIfWasPaused = true;  // not configurable for now, but could be later.
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &mShowSplashScreen, true);
   gPrefs->Read(wxT("/GUI/Solo"), &mSoloPref, wxT("Simple"));
   // Update the old default to the NEW default.
   if (mSoloPref == wxT("Standard"))
      mSoloPref = wxT("Simple");
   gPrefs->Read(wxT("/GUI/TracksFitVerticallyZoomed"), &mTracksFitVerticallyZoomed, false);
   //   gPrefs->Read(wxT("/GUI/UpdateSpectrogram"), &mViewInfo.bUpdateSpectrogram, true);

   gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), &mRate, AudioIO::GetOptimalSupportedSampleRate());
   mDefaultFormat = (sampleFormat) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

   gPrefs->Read(wxT("/AudioIO/SeekShortPeriod"), &mSeekShort, 1.0);
   gPrefs->Read(wxT("/AudioIO/SeekLongPeriod"), &mSeekLong, 15.0);
}

void AudacityProject::UpdatePrefs()
{
   UpdatePrefsVariables();

   SetProjectTitle();

   if (mTrackPanel) {
      mTrackPanel->UpdatePrefs();
   }
   if (mMixerBoard)
      mMixerBoard->UpdatePrefs();

   if (mToolManager) {
      mToolManager->UpdatePrefs();
   }

   if (mRuler) {
      mRuler->UpdatePrefs();
   }

   // The toolbars will be recreated, so make sure we don't leave
   // a stale pointer hanging around.
   mLastFocusedWindow = NULL;
}

void AudacityProject::SetMissingAliasFileDialog(wxDialog *dialog)
{
   mAliasMissingWarningDialog = dialog;
}

wxDialog *AudacityProject::GetMissingAliasFileDialog()
{
   return mAliasMissingWarningDialog;
}

void AudacityProject::RedrawProject(const bool bForceWaveTracks /*= false*/)
{
   FixScrollbars();
   if (bForceWaveTracks && GetTracks())
   {
      TrackListIterator iter(GetTracks());
      Track* pTrack = iter.First();
      while (pTrack)
      {
         if (pTrack->GetKind() == Track::Wave)
         {
            WaveTrack* pWaveTrack = static_cast<WaveTrack*>(pTrack);
            for (const auto &clip: pWaveTrack->GetClips())
               clip->MarkChanged();
         }
         pTrack = iter.Next();
      }
   }
   mTrackPanel->Refresh(false);
}

void AudacityProject::RefreshCursor()
{
   mTrackPanel->HandleCursorForLastMouseEvent();
}

void AudacityProject::SetSel0(double newSel0)
{
   //Bound checking should go on here

   mViewInfo.selectedRegion.setT0(newSel0);
}

void AudacityProject::SetSel1(double newSel1)
{
   //Bound checking should go on here

   mViewInfo.selectedRegion.setT1(newSel1);
}

void AudacityProject::OnCapture(wxCommandEvent& evt)
{
   evt.Skip();

   if (evt.GetInt() != 0)
      mIsCapturing = true;
   else
      mIsCapturing = false;
}


const std::shared_ptr<DirManager> &AudacityProject::GetDirManager()
{
   return mDirManager;
}

TrackFactory *AudacityProject::GetTrackFactory()
{
   return mTrackFactory.get();
}

AdornedRulerPanel *AudacityProject::GetRulerPanel()
{
   return mRuler;
}

int AudacityProject::GetAudioIOToken() const
{
   return mAudioIOToken;
}

void AudacityProject::SetAudioIOToken(int token)
{
   mAudioIOToken = token;
}

bool AudacityProject::IsAudioActive() const
{
   return GetAudioIOToken() > 0 &&
      gAudioIO->IsStreamActive(GetAudioIOToken());
}

const Tags *AudacityProject::GetTags()
{
   return mTags.get();
}

wxString AudacityProject::GetName()
{
   wxString name = wxFileNameFromPath(mFileName);

   // Chop off the extension
   size_t len = name.Len();
   if (len > 4 && name.Mid(len - 4) == wxT(".aup"))
      name = name.Mid(0, len - 4);

   return name;
}

// Pass a number in to show project number, or -1 not to.
void AudacityProject::SetProjectTitle( int number)
{
   wxString name = GetName();

   // If we are showing project numbers, then we also explicitly show "<untitled>" if there
   // is none.
   if( number >= 0 ){
      /* i18n-hint: The %02i is the project number, the %s is the project name.*/
      name = wxString::Format( _TS("[Project %02i] Audacity \"%s\""), number+1 ,
         name.IsEmpty() ? "<untitled>" : (const char *)name.c_str() );
   }
   // If we are not showing numbers, then <untitled> shows as 'Audacity'.
   else if( name.IsEmpty() )
   {
      mbLoadedFromAup = false;
      name = _TS("Audacity");
   }

   if (mIsRecovered)
   {
      name += wxT(" ");
      /* i18n-hint: E.g this is recovered audio that had been lost.*/
      name += _("(Recovered)");
   }

   SetTitle( name );
   SetName(name);       // to make the nvda screen reader read the correct title
}

bool AudacityProject::GetIsEmpty()
{
   return mTracks->IsEmpty();
}

bool AudacityProject::SnapSelection()
{
   if (mSnapTo != SNAP_OFF) {
      SelectedRegion &selectedRegion = mViewInfo.selectedRegion;
      NumericConverter nc(NumericConverter::TIME, GetSelectionFormat(), 0, GetRate());
      const bool nearest = (mSnapTo == SNAP_NEAREST);

      const double oldt0 = selectedRegion.t0();
      const double oldt1 = selectedRegion.t1();

      nc.ValueToControls(oldt0, nearest);
      nc.ControlsToValue();
      const double t0 = nc.GetValue();

      nc.ValueToControls(oldt1, nearest);
      nc.ControlsToValue();
      const double t1 = nc.GetValue();

      if (t0 != oldt0 || t1 != oldt1) {
         selectedRegion.setTimes(t0, t1);
         TP_DisplaySelection();
         return true;
      }
   }

   return false;
}

double AudacityProject::AS_GetRate()
{
   return mRate;
}

void AudacityProject::AS_SetRate(double rate)
{
   mRate = rate;
}

int AudacityProject::AS_GetSnapTo()
{
   return GetSnapTo();
}

void AudacityProject::AS_SetSnapTo(int snap)
{
   mSnapTo = snap;

// LLL: TODO - what should this be changed to???
// mCommandManager.Check(wxT("Snap"), mSnapTo);
   gPrefs->Write(wxT("/SnapTo"), mSnapTo);
   gPrefs->Flush();

   SnapSelection();

   RedrawProject();
}

const wxString & AudacityProject::AS_GetSelectionFormat()
{
   return GetSelectionFormat();
}

void AudacityProject::AS_SetSelectionFormat(const wxString & format)
{
   mSelectionFormat = format;

   gPrefs->Write(wxT("/SelectionFormat"), mSelectionFormat);
   gPrefs->Flush();

   if (SnapSelection() && GetTrackPanel())
      GetTrackPanel()->Refresh(false);
}

double AudacityProject::SSBL_GetRate() const
{
   // Return maximum of project rate and all track rates.
   double rate = mRate;

   TrackListOfKindIterator iterWaveTrack(Track::Wave, mTracks.get());
   WaveTrack *pWaveTrack = static_cast<WaveTrack*>(iterWaveTrack.First());
   while (pWaveTrack)
   {
      rate = std::max(rate, pWaveTrack->GetRate());
      pWaveTrack = static_cast<WaveTrack*>(iterWaveTrack.Next());
   }

   return rate;
}

const wxString & AudacityProject::SSBL_GetFrequencySelectionFormatName()
{
   return GetFrequencySelectionFormatName();
}

void AudacityProject::SSBL_SetFrequencySelectionFormatName(const wxString & formatName)
{
   mFrequencySelectionFormatName = formatName;

   gPrefs->Write(wxT("/FrequencySelectionFormatName"), mFrequencySelectionFormatName);
   gPrefs->Flush();
}

const wxString & AudacityProject::SSBL_GetBandwidthSelectionFormatName()
{
   return GetBandwidthSelectionFormatName();
}

void AudacityProject::SSBL_SetBandwidthSelectionFormatName(const wxString & formatName)
{
   mBandwidthSelectionFormatName = formatName;

   gPrefs->Write(wxT("/BandwidthSelectionFormatName"), mBandwidthSelectionFormatName);
   gPrefs->Flush();
}

void AudacityProject::SSBL_ModifySpectralSelection(double &bottom, double &top, bool done)
{
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   double nyq = SSBL_GetRate() / 2.0;
   if (bottom >= 0.0)
      bottom = std::min(nyq, bottom);
   if (top >= 0.0)
      top = std::min(nyq, top);
   mViewInfo.selectedRegion.setFrequencies(bottom, top);
   mTrackPanel->Refresh(false);
   if (done) {
      ModifyState(false);
   }
#else
   bottom; top; done;
#endif
}

const wxString & AudacityProject::GetFrequencySelectionFormatName() const
{
   return mFrequencySelectionFormatName;
}

void AudacityProject::SetFrequencySelectionFormatName(const wxString & formatName)
{
   SSBL_SetFrequencySelectionFormatName(formatName);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   if (GetSpectralSelectionBar()) {
      GetSpectralSelectionBar()->SetFrequencySelectionFormatName(formatName);
   }
#endif
}

const wxString & AudacityProject::GetBandwidthSelectionFormatName() const
{
   return mBandwidthSelectionFormatName;
}

void AudacityProject::SetBandwidthSelectionFormatName(const wxString & formatName)
{
   SSBL_SetBandwidthSelectionFormatName(formatName);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   if (GetSpectralSelectionBar()) {
      GetSpectralSelectionBar()->SetBandwidthSelectionFormatName(formatName);
   }
#endif
}

void AudacityProject::SetSelectionFormat(const wxString & format)
{
   AS_SetSelectionFormat(format);
   if (GetSelectionBar()) {
      GetSelectionBar()->SetSelectionFormat(format);
   }
}

const wxString & AudacityProject::GetSelectionFormat() const
{
   return mSelectionFormat;
}


void AudacityProject::AS_ModifySelection(double &start, double &end, bool done)
{
   mViewInfo.selectedRegion.setTimes(start, end);
   mTrackPanel->Refresh(false);
   if (done) {
      ModifyState(false);
   }
}

void AudacityProject::FinishAutoScroll()
{
   // Set a flag so we don't have to generate two update events
   mAutoScrolling = true;

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   DoScroll();

   mAutoScrolling = false;
}


///
/// This method handles general left-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the left of the thumb
///
void AudacityProject::OnScrollLeft()
{
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   pos -= wxMax((wxInt64)(sbarHjump * mViewInfo.sbarScale), 1);
   pos = wxMax(pos, 0);
   mViewInfo.sbarH -= sbarHjump;
   mViewInfo.sbarH = std::max(mViewInfo.sbarH,
      -(wxInt64) PixelWidthBeforeTime(0.0));


   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      FinishAutoScroll();
   }
}
///
/// This method handles general right-scrolling, either for drag-scrolling
/// or when the scrollbar is clicked to the right of the thumb
///

void AudacityProject::OnScrollRight()
{
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += wxMax((wxInt64)(sbarHjump * mViewInfo.sbarScale), 1);
   wxInt64 max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = wxMin(pos, max);
   mViewInfo.sbarH += sbarHjump;
   mViewInfo.sbarH = std::min(mViewInfo.sbarH,
      mViewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - mViewInfo.sbarScreen);

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      FinishAutoScroll();
   }
}

///
///  This handles the event when the left direction button on the scrollbar is depresssed
///
void AudacityProject::OnScrollLeftButton(wxScrollEvent & /*event*/)
{
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   pos -= wxMax((wxInt64)(sbarHjump * mViewInfo.sbarScale), 1);
   pos = wxMax(pos, 0);
   mViewInfo.sbarH -= sbarHjump;
   mViewInfo.sbarH = std::max(mViewInfo.sbarH,
      - (wxInt64) PixelWidthBeforeTime(0.0));

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      DoScroll();
   }
}

///
///  This handles  the event when the right direction button on the scrollbar is depresssed
///
void AudacityProject::OnScrollRightButton(wxScrollEvent & /*event*/)
{
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += wxMax((wxInt64)(sbarHjump * mViewInfo.sbarScale), 1);
   wxInt64 max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = wxMin(pos, max);
   mViewInfo.sbarH += sbarHjump;
   mViewInfo.sbarH = std::min(mViewInfo.sbarH,
      mViewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - mViewInfo.sbarScreen);

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      DoScroll();
   }
}


bool AudacityProject::MayScrollBeyondZero() const
{
   if (mViewInfo.bScrollBeyondZero)
      return true;

   if (GetScrubber().HasStartedScrubbing() ||
       IsAudioActive()) {
      if (mPlaybackScroller) {
         auto mode = mPlaybackScroller->GetMode();
         if (mode == PlaybackScroller::Mode::Centered ||
             mode == PlaybackScroller::Mode::Right)
            return true;
      }
   }

   return false;
}

double AudacityProject::ScrollingLowerBoundTime() const
{
   if (!MayScrollBeyondZero())
      return 0;
   const double screen = mTrackPanel->GetScreenEndTime() - mViewInfo.h;
   return std::min(mTracks->GetStartTime(), -screen / 2.0);
}

// PRL: Bug1197: we seem to need to compute all in double, to avoid differing results on Mac
// That's why ViewInfo::TimeRangeToPixelWidth was defined, with some regret.
double AudacityProject::PixelWidthBeforeTime(double scrollto) const
{
   const double lowerBound = ScrollingLowerBoundTime();
   return
      // Ignoring fisheye is correct here
      mViewInfo.TimeRangeToPixelWidth(scrollto - lowerBound);
}

void AudacityProject::SetHorizontalThumb(double scrollto)
{
   const auto unscaled = PixelWidthBeforeTime(scrollto);
   const int max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   const int pos =
      std::min(max,
         std::max(0,
            (int)(floor(0.5 + unscaled * mViewInfo.sbarScale))));
   mHsbar->SetThumbPosition(pos);
   mViewInfo.sbarH = floor(0.5 + unscaled - PixelWidthBeforeTime(0.0));
   mViewInfo.sbarH = std::max(mViewInfo.sbarH,
      - (wxInt64) PixelWidthBeforeTime(0.0));
   mViewInfo.sbarH = std::min(mViewInfo.sbarH,
      mViewInfo.sbarTotal
         - (wxInt64) PixelWidthBeforeTime(0.0) - mViewInfo.sbarScreen);
}

//
// This method, like the other methods prefaced with TP, handles TrackPanel
// 'callback'.
//
void AudacityProject::TP_ScrollWindow(double scrollto)
{
   SetHorizontalThumb(scrollto);

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   DoScroll();
}

//
// Scroll vertically. This is called for example by the mouse wheel
// handler in Track Panel. A positive argument makes the window
// scroll down, while a negative argument scrolls up.
//
bool AudacityProject::TP_ScrollUpDown(int delta)
{
   int oldPos = mVsbar->GetThumbPosition();
   int pos = oldPos + delta;
   int max = mVsbar->GetRange() - mVsbar->GetThumbSize();

   // Can be negative in case of only one track
   if (max < 0)
      max = 0;

   if (pos > max)
      pos = max;
   else if (pos < 0)
      pos = 0;

   if (pos != oldPos)
   {
      mVsbar->SetThumbPosition(pos);

      DoScroll();
      return true;
   }
   else
      return false;
}

void AudacityProject::FixScrollbars()
{
   if (!GetTracks())
      return;

   bool refresh = false;
   bool rescroll = false;

   int totalHeight = (mTracks->GetHeight() + 32);

   int panelWidth, panelHeight;
   mTrackPanel->GetTracksUsableArea(&panelWidth, &panelHeight);

   // (From Debian...at least I think this is the change cooresponding
   // to this comment)
   //
   // (2.) GTK critical warning "IA__gtk_range_set_range: assertion
   // 'min < max' failed" because of negative numbers as result of window
   // size checking. Added a sanity check that straightens up the numbers
   // in edge cases.
   if (panelWidth < 0) {
      panelWidth = 0;
   }
   if (panelHeight < 0) {
      panelHeight = 0;
   }

   double LastTime =
      std::max(mTracks->GetEndTime(), mViewInfo.selectedRegion.t1());

   const double screen = GetScreenEndTime() - mViewInfo.h;
   const double halfScreen = screen / 2.0;

   // If we can scroll beyond zero,
   // Add 1/2 of a screen of blank space to the end
   // and another 1/2 screen before the beginning
   // so that any point within the union of the selection and the track duration
   // may be scrolled to the midline.
   // May add even more to the end, so that you can always scroll the starting time to zero.
   const double lowerBound = ScrollingLowerBoundTime();
   const double additional = MayScrollBeyondZero()
      ? -lowerBound + std::max(halfScreen, screen - LastTime)
      : screen / 4.0;

   mViewInfo.total = LastTime + additional;

   // Don't remove time from total that's still on the screen
   mViewInfo.total = std::max(mViewInfo.total, mViewInfo.h + screen);

   if (mViewInfo.h < lowerBound) {
      mViewInfo.h = lowerBound;
      rescroll = true;
   }

   mViewInfo.sbarTotal = (wxInt64) (mViewInfo.GetTotalWidth());
   mViewInfo.sbarScreen = (wxInt64)(panelWidth);
   mViewInfo.sbarH = (wxInt64) (mViewInfo.GetBeforeScreenWidth());

   int lastv = mViewInfo.vpos;
   // PRL:  Can someone else find a more elegant solution to bug 812, than
   // introducing this boolean member variable?
   // Setting mVSbar earlier, int HandlXMLTag, didn't succeed in restoring
   // the vertical scrollbar to its saved position.  So defer that till now.
   // mbInitializingScrollbar should be true only at the start of the life
   // of an AudacityProject reopened from disk.
   if (!mbInitializingScrollbar) {
      mViewInfo.vpos = mVsbar->GetThumbPosition() * mViewInfo.scrollStep;
   }
   mbInitializingScrollbar = false;

   if (mViewInfo.vpos >= totalHeight)
      mViewInfo.vpos = totalHeight - 1;
   if (mViewInfo.vpos < 0)
      mViewInfo.vpos = 0;

   bool oldhstate;
   bool oldvstate;
   bool newhstate = (GetScreenEndTime() - mViewInfo.h) < mViewInfo.total;
   bool newvstate = panelHeight < totalHeight;

#ifdef __WXGTK__
   oldhstate = mHsbar->IsShown();
   oldvstate = mVsbar->IsShown();
   mHsbar->Show(newhstate);
   mVsbar->Show(panelHeight < totalHeight);
#else
   oldhstate = mHsbar->IsEnabled();
   oldvstate = mVsbar->IsEnabled();
   mHsbar->Enable(newhstate);
   mVsbar->Enable(panelHeight < totalHeight);
#endif

   if (panelHeight >= totalHeight && mViewInfo.vpos != 0) {
      mViewInfo.vpos = 0;

      refresh = true;
      rescroll = false;
   }
   if (!newhstate && mViewInfo.sbarH != 0) {
      mViewInfo.sbarH = 0;

      refresh = true;
      rescroll = false;
   }

   if (lastv != mViewInfo.vpos) {
      UpdateFirstVisible();
   }

   // wxScrollbar only supports int values but we need a greater range, so
   // we scale the scrollbar coordinates on demand. We only do this if we
   // would exceed the int range, so we can always use the maximum resolution
   // available.

   // Don't use the full 2^31 max int range but a bit less, so rounding
   // errors in calculations do not overflow max int
   wxInt64 maxScrollbarRange = (wxInt64)(2147483647 * 0.999);
   if (mViewInfo.sbarTotal > maxScrollbarRange)
      mViewInfo.sbarScale = ((double)maxScrollbarRange) / mViewInfo.sbarTotal;
   else
      mViewInfo.sbarScale = 1.0; // use maximum resolution

   {
      int scaledSbarH = (int)(mViewInfo.sbarH * mViewInfo.sbarScale);
      int scaledSbarScreen = (int)(mViewInfo.sbarScreen * mViewInfo.sbarScale);
      int scaledSbarTotal = (int)(mViewInfo.sbarTotal * mViewInfo.sbarScale);
      const int offset =
         (int)(floor(0.5 + mViewInfo.sbarScale * PixelWidthBeforeTime(0.0)));

      mHsbar->SetScrollbar(scaledSbarH + offset, scaledSbarScreen, scaledSbarTotal,
         scaledSbarScreen, TRUE);
   }

   // Vertical scrollbar
   mVsbar->SetScrollbar(mViewInfo.vpos / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep,
                        totalHeight / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep, TRUE);

   if (refresh || (rescroll &&
       (GetScreenEndTime() - mViewInfo.h) < mViewInfo.total)) {
      mTrackPanel->Refresh(false);
   }

   UpdateMenus();

   if (oldhstate != newhstate || oldvstate != newvstate) {
      UpdateLayout();
   }
}

Track *AudacityProject::GetFirstVisible()
{
   if (!mViewInfo.track && GetTracks()) {
      TrackListIterator iter(GetTracks());
      for (Track *t = iter.First(); t; t = iter.Next()) {
         int y = t->GetY();
         int h = t->GetHeight();
         if (y >= mViewInfo.vpos || y + h >= mViewInfo.vpos) {
            mViewInfo.track = t;
            break;
         }
      }
   }

   return mViewInfo.track;
}

void AudacityProject::UpdateFirstVisible()
{
   if (!mViewInfo.track || !GetTracks()) {
      return;
   }

   Track *t = mViewInfo.track;
   mViewInfo.track = NULL;

   if (t->GetY() > mViewInfo.vpos) {
      while (t && t->GetY() > mViewInfo.vpos) {
         t = mTracks->GetPrev(t);
      }
   }

   while (t) {
      int y = t->GetY();
      int h = t->GetHeight();
      if (y >= mViewInfo.vpos || y + h >= mViewInfo.vpos) {
         mViewInfo.track = t;
         return;
      }
      t = mTracks->GetNext(t);
   }

   return;
}

void AudacityProject::UpdateLayout()
{
   if (!mTrackPanel)
      return;

   mToolManager->LayoutToolBars();
   Layout();

   // Retrieve size of this projects window
   wxSize mainsz = GetSize();

   // Retrieve position of the track panel to use as the size of the top
   // third of the window
   wxPoint tppos = ClientToScreen(mTrackPanel->GetParent()->GetPosition());

   // Retrieve position of bottom dock to use as the size of the bottom
   // third of the window
   wxPoint sbpos = ClientToScreen(mToolManager->GetBotDock()->GetPosition());

   // The "+ 50" is the minimum height of the TrackPanel
   SetSizeHints(250, (mainsz.y - sbpos.y) + tppos.y + 50, 20000, 20000);
}

void AudacityProject::HandleResize()
{
   if (!mTrackPanel)
      return;

   FixScrollbars();

   UpdateLayout();
}

// How many projects that do not have a name yet?
int AudacityProject::CountUnnamed()
{
   int j = 0;
   for ( size_t i = 0; i < gAudacityProjects.size(); i++) {
      if ( gAudacityProjects[i] )
         if ( gAudacityProjects[i]->GetName().IsEmpty() )
            j++;
   }
   return j;
}

void AudacityProject::RefreshAllTitles(bool bShowProjectNumbers )
{
   for ( size_t i = 0; i < gAudacityProjects.size(); i++) {
      if ( gAudacityProjects[i] ) {
         if ( !gAudacityProjects[i]->mIconized ) {
            AudacityProject * p;
            p = gAudacityProjects[i].get();
            p->SetProjectTitle( bShowProjectNumbers ? p->GetProjectNumber() : -1 );
         }
      }
   }
}

void AudacityProject::OnIconize(wxIconizeEvent &event)
{

   int VisibleProjectCount = 0;

   //JKC: On Iconizing we get called twice.  Don't know
   // why but it does no harm.
   // Should we be returning true/false rather than
   // void return?  I don't know.
   mIconized = event.IsIconized();

   unsigned int i;

   for(i=0;i<gAudacityProjects.size();i++){
      if(gAudacityProjects[i]){
         if( !gAudacityProjects[i]->mIconized )
            VisibleProjectCount++;
      }
   }

   event.Skip();
}

void AudacityProject::OnMove(wxMoveEvent & event)
{
   if (!this->IsMaximized() && !this->IsIconized())
      SetNormalizedWindowState(this->GetRect());
   event.Skip();
}

void AudacityProject::OnSize(wxSizeEvent & event)
{
   // (From Debian)
   //
   // (3.) GTK critical warning "IA__gdk_window_get_origin: assertion
   // 'GDK_IS_WINDOW (window)' failed": Received events of type wxSizeEvent
   // on the main project window cause calls to "ClientToScreen" - which is
   // not available until the window is first shown. So the class has to
   // keep track of wxShowEvent events and inhibit those actions until the
   // window is first shown.
   if (mShownOnce) {
      HandleResize();
      if (!this->IsMaximized() && !this->IsIconized())
         SetNormalizedWindowState(this->GetRect());
   }
   event.Skip();
}

void AudacityProject::OnShow(wxShowEvent & event)
{
   // Remember that the window has been shown at least once
   mShownOnce = true;

   // (From Debian...see also TrackPanel::OnTimer and AudacityTimer::Notify)
   //
   // Description: Workaround for wxWidgets bug: Reentry in clipboard
   //  The wxWidgets bug http://trac.wxwidgets.org/ticket/16636 prevents
   //  us from doing clipboard operations in wxShowEvent and wxTimerEvent
   //  processing because those event could possibly be processed during
   //  the (not sufficiently protected) Yield() of a first clipboard
   //  operation, causing reentry. Audacity had a workaround in place
   //  for this problem (the class "CaptureEvents"), which however isn't
   //  applicable with wxWidgets 3.0 because it's based on changing the
   //  gdk event handler, a change that would be overridden by wxWidgets's
   //  own gdk event handler change.
   //  Instead, as a NEW workaround, specifically protect those processings
   //  of wxShowEvent and wxTimerEvent that try to do clipboard operations
   //  from being executed within Yield(). This is done by delaying their
   //  execution by posting pure wxWidgets events - which are never executed
   //  during Yield().
   // Author: Martin Stegh  fer <martin@steghoefer.eu>
   //  Bug-Debian: https://bugs.debian.org/765341

   // the actual creation/showing of the window).
   // Post the event instead of calling OnSize(..) directly. This ensures that
   // this is a pure wxWidgets event (no GDK event behind it) and that it
   // therefore isn't processed within the YieldFor(..) of the clipboard
   // operations (workaround for Debian bug #765341).
   // QueueEvent() will take ownership of the event
   GetEventHandler()->QueueEvent(safenew wxSizeEvent(GetSize()));

   // Further processing by default handlers
   event.Skip();
}

///
///  A toolbar has been updated, so handle it like a sizing event.
///
void AudacityProject::OnToolBarUpdate(wxCommandEvent & event)
{
   HandleResize();

   event.Skip(false);             /* No need to propagate any further */
}

// The projects tracklist has been updated
void AudacityProject::OnTrackListUpdated(wxCommandEvent & event)
{
   mViewInfo.track = NULL;

   event.Skip();
}

///Handles the redrawing necessary for tasks as they partially update in the background.
void AudacityProject::OnODTaskUpdate(wxCommandEvent & WXUNUSED(event))
{
   //todo: add track data to the event - check to see if the project contains it before redrawing.
   if(mTrackPanel)
      mTrackPanel->Refresh(false);

}

//redraws the task and does other book keeping after the task is complete.
void AudacityProject::OnODTaskComplete(wxCommandEvent & WXUNUSED(event))
{
  if(mTrackPanel)
      mTrackPanel->Refresh(false);
 }

void AudacityProject::OnScroll(wxScrollEvent & WXUNUSED(event))
{
   const wxInt64 offset = PixelWidthBeforeTime(0.0);
   mViewInfo.sbarH =
      (wxInt64)(mHsbar->GetThumbPosition() / mViewInfo.sbarScale) - offset;
   DoScroll();
}

void AudacityProject::DoScroll()
{
   const double lowerBound = ScrollingLowerBoundTime();

   int width;
   mTrackPanel->GetTracksUsableArea(&width, NULL);
   mViewInfo.SetBeforeScreenWidth(mViewInfo.sbarH, width, lowerBound);


   if (MayScrollBeyondZero()) {
      enum { SCROLL_PIXEL_TOLERANCE = 10 };
      if (std::abs(mViewInfo.TimeToPosition(0.0, 0
                                   )) < SCROLL_PIXEL_TOLERANCE) {
         // Snap the scrollbar to 0
         mViewInfo.h = 0;
         SetHorizontalThumb(0.0);
      }
   }

   int lastv = mViewInfo.vpos;
   mViewInfo.vpos = mVsbar->GetThumbPosition() * mViewInfo.scrollStep;

   if (lastv != mViewInfo.vpos) {
      UpdateFirstVisible();
   }

   //mchinen: do not always set this project to be the active one.
   //a project may autoscroll while playing in the background
   //I think this is okay since OnMouseEvent has one of these.
   //SetActiveProject(this);

   if (!mAutoScrolling) {
      mTrackPanel->Refresh(false);
   }
}

/// Determines if flags for command are compatible with current state.
/// If not, then try some recovery action to make it so.
/// @return whether compatible or not after any actions taken.
bool AudacityProject::TryToMakeActionAllowed
   ( CommandFlag & flags, CommandFlag flagsRqd, CommandFlag mask )
{
   bool bAllowed;

   if( !flags )
      flags = GetUpdateFlags();

   bAllowed = ((flags & mask) == (flagsRqd & mask));
   if( bAllowed )
      return true;

   // Why is action not allowed?
   // 1's wherever a required flag is missing.
   auto MissingFlags = (~flags & flagsRqd) & mask;

   if( mStopIfWasPaused && (MissingFlags & AudioIONotBusyFlag ) ){
      StopIfPaused();
      // Hope this will now reflect stopped audio.
      flags = GetUpdateFlags();
      bAllowed = ((flags & mask) == (flagsRqd & mask));
      if( bAllowed )
         return true;
   }

   // Why is action still not allowed?
   // 0's wherever a required flag is missing (or is don't care)
   MissingFlags = (flags & ~flagsRqd) & mask;
   // IF not set up to select all audio in that case, THEN return with failure.
   if( !mSelectAllOnNone )
      return false;

   // IF selecting all audio won't do any good, THEN return with failure.
   if( !(flags & WaveTracksExistFlag) )
      return false;
   // returns if mask wants a zero in some flag and that's not present.
   // logic seems a bit peculiar and worth revisiting.
   if( (MissingFlags & ~( TimeSelectedFlag | WaveTracksSelectedFlag)) )
      return false;

   OnSelectAll();
   flags = GetUpdateFlags();
   bAllowed = ((flags & mask) == (flagsRqd & mask));
   return bAllowed;
}

void AudacityProject::OnMenu(wxCommandEvent & event)
{

   bool handled = mCommandManager.HandleMenuID(event.GetId(),
                                               GetUpdateFlags(),
                                               NoFlagsSpecifed);

   if (handled)
      event.Skip(false);
   else
      event.Skip(true);
}

void AudacityProject::OnUpdateUI(wxUpdateUIEvent & WXUNUSED(event))
{
   UpdateMenus();
}

void AudacityProject::MacShowUndockedToolbars(bool show)
{
   (void)show;//compiler food
#ifdef __WXMAC__
   // Find all the floating toolbars, and show or hide them
   const auto &children = GetChildren();
   for(const auto &child : children) {
      if (auto frame = dynamic_cast<ToolFrame*>(child)) {
         if (!show)
            frame->Hide();
         else if (frame->GetBar() &&
                  frame->GetBar()->IsVisible())
            frame->Show();
      }
   }
#endif
}

void AudacityProject::OnActivate(wxActivateEvent & event)
{
   // Activate events can fire during window teardown, so just
   // ignore them.
   if (mIsDeleting) {
      return;
   }

   mActive = event.GetActive();

#if defined(__WXGTK__)
   // See bug #294 for explanation
   mTrackPanel->SetFocus();
#endif

   // Under Windows, focus can be "lost" when returning to
   // Audacity from a different application.
   //
   // This was observed by minimizing all windows using WINDOWS+M and
   // then ALT+TAB to return to Audacity.  Focus will be given to the
   // project window frame which is not at all useful.
   //
   // So, when the project window receives a deactivate event, we
   // remember which child had the focus.  Then, when we receive the
   // activate event, we restore that focus to the child or the track
   // panel if no child had the focus (which probably should never happen).
   if (!mActive) {
      // We only want to remember the last focused window if FindFocus() returns
      // a window within the current project frame.
      wxWindow *w = FindFocus();
      if (wxGetTopLevelParent(w) ==this) {
         mLastFocusedWindow = w;
      }
#ifdef __WXMAC__
      if (IsIconized())
         MacShowUndockedToolbars(false);
#endif
   }
   else {
      SetActiveProject(this);
      if (mLastFocusedWindow) {
         mLastFocusedWindow->SetFocus();
      }
      else {
         if (mTrackPanel) {
            mTrackPanel->SetFocus();
         }
      }
      // No longer need to remember the last focused window
      mLastFocusedWindow = NULL;

#ifdef __WXMAC__
      MacShowUndockedToolbars(true);
#endif
   }
   event.Skip();
}

bool AudacityProject::IsActive()
{
   return mActive;
}

void AudacityProject::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown())
      SetActiveProject(this);
}

// TitleRestorer restores project window titles to what they were, in its destructor.
class TitleRestorer{
public:
   TitleRestorer(AudacityProject * p ){
      if( p->IsIconized() )
         p->Restore();
      p->Raise(); // May help identifying the window on Mac

      // Construct this projects name and number.
      sProjNumber = "";
      sProjName = p->GetName();
      if (sProjName.IsEmpty()){
         sProjName = _("<untitled>");
         UnnamedCount=AudacityProject::CountUnnamed();
         if( UnnamedCount > 1 ){
            sProjNumber.Printf( "[Project %02i] ", p->GetProjectNumber()+1 );
            AudacityProject::RefreshAllTitles( true ); 
         } 
      } else {
         UnnamedCount = 0;
      }
   };
   ~TitleRestorer() { 
      if( UnnamedCount > 1 )
         AudacityProject::RefreshAllTitles( false ); 
   };
   wxString sProjNumber;
   wxString sProjName;
   int UnnamedCount;
};


// LL: All objects that have a reference to the DirManager should
//     be deleted before the final mDirManager->Deref() in this
//     routine.  Failing to do so can cause unwanted recursion
//     and/or attempts to DELETE objects twice.
void AudacityProject::OnCloseWindow(wxCloseEvent & event)
{
   // We are called for the wxEVT_CLOSE_WINDOW, wxEVT_END_SESSION, and
   // wxEVT_QUERY_END_SESSION, so we have to protect against multiple
   // entries.  This is a hack until the whole application termination
   // process can be reviewed and reworked.  (See bug #964 for ways
   // to exercise the bug that instigated this hack.)
   if (mIsBeingDeleted)
   {
      event.Skip();
      return;
   }

   if (event.CanVeto() && (::wxIsBusy() || mbBusyImporting))
   {
      event.Veto();
      return;
   }

   if (mFreqWindow) {
      mFreqWindow->Destroy();
      mFreqWindow = NULL;
   }

   if (mContrastDialog) {
      mContrastDialog->Destroy();
      mContrastDialog = NULL;
   }

   // Check to see if we were playing or recording
   // audio, and if so, make sure Audio I/O is completely finished.
   // The main point of this is to properly push the state
   // and flush the tracks once we've completely finished
   // recording NEW state.
   // This code is derived from similar code in
   // AudacityProject::~AudacityProject() and TrackPanel::OnTimer().
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) {

      // We were playing or recording audio, but we've stopped the stream.
      wxCommandEvent dummyEvent;
      GetControlToolBar()->OnStop(dummyEvent);

      FixScrollbars();
      SetAudioIOToken(0);
      RedrawProject();
   }
   else if (gAudioIO->IsMonitoring()) {
      gAudioIO->StopStream();
   }

   // MY: Use routine here so other processes can make same check
   bool bHasTracks = ProjectHasTracks();

   // We may not bother to prompt the user to save, if the
   // project is now empty.
   if (event.CanVeto() && (mEmptyCanBeDirty || bHasTracks)) {
      if (GetUndoManager()->UnsavedChanges()) {
         TitleRestorer Restorer( this );// RAII
         /* i18n-hint: The first %s numbers the project, the second %s is the project name.*/
         wxString Title =  wxString::Format(_("%sSave changes to %s?"), Restorer.sProjNumber.c_str(), Restorer.sProjName.c_str());
         wxString Message = _("Save project before closing?");
         if( !bHasTracks )
         {
          Message += _("\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nCancel, Edit > Undo until all tracks\nare open, then File > Save Project.");
         }
         int result = wxMessageBox( Message,
                                    Title,
                                   wxYES_NO | wxCANCEL | wxICON_QUESTION,
                                   this);

         if (result == wxCANCEL || (result == wxYES && !Save())) {
            event.Veto();
            return;
         }
      }
   }

   ModuleManager::Get().Dispatch(ProjectClosing);

   // Stop the timer since there's no need to update anything anymore
   mTimer.reset();

   // The project is now either saved or the user doesn't want to save it,
   // so there's no need to keep auto save info around anymore
   DeleteCurrentAutoSaveFile();

   // DMM: Save the size of the last window the user closes
   //
   // LL: Save before doing anything else to the window that might make
   //     its size change.
      SaveWindowSize();

   mLastFocusedWindow = NULL;
   mIsDeleting = true;

   // Mac: we never quit as the result of a close.
   // Other systems: we quit only when the close is the result of an external
   // command (on Windows, those are taskbar closes, "X" box, Alt+F4, etc.)
   bool quitOnClose;
#ifdef __WXMAC__
   quitOnClose = false;
#else
   quitOnClose = !mMenuClose;
#endif

   // DanH: If we're definitely about to quit, DELETE the clipboard.
   //       Doing this after Deref'ing the DirManager causes problems.
   if ((gAudacityProjects.size() == 1) && (quitOnClose || gIsQuitting))
      DeleteClipboard();

   // JKC: For Win98 and Linux do not detach the menu bar.
   // We want wxWidgets to clean it up for us.
   // TODO: Is there a Mac issue here??
   // SetMenuBar(NULL);

   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we DELETE the blockfiles
   // in memory.  After it's locked, DELETE the data structure so that
   // there's no memory leak.
   if (mLastSavedTracks) {
      TrackListIterator iter(mLastSavedTracks.get());
      Track *t = iter.First();
      while (t) {
         if (t->GetKind() == Track::Wave)
            ((WaveTrack *) t)->CloseLock();
         t = iter.Next();
      }

      mLastSavedTracks->Clear(); // sends an event
      mLastSavedTracks.reset();
   }

   // Get rid of the history window
   // LL:  Destroy it before the TrackPanel and ToolBars since they
   //      may/will get additional wxEVT_PAINT events since window
   //      destruction may be queued.  This seems to only be a problem
   //      on the Mac.
   if (mHistoryWindow) {
      mHistoryWindow->Destroy();
      mHistoryWindow = NULL;
   }

   // Destroy the TrackPanel early so it's not around once we start
   // deleting things like tracks and such out from underneath it.
   // Check validity of mTrackPanel per bug 584 Comment 1.
   // Deeper fix is in the Import code, but this failsafes against crash.
   if (mTrackPanel)
   {
      mTrackPanel->Destroy();
      mTrackPanel = NULL;              // Make sure this gets set...see HandleResize()
   }

   // Delete the tool manager before the children since it needs
   // to save the state of the toolbars.
   mToolManager.reset();

   DestroyChildren();

   mTrackFactory.reset();

   mTags.reset();

   mImportXMLTagHandler.reset();

   // Unregister for tracklist updates
   mTracks->Disconnect(EVT_TRACKLIST_UPDATED,
                       wxCommandEventHandler(AudacityProject::OnTrackListUpdated),
                       NULL,
                       this);

   // Delete all the tracks to free up memory and DirManager references.
   mTracks->Clear();
   mTracks.reset();

   // This must be done before the following Deref() since it holds
   // references to the DirManager.
   GetUndoManager()->ClearStates();

   // MM: Tell the DirManager it can now DELETE itself
   // if it finds it is no longer needed. If it is still
   // used (f.e. by the clipboard), it will recognize this
   // and will destroy itself later.
   //
   // LL: All objects with references to the DirManager should
   //     have been deleted before this.
   mDirManager.reset();

   AProjectHolder pSelf;
   {
      ODLocker locker{ &AudacityProject::AllProjectDeleteMutex() };
      auto end = gAudacityProjects.end();
      auto it = std::find_if(gAudacityProjects.begin(), end,
         [this] (const AProjectHolder &p) { return p.get() == this; });
      wxASSERT( it != end );
      pSelf = std::move( *it );
      gAudacityProjects.erase(it);
   }

   if (gActiveProject == this) {
      // Find a NEW active project
      if (gAudacityProjects.size() > 0) {
         SetActiveProject(gAudacityProjects[0].get());
      }
      else {
         SetActiveProject(NULL);
      }
   }

   // Since we're going to be destroyed, make sure we're not to
   // receive audio notifications anymore.
   if (gAudioIO->GetListener() == this) {
      gAudioIO->SetListener(gActiveProject);
   }

   if (gAudacityProjects.empty() && !gIsQuitting) {

#if !defined(__WXMAC__)
      if (quitOnClose) {
         QuitAudacity();
      }
      else {
         wxGetApp().SetWindowRectAlreadySaved(FALSE);
         CreateNewAudacityProject();
      }
#endif
   }

   this->Disconnect(EVT_TRACK_PANEL_TIMER,
      wxCommandEventHandler(ViewInfo::OnTimer),
      NULL,
      &mViewInfo);

   // Destroys this
   pSelf.reset();
   mRuler = nullptr;

   mIsBeingDeleted = true;

}

void AudacityProject::OnOpenAudioFile(wxCommandEvent & event)
{
   const wxString &cmd = event.GetString();

   if (!cmd.IsEmpty()) {
      OpenFile(cmd);
   }

   RequestUserAttention();
}

// static method, can be called outside of a project
wxArrayString AudacityProject::ShowOpenDialog(const wxString &extraformat, const wxString &extrafilter)
{
   FormatList l;
   wxString filter;  ///< List of file format names and extensions, separated
   /// by | characters between _formats_ and extensions for each _format_, i.e.
   /// format1name | *.ext | format2name | *.ex1;*.ex2
   wxString all;  ///< One long list of all supported file extensions,
   /// semicolon separated

   if (extraformat != wxEmptyString)
   {  // additional format specified
      all = extrafilter + wxT(';');
      // add it to the "all supported files" filter string
   }

   // Construct the filter
   Importer::Get().GetSupportedImportFormats(&l);

   for (const auto &format : l) {
      /* this loop runs once per supported _format_ */
      const Format *f = &format;

      wxString newfilter = f->formatName + wxT("|");
      // bung format name into string plus | separator
      for (size_t i = 0; i < f->formatExtensions.size(); i++) {
         /* this loop runs once per valid _file extension_ for file containing
          * the current _format_ */
         if (!newfilter.Contains(wxT("*.") + f->formatExtensions[i] + wxT(";")))
            newfilter += wxT("*.") + f->formatExtensions[i] + wxT(";");
         if (!all.Contains(wxT("*.") + f->formatExtensions[i] + wxT(";")))
            all += wxT("*.") + f->formatExtensions[i] + wxT(";");
      }
      newfilter.RemoveLast(1);
      filter += newfilter;
      filter += wxT("|");
   }
   all.RemoveLast(1);
   filter.RemoveLast(1);

   // For testing long filters
#if 0
   wxString test = wxT("*.aaa;*.bbb;*.ccc;*.ddd;*.eee");
   all = test + wxT(';') + test + wxT(';') + test + wxT(';') +
         test + wxT(';') + test + wxT(';') + test + wxT(';') +
         test + wxT(';') + test + wxT(';') + test + wxT(';') +
         all;
#endif

   /* i18n-hint: The vertical bars and * are essential here.*/
   wxString mask = _("All files|*|All supported files|") +
                   all + wxT("|"); // "all" and "all supported" entries
   if (extraformat != wxEmptyString)
   {  // append caller-defined format if supplied
      mask +=  extraformat + wxT("|") + extrafilter + wxT("|");
   }
   mask += filter;   // put the names and extensions of all the importer formats
   // we built up earlier into the mask

   // Retrieve saved path and type
   wxString path = gPrefs->Read(wxT("/DefaultOpenPath"),::wxGetCwd());
   wxString type = gPrefs->Read(wxT("/DefaultOpenType"),mask.BeforeFirst(wxT('|')));

   // Convert the type to the filter index
   int index = mask.First(type + wxT("|"));
   if (index == wxNOT_FOUND) {
      index = 0;
   }
   else {
      index = mask.Left(index).Freq(wxT('|')) / 2;
      if (index < 0) {
         index = 0;
      }
   }

   // Construct and display the file dialog
   wxArrayString selected;

   FileDialog dlog(NULL,
                   _("Select one or more audio files..."),
                   path,
                   wxT(""),
                   mask,
                   wxFD_OPEN | wxFD_MULTIPLE | wxRESIZE_BORDER);

   dlog.SetFilterIndex(index);

   int dialogResult = dlog.ShowModal();

   // Convert the filter index to type and save
   index = dlog.GetFilterIndex();
   for (int i = 0; i < index; i++) {
      mask = mask.AfterFirst(wxT('|')).AfterFirst(wxT('|'));
   }
   gPrefs->Write(wxT("/DefaultOpenType"), mask.BeforeFirst(wxT('|')));
   gPrefs->Write(wxT("/LastOpenType"), mask.BeforeFirst(wxT('|')));
   gPrefs->Flush();

   if (dialogResult == wxID_OK) {
      // Return the selected files
      dlog.GetPaths(selected);
   }
   return selected;
}

// static method, can be called outside of a project
bool AudacityProject::IsAlreadyOpen(const wxString & projPathName)
{
   const wxFileName newProjPathName(projPathName);
   size_t numProjects = gAudacityProjects.size();
   for (size_t i = 0; i < numProjects; i++)
   {
      if (newProjPathName.SameAs(gAudacityProjects[i]->mFileName))
      {
         wxString errMsg =
            wxString::Format(_("%s is already open in another window."),
                              newProjPathName.GetName().c_str());
         wxLogError(errMsg);
         wxMessageBox(errMsg, _("Error Opening Project"), wxOK | wxCENTRE);
         return true;
      }
   }
   return false;
}

// static method, can be called outside of a project
void AudacityProject::OpenFiles(AudacityProject *proj)
{
   /* i18n-hint: This string is a label in the file type filter in the open
    * and save dialogues, for the option that only shows project files created
    * with Audacity. Do not include pipe symbols or .aup (this extension will
    * now be added automatically for the Save Projects dialogues).*/
   wxArrayString selectedFiles = ShowOpenDialog(_("Audacity projects"), wxT("*.aup"));
   if (selectedFiles.GetCount() == 0) {
      gPrefs->Write(wxT("/LastOpenType"),wxT(""));
      gPrefs->Flush();
      return;
   }

   //sort selected files by OD status.
   //For the open menu we load OD first so user can edit asap.
   //first sort selectedFiles.
   selectedFiles.Sort(CompareNoCaseFileName);
   ODManager::Pauser pauser;

   for (size_t ff = 0; ff < selectedFiles.GetCount(); ff++) {
      const wxString &fileName = selectedFiles[ff];

      // Make sure it isn't already open.
      if (AudacityProject::IsAlreadyOpen(fileName))
         continue; // Skip ones that are already open.

      gPrefs->Write(wxT("/DefaultOpenPath"), wxPathOnly(fileName));
      gPrefs->Flush();

      // DMM: If the project is dirty, that means it's been touched at
      // all, and it's not safe to open a NEW project directly in its
      // place.  Only if the project is brand-NEW clean and the user
      // hasn't done any action at all is it safe for Open to take place
      // inside the current project.
      //
      // If you try to Open a NEW project inside the current window when
      // there are no tracks, but there's an Undo history, etc, then
      // bad things can happen, including data files moving to the NEW
      // project directory, etc.
      if (!proj || proj->mDirty || !proj->mTracks->IsEmpty()) {
         // Open in a NEW window
         proj = CreateNewAudacityProject();
      }
      // This project is clean; it's never been touched.  Therefore
      // all relevant member variables are in their initial state,
      // and it's okay to open a NEW project inside this window.
      proj->OpenFile(fileName);
   }

   gPrefs->Write(wxT("/LastOpenType"),wxT(""));
   gPrefs->Flush();
}

// Most of this string was duplicated 3 places. Made the warning consistent in this global.
// The %s is to be filled with the version string.
static wxString gsLegacyFileWarning =
_("This file was saved by Audacity version %s. The format has changed. \
\n\nAudacity can try to open and save this file, but saving it in this \
\nversion will then prevent any 1.2 or earlier version opening it. \
\n\nAudacity might corrupt the file in opening it, so you should \
back it up first. \
\n\nOpen this file now?");

bool AudacityProject::WarnOfLegacyFile( )
{
   wxString msg;
   msg.Printf(gsLegacyFileWarning, _("1.0 or earlier"));

   // Stop icon, and choose 'NO' by default.
   int action =
      wxMessageBox(msg,
                   _("Warning - Opening Old Project File"),
                   wxYES_NO | wxICON_STOP | wxNO_DEFAULT | wxCENTRE,
                   this);
   return (action != wxNO);
}


// FIXME:? TRAP_ERR This should return a result that is checked.
//    See comment in AudacityApp::MRUOpen().
void AudacityProject::OpenFile(const wxString &fileNameArg, bool addtohistory)
{
   // On Win32, we may be given a short (DOS-compatible) file name on rare
   // occassions (e.g. stuff like "C:\PROGRA~1\AUDACI~1\PROJEC~1.AUP"). We
   // convert these to long file name first.
   wxString fileName = PlatformCompatibility::ConvertSlashInFileName(
      PlatformCompatibility::GetLongFileName(fileNameArg));

   // Make sure it isn't already open.
   // Vaughan, 2011-03-25: This was done previously in AudacityProject::OpenFiles()
   //    and AudacityApp::MRUOpen(), but if you open an aup file by double-clicking it
   //    from, e.g., Win Explorer, it would bypass those, get to here with no check,
   //    then open a NEW project from the same data with no warning.
   //    This was reported in http://bugzilla.audacityteam.org/show_bug.cgi?id=137#c17,
   //    but is not really part of that bug. Anyway, prevent it!
   if (AudacityProject::IsAlreadyOpen(fileName))
      return;


   // Data loss may occur if users mistakenly try to open ".aup.bak" files
   // left over from an unsuccessful save or by previous versions of Audacity.
   // So we always refuse to open such files.
   if (fileName.Lower().EndsWith(wxT(".aup.bak")))
   {
      wxMessageBox(
         _("You are trying to open an automatically created backup file.\nDoing this may result in severe data loss.\n\nPlease open the actual Audacity project file instead."),
         _("Warning - Backup File Detected"),
         wxOK | wxCENTRE, this);
      return;
   }

   if (!::wxFileExists(fileName)) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error Opening File"),
                   wxOK | wxCENTRE, this);
      return;
   }

   // We want to open projects using wxTextFile, but if it's NOT a project
   // file (but actually a WAV file, for example), then wxTextFile will spin
   // for a long time searching for line breaks.  So, we look for our
   // signature at the beginning of the file first:

   char buf[16];
   {
      wxFFile ff(fileName, wxT("rb"));
      if (!ff.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName,
            _("Error opening file"),
            wxOK | wxCENTRE, this);
      }
      int numRead = ff.Read(buf, 15);
      if (numRead != 15) {
         wxMessageBox(wxString::Format(_("File may be invalid or corrupted: \n%s"),
            (const wxChar*)fileName), _("Error Opening File or Project"),
            wxOK | wxCENTRE, this);
         ff.Close();
         return;
      }
      buf[15] = 0;
   }

   wxString temp = LAT1CTOWX(buf);

   if (temp == wxT("AudacityProject")) {
      // It's an Audacity 1.0 (or earlier) project file.
      // If they bail out, return and do no more.
      if( !WarnOfLegacyFile() )
         return;
      // Convert to the NEW format.
      bool success = ConvertLegacyProjectFile(wxFileName{ fileName });
      if (!success) {
         wxMessageBox(_("Audacity was unable to convert an Audacity 1.0 project to the new project format."),
                      _("Error Opening Project"),
                      wxOK | wxCENTRE, this);
         return;
      }
      else {
         temp = wxT("<?xml ");
      }
   }

   // FIXME: //v Surely we could be smarter about this, like checking much earlier that this is a .aup file.
   if (temp.Mid(0, 6) != wxT("<?xml ")) {
      // If it's not XML, try opening it as any other form of audio
      Import(fileName);
      return;
   }

   ///
   /// Parse project file
   ///

   mFileName = fileName;
   mbLoadedFromAup = true;

   mRecoveryAutoSaveDataDir = wxT("");
   mIsRecovered = false;

   SetProjectTitle();

   const wxString autoSaveExt = wxT(".autosave");
   if (mFileName.Length() >= autoSaveExt.Length() &&
       mFileName.Right(autoSaveExt.Length()) == autoSaveExt)
   {
      AutoSaveFile asf;
      if (!asf.Decode(fileName))
      {
         wxMessageBox(_("Could not decode file: ") + fileName,
                      _("Error decoding file"),
                      wxOK | wxCENTRE, this);
         return;
      }
   }

   XMLFileReader xmlFile;

   bool bParseSuccess = xmlFile.Parse(this, fileName);
   if (bParseSuccess) {
      // By making a duplicate set of pointers to the existing blocks
      // on disk, we add one to their reference count, guaranteeing
      // that their reference counts will never reach zero and thus
      // the version saved on disk will be preserved until the
      // user selects Save().

      bool err = false;
      Track *t;
      TrackListIterator iter(GetTracks());
      mLastSavedTracks = std::make_unique<TrackList>();

      t = iter.First();
      while (t) {
         if (t->GetErrorOpening())
         {
            wxLogWarning(
               wxT("Track %s had error reading clip values from project file."),
               t->GetName().c_str());
            err = true;
         }

         // Sanity checks for linked tracks; unsetting the linked property
         // doesn't fix the problem, but it likely leaves us with orphaned
         // blockfiles instead of much worse problems.
         if (t->GetLinked())
         {
            Track *l = t->GetLink();
            if (l)
            {
               // A linked track's partner should never itself be linked
               if (l->GetLinked())
               {
                  wxLogWarning(
                     wxT("Left track %s had linked right track %s with extra right track link.\n   Removing extra link from right track."),
                     t->GetName().c_str(), l->GetName().c_str());
                  err = true;
                  l->SetLinked(false);
               }

               // Channels should be left and right
               if ( !(  (t->GetChannel() == Track::LeftChannel &&
                           l->GetChannel() == Track::RightChannel) ||
                        (t->GetChannel() == Track::RightChannel &&
                           l->GetChannel() == Track::LeftChannel) ) )
               {
                  wxLogWarning(
                     wxT("Track %s and %s had left/right track links out of order. Setting tracks to not be linked."),
                     t->GetName().c_str(), l->GetName().c_str());
                  err = true;
                  t->SetLinked(false);
               }
            }
            else
            {
               wxLogWarning(
                  wxT("Track %s had link to NULL track. Setting it to not be linked."),
                  t->GetName().c_str());
               err = true;
               t->SetLinked(false);
            }
         }

         mLastSavedTracks->Add(t->Duplicate());
         t = iter.Next();
      }

      InitialState();
      mTrackPanel->SetFocusedTrack(iter.First());
      HandleResize();
      mTrackPanel->Refresh(false);
      mTrackPanel->Update(); // force any repaint to happen now,
      // else any asynch calls into the blockfile code will not have
      // finished logging errors (if any) before the call to ProjectFSCK()

      if (addtohistory) {
         wxGetApp().AddFileToHistory(fileName);
      }

      if (mIsRecovered)
      {
         // This project has been recovered, so write a NEW auto-save file
         // now and then DELETE the old one in the auto-save folder. Note that
         // at this point mFileName != fileName, because when opening a
         // recovered file mFileName is faked to point to the original file
         // which has been recovered, not the one in the auto-save folder.
         GetDirManager()->ProjectFSCK(err, true); // Correct problems in auto-recover mode.

         // PushState calls AutoSave(), so no longer need to do so here.
         this->PushState(_("Project was recovered"), _("Recover"));

         if (!wxRemoveFile(fileName))
            wxMessageBox(_("Could not remove old auto save file"),
                         _("Error"), wxICON_STOP, this);
      }
      else
      {
         // This is a regular project, check it and ask user
         int status = GetDirManager()->ProjectFSCK(err, false);
         if (status & FSCKstatus_CLOSE_REQ)
         {
            // Vaughan, 2010-08-23: Note this did not do a real close.
            // It could cause problems if you get this, say on missing alias files,
            // then try to open a project with, e.g., missing blockfiles.
            // It then failed in SetProject, saying it cannot find the files,
            // then never go through ProjectFSCK to give more info.
            // Going through OnClose() may be overkill, but it's safe.
            /*
               // There was an error in the load/check and the user
               // explictly opted to close the project.
               mTracks->Clear(true);
               mFileName = wxT("");
               SetProjectTitle();
               mTrackPanel->Refresh(true);
               */
            this->OnClose();
         }
         else if (status & FSCKstatus_CHANGED)
         {
            // Mark the wave tracks as changed and redraw.
            t = iter.First();
            while (t) {
               if (t->GetKind() == Track::Wave)
               {
                  // Only wave tracks have a notion of "changed".
                  for (const auto &clip: static_cast<WaveTrack*>(t)->GetClips())
                     clip->MarkChanged();
               }
               t = iter.Next();
            }
            mTrackPanel->Refresh(true);

            // Vaughan, 2010-08-20: This was bogus, as all the actions in DirManager::ProjectFSCK
            // that return FSCKstatus_CHANGED cannot be undone.
            //    this->PushState(_("Project checker repaired file"), _("Project Repair"));

            if (status & FSCKstatus_SAVE_AUP)
               this->Save();
         }
      }
   } else {
      // Vaughan, 2011-10-30:
      // See first topic at http://bugzilla.audacityteam.org/show_bug.cgi?id=451#c16.
      // Calling mTracks->Clear() with deleteTracks true results in data loss.
      mTracks->Clear(); //mTracks->Clear(true);

      mFileName = wxT("");
      SetProjectTitle();

      wxLogError(wxT("Could not parse file \"%s\". \nError: %s"), fileName.c_str(), xmlFile.GetErrorStr().c_str());
      wxMessageBox(xmlFile.GetErrorStr(),
                   _("Error Opening Project"),
                   wxOK | wxCENTRE, this);
   }

   // Clean up now unused recording recovery handler if any
   mRecordingRecoveryHandler.reset();

   if (!bParseSuccess)
      return; // No need to do further processing if parse failed.

   GetDirManager()->FillBlockfilesCache();

   //check the ODManager to see if we should add the tracks to the ODManager.
   //this flag would have been set in the HandleXML calls from above, if there were
   //OD***Blocks.
   if(ODManager::HasLoadedODFlag())
   {
      Track *tr;
      TrackListIterator triter(GetTracks());
      tr = triter.First();

      std::vector<movable_ptr<ODTask>> newTasks;
      //std::vector<ODDecodeTask*> decodeTasks;
      unsigned int createdODTasks=0;
      while (tr) {
         if (tr->GetKind() == Track::Wave) {
            //check the track for blocks that need decoding.
            //There may be more than one type e.g. FLAC/FFMPEG/lame
            unsigned int odFlags;
            odFlags=((WaveTrack*)tr)->GetODFlags();

            //add the track to the already created tasks that correspond to the od flags in the wavetrack.
            for(unsigned int i=0;i<newTasks.size();i++) {
               if(newTasks[i]->GetODType() & odFlags)
                  newTasks[i]->AddWaveTrack((WaveTrack*)tr);
            }

            //create whatever NEW tasks we need to.
            //we want at most one instance of each class for the project
            while((odFlags|createdODTasks) != createdODTasks)
            {
               movable_ptr<ODTask> newTask;
#ifdef EXPERIMENTAL_OD_FLAC
               if(!(createdODTasks&ODTask::eODFLAC) && odFlags & ODTask::eODFLAC) {
                  newTask = make_movable<ODDecodeFlacTask>();
                  createdODTasks= createdODTasks | ODTask::eODFLAC;
               }
               else
#endif
               if(!(createdODTasks&ODTask::eODPCMSummary) && odFlags & ODTask::eODPCMSummary) {
                  newTask = make_movable<ODComputeSummaryTask>();
                  createdODTasks= createdODTasks | ODTask::eODPCMSummary;
               }
               else {
                  printf("unrecognized OD Flag in block file.\n");
                  //TODO:ODTODO: display to user.  This can happen when we build audacity on a system that doesnt have libFLAC
                  break;
               }
               if(newTask)
               {
                  newTask->AddWaveTrack((WaveTrack*)tr);
                  newTasks.push_back(std::move(newTask));
               }
            }
         }
         tr = triter.Next();
      }
      for(unsigned int i=0;i<newTasks.size();i++)
         ODManager::Instance()->AddNewTask(std::move(newTasks[i]));

         //release the flag.
      ODManager::UnmarkLoadedODFlag();
   }

   // For an unknown reason, OSX requires that the project window be
   // raised if a recovery took place.
   Raise();
}

bool AudacityProject::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   bool bFileVersionFound = false;
   wxString fileVersion = _("<unrecognized version -- possibly corrupt project file>");
   wxString audacityVersion = _("<unrecognized version -- possibly corrupt project file>");
   int requiredTags = 0;
   long longVpos = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value || !XMLValueChecker::IsGoodString(value))
         break;

      if (mViewInfo.ReadXMLAttribute(attr, value)) {
         // We need to save vpos now and restore it below
         longVpos = std::max(longVpos, long(mViewInfo.vpos));
         continue;
      }

      if (!wxStrcmp(attr, wxT("datadir")))
      {
         //
         // This is an auto-saved version whose data is in another directory
         //
         // Note: This attribute must currently be written and parsed before
         //       any other attributes
         //
         if ((value[0] != 0) && XMLValueChecker::IsGoodPathString(value))
         {
            // Remember that this is a recovered project
            mIsRecovered = true;
            mRecoveryAutoSaveDataDir = value;
         }
      }

      else if (!wxStrcmp(attr, wxT("version")))
      {
         fileVersion = value;
         bFileVersionFound = true;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("audacityversion"))) {
         audacityVersion = value;
         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("projname"))) {
         wxString projName;
         wxString projPath;

         if (mIsRecovered) {
            // Fake the filename as if we had opened the original file
            // (which was lost by the crash) rather than the one in the
            // auto save folder
            wxFileName realFileDir;
            realFileDir.AssignDir(mRecoveryAutoSaveDataDir);
            realFileDir.RemoveLastDir();

            wxString realFileName = value;
            if (realFileName.Length() >= 5 &&
                realFileName.Right(5) == wxT("_data"))
            {
               realFileName = realFileName.Left(realFileName.Length() - 5);
            }

            if (realFileName.IsEmpty())
            {
               // A previously unsaved project has been recovered, so fake
               // an unsaved project. The data files just stay in the temp
               // directory
               mDirManager->SetLocalTempDir(mRecoveryAutoSaveDataDir);
               mFileName = wxT("");
               projName = wxT("");
               projPath = wxT("");
            } else
            {
               realFileName += wxT(".aup");
               projPath = realFileDir.GetFullPath();
               mFileName = wxFileName(projPath, realFileName).GetFullPath();
               mbLoadedFromAup = true;
               projName = value;
            }

            SetProjectTitle();
         } else {
            projName = value;
            projPath = wxPathOnly(mFileName);
         }

         if (!projName.IsEmpty())
         {
            // First try to load the data files based on the _data dir given in the .aup file
            // If this fails then try to use the filename of the .aup as the base directory
            // This is because unzipped projects e.g. those that get transfered between mac-pc
            // have encoding issues and end up expanding the wrong filenames for certain
            // international characters (such as capital 'A' with an umlaut.)
            if (!mDirManager->SetProject(projPath, projName, false))
            {
               projName = GetName() + wxT("_data");
               if (!mDirManager->SetProject(projPath, projName, false)) {
                  wxMessageBox(wxString::Format(_("Couldn't find the project data folder: \"%s\""),
                                             projName.c_str()),
                                             _("Error Opening Project"),
                                             wxOK | wxCENTRE, this);
                  return false;
               }
            }
         }

         requiredTags++;
      }

      else if (!wxStrcmp(attr, wxT("rate"))) {
         Internat::CompatibleToDouble(value, &mRate);
         GetSelectionBar()->SetRate(mRate);
      }

      else if (!wxStrcmp(attr, wxT("snapto"))) {
         SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      else if (!wxStrcmp(attr, wxT("selectionformat")))
         SetSelectionFormat(value);

      else if (!wxStrcmp(attr, wxT("frequencyformat")))
         SetFrequencySelectionFormatName(value);

      else if (!wxStrcmp(attr, wxT("bandwidthformat")))
         SetBandwidthSelectionFormatName(value);
   } // while

   mViewInfo.UpdatePrefs();

   if (longVpos != 0) {
      // PRL: It seems this must happen after SetSnapTo
       mViewInfo.track = NULL;
       mViewInfo.vpos = longVpos;
       mbInitializingScrollbar = true;
   }

   // Specifically detect newer versions of Audacity
   // WARNING: This will need review/revision if we ever have a version string
   // such as 1.5.10, i.e. with 2 digit numbers.
   // We're able to do a shortcut and use string comparison because we know
   // that does not happen.

   if (!bFileVersionFound ||
         (fileVersion.Length() != 5) || // expecting '1.1.0', for example
         !XMLValueChecker::IsGoodInt(fileVersion) ||
         (fileVersion > wxT(AUDACITY_FILE_FORMAT_VERSION)))
   {
      wxString msg;
      /* i18n-hint: %s will be replaced by the version number.*/
      msg.Printf(_("This file was saved using Audacity %s.\nYou are using Audacity %s. You may need to upgrade to a newer version to open this file."),
                 audacityVersion.c_str(),
                 AUDACITY_VERSION_STRING);
      wxMessageBox(msg,
                   _("Can't open project file"),
                   wxOK | wxICON_EXCLAMATION | wxCENTRE, this);
      return false;
   }

   // NOTE: It looks as if there was some confusion about fileversion and audacityversion.
   // fileversion NOT being increased when file formats changed, so unfortunately we're
   // using audacityversion to rescue the situation.

   // KLUDGE: guess the true 'fileversion' by stripping away any '-beta-Rc' stuff on
   // audacityVersion.
   // It's fairly safe to do this as it has already been established that the
   // puported file version was five chars long.
   fileVersion = audacityVersion.Mid(0,5);

   bool bIsOld = fileVersion < wxT(AUDACITY_FILE_FORMAT_VERSION);
   bool bIsVeryOld = fileVersion < wxT("1.1.9" );
   // Very old file versions could even have the file version starting
   // with text: 'AudacityProject Version 0.95'
   // Atoi return zero in this case.
   bIsVeryOld |= wxAtoi( fileVersion )==0;
   // Specifically detect older versions of Audacity
   if ( bIsOld | bIsVeryOld ) {
      wxString msg;
      msg.Printf(gsLegacyFileWarning, audacityVersion.c_str());

      int icon_choice = wxICON_EXCLAMATION;
      if( bIsVeryOld )
         // Stop icon, and choose 'NO' by default.
         icon_choice = wxICON_STOP | wxNO_DEFAULT;
      int action =
         wxMessageBox(msg,
                      _("Warning - Opening Old Project File"),
                      wxYES_NO | icon_choice | wxCENTRE,
                      this);
      if (action == wxNO)
         return false;
   }

   if (wxStrcmp(tag, wxT("audacityproject")) &&
       wxStrcmp(tag, wxT("project"))) {
      // If the tag name is not one of these two (the NEW name is
      // "project" with an Audacity namespace, but we don't detect
      // the namespace yet), then we don't know what the error is
      return false;
   }

   if (requiredTags < 3)
      return false;

   // All other tests passed, so we succeed
   return true;
}

XMLTagHandler *AudacityProject::HandleXMLChild(const wxChar *tag)
{
   if (!wxStrcmp(tag, wxT("tags"))) {
      return mTags.get();
   }

   if (!wxStrcmp(tag, wxT("wavetrack"))) {
      return mTracks->Add(mTrackFactory->NewWaveTrack());
   }

   #ifdef USE_MIDI
   if (!wxStrcmp(tag, wxT("notetrack"))) {
      return mTracks->Add(mTrackFactory->NewNoteTrack());
   }
   #endif // USE_MIDI

   if (!wxStrcmp(tag, wxT("labeltrack"))) {
      return mTracks->Add(mTrackFactory->NewLabelTrack());
   }

   if (!wxStrcmp(tag, wxT("timetrack"))) {
      return mTracks->Add(mTrackFactory->NewTimeTrack());
   }

   if (!wxStrcmp(tag, wxT("recordingrecovery"))) {
      if (!mRecordingRecoveryHandler)
         mRecordingRecoveryHandler = std::make_unique<RecordingRecoveryHandler>(this);
      return mRecordingRecoveryHandler.get();
   }

   if (!wxStrcmp(tag, wxT("import"))) {
      if (!mImportXMLTagHandler)
         mImportXMLTagHandler = std::make_unique<ImportXMLTagHandler>(this);
      return mImportXMLTagHandler.get();
   }

   return NULL;
}

void AudacityProject::WriteXMLHeader(XMLWriter &xmlFile)
{
   xmlFile.Write(wxT("<?xml "));
   xmlFile.Write(wxT("version=\"1.0\" "));
   xmlFile.Write(wxT("standalone=\"no\" "));
   xmlFile.Write(wxT("?>\n"));

   xmlFile.Write(wxT("<!DOCTYPE "));
   xmlFile.Write(wxT("project "));
   xmlFile.Write(wxT("PUBLIC "));
   xmlFile.Write(wxT("\"-//audacityproject-1.3.0//DTD//EN\" "));
   xmlFile.Write(wxT("\"http://audacity.sourceforge.net/xml/audacityproject-1.3.0.dtd\" "));
   xmlFile.Write(wxT(">\n"));
}

void AudacityProject::WriteXML(XMLWriter &xmlFile)
{
   //TIMER_START( "AudacityProject::WriteXML", xml_writer_timer );
   // Warning: This block of code is duplicated in Save, for now...
   wxString project = mFileName;
   if (project.Len() > 4 && project.Mid(project.Len() - 4) == wxT(".aup"))
      project = project.Mid(0, project.Len() - 4);
   wxString projName = wxFileNameFromPath(project) + wxT("_data");
   // End Warning -DMM

   xmlFile.StartTag(wxT("project"));
   xmlFile.WriteAttr(wxT("xmlns"), wxT("http://audacity.sourceforge.net/xml/"));

   if (mAutoSaving)
   {
      //
      // When auto-saving, remember full path to data files directory
      //
      // Note: This attribute must currently be written and parsed before
      //       all other attributes
      //
      xmlFile.WriteAttr(wxT("datadir"), mDirManager->GetDataFilesDir());

      // Note that the code at the start assumes that if mFileName has a value
      // then the file has been saved.  This is not neccessarily true when
      // autosaving as it gets set by AddImportedTracks (presumably as a proposal).
      // I don't think that mDirManager.projName gets set without a save so check that.
      if( mDirManager->GetProjectName() == wxT("") )
         projName = wxT("_data");
   }

   xmlFile.WriteAttr(wxT("projname"), projName);
   xmlFile.WriteAttr(wxT("version"), wxT(AUDACITY_FILE_FORMAT_VERSION));
   xmlFile.WriteAttr(wxT("audacityversion"), AUDACITY_VERSION_STRING);

   mViewInfo.WriteXMLAttributes(xmlFile);
   xmlFile.WriteAttr(wxT("rate"), mRate);
   xmlFile.WriteAttr(wxT("snapto"), GetSnapTo() ? wxT("on") : wxT("off"));
   xmlFile.WriteAttr(wxT("selectionformat"), GetSelectionFormat());
   xmlFile.WriteAttr(wxT("frequencyformat"), GetFrequencySelectionFormatName());
   xmlFile.WriteAttr(wxT("bandwidthformat"), GetBandwidthSelectionFormatName());

   mTags->WriteXML(xmlFile);

   Track *t;
   WaveTrack* pWaveTrack;
   TrackListIterator iter(GetTracks());
   t = iter.First();
   unsigned int ndx = 0;
   while (t) {
      if ((t->GetKind() == Track::Wave) && mWantSaveCompressed)
      {
         //vvv This should probably be a method, WaveTrack::WriteCompressedTrackXML().
         xmlFile.StartTag(wxT("import"));
         xmlFile.WriteAttr(wxT("filename"), mStrOtherNamesArray[ndx]); // Assumes mTracks order hasn't changed!

         // Don't store "channel" and "linked" tags because the importer can figure that out,
         // e.g., from stereo Ogg files.
         //    xmlFile.WriteAttr(wxT("channel"), t->GetChannel());
         //    xmlFile.WriteAttr(wxT("linked"), t->GetLinked());

         xmlFile.WriteAttr(wxT("offset"), t->GetOffset(), 8);
         xmlFile.WriteAttr(wxT("mute"), t->GetMute());
         xmlFile.WriteAttr(wxT("solo"), t->GetSolo());
         xmlFile.WriteAttr(wxT("height"), t->GetActualHeight());
         xmlFile.WriteAttr(wxT("minimized"), t->GetMinimized());

         pWaveTrack = (WaveTrack*)t;
         // Don't store "rate" tag because the importer can figure that out.
         //    xmlFile.WriteAttr(wxT("rate"), pWaveTrack->GetRate());
         xmlFile.WriteAttr(wxT("gain"), (double)pWaveTrack->GetGain());
         xmlFile.WriteAttr(wxT("pan"), (double)pWaveTrack->GetPan());
         xmlFile.EndTag(wxT("import"));

         ndx++;
         if (t->GetLinked())
            t = iter.Next();
      }
      else if (t->GetKind() == Track::Wave)
      {
         pWaveTrack = (WaveTrack*)t;
         pWaveTrack->SetAutoSaveIdent(mAutoSaving ? ++ndx : 0);
         t->WriteXML(xmlFile);
      }
      else
      {
         t->WriteXML(xmlFile);
      }

      t = iter.Next();
   }
   mStrOtherNamesArray.Clear();

   if (!mAutoSaving)
   {
      // Only write closing bracket when not auto-saving, since we may add
      // recording log data to the end of the file later
      xmlFile.EndTag(wxT("project"));
   }
   //TIMER_STOP( xml_writer_timer );

}

#if 0
// I added this to "fix" bug #334.  At that time, we were on wxWidgets 2.8.12 and
// there was a window between the closing of the "Save" progress dialog and the
// end of the actual save where the user was able to close the project window and
// recursively enter the Save code (where they could inadvertently cause the issue
// described in #334).
//
// When we converted to wx3, this "disabler" caused focus problems when returning
// to the project after the save (bug #1172) because the focus and activate events
// weren't being dispatched and the focus would get lost.
//
// After some testing, it looks like the window described above no longer exists,
// so I've disabled the disabler.  However, I'm leaving it here in case we run
// into the problem in the future.  (even though it can't be used as-is)
class ProjectDisabler
{
public:
   ProjectDisabler(wxWindow *w)
   :  mWindow(w)
   {
      mWindow->GetEventHandler()->SetEvtHandlerEnabled(false);
   }
   ~ProjectDisabler()
   {
      mWindow->GetEventHandler()->SetEvtHandlerEnabled(true);
   }
private:
   wxWindow *mWindow;
};
#endif

bool AudacityProject::Save(bool overwrite /* = true */ ,
                           bool fromSaveAs /* = false */,
                           bool bWantSaveCompressed /*= false*/)
{
   // See explanation above
   // ProjectDisabler disabler(this);

   if (bWantSaveCompressed)
      wxASSERT(fromSaveAs);
   else
   {
      TrackListIterator iter(GetTracks());
      bool bHasTracks = (iter.First() != NULL);
      if (!bHasTracks)
      {
         if (GetUndoManager()->UnsavedChanges() && mEmptyCanBeDirty) {
            int result = wxMessageBox(_("Your project is now empty.\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nClick 'No', Edit > Undo until all tracks\nare open, then File > Save Project.\n\nSave anyway?"),
                                      _("Warning - Empty Project"),
                                      wxYES_NO | wxICON_QUESTION, this);
            if (result == wxNO)
               return false;
         }
      }

      if (!fromSaveAs && mDirManager->GetProjectName() == wxT(""))
         return SaveAs();

      // If the user has recently imported dependencies, show
      // a dialog where the user can see audio files that are
      // aliased by this project.  The user may make the project
      // self-contained during this dialog, it modifies the project!
      if (mImportedDependencies)
      {
         bool bSuccess = ShowDependencyDialogIfNeeded(this, true);
         if (!bSuccess)
            return false;
         mImportedDependencies = false; // do not show again
      }
   }

   //
   // Always save a backup of the original project file
   //

   wxString safetyFileName = wxT("");
   if (wxFileExists(mFileName)) {

#ifdef __WXGTK__
      safetyFileName = mFileName + wxT("~");
#else
      safetyFileName = mFileName + wxT(".bak");
#endif

      if (wxFileExists(safetyFileName))
         wxRemoveFile(safetyFileName);

      wxRename(mFileName, safetyFileName);
   }

   if (fromSaveAs || mDirManager->GetProjectName() == wxT("")) {
      // Write the tracks.

      // This block of code is duplicated in WriteXML, for now...
      wxString project = mFileName;
      if (project.Len() > 4 && project.Mid(project.Len() - 4) == wxT(".aup"))
         project = project.Mid(0, project.Len() - 4);
      wxString projName = wxFileNameFromPath(project) + wxT("_data");
      wxString projPath = wxPathOnly(project);

      mWantSaveCompressed = bWantSaveCompressed;
      bool success = false;

      if( !wxDir::Exists( projPath ) ){
         if (safetyFileName != wxT(""))
            wxRename(safetyFileName, mFileName);
         wxMessageBox(wxString::Format(
            _("Could not save project. Path not found.  Try creating \ndirectory \"%s\" before saving project with this name."),
            projPath.c_str()),
                      _("Error Saving Project"),
                      wxICON_ERROR, this);
         return false;
      }

      if (bWantSaveCompressed)
      {
         //v Move this condition into SaveCompressedWaveTracks() if want to support other formats.
         #ifdef USE_LIBVORBIS
            success = this->SaveCompressedWaveTracks(project);
         #endif
      }
      else
      {
         // We are about to move files from the current directory to
         // the NEW directory.  We need to make sure files that belonged
         // to the last saved project don't get erased, so we "lock" them, so that
         // SetProject() copies instead of moves the files.
         // (Otherwise the NEW project would be fine, but the old one would
         // be empty of all of its files.)

         std::vector<movable_ptr<WaveTrack::Locker>> lockers;
         if (mLastSavedTracks && !overwrite) {
            lockers.reserve(mLastSavedTracks->size());
            TrackListIterator iter(mLastSavedTracks.get());
            Track *t = iter.First();
            while (t) {
               if (t->GetKind() == Track::Wave)
                  lockers.push_back(
                     make_movable<WaveTrack::Locker>(
                        static_cast<const WaveTrack*>(t)));
               t = iter.Next();
            }
         }

         // This renames the project directory, and moves or copies
         // all of our block files over.
         success = mDirManager->SetProject(projPath, projName, !overwrite);
      }

      if (!success) {
         if (safetyFileName != wxT(""))
            wxRename(safetyFileName, mFileName);
         wxMessageBox(wxString::Format(_("Could not save project. Perhaps %s \nis not writable or the disk is full."),
                                       project.c_str()),
                      _("Error Saving Project"),
                      wxICON_ERROR, this);
         return false;
      }
   }

   // Write the AUP file.
   XMLFileWriter saveFile;

   try
   {
      saveFile.Open(mFileName, wxT("wb"));

      WriteXMLHeader(saveFile);
      WriteXML(saveFile);

      saveFile.Close();
   }
   catch (const XMLFileWriterException &exception)
   {
      wxMessageBox(wxString::Format(
         _("Couldn't write to file \"%s\": %s"),
         mFileName.c_str(), exception.GetMessage().c_str()),
         _("Error Saving Project"), wxICON_ERROR);

      // When XMLWriter throws an exception, it tries to close it before,
      // so we can at least try to DELETE the incomplete file and move the
      // backup file over.
      if (safetyFileName != wxT(""))
      {
         wxRemove(mFileName);
         wxRename(safetyFileName, mFileName);
      }

      return false;
   }

   if (bWantSaveCompressed)
      mWantSaveCompressed = false; // Don't want this mode for AudacityProject::WriteXML() any more.
   else
   {
      // Now that we have saved the file, we can DELETE the auto-saved version
      DeleteCurrentAutoSaveFile();

      if (mIsRecovered)
      {
         // This was a recovered file, that is, we have just overwritten the
         // old, crashed .aup file. There may still be orphaned blockfiles in
         // this directory left over from the crash, so we DELETE them now
         mDirManager->RemoveOrphanBlockfiles();

         // Before we saved this, this was a recovered project, but now it is
         // a regular project, so remember this.
         mIsRecovered = false;
         mRecoveryAutoSaveDataDir = wxT("");
         SetProjectTitle();
      } else if (fromSaveAs)
      {
         // On save as, always remove orphaned blockfiles that may be left over
         // because the user is trying to overwrite another project
         mDirManager->RemoveOrphanBlockfiles();
      }

      if (mLastSavedTracks)
         mLastSavedTracks->Clear();
      mLastSavedTracks = std::make_unique<TrackList>();

      TrackListIterator iter(GetTracks());
      Track *t = iter.First();
      while (t) {
         mLastSavedTracks->Add(t->Duplicate());

         //only after the xml has been saved we can mark it saved.
         //thus is because the OD blockfiles change on  background thread while this is going on.
         //         if(dupT->GetKind() == Track::Wave)
         //         ((WaveTrack*)dupT)->MarkSaved();

         t = iter.Next();
      }

      GetUndoManager()->StateSaved();
   }

   // If we get here, saving the project was successful, so we can DELETE
   // the .bak file (because it now does not fit our block files anymore
   // anyway).
   if (safetyFileName != wxT(""))
      wxRemoveFile(safetyFileName);

   mStatusBar->SetStatusText(wxString::Format(_("Saved %s"),
                                              mFileName.c_str()), mainStatusBarField);

   return true;
}

#ifdef USE_LIBVORBIS
   bool AudacityProject::SaveCompressedWaveTracks(const wxString & strProjectPathName) // full path for aup except extension
   {
      // Some of this is similar to code in ExportMultiple::ExportMultipleByTrack
      // but that code is really tied into the dialogs.

      // Copy the tracks because we're going to do some state changes before exporting.
      Track* pTrack;
      WaveTrack* pWaveTrack;
      TrackListOfKindIterator iter(Track::Wave, GetTracks());
      unsigned int numWaveTracks = 0;

      TrackList pSavedTrackList;
      for (pTrack = iter.First(); pTrack != NULL; pTrack = iter.Next())
      {
         numWaveTracks++;
         pWaveTrack = (WaveTrack*)pTrack;
         pSavedTrackList.Add(mTrackFactory->DuplicateWaveTrack(*pWaveTrack));
      }

      if (numWaveTracks == 0)
         // Nothing to save compressed => success. Delete the copies and go.
         return true;

      // Okay, now some bold state-faking to default values.
      for (pTrack = iter.First(); pTrack != NULL; pTrack = iter.Next())
      {
         pWaveTrack = (WaveTrack*)pTrack;
         pWaveTrack->SetSelected(false);
         pWaveTrack->SetMute(false);
         pWaveTrack->SetSolo(false);

         pWaveTrack->SetGain(1.0);
         pWaveTrack->SetPan(0.0);
      }

      wxString strDataDirPathName = strProjectPathName + wxT("_data");
      if (!wxFileName::DirExists(strDataDirPathName) &&
            !wxFileName::Mkdir(strDataDirPathName, 0777, wxPATH_MKDIR_FULL))
         return false;
      strDataDirPathName += wxFileName::GetPathSeparator();

      // Export all WaveTracks to OGG.
      bool bSuccess = true;
      Exporter theExporter;
      Track* pRightTrack;
      wxFileName uniqueTrackFileName;
      for (pTrack = iter.First(); ((pTrack != NULL) && bSuccess); pTrack = iter.Next())
      {
         if (pTrack->GetKind() == Track::Wave)
         {
            pTrack->SetSelected(true);
            if (pTrack->GetLinked())
            {
               pRightTrack = iter.Next();
               pRightTrack->SetSelected(true);
            }
            else
               pRightTrack = NULL;

            uniqueTrackFileName = wxFileName(strDataDirPathName, pTrack->GetName(), wxT("ogg"));
            FileNames::MakeNameUnique(mStrOtherNamesArray, uniqueTrackFileName);
            bSuccess =
               theExporter.Process(this, pRightTrack ? 2 : 1,
                                    wxT("OGG"), uniqueTrackFileName.GetFullPath(), true,
                                    pTrack->GetStartTime(), pTrack->GetEndTime());

            pTrack->SetSelected(false);
            if (pRightTrack)
               pRightTrack->SetSelected(false);
         }
      }

      // Restore the saved track states and clean up.
      TrackListIterator savedTrackIter(&pSavedTrackList);
      Track *pSavedTrack;
      for (pTrack = iter.First(), pSavedTrack = savedTrackIter.First();
            ((pTrack != NULL) && (pSavedTrack != NULL));
            pTrack = iter.Next(), pSavedTrack = savedTrackIter.Next())
      {
         pWaveTrack = (WaveTrack*)pTrack;
         pWaveTrack->SetSelected(pSavedTrack->GetSelected());
         pWaveTrack->SetMute(pSavedTrack->GetMute());
         pWaveTrack->SetSolo(pSavedTrack->GetSolo());

         pWaveTrack->SetGain(((WaveTrack*)pSavedTrack)->GetGain());
         pWaveTrack->SetPan(((WaveTrack*)pSavedTrack)->GetPan());
      }

      return bSuccess;
   }
#endif


void AudacityProject::AddImportedTracks(const wxString &fileName,
                                        TrackHolders &&newTracks)
{
   const auto numTracks = newTracks.size();
   SelectNone();

   bool initiallyEmpty = mTracks->IsEmpty();
   double newRate = 0;
   wxString trackNameBase = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
   bool isLinked = false;
   int i = -1;
   for (auto &uNewTrack : newTracks) {
      ++i;

      auto newTrack = mTracks->Add(std::move(uNewTrack));
      if (newRate == 0 && newTrack->GetKind() == Track::Wave) {
         newRate = ((WaveTrack *)newTrack)->GetRate();
      }
      newTrack->SetSelected(true);
      //we need to check link status based on the first channel only.
      if(0==i)
         isLinked = newTrack->GetLinked();
      if (numTracks > 2 || (numTracks > 1 && !isLinked) ) {
         newTrack->SetName(trackNameBase + wxString::Format(wxT(" %d" ), i + 1));
      }
      else {
         newTrack->SetName(trackNameBase);
      }

      // Check if NEW track contains aliased blockfiles and if yes,
      // remember this to show a warning later
      if (newTrack->GetKind() == WaveTrack::Wave)
      {
         WaveClip* clip = ((WaveTrack*)newTrack)->GetClipByIndex(0);
         BlockArray &blocks = clip->GetSequence()->GetBlockArray();
         if (clip && blocks.size())
         {
            SeqBlock& block = blocks[0];
            if (block.f->IsAlias())
            {
               mImportedDependencies = true;
            }
         }
      }
   }

   // Automatically assign rate of imported file to whole project,
   // if this is the first file that is imported
   if (initiallyEmpty && newRate > 0) {
      mRate = newRate;
      GetSelectionBar()->SetRate(mRate);
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName.c_str()),
             _("Import"));

#if defined(__WXGTK__)
   // See bug #1224
   // The track panel hasn't we been fully created, so the OnZoomFit() will not give
   // expected results due to a window width of zero.  Should be safe to yield here to
   // allow the creattion to complete.  If this becomes a problem, it "might" be possible
   // to queue a dummy event to trigger the OnZoomFit().
   wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI | wxEVT_CATEGORY_USER_INPUT);
#endif

   OnZoomFit();

   mTrackPanel->SetFocus();
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());
   mTrackPanel->Refresh(false);

   if (initiallyEmpty && mDirManager->GetProjectName() == wxT("")) {
      wxString name = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast(wxT('.'));
      mFileName =::wxPathOnly(fileName) + wxFILE_SEP_PATH + name + wxT(".aup");
      mbLoadedFromAup = false;
      SetProjectTitle();
   }

   // Moved this call to higher levels to prevent flicker redrawing everything on each file.
   //   HandleResize();

   newTracks.clear();
}

// If pNewTrackList is passed in non-NULL, it gets filled with the pointers to NEW tracks.
bool AudacityProject::Import(const wxString &fileName, WaveTrackArray* pTrackArray /*= NULL*/)
{
   TrackHolders newTracks;
   wxString errorMessage = wxEmptyString;

   // Backup Tags, before the import.  Be prepared to roll back changes.
   struct TempTags {
      TempTags(std::shared_ptr<Tags> & pTags_)
         : pTags(pTags_)
      {
         oldTags = pTags;
         if (oldTags)
            pTags = oldTags->Duplicate();
      }

      ~TempTags()
      {
         if (oldTags) {
            // roll back
            pTags = oldTags;
         }
      }

      void Commit()
      {
         oldTags.reset();
      }

      std::shared_ptr<Tags> & pTags;
      std::shared_ptr<Tags> oldTags;
   };
   TempTags tempTags(mTags);

   bool success = Importer::Get().Import(fileName,
                                            GetTrackFactory(),
                                            newTracks,
                                            mTags.get(),
                                            errorMessage);

   if (!errorMessage.IsEmpty()) {
// Version that goes to internet...
//      ShowErrorDialog(this, _("Error Importing"),
//                 errorMessage, wxT("http://audacity.sourceforge.net/help/faq?s=files&i=wma-proprietary"));
// Version that looks locally for the text.
      ShowErrorDialog(this, _("Error Importing"),
                 errorMessage, wxT("innerlink:wma-proprietary"));
   }
   if (!success)
      return false;

   wxGetApp().AddFileToHistory(fileName);

   // no more errors
   tempTags.Commit();

   // for LOF ("list of files") files, do not import the file as if it
   // were an audio file itself
   if (fileName.AfterLast('.').IsSameAs(wxT("lof"), false)) {
      // PRL: don't redundantly do the steps below, because we already
      // did it in case of LOF, because of some weird recursion back to this
      // same function.  I think this should be untangled.

      // So Undo history push is not bypassed, despite appearances.
      return false;
   }

   // Have to set up newTrackList before calling AddImportedTracks,
   // because AddImportedTracks deletes newTracks.
   if (pTrackArray) {
      for (const auto &newTrack : newTracks) {
         if (newTrack->GetKind() == Track::Wave) {
            pTrackArray->push_back(static_cast<WaveTrack *>(newTrack.get()));
         }
      }
   }

   // PRL: Undo history is incremented inside this:
   AddImportedTracks(fileName, std::move(newTracks));

   int mode = gPrefs->Read(wxT("/AudioFiles/NormalizeOnLoad"), 0L);
   if (mode == 1) {
      //TODO: All we want is a SelectAll()
      SelectNone();
      SelectAllIfNone();
      OnEffect(EffectManager::Get().GetEffectByIdentifier(wxT("Normalize")),
               OnEffectFlags::kConfigured);
   }

   GetDirManager()->FillBlockfilesCache();
   return true;
}

bool AudacityProject::SaveAs(const wxString & newFileName, bool bWantSaveCompressed /*= false*/, bool addToHistory /*= true*/)
{
   wxString oldFileName = mFileName;

   bool bOwnsNewAupName = mbLoadedFromAup && (mFileName==newFileName);
   //check to see if the NEW project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if( !bOwnsNewAupName && wxFileExists(newFileName)) {
      wxMessageDialog m(
         NULL,
         _("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   mFileName = newFileName;
   //Don't change the title, unless we succeed.
   //SetProjectTitle();

   bool success = Save(false, true, bWantSaveCompressed);

   if (success && addToHistory) {
      wxGetApp().AddFileToHistory(mFileName);
   }
   if (!success || bWantSaveCompressed) // bWantSaveCompressed doesn't actually change current project.
   {
      // Restore file name on error
      mFileName = oldFileName;
   } else {
      mbLoadedFromAup = true;
      SetProjectTitle();
   }

   return(success);
}


bool AudacityProject::SaveAs(bool bWantSaveCompressed /*= false*/)
{
   TitleRestorer Restorer(this); // RAII

   wxFileName filename(mFileName);

   wxString sDialogTitle;
   if (bWantSaveCompressed)
   {
      if (ShowWarningDialog(this, wxT("FirstProjectSave"),
                           _("\
'Save Compressed Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n\n\
\
Compressed project files are a good way to transmit your project online, \n\
but they have some loss of fidelity.\n\n\
\
To open a compressed project takes longer than usual, as it imports \n\
each compressed track.\n"),
                           true) != wxID_OK)
         return false;
      sDialogTitle.Printf(_("%sSave Compressed Project \"%s\" As..."), Restorer.sProjNumber.c_str(),Restorer.sProjName.c_str());
   }
   else
   {
      if (ShowWarningDialog(this, wxT("FirstProjectSave"),
                           _("\
'Save Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n"),
                           true) != wxID_OK)
         return false;
      sDialogTitle.Printf(_("%sSave Project \"%s\" As..."), Restorer.sProjNumber.c_str(), Restorer.sProjName.c_str());
   }

   // JKC: I removed 'wxFD_OVERWRITE_PROMPT' because we are checking
   // for overwrite ourselves later, and we disallow it.
   // We disallow overwrite because we would have to DELETE the many
   // smaller files too, or prompt to move them.
   wxString fName = FileSelector(sDialogTitle,
                                 filename.GetPath(),
                                 filename.GetFullName(),
                                 wxT("aup"),
                                 _("Audacity projects") + wxT(" (*.aup)|*.aup"),
                                 wxFD_SAVE | wxRESIZE_BORDER,
                                 this);

   if (fName == wxT(""))
      return false;

   filename = fName;
   filename.SetExt(wxT("aup"));
   fName = filename.GetFullPath();

   bool bOwnsNewAupName = mbLoadedFromAup && (mFileName==fName);
   //check to see if the NEW project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if (!bOwnsNewAupName && filename.FileExists()) {
      wxMessageDialog m(
         NULL,
         _("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   wxString oldFileName = mFileName;
   mFileName = fName;

   bool success = Save(false, true, bWantSaveCompressed);

   if (success) {
      wxGetApp().AddFileToHistory(mFileName);
   }
   if (!success || bWantSaveCompressed) // bWantSaveCompressed doesn't actually change current project.
   {
      // Reset file name on error
      mFileName = oldFileName;
   } else {
      mbLoadedFromAup = true;
      SetProjectTitle();
   }


   return(success);
}

//
// Undo/History methods
//

void AudacityProject::InitialState()
{
   if (mImportXMLTagHandler) {
      // We processed an <import> tag, so save it as a normal project, with no <import> tags.
      this->Save();

      // Shouldn't need it any more.
      mImportXMLTagHandler.reset();
   }

   GetUndoManager()->ClearStates();

   GetUndoManager()->PushState(GetTracks(), mViewInfo.selectedRegion, mTags,
                          _("Created new project"), wxT(""));

   GetUndoManager()->StateSaved();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenuItems();

   UpdateMenus();
   this->UpdateLyrics();
   this->UpdateMixerBoard();
}

void AudacityProject::PushState(const wxString &desc, const wxString &shortDesc)
{
   PushState(desc, shortDesc, UndoPush::AUTOSAVE);
}

void AudacityProject::PushState(const wxString &desc,
                                const wxString &shortDesc,
                                UndoPush flags )
{
   GetUndoManager()->PushState(GetTracks(), mViewInfo.selectedRegion, mTags,
                          desc, shortDesc, flags);

   mDirty = true;

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenuItems();

   UpdateMenus();

   // Some state pushes, like changing a track gain control (& probably others),
   // should not repopulate Lyrics Window and MixerBoard.
   // Others, such as deleting a label or adding a wave track, obviously do.
   // Could categorize these state changes, but for now...
   // It's crucial to not do that repopulating during playback.
   if (!gAudioIO->IsStreamActive(GetAudioIOToken()))
   {
      this->UpdateLyrics();
      this->UpdateMixerBoard();
   }

   if (GetTracksFitVerticallyZoomed())
      this->DoZoomFitV();
   if((flags & UndoPush::AUTOSAVE) != UndoPush::MINIMAL)
      AutoSave();
}

void AudacityProject::RollbackState()
{
   SetStateTo(GetUndoManager()->GetCurrentState());
}

void AudacityProject::ModifyState(bool bWantsAutoSave)
{
   GetUndoManager()->ModifyState(GetTracks(), mViewInfo.selectedRegion, mTags);
   if (bWantsAutoSave)
      AutoSave();
}

// LL:  Is there a memory leak here as "l" and "t" are not deleted???
// Vaughan, 2010-08-29: No, as "l" is a TrackList* of an Undo stack state.
//    Need to keep it and its tracks "t" available for Undo/Redo/SetStateTo.
void AudacityProject::PopState(const UndoState &state)
{
   // Restore tags
   mTags = state.tags;

   TrackList *const tracks = state.tracks.get();

   mTracks->Clear();
   TrackListIterator iter(tracks);
   Track *t = iter.First();
   bool odUsed = false;
   movable_ptr<ODComputeSummaryTask> computeTask;

   while (t)
   {
      auto copyTrack = mTracks->Add(t->Duplicate());

      //add the track to OD if the manager exists.  later we might do a more rigorous check...
      if (copyTrack->GetKind() == Track::Wave)
      {
         //if the ODManager hasn't been initialized, there's no chance this track has OD blocks since this
         //is a "Redo" operation.
         //TODO: update this to look like the update loop in OpenFile that handles general purpose ODTasks.
         //BUT, it is too slow to go thru every blockfile and check the odtype, so maybe put a flag in wavetrack
         //that gets unset on OD Completion, (and we could also update the drawing there too.)  The hard part is that
         //we would need to watch every possible way a OD Blockfile could get inserted into a wavetrack and change the
         //flag there.
         if(ODManager::IsInstanceCreated())
         {
            if(!odUsed)
            {
               computeTask = make_movable<ODComputeSummaryTask>();
               odUsed=true;
            }
            computeTask->AddWaveTrack((WaveTrack*)copyTrack);
         }
      }
      t = iter.Next();
   }

   //add the task.
   if(odUsed)
      ODManager::Instance()->AddNewTask(std::move(computeTask));

   HandleResize();

   UpdateMenus();
   this->UpdateLyrics();
   this->UpdateMixerBoard();

   AutoSave();
}

void AudacityProject::SetStateTo(unsigned int n)
{
   const UndoState &state =
       GetUndoManager()->SetStateTo(n, &mViewInfo.selectedRegion);
   PopState(state);

   HandleResize();
   mTrackPanel->SetFocusedTrack(NULL);
   mTrackPanel->Refresh(false);
   ModifyUndoMenuItems();
   this->UpdateLyrics();
   this->UpdateMixerBoard();
}

void AudacityProject::UpdateLyrics()
{
   // JKC: Previously we created a lyrics window,
   // if it did not exist.  But we don't need to.
   if (!mLyricsWindow)
      return;

   TrackListOfKindIterator iter(Track::Label, GetTracks());
   LabelTrack* pLabelTrack = (LabelTrack*)(iter.First()); // Lyrics come from only the first label track.
   if (!pLabelTrack)
      return;

   // The code that updates the lyrics is rather expensive when there
   // are a lot of labels.
   // So - bail out early if the lyrics window is not visible.
   // We will later force an update when the lyrics window is made visible.
   if( !mLyricsWindow->IsVisible() )
      return;

   Lyrics* pLyricsPanel = mLyricsWindow->GetLyricsPanel();
   pLyricsPanel->Clear();
   pLyricsPanel->AddLabels(pLabelTrack);
   pLyricsPanel->Finish(pLabelTrack->GetEndTime());
   pLyricsPanel->Update(this->GetSel0());
}

void AudacityProject::UpdateMixerBoard()
{
   if (!mMixerBoard)
      return;
   mMixerBoard->UpdateTrackClusters();

   // Vaughan, 2011-01-28: AudacityProject::UpdateMixerBoard() is called on state changes,
   //   so don't really need to call UpdateMeters().
   //mMixerBoard->UpdateMeters(gAudioIO->GetStreamTime(), (mLastPlayMode == loopedPlay));
}

//
// Clipboard methods
//

//static
TrackList *AudacityProject::GetClipboardTracks()
{
   return msClipboard.get();
}

//static
void AudacityProject::DeleteClipboard()
{
   msClipboard.reset();
}

void AudacityProject::ClearClipboard()
{
   msClipT0 = 0.0;
   msClipT1 = 0.0;
   msClipProject = NULL;
   if (msClipboard) {
      msClipboard->Clear();
   }
}

void AudacityProject::Clear()
{
   TrackListIterator iter(GetTracks());

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected() || n->IsSyncLockSelected()) {
         n->Clear(mViewInfo.selectedRegion.t0(), mViewInfo.selectedRegion.t1());
      }
      n = iter.Next();
   }

   double seconds = mViewInfo.selectedRegion.duration();

   mViewInfo.selectedRegion.collapseToT0();

   PushState(wxString::Format(_("Deleted %.2f seconds at t=%.2f"),
                              seconds,
                              mViewInfo.selectedRegion.t0()),
             _("Delete"));

   RedrawProject();
}

void AudacityProject::SelectNone()
{
   TrackListIterator iter(GetTracks());
   Track *t = iter.First();
   while (t) {
      t->SetSelected(false);
      t = iter.Next();
   }
   mTrackPanel->Refresh(false);
   if (mMixerBoard)
      mMixerBoard->Refresh(false);
}

// Utility function called by other zoom methods
void AudacityProject::Zoom(double level)
{
   mViewInfo.SetZoom(level);
   FixScrollbars();
}

// Utility function called by other zoom methods
void AudacityProject::ZoomBy(double multiplier)
{
   mViewInfo.ZoomBy(multiplier);
   FixScrollbars();
}

///////////////////////////////////////////////////////////////////
// This method 'rewinds' the track, by setting the cursor to 0 and
// scrolling the window to fit 0 on the left side of it
// (maintaining  current zoom).
// If shift is held down, it will extend the left edge of the
// selection to 0 (holding right edge constant), otherwise it will
// move both left and right edge of selection to 0
///////////////////////////////////////////////////////////////////
void AudacityProject::Rewind(bool shift)
{
   mViewInfo.selectedRegion.setT0(0, false);
   if (!shift)
      mViewInfo.selectedRegion.setT1(0);

   TP_ScrollWindow(0);
}


///////////////////////////////////////////////////////////////////
// This method 'fast-forwards' the track, by setting the cursor to
// the end of the samples on the selected track and  scrolling the
//  window to fit the end on its right side (maintaining  current zoom).
// If shift is held down, it will extend the right edge of the
// selection to the end (holding left edge constant), otherwise it will
// move both left and right edge of selection to the end
///////////////////////////////////////////////////////////////////
void AudacityProject::SkipEnd(bool shift)
{
   double len = mTracks->GetEndTime();

   mViewInfo.selectedRegion.setT1(len, false);
   if (!shift)
      mViewInfo.selectedRegion.setT0(len);

   // Make sure the end of the track is visible
   mTrackPanel->ScrollIntoView(len);
   mTrackPanel->Refresh(false);
}


////////////////////////////////////////////////////////////
//  This fetches a pointer to the Transport Toolbar.  It may
//  either be docked or floating out in the open.
////////////////////////////////////////////////////////////
ControlToolBar *AudacityProject::GetControlToolBar()
{
   return (ControlToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(TransportBarID) :
           NULL);
}

ToolsToolBar * AudacityProject::TP_GetToolsToolBar()
{
   return GetToolsToolBar();
}

DeviceToolBar *AudacityProject::GetDeviceToolBar()
{
   return (DeviceToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(DeviceBarID) :
           NULL);
}

EditToolBar *AudacityProject::GetEditToolBar()
{
   return (EditToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(EditBarID) :
           NULL);
}

MixerToolBar *AudacityProject::GetMixerToolBar()
{
   return (MixerToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(MixerBarID) :
           NULL);
}

ScrubbingToolBar *AudacityProject::GetScrubbingToolBar()
{
   return dynamic_cast<ScrubbingToolBar*>
   (mToolManager ?
    mToolManager->GetToolBar(ScrubbingBarID) :
    nullptr);
}

SelectionBar *AudacityProject::GetSelectionBar()
{
   return (SelectionBar *)
      (mToolManager ?
      mToolManager->GetToolBar(SelectionBarID) :
      NULL);
}

#ifdef EXPERIMENTAL_SPECTRAL_EDITING
SpectralSelectionBar *AudacityProject::GetSpectralSelectionBar()
{
   return static_cast<SpectralSelectionBar*>(
      (mToolManager ?
      mToolManager->GetToolBar(SpectralSelectionBarID) :
      NULL));
}
#endif

ToolsToolBar *AudacityProject::GetToolsToolBar()
{
   return (ToolsToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(ToolsBarID) :
           NULL);
}

TranscriptionToolBar *AudacityProject::GetTranscriptionToolBar()
{
   return (TranscriptionToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(TranscriptionBarID) :
           NULL);
}

Meter *AudacityProject::GetPlaybackMeter()
{
   return mPlaybackMeter;
}

void AudacityProject::SetPlaybackMeter(Meter *playback)
{
   mPlaybackMeter = playback;
   if (gAudioIO)
   {
      gAudioIO->SetPlaybackMeter(this, mPlaybackMeter);
   }
}

Meter *AudacityProject::GetCaptureMeter()
{
   return mCaptureMeter;
}

void AudacityProject::SetCaptureMeter(Meter *capture)
{
   mCaptureMeter = capture;

   if (gAudioIO)
   {
      gAudioIO->SetCaptureMeter(this, mCaptureMeter);
   }
}

void AudacityProject::OnTimer(wxTimerEvent& WXUNUSED(event))
{
   MixerToolBar *mixerToolBar = GetMixerToolBar();
   if( mixerToolBar )
      mixerToolBar->UpdateControls();

   if (::wxGetUTCTime() - mLastStatusUpdateTime < 3)
      return;

   // gAudioIO->GetNumCaptureChannels() should only be positive
   // when we are recording.
   if (GetAudioIOToken() > 0 && gAudioIO->GetNumCaptureChannels() > 0) {
      wxLongLong freeSpace = mDirManager->GetFreeDiskSpace();
      if (freeSpace >= 0) {
         wxString msg;
         double recTime;
         int recMins;
         // JKC: Bug 50: Use preferences to get actual sample format.
         // However there is a slight performance impact due to Bug 1436
         // So have left the old code in that gets the size (in RAM) but 
         // #ifdeffed out.
#if 1
         sampleFormat oCaptureFormat = (sampleFormat)
               gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);
#else
         sampleFormat oCaptureFormat = gAudioIO->GetCaptureFormat();
#endif
         double bytesOnDiskPerSample = SAMPLE_SIZE_DISK(oCaptureFormat);
         recTime = freeSpace.GetHi() * 4294967296.0 + freeSpace.GetLo();
         recTime /= bytesOnDiskPerSample;
         // note size on disk (=3 for 24-bit) not in memory (=4 for 24-bit)
         recTime /= gAudioIO->GetNumCaptureChannels();
         recTime /= GetRate();
         recMins = (int)(recTime / 60.0);

         if (recMins >= 120)
            msg.Printf(_("Disk space remains for recording %d hours and %d minutes."),
                       recMins/60, recMins%60);
         else if (recMins >= 60)
            msg.Printf(_("Disk space remains for recording 1 hour and %d minutes."),
                       recMins-60);
         else if (recMins > 3)
            msg.Printf(_("Disk space remains for recording %d minutes."),
                       recMins);
         else if (recTime >= 2)
            msg.Printf(_("Disk space remains for recording %d seconds."),
                       (int)recTime);
         else
            msg.Printf(_("Out of disk space"));

         mStatusBar->SetStatusText(msg, mainStatusBarField);
      }
   }
   else if(ODManager::IsInstanceCreated())
   {
      //if we have some tasks running, we should say something about it.
      int numTasks = ODManager::Instance()->GetTotalNumTasks();
      if(numTasks)
      {
         wxString msg;
         float ratioComplete= ODManager::Instance()->GetOverallPercentComplete();

         if(ratioComplete>=1.0f)
         {
            //if we are 100 percent complete and there is still a task in the queue, we should wake the ODManager
            //so it can clear it.
            //signal the od task queue loop to wake up so it can remove the tasks from the queue and the queue if it is empty.
            ODManager::Instance()->SignalTaskQueueLoop();


            msg.Printf(_("On-demand import and waveform calculation complete."));
            mStatusBar->SetStatusText(msg, mainStatusBarField);

         }
         else if(numTasks>1)
            msg.Printf(_("Import(s) complete. Running %d on-demand waveform calculations. Overall %2.0f%% complete."),
              numTasks,ratioComplete*100.0);
         else
            msg.Printf(_("Import complete. Running an on-demand waveform calculation. %2.0f%% complete."),
             ratioComplete*100.0);


         mStatusBar->SetStatusText(msg, mainStatusBarField);
      }
   }
}

//get regions selected by selected labels
//removes unnecessary regions, overlapping regions are merged
//regions memory need to be deleted by the caller
void AudacityProject::GetRegionsByLabel( Regions &regions )
{
   TrackListIterator iter(GetTracks());
   Track *n;

   //determine labelled regions
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Label && n->GetSelected() )
      {
         LabelTrack *lt = ( LabelTrack* )n;
         for( int i = 0; i < lt->GetNumLabels(); i++ )
         {
            const LabelStruct *ls = lt->GetLabel( i );
            if( ls->selectedRegion.t0() >= mViewInfo.selectedRegion.t0() &&
                ls->selectedRegion.t1() <= mViewInfo.selectedRegion.t1() )
               regions.push_back(Region(ls->getT0(), ls->getT1()));
         }
      }

   //anything to do ?
   if( regions.size() == 0 )
      return;

   //sort and remove unnecessary regions
   std::sort(regions.begin(), regions.end());
   unsigned int selected = 1;
   while( selected < regions.size() )
   {
      const Region &cur = regions.at( selected );
      Region &last = regions.at( selected - 1 );
      if( cur.start < last.end )
      {
         if( cur.end > last.end )
            last.end = cur.end;
         regions.erase( regions.begin() + selected );
      }
      else
         selected++;
   }
}

//Executes the edit function on all selected wave tracks with
//regions specified by selected labels
//If No tracks selected, function is applied on all tracks
//If the function replaces the selection with audio of a different length,
// bSyncLockedTracks should be set true to perform the same action on sync-lock selected
// tracks.
void AudacityProject::EditByLabel( EditFunction action,
                                   bool bSyncLockedTracks )
{
   Regions regions;

   GetRegionsByLabel( regions );
   if( regions.size() == 0 )
      return;

   TrackListIterator iter(GetTracks());
   Track *n;
   bool allTracks = true;

   // if at least one wave track is selected
   // apply only on the selected track
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Wave && n->GetSelected() )
      {
         allTracks = false;
         break;
      }

   //Apply action on wavetracks starting from
   //labeled regions in the end. This is to correctly perform
   //actions like 'Delete' which collapse the track area.
   n = iter.First();
   while (n)
   {
      if ((n->GetKind() == Track::Wave) &&
            (allTracks || n->GetSelected() || (bSyncLockedTracks && n->IsSyncLockSelected())))
      {
         WaveTrack *wt = ( WaveTrack* )n;
         for (int i = (int)regions.size() - 1; i >= 0; i--) {
            const Region &region = regions.at(i);
            (wt->*action)(region.start, region.end);
         }
      }
      n = iter.Next();
   }
}

//Executes the edit function on all selected wave tracks with
//regions specified by selected labels
//If No tracks selected, function is applied on all tracks
//Functions copy the edited regions to clipboard, possibly in multiple tracks
//This probably should not be called if *action() changes the timeline, because
// the copy needs to happen by track, and the timeline change by group.
void AudacityProject::EditClipboardByLabel( EditDestFunction action )
{
   Regions regions;

   GetRegionsByLabel( regions );
   if( regions.size() == 0 )
      return;

   TrackListIterator iter(GetTracks());
   Track *n;
   bool allTracks = true;

   // if at least one wave track is selected
   // apply only on the selected track
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Wave && n->GetSelected() )
      {
         allTracks = false;
         break;
      }

   ClearClipboard();
   //Apply action on wavetracks starting from
   //labeled regions in the end. This is to correctly perform
   //actions like 'Cut' which collapse the track area.
   for( n = iter.First(); n; n = iter.Next() )
   {
      if( n->GetKind() == Track::Wave && ( allTracks || n->GetSelected() ) )
      {
         WaveTrack *wt = ( WaveTrack* )n;
         Track::Holder merged;
         for( int i = (int)regions.size() - 1; i >= 0; i-- )
         {
            const Region &region = regions.at(i);
            auto dest = ( wt->*action )( region.start, region.end );
            if( dest )
            {
               dest->SetChannel( wt->GetChannel() );
               dest->SetLinked( wt->GetLinked() );
               dest->SetName( wt->GetName() );
               if( !merged )
                  merged = std::move(dest);
               else
               {
                  // Paste to the beginning; unless this is the first region,
                  // offset the track to account for time between the regions
                  if (i < (int)regions.size() - 1)
                     merged->Offset(
                        regions.at(i + 1).start - region.end);

                  bool bResult = merged->Paste( 0.0 , dest.get() );
                  wxASSERT(bResult); // TO DO: Actually handle this.
                  wxUnusedVar(bResult);
               }
            }
            else  // nothing copied but there is a 'region', so the 'region' must be a 'point label' so offset
               if (i < (int)regions.size() - 1)
                  if (merged)
                     merged->Offset(
                        regions.at(i + 1).start - region.end);
         }
         if( merged )
            msClipboard->Add( std::move(merged) );
      }
   }

   msClipT0 = regions.front().start;
   msClipT1 = regions.back().end;
}


// TrackPanel callback method
void AudacityProject::TP_DisplayStatusMessage(const wxString &msg)
{
   mStatusBar->SetStatusText(msg, mainStatusBarField);
   mLastStatusUpdateTime = ::wxGetUTCTime();
}

// Set the status indirectly, using the command system
// (more overhead, but can be used from a non-GUI thread)
void AudacityProject::SafeDisplayStatusMessage(const wxChar *msg)
{
   auto target
      = std::make_unique<CommandOutputTarget>(TargetFactory::ProgressDefault(),
                                std::make_shared<StatusBarTarget>(*mStatusBar),
                                TargetFactory::MessageDefault());
   CommandType *type = CommandDirectory::Get()->LookUp(wxT("Message"));
   wxASSERT_MSG(type != NULL, wxT("Message command not found!"));
   CommandHolder statusCmd = type->Create(std::move(target));
   statusCmd->SetParameter(wxT("MessageString"), msg);
   ScriptCommandRelay::PostCommand(this, statusCmd);

   // Although the status hasn't actually been set yet, updating the time now
   // is probably accurate enough
   mLastStatusUpdateTime = ::wxGetUTCTime();
}

void AudacityProject::TP_DisplaySelection()
{
   double audioTime;

   if (mRuler) {
      if (!gAudioIO->IsBusy() && !mLockPlayRegion)
         mRuler->SetPlayRegion(mViewInfo.selectedRegion.t0(),
         mViewInfo.selectedRegion.t1());
      else
         // Cause ruler redraw anyway, because we may be zooming or scrolling
         mRuler->Refresh();
   }

   if (gAudioIO->IsBusy())
      audioTime = gAudioIO->GetStreamTime();
   else {
      double playEnd;
      GetPlayRegion(&audioTime, &playEnd);
   }

   GetSelectionBar()->SetTimes(mViewInfo.selectedRegion.t0(),
                               mViewInfo.selectedRegion.t1(), audioTime);
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   GetSpectralSelectionBar()->SetFrequencies
      (mViewInfo.selectedRegion.f0(), mViewInfo.selectedRegion.f1());
#endif

}


// TrackPanel access

wxSize AudacityProject::GetTPTracksUsableArea()
{
   wxSize s;
   mTrackPanel->GetTracksUsableArea(&s.x, &s.y);
   return s;
}

void AudacityProject::RefreshTPTrack(Track* pTrk, bool refreshbacking /*= true*/)
{
   mTrackPanel->RefreshTrack(pTrk, refreshbacking);
}


// TrackPanel callback method
void AudacityProject::TP_PushState(const wxString &desc, const wxString &shortDesc,
                                   UndoPush flags)
{
   PushState(desc, shortDesc, flags);
}

// TrackPanel callback method
void AudacityProject::TP_ModifyState(bool bWantsAutoSave)
{
   ModifyState(bWantsAutoSave);
}

// TrackPanel callback method
void AudacityProject::TP_ScrollLeft()
{
   OnScrollLeft();
}

// TrackPanel callback method
void AudacityProject::TP_ScrollRight()
{
   OnScrollRight();
}

// TrackPanel callback method
void AudacityProject::TP_RedrawScrollbars()
{
   FixScrollbars();
}

void AudacityProject::TP_HandleResize()
{
   HandleResize();
}

void AudacityProject::GetPlayRegion(double* playRegionStart,
                                    double *playRegionEnd)
{
   if (mRuler)
      mRuler->GetPlayRegion(playRegionStart, playRegionEnd);
   else
      *playRegionEnd = *playRegionStart = 0;
}

void AudacityProject::AutoSave()
{
   //    SonifyBeginAutoSave(); // part of RBD's r10680 stuff now backed out

   // To minimize the possibility of race conditions, we first write to a
   // file with the extension ".tmp", then rename the file to .autosave
   wxString projName;

   if (mFileName.IsEmpty())
      projName = wxT("New Project");
   else
      projName = wxFileName(mFileName).GetName();

   wxString fn = wxFileName(FileNames::AutoSaveDir(),
      projName + wxString(wxT(" - ")) + CreateUniqueName()).GetFullPath();

   try
   {
      VarSetter<bool> setter(&mAutoSaving, true, false);

      AutoSaveFile buffer;
      WriteXMLHeader(buffer);
      WriteXML(buffer);

      wxFFile saveFile;
      saveFile.Open(fn + wxT(".tmp"), wxT("wb"));
      buffer.Write(saveFile);
      saveFile.Close();
   }
   catch (const XMLFileWriterException &exception)
   {
      wxMessageBox(wxString::Format(
         _("Couldn't write to file \"%s\": %s"),
         (fn + wxT(".tmp")).c_str(), exception.GetMessage().c_str()),
         _("Error Writing Autosave File"), wxICON_ERROR, this);

      return;
   }

   // Now that we have a NEW auto-save file, DELETE the old one
   DeleteCurrentAutoSaveFile();

   if (!mAutoSaveFileName.IsEmpty())
      return; // could not remove auto-save file

   if (!wxRenameFile(fn + wxT(".tmp"), fn + wxT(".autosave")))
   {
      wxMessageBox(_("Could not create autosave file: ") + fn +
                   wxT(".autosave"), _("Error"), wxICON_STOP, this);
      return;
   }

   mAutoSaveFileName += fn + wxT(".autosave");
   // no-op cruft that's not #ifdefed for NoteTrack
   // See above for further comments.
   //   SonifyEndAutoSave();
}

void AudacityProject::DeleteCurrentAutoSaveFile()
{
   if (!mAutoSaveFileName.IsEmpty())
   {
      if (wxFileExists(mAutoSaveFileName))
      {
         if (!wxRemoveFile(mAutoSaveFileName))
         {
            wxMessageBox(_("Could not remove old autosave file: ") +
                         mAutoSaveFileName, _("Error"), wxICON_STOP, this);
            return;
         }
      }

      mAutoSaveFileName = wxT("");
   }
}


void AudacityProject::MayStartMonitoring()
{
#ifdef EXPERIMENTAL_EXTRA_MONITORING
   bool bAlwaysMonitor;
   gPrefs->Read( wxT("GUI/AlwaysMonitor"), &bAlwaysMonitor, true );
   if( !bAlwaysMonitor )
      return;

   MeterToolBar * pToolBar = GetMeterToolBar();
   if( pToolBar == NULL )
      return;
   pToolBar->StartMonitoring();
#endif
}

void AudacityProject::OnAudioIORate(int rate)
{
   wxString display;
   if (rate > 0) {
      display = wxString::Format(_("Actual Rate: %d"), rate);
   }
   else
      // clear the status field
      ;

   int x, y;
   mStatusBar->GetTextExtent(display, &x, &y);
   int widths[] = {0, GetControlToolBar()->WidthForStatusBar(mStatusBar), -1, x+50};
   mStatusBar->SetStatusWidths(4, widths);
   mStatusBar->SetStatusText(display, rateStatusBarField);
}

void AudacityProject::OnAudioIOStartRecording()
{
   // Before recording is started, auto-save the file. The file will have
   // empty tracks at the bottom where the recording will be put into
   AutoSave();
}

// This is called after recording has stopped and all tracks have flushed.
void AudacityProject::OnAudioIOStopRecording()
{
   // Only push state if we were capturing and not monitoring
   if (GetAudioIOToken() > 0)
   {
      // Add to history
      PushState(_("Recorded Audio"), _("Record"));

      // Reset timer record 
      if (IsTimerRecordCancelled())
      {
         OnUndo();
         ResetTimerRecordFlag();
      }

      // Refresh the project window
      FixScrollbars();
      RedrawProject();
   }

   // Write all cached files to disk, if any
   mDirManager->WriteCacheToDisk();

   // Now we auto-save again to get the project to a "normal" state again.
   AutoSave();
}

void AudacityProject::OnAudioIONewBlockFiles(const AutoSaveFile & blockFileLog)
{
   // New blockfiles have been created, so add them to the auto-save file
   if (!mAutoSaveFileName.IsEmpty())
   {
      wxFFile f(mAutoSaveFileName, wxT("ab"));
      if (!f.IsOpened())
         return; // Keep recording going, there's not much we can do here
      blockFileLog.Append(f);
      f.Close();
   }
}

void AudacityProject::SetSnapTo(int snap)
{
   AS_SetSnapTo(snap);
   if (GetSelectionBar()) {
      GetSelectionBar()->SetSnapTo(snap);
   }
}

int AudacityProject::GetSnapTo() const
{
   return mSnapTo;
}

bool AudacityProject::IsSyncLocked()
{
#ifdef EXPERIMENTAL_SYNC_LOCK
   return mIsSyncLocked;
#else
   return false;
#endif
}

void AudacityProject::SetSyncLock(bool flag)
{
   if (flag != mIsSyncLocked) {
      mIsSyncLocked = flag;
      if (GetTrackPanel())
         GetTrackPanel()->Refresh(false);
   }
}

void AudacityProject::DoTrackMute(Track *t, bool exclusive)
{
   HandleTrackMute(t, exclusive);

   // Update mixer board, too.
   MixerBoard* pMixerBoard = this->GetMixerBoard();
   if (pMixerBoard)
   {
      pMixerBoard->UpdateMute(); // Update for all tracks.
      pMixerBoard->UpdateSolo(); // Update for all tracks.
   }

   mTrackPanel->UpdateAccessibility();
   mTrackPanel->Refresh(false);
}

void AudacityProject::DoTrackSolo(Track *t, bool exclusive)
{
   HandleTrackSolo(t, exclusive);

   // Update mixer board, too.
   MixerBoard* pMixerBoard = this->GetMixerBoard();
   if (pMixerBoard)
   {
      pMixerBoard->UpdateMute(); // Update for all tracks.
      pMixerBoard->UpdateSolo(); // Update for all tracks.
   }

   mTrackPanel->UpdateAccessibility();
   mTrackPanel->Refresh(false);
}

void AudacityProject::SetTrackGain(WaveTrack * wt, LWSlider * slider)
{
   wxASSERT(wt);
   float newValue = slider->Get();

   // Assume linked track is wave or null
   const auto link = static_cast<WaveTrack*>(mTracks->GetLink(wt));
   wt->SetGain(newValue);
   if (link)
      link->SetGain(newValue);

   PushState(_("Adjusted gain"), _("Gain"), UndoPush::CONSOLIDATE);

   GetTrackPanel()->RefreshTrack(wt);
}

void AudacityProject::SetTrackPan(WaveTrack * wt, LWSlider * slider)
{
   wxASSERT(wt);
   float newValue = slider->Get();

   // Assume linked track is wave or null
   const auto link = static_cast<WaveTrack*>(mTracks->GetLink(wt));
   wt->SetPan(newValue);
   if (link)
      link->SetPan(newValue);

   PushState(_("Adjusted Pan"), _("Pan"), UndoPush::CONSOLIDATE);

   GetTrackPanel()->RefreshTrack(wt);
}

/// Removes the specified track.  Called from HandleClosing.
void AudacityProject::RemoveTrack(Track * toRemove)
{
   // If it was focused, reassign focus to the next or, if
   // unavailable, the previous track.
   if (mTrackPanel->GetFocusedTrack() == toRemove) {
      Track *t = mTracks->GetNext(toRemove, true);
      if (t == NULL) {
         t = mTracks->GetPrev(toRemove, true);
      }
      mTrackPanel->SetFocusedTrack(t);  // It's okay if this is NULL
   }

   wxString name = toRemove->GetName();
   Track *partner = toRemove->GetLink();

   if (toRemove->GetKind() == Track::Wave)
   {
      // Update mixer board displayed tracks.
      MixerBoard* pMixerBoard = this->GetMixerBoard();
      if (pMixerBoard)
         pMixerBoard->RemoveTrackCluster((WaveTrack*)toRemove); // Will remove partner shown in same cluster.
   }

   mTracks->Remove(toRemove);
   if (partner) {
      mTracks->Remove(partner);
   }

   if (mTracks->IsEmpty()) {
      mTrackPanel->SetFocusedTrack(NULL);
   }

   PushState(
      wxString::Format(_("Removed track '%s.'"),
      name.c_str()),
      _("Track Remove"));

   TP_RedrawScrollbars();
   HandleResize();
   GetTrackPanel()->Refresh(false);
}

void AudacityProject::HandleTrackMute(Track *t, const bool exclusive)
{
   // "exclusive" mute means mute the chosen track and unmute all others.
   if (exclusive)
   {
      TrackListIterator iter(GetTracks());
      Track *i = iter.First();
      while (i) {
         if (i == t) {
            i->SetMute(true);
            if(i->GetLinked()) { // also mute the linked track
               i = iter.Next();
               i->SetMute(true);
            }
         }
         else {
            i->SetMute(false);
         }
         i->SetSolo(false);
         i = iter.Next();
      }
   }
   else
   {
      // Normal click toggles this track.
      t->SetMute(!t->GetMute());
      if(t->GetLinked())   // set mute the same on both, if a pair
      {
         bool muted = t->GetMute();
         TrackListIterator iter(GetTracks());
         Track *i = iter.First();
         while (i != t) {  // search for this track
            i = iter.Next();
         }
         i = iter.Next();  // get the next one, since linked
         i->SetMute(muted);   // and mute it as well
      }

      if (IsSoloSimple() || IsSoloNone())
      {
         TrackListIterator iter(GetTracks());
         Track *i = iter.First();
         int nPlaying=0;

         // We also set a solo indicator if we have just one track / stereo pair playing.
         // otherwise clear solo on everything.
         while (i) {
            if( !i->GetMute())
            {
               nPlaying += 1;
               if(i->GetLinked())
                  i = iter.Next();  // don't count this one as it is linked
            }
            i = iter.Next();
         }

         i = iter.First();
         while (i) {
            i->SetSolo( (nPlaying==1) && !i->GetMute() );   // will set both of a stereo pair
            i = iter.Next();
         }
      }
   }
   ModifyState(true);
}

// Type of solo (standard or simple) follows the set preference, unless
// alternate == true, which causes the opposite behavior.
void AudacityProject::HandleTrackSolo(Track *t, const bool alternate)
{
   bool bSoloMultiple = !IsSoloSimple() ^ alternate;

   // Standard and Simple solo have opposite defaults:
   //   Standard - Behaves as individual buttons, shift=radio buttons
   //   Simple   - Behaves as radio buttons, shift=individual
   // In addition, Simple solo will mute/unmute tracks
   // when in standard radio button mode.
   if ( bSoloMultiple )
   {
      t->SetSolo( !t->GetSolo() );
      if(t->GetLinked())
      {
         bool soloed = t->GetSolo();
         TrackListIterator iter(GetTracks());
         Track *i = iter.First();
         while (i != t) {  // search for this track
            i = iter.Next();
         }
         i = iter.Next();  // get the next one, since linked
         i->SetSolo(soloed);   // and solo it as well
      }
   }
   else
   {
      // Normal click solo this track only, mute everything else.
      // OR unmute and unsolo everything.
      TrackListIterator iter(GetTracks());
      Track *i = iter.First();
      bool bWasSolo = t->GetSolo();
      while (i) {
         if( i==t )
         {
            i->SetSolo(!bWasSolo);
            if( IsSoloSimple() )
               i->SetMute(false);
            if(t->GetLinked())
            {
               i = iter.Next();
               i->SetSolo(!bWasSolo);
               if( IsSoloSimple() )
                  i->SetMute(false);
            }
         }
         else
         {
            i->SetSolo(false);
            if( IsSoloSimple() )
               i->SetMute(!bWasSolo);
         }
         i = iter.Next();
      }
   }
   ModifyState(true);
}

// Keyboard capture

// static
bool AudacityProject::HasKeyboardCapture(const wxWindow *handler)
{
   return GetKeyboardCaptureHandler() == handler;
}

// static
wxWindow *AudacityProject::GetKeyboardCaptureHandler()
{
   AudacityProject *project = GetActiveProject();
   if (project)
   {
      return project->mKeyboardCaptureHandler;
   }

   return NULL;
}

// static
void AudacityProject::CaptureKeyboard(wxWindow *handler)
{
   AudacityProject *project = GetActiveProject();
   if (project)
   {
//      wxASSERT(project->mKeyboardCaptureHandler == NULL);
      project->mKeyboardCaptureHandler = handler;
   }
}

// static
void AudacityProject::ReleaseKeyboard(wxWindow * /* handler */)
{
   AudacityProject *project = GetActiveProject();
   if (project)
   {
//      wxASSERT(project->mKeyboardCaptureHandler == handler);
      project->mKeyboardCaptureHandler = NULL;
   }

   return;
}

bool AudacityProject::ExportFromTimerRecording(wxFileName fnFile, int iFormat, int iSubFormat, int iFilterIndex)
{
   Exporter e;

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);
   return e.ProcessFromTimerRecording(this, false, 0.0, mTracks->GetEndTime(), fnFile, iFormat, iSubFormat, iFilterIndex);
}

int AudacityProject::GetOpenProjectCount() {
   return gAudacityProjects.size();
}

bool AudacityProject::IsProjectSaved() {
   wxString sProjectName = mDirManager->GetProjectName();
   return (sProjectName != wxT(""));
}

bool AudacityProject::SaveFromTimerRecording(wxFileName fnFile) {
   // MY: Will save the project to a NEW location a-la Save As
   // and then tidy up after itself.

   wxString sNewFileName = fnFile.GetFullPath();

   // MY: To allow SaveAs from Timer Recording we need to check what
   // the value of mFileName is befoer we change it.
   wxString sOldFilename = "";
   if (IsProjectSaved()) {
      sOldFilename = mFileName;
   }

   // MY: If the project file already exists then bail out
   // and send populate the message string (pointer) so
   // we can tell the user what went wrong.
   if (wxFileExists(sNewFileName)) {
      return false;
   }

   mFileName = sNewFileName;

   bool bSuccess = Save(false, true, false);

   if (bSuccess) {
      wxGetApp().AddFileToHistory(mFileName);
      mbLoadedFromAup = true;
      SetProjectTitle();
   } else  {
      // Restore file name on error
      mFileName = sOldFilename;
   }

   return bSuccess;
}

// MY: Does the project have any tracks?
bool AudacityProject::ProjectHasTracks() {
   // These two lines test for an 'empty' project.
   // of course it could still have a history at this stage.
   TrackListIterator iter2(GetTracks());
   bool bHasTracks = (iter2.First() != NULL);
   return bHasTracks;
}

// MY: This routine will give an estimate of how many
// minutes of recording time we have available.
// This is called from TimerRecordDialog::OnOK() to allow
// the user to resolve a potential disk space issue before
// Timer Recording starts.
// The calculations made are based on the user's current
// preferences.
int AudacityProject::GetEstimatedRecordingMinsLeftOnDisk() {

   // Obtain the current settings
   sampleFormat oCaptureFormat = (sampleFormat)
      gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);
   long lCaptureChannels;
   gPrefs->Read(wxT("/AudioIO/RecordChannels"), &lCaptureChannels, 2L);

   // Find out how much free space we have on disk
   wxLongLong lFreeSpace = mDirManager->GetFreeDiskSpace();
   if (lFreeSpace < 0) {
      return 0;
   }

   // Calculate the remaining time
   double dRecTime = 0.0;
   double bytesOnDiskPerSample = SAMPLE_SIZE_DISK(oCaptureFormat);
   dRecTime = lFreeSpace.GetHi() * 4294967296.0 + lFreeSpace.GetLo();
   dRecTime /= bytesOnDiskPerSample;   
   dRecTime /= lCaptureChannels;
   dRecTime /= GetRate();

   // Convert to minutes before returning
   int iRecMins = (int)(dRecTime / 60.0);
   return iRecMins;
}

AudacityProject::PlaybackScroller::PlaybackScroller(AudacityProject *project)
: mProject(project)
{
   mProject->Connect(EVT_TRACK_PANEL_TIMER,
                     wxCommandEventHandler(PlaybackScroller::OnTimer),
                     NULL,
                     this);
}

AudacityProject::PlaybackScroller::~PlaybackScroller()
{
   mProject->Disconnect(EVT_TRACK_PANEL_TIMER,
                        wxCommandEventHandler(PlaybackScroller::OnTimer),
                        NULL,
                        this);
}

void AudacityProject::PlaybackScroller::OnTimer(wxCommandEvent &event)
{
   // Let other listeners get the notification
   event.Skip();

   if(!mProject->IsAudioActive())
      return;
   else if (mMode == Mode::Refresh) {
      // PRL:  see comments in Scrubbing.cpp for why this is sometimes needed.
      // These unnecessary refreshes cause wheel rotation events to be delivered more uniformly
      // to the application, so scrub speed control is smoother.
      // (So I see at least with OS 10.10 and wxWidgets 3.0.2.)
      // Is there another way to ensure that than by refreshing?
      const auto trackPanel = mProject->GetTrackPanel();
      trackPanel->Refresh(false);
   }
   else if (mMode != Mode::Off) {
      // Pan the view, so that we center the play indicator.

      ViewInfo &viewInfo = mProject->GetViewInfo();
      TrackPanel *const trackPanel = mProject->GetTrackPanel();
      const int posX = viewInfo.TimeToPosition(viewInfo.mRecentStreamTime);
      int width;
      trackPanel->GetTracksUsableArea(&width, NULL);
      int deltaX;
      switch (mMode)
      {
         default:
            wxASSERT(false);
            /* fallthru */
         case Mode::Centered:
            deltaX = posX - width / 2;    break;
         case Mode::Right:
            deltaX = posX - width;        break;
      }
      viewInfo.h =
         viewInfo.OffsetTimeByPixels(viewInfo.h, deltaX, true);
      if (!mProject->MayScrollBeyondZero())
         // Can't scroll too far left
         viewInfo.h = std::max(0.0, viewInfo.h);
      trackPanel->Refresh(false);
   }
}
