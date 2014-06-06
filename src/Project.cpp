/**********************************************************************

  Audacity: A Digital Audio Editor

  Project.cpp

  Dominic Mazzoni
  Vaughan Johnson

*******************************************************************//**

\file Project.cpp
\brief Implements AudacityProject, DropTarget, and FileObject.
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
#include <wx/generic/filedlgg.h>
#include <wx/display.h>

#include <wx/arrimpl.cpp>       // this allows for creation of wxObjArray

#if defined(__WXMAC__)
#include <CoreServices/CoreServices.h>
#include <wx/mac/private.h>
#endif

#include "Project.h"

#include "AutoRecovery.h"
#include "AudacityApp.h"
#include "AColor.h"
#include "AudioIO.h"
#include "Dependencies.h"
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
#include "Snap.h"
#include "Tags.h"
#include "Track.h"
#include "TrackPanel.h"
#include "WaveTrack.h"
#include "DirManager.h"
#include "effects/Effect.h"
#include "prefs/PrefsDialog.h"
#include "widgets/LinkingHtmlWindow.h"
#include "widgets/ErrorDialog.h"
#include "widgets/Meter.h"
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
#include "LoadModules.h"

#include "Theme.h"
#include "AllThemeResources.h"

#include "FileDialog.h"

#include "toolbars/ToolManager.h"
#include "toolbars/ControlToolBar.h"
#include "toolbars/DeviceToolBar.h"
#include "toolbars/EditToolBar.h"
#include "toolbars/MeterToolBar.h"
#include "toolbars/MixerToolBar.h"
#include "toolbars/SelectionBar.h"
#include "toolbars/ToolsToolBar.h"
#include "toolbars/TranscriptionToolBar.h"

#include "commands/ScriptCommandRelay.h"
#include "commands/CommandDirectory.h"
#include "commands/CommandTargets.h"
#include "commands/Command.h"
#include "commands/CommandType.h"

#include "CaptureEvents.h"

#include "../images/AudacityLogoAlpha.xpm"

TrackList *AudacityProject::msClipboard = new TrackList();
double AudacityProject::msClipT0 = 0.0;
double AudacityProject::msClipT1 = 0.0;
AudacityProject *AudacityProject::msClipProject = NULL;
ODLock   *AudacityProject::msAllProjectDeleteMutex = new ODLock();

#if defined(__WXMAC__)
const int sbarSpaceWidth = 15;
const int sbarControlWidth = 16;
const int sbarExtraLen = 1;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#elif defined(__WXMSW__)
const int sbarSpaceWidth = 16;
const int sbarControlWidth = 16;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#else // wxGTK, wxMOTIF, wxX11
const int sbarSpaceWidth = 15;
const int sbarControlWidth = 15;
const int sbarExtraLen = 0;
const int sbarHjump = 30;       //STM: This is how far the thumb jumps when the l/r buttons are pressed, or auto-scrolling occurs
#include "Theme.h"
#include "AllThemeResources.h"
#endif

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
class ScrollBar:public wxScrollBar
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

private:
   DECLARE_EVENT_TABLE()
};

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
class FileObject: public wxFileDataObject
{
public:
   FileObject()
   {
   }

   bool IsSupportedFormat(const wxDataFormat & format, Direction WXUNUSED(dir = Get)) const
   {
      if (format.GetType() == wxDF_FILENAME) {
         return true;
      }

#if defined(__WXMAC__)
      if (format.GetFormatId() == kDragPromisedFlavorFindFile) {
         return true;
      }
#endif

      return false;
   }
};

class DropTarget: public wxFileDropTarget
{
public:
   DropTarget(AudacityProject *proj)
   {
      mProject = proj;

      SetDataObject(new FileObject());
   }

   ~DropTarget()
   {
   }

#if defined(__WXMAC__)
   bool GetData()
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

   bool OnDrop(wxCoord x, wxCoord y)
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
            return true;
         }
      }

      return CurrentDragHasSupportedFormat();
   }

#endif

   bool OnDropFiles(wxCoord WXUNUSED(x), wxCoord WXUNUSED(y), const wxArrayString& filenames)
   {
      //sort by OD non OD.  load Non OD first so user can start editing asap.
      wxArrayString sortednames(filenames);

      ODManager::Pause();

      sortednames.Sort(CompareNoCaseFileName);
      for (unsigned int i = 0; i < sortednames.GetCount(); i++) {

         mProject->Import(sortednames[i]);
      }
      mProject->HandleResize(); // Adjust scrollers for new track sizes.

      ODManager::Resume();

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
   if (trackArray.IsEmpty())
      return false;

   // Handle other attributes, now that we have the tracks.
   attrs++;
   const wxChar** pAttr;
   bool bSuccess = true;

   for (size_t i = 0; i < trackArray.GetCount(); i++)
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

   //Create and show a new project
   AudacityProject *p = new AudacityProject(NULL, -1,
                                            wxPoint(wndRect.x, wndRect.y),
                                            wxSize(wndRect.width, wndRect.height));

   gAudacityProjects.Add(p);

   if(bMaximized) {
      p->Maximize(true);
   }
   else if (bIconized) {
      // if the user close down and iconized state we could start back up and iconized state
      // p->Iconize(TRUE);
   }

   //Initialise the Listener
   gAudioIO->SetListener(p);

   //Set the new project as active:
   SetActiveProject(p);

   // Okay, GetActiveProject() is ready. Now we can get its CommandManager,
   // and add the shortcut keys to the tooltips.
   p->GetControlToolBar()->RegenerateToolsTooltips();

   ModuleManager::Dispatch(ProjectInitialized);

   p->Show(true);

   return p;
}

void RedrawAllProjects()
{
   size_t len = gAudacityProjects.GetCount();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->RedrawProject();
}

void RefreshCursorForAllProjects()
{
   size_t len = gAudacityProjects.GetCount();
   for (size_t i = 0; i < len; i++)
      gAudacityProjects[i]->RefreshCursor();
}

AUDACITY_DLL_API void CloseAllProjects()
{
   size_t len = gAudacityProjects.GetCount();
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

   defRect->width = 780;
   defRect->height = 580;

   //These conditional values assist in improving placement and size
   //of new windows on different platforms.
#ifdef __WXGTK__
   defRect->height += 20;
#endif

#ifdef __WXMSW__
   defRect->height += 40;
#endif
#ifdef __WXMAC__
   defRect->height += 55;
#endif
}

bool IsWindowAccessible(wxRect *requestedRect)
{
   wxDisplay display;
   wxRect targetTitleRect(requestedRect->GetLeftTop(), requestedRect->GetBottomRight());
   targetTitleRect.x += 15;
   targetTitleRect.width -= 100;
   if (targetTitleRect.width <  165) targetTitleRect.width = 165;
   targetTitleRect.height = 15;
   int targetBottom = targetTitleRect.GetBottom();
   int targetRight = targetTitleRect.GetRight();
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

// BG: Calculate where to place the next window (could be the first window)
// BG: Does not store X and Y in prefs. This is intentional.
void GetNextWindowPlacement(wxRect *nextRect, bool *pMaximized, bool *pIconized)
{
   int inc = 25;
   *pMaximized = FALSE;
   *pIconized = FALSE;
   wxRect defaultWindowRect;
   GetDefaultWindowRect(&defaultWindowRect);

   if (gAudacityProjects.IsEmpty()) {
      // Read the values from the registry, or use the defaults.
      // In version 1.3 and above, using the registry has been replaced
      // by a configuration file -- audacity.cfg. Different OSes store
      // this file in different locations.
      gPrefs->Read(wxT("/Window/Maximized"), pMaximized);
      gPrefs->Read(wxT("/Window/Iconized"), pIconized);
      if (*pMaximized || *pIconized) {
         nextRect->SetX(gPrefs->Read(wxT("/Window/Normal_X"), defaultWindowRect.GetX()));
         nextRect->SetY(gPrefs->Read(wxT("/Window/Normal_Y"), defaultWindowRect.GetY()));
         nextRect->SetWidth(gPrefs->Read(wxT("/Window/Normal_Width"), defaultWindowRect.GetWidth()));
         nextRect->SetHeight(gPrefs->Read(wxT("/Window/Normal_Height"), defaultWindowRect.GetHeight()));
      }
      else {
         nextRect->SetX(gPrefs->Read(wxT("/Window/X"), defaultWindowRect.GetX()));
         nextRect->SetY(gPrefs->Read(wxT("/Window/Y"), defaultWindowRect.GetY()));
         nextRect->SetWidth(gPrefs->Read(wxT("/Window/Width"), defaultWindowRect.GetWidth()));
         nextRect->SetHeight(gPrefs->Read(wxT("/Window/Height"), defaultWindowRect.GetHeight()));
      }
      if (!IsWindowAccessible(nextRect)) {
         nextRect->SetX(defaultWindowRect.GetX());
         nextRect->SetY(defaultWindowRect.GetY());
         nextRect->SetWidth(defaultWindowRect.GetWidth());
         nextRect->SetHeight(defaultWindowRect.GetHeight());
      }
   }
   else {
      bool validWindowSize = FALSE;
      AudacityProject * validProject = NULL;
      size_t numProjects = gAudacityProjects.Count();
      for (int i = numProjects; i > 0 ; i--)
      {
         if (!gAudacityProjects[i-1]->IsIconized()) {
             validWindowSize = TRUE;
             validProject = gAudacityProjects[i-1];
             i = 0;
         }
      }
      if (validWindowSize)
      {
         *nextRect = validProject->GetRect();
         *pMaximized = validProject->IsMaximized();
         *pIconized = validProject->IsIconized();
      }
      else
      {
          nextRect->SetX(gPrefs->Read(wxT("/Window/Normal_X"), defaultWindowRect.GetX()));
          nextRect->SetY(gPrefs->Read(wxT("/Window/Normal_Y"), defaultWindowRect.GetY()));
          nextRect->SetWidth(gPrefs->Read(wxT("/Window/Normal_Width"), defaultWindowRect.GetWidth()));
          nextRect->SetHeight(gPrefs->Read(wxT("/Window/Normal_Height"), defaultWindowRect.GetHeight()));
          gPrefs->Read(wxT("/Window/Maximized"), pMaximized);
          gPrefs->Read(wxT("/Window/Iconized"), pIconized);
      }

      //Placement depends on the increments
      nextRect->SetX(nextRect->GetX() + inc);
      nextRect->SetY(nextRect->GetY() + inc);
   }

   wxRect screenRect = wxGetClientDisplayRect();

   //Have we hit the right side of the screen?
   wxPoint bottomRight = nextRect->GetBottomRight();
   if (bottomRight.x > screenRect.GetRight()) {
      int newWidth = screenRect.GetWidth() - nextRect->GetLeft();
      if (newWidth < defaultWindowRect.GetWidth()) {
         nextRect->SetX(gPrefs->Read(wxT("/Window/X"), defaultWindowRect.GetX()));
         nextRect->SetY(gPrefs->Read(wxT("/Window/Y"), defaultWindowRect.GetY()));
         nextRect->SetWidth(gPrefs->Read(wxT("/Window/Width"), defaultWindowRect.GetWidth()));
      }
      else {
         nextRect->SetWidth(newWidth);
      }
   }
   bottomRight = nextRect->GetBottomRight();
   //Have we hit the bottom of the screen?
   if (bottomRight.y > screenRect.GetBottom()) {
      nextRect->y  -= inc;
      bottomRight = nextRect->GetBottomRight();
      if (bottomRight.y > screenRect.GetBottom()) {
         nextRect->SetBottom(screenRect.GetBottom());
      }
   }
   if (!IsWindowAccessible(nextRect)) {
      nextRect->SetX(defaultWindowRect.GetX());
      nextRect->SetY(defaultWindowRect.GetY());
      nextRect->SetWidth(defaultWindowRect.GetWidth());
      nextRect->SetHeight(defaultWindowRect.GetHeight());
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
   EVT_MENU_OPEN(AudacityProject::OnMenuEvent)
   EVT_MENU_CLOSE(AudacityProject::OnMenuEvent)
   EVT_MENU(wxID_ANY, AudacityProject::OnMenu)
   EVT_MOUSE_EVENTS(AudacityProject::OnMouseEvent)
   EVT_CLOSE(AudacityProject::OnCloseWindow)
   EVT_SIZE(AudacityProject::OnSize)
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
   EVT_COMMAND(wxID_ANY, EVT_CAPTURE_KEYBOARD, AudacityProject::OnCaptureKeyboard)
   EVT_COMMAND(wxID_ANY, EVT_RELEASE_KEYBOARD, AudacityProject::OnReleaseKeyboard)
   //mchinen:multithreaded calls - may not be threadsafe with CommandEvent: may have to change.
   EVT_COMMAND(wxID_ANY, EVT_ODTASK_UPDATE, AudacityProject::OnODTaskUpdate)
   EVT_COMMAND(wxID_ANY, EVT_ODTASK_COMPLETE, AudacityProject::OnODTaskComplete)
END_EVENT_TABLE()

AudacityProject::AudacityProject(wxWindow * parent, wxWindowID id,
                                 const wxPoint & pos,
                                 const wxSize & size)
   : wxFrame(parent, id, wxT("Audacity"), pos, size),
     mLastPlayMode(normalPlay),
     mRate((double) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), AudioIO::GetOptimalSupportedSampleRate())),
     mDefaultFormat((sampleFormat) gPrefs->
           Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample)),
     mSnapTo(gPrefs->Read(wxT("/SnapTo"), SNAP_OFF)),
     mSelectionFormat(gPrefs->Read(wxT("/SelectionFormat"), wxT(""))),
     mDirty(false),
     mTrackPanel(NULL),
     mTrackFactory(NULL),
     mAutoScrolling(false),
     mActive(true),
     mHistoryWindow(NULL),
     mLyricsWindow(NULL),
     mMixerBoard(NULL),
     mMixerBoardFrame(NULL),
     mFreqWindow(NULL),
     mAliasMissingWarningDialog(NULL),
     mToolManager(NULL),
     mbBusyImporting(false),
     mAudioIOToken(-1),
     mIsDeleting(false),
     mTracksFitVerticallyZoomed(false),  //lda
     mShowId3Dialog(true),               //lda
     mLastFocusedWindow(NULL),
     mKeyboardCaptured(NULL),
     mImportXMLTagHandler(NULL),
     mAutoSaving(false),
     mIsRecovered(false),
     mRecordingRecoveryHandler(NULL),
     mImportedDependencies(false),
     mWantSaveCompressed(false),
     mLastEffect(NULL),
     mLastEffectType(0),
     mTimerRecordCanceled(false),
     mMenuClose(false)
{
   int widths[] = {-2, -1};
   mStatusBar = CreateStatusBar(2);
   mStatusBar->SetStatusWidths(2, widths);

   wxGetApp().SetMissingAliasedFileWarningShouldShow(true);

   // MM: DirManager is created dynamically, freed on demand via ref-counting
   // MM: We don't need to Ref() here because it start with refcount=1
   mDirManager = new DirManager();

   // Create track list
   mTracks = new TrackList();
   mLastSavedTracks = NULL;

   // Register for tracklist updates
   mTracks->Connect(EVT_TRACKLIST_UPDATED,
                    wxCommandEventHandler(AudacityProject::OnTrackListUpdated),
                    NULL,
                    this);

   //
   // Initialize view info (shared with TrackPanel)
   //

   // Selection
   mViewInfo.sel0 = 0.0;
   mViewInfo.sel1 = 0.0;

   // Horizontal scrollbar
   mViewInfo.total = 1.0;
   mViewInfo.screen = 1.0;
   mViewInfo.h = 0.0;
   mViewInfo.zoom = 44100.0 / 512.0;
   mViewInfo.lastZoom = mViewInfo.zoom;

   // Vertical scrollbar
   mViewInfo.track = NULL;
   mViewInfo.vpos = 0;

   mViewInfo.scrollStep = 16;

   mViewInfo.sbarH = 0;
   mViewInfo.sbarScreen = 1;
   mViewInfo.sbarTotal = 1;
   mViewInfo.sbarScale = 1.0;

   UpdatePrefs();

   // Some extra information
   mViewInfo.bIsPlaying = false;
   mViewInfo.bRedrawWaveform = false;

   mLockPlayRegion = false;

   // Make sure valgrind sees mIsSyncLocked is initialized, even
   // though we're about to set it from prefs.
   mIsSyncLocked = false;
   gPrefs->Read(wxT("/GUI/SyncLockTracks"), &mIsSyncLocked, false);

   CreateMenusAndCommands();

   // LLL:  Read this!!!
   //
   // Until the time (and cpu) required to refresh the track panel is
   // reduced, leave the following window creations in the order specified.
   // This will place the refresh of the track panel last, allowing all
   // the others to get done quickly.
   //
   // Near as I can tell, this is only a problem under Windows.
   //

   //
   // Create the ToolDock
   //
   mToolManager = new ToolManager( this );
   GetSelectionBar()->SetListener(this);
   mToolManager->LayoutToolBars();

   // Fix the sliders on the mixer toolbar so that the tip windows
   // actually pop-up on top of everything else.  Sorry for the hack -
   // it's necessary to do it this way to avoid flicker.
#if 0
   MixerToolBar *mtb = GetMixerToolBar();
   if (mtb)
      mtb->RecreateTipWindows();
#endif

   //
   // Create the horizontal ruler
   //
   mRuler = new AdornedRulerPanel( this,
                                   wxID_ANY,
                                   wxDefaultPosition,
                                   wxSize( -1, AdornedRulerPanel::GetRulerHeight() ),
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
   mMainPanel = new wxPanel(this, -1,
      wxDefaultPosition,
      wxDefaultSize,
      wxNO_BORDER);
   mMainPanel->SetSizer( new wxBoxSizer(wxVERTICAL) );
   pPage = mMainPanel;
   // Set the colour here to the track panel background to avoid
   // flicker when Audacity starts up.
   // However, that leads to areas next to the horizontal scroller
   // being painted in background colour and not scroller background
   // colour, so suppress this for now.
   //pPage->SetBackgroundColour( theTheme.Colour( clrDark ));
#endif

   wxBoxSizer *bs = new wxBoxSizer( wxVERTICAL );
   bs->Add( mToolManager->GetTopDock(), 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );
   bs->Add( mRuler, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_CENTRE );
   bs->Add( pPage, 1, wxEXPAND | wxALIGN_LEFT );
   bs->Add( mToolManager->GetBotDock(), 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_BOTTOM );
   SetAutoLayout( true );
   SetSizer( bs );
   bs->Layout();

   // The right hand side translates to new TrackPanel(... in normal
   // Audacity without additional DLLs.
   mTrackPanel = TrackPanel::FactoryFunction(pPage,
                                             TrackPanelID,
                                             wxDefaultPosition,
                                             wxDefaultSize,
                                             mTracks,
                                             &mViewInfo,
                                             this,
                                             mRuler);

   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      creating the scrollbars after the TrackPanel, we resolve
   //      several focus problems.
   mHsbar = new ScrollBar(pPage, HSBarID, wxSB_HORIZONTAL);
   mVsbar = new ScrollBar(pPage, VSBarID, wxSB_VERTICAL);

   // LLL: When Audacity starts or becomes active after returning from
   //      another application, the first window that can accept focus
   //      will be given the focus even if we try to SetFocus().  By
   //      making the TrackPanel that first window, we resolve several
   //      keyboard focus problems.
   pPage->MoveBeforeInTabOrder(mToolManager->GetTopDock());

   bs = (wxBoxSizer *) pPage->GetSizer();

   wxBoxSizer *hs;
   wxBoxSizer *vs;

   // Top horizontal grouping
   hs = new wxBoxSizer( wxHORIZONTAL );

   // Track panel
   hs->Add( mTrackPanel, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );

   // Vertical grouping
   vs = new wxBoxSizer( wxVERTICAL );

   // Vertical scroll bar
   vs->Add( mVsbar, 1, wxEXPAND | wxALIGN_RIGHT | wxALIGN_TOP );
   hs->Add( vs, 0, wxEXPAND | wxALIGN_RIGHT | wxALIGN_TOP );
   bs->Add( hs, 1, wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP );

   // Bottom horizontal grouping
   hs = new wxBoxSizer( wxHORIZONTAL );

   // Bottom scrollbar
   hs->Add( mTrackPanel->GetLeftOffset() - 1, 0 );
   hs->Add( mHsbar, 1, wxALIGN_BOTTOM );
   hs->Add( mVsbar->GetSize().GetWidth(), 0 );
   bs->Add( hs, 0, wxEXPAND | wxALIGN_LEFT | wxALIGN_BOTTOM );

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

   InitialState();
   FixScrollbars();
   mRuler->SetLeftOffset(mTrackPanel->GetLeftOffset());  // bevel on AdornedRuler
   mRuler->SetProject(this);

   //
   // Set the Icon
   //

   // loads either the XPM or the windows resource, depending on the platform
#if !defined(__WXMAC__) && !defined(__WXX11__)
   #if defined(__WXMSW__)
      wxIcon ic(wxICON(AudacityLogo));
   #elif defined(__WXGTK__)
      wxIcon ic(wxICON(AudacityLogoAlpha));
   #else
      wxIcon ic;
      ic.CopyFromBitmap(theTheme.Bitmap(bmpAudacityLogo48x48));
   #endif
   SetIcon(ic);
#endif
   mIconized = false;

   // Create tags object
   mTags = new Tags();

   mTrackFactory = new TrackFactory(mDirManager);

   wxString msg = wxString::Format(_("Welcome to Audacity version %s"),
                                   AUDACITY_VERSION_STRING);
   mStatusBar->SetStatusText(msg);
   mLastStatusUpdateTime = ::wxGetUTCTime();

   mTimer = new wxTimer(this, AudacityProjectTimerID);
   mTimer->Start(200);

#if wxUSE_DRAG_AND_DROP
   // We can import now, so become a drag target
//   SetDropTarget(new AudacityDropTarget(this));
//   mTrackPanel->SetDropTarget(new AudacityDropTarget(this));
   mTrackPanel->SetDropTarget(new DropTarget(this));
#endif
}

AudacityProject::~AudacityProject()
{
   wxGetApp().GetRecentFiles()->RemoveMenu(mRecentFilesMenu);
}

void AudacityProject::UpdatePrefsVariables()
{
   gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &mShowId3Dialog, true);
   gPrefs->Read(wxT("/AudioFiles/NormalizeOnLoad"),&mNormalizeOnLoad, false);
   gPrefs->Read(wxT("/GUI/AutoScroll"), &mViewInfo.bUpdateTrackIndicator, true);
   gPrefs->Read(wxT("/GUI/EmptyCanBeDirty"), &mEmptyCanBeDirty, true );
   gPrefs->Read(wxT("/GUI/Help"), &mHelpPref, wxT("InBrowser") );
   gPrefs->Read(wxT("/GUI/SelectAllOnNone"), &mSelectAllOnNone, true);
   gPrefs->Read(wxT("/GUI/ShowSplashScreen"), &mShowSplashScreen, true);
   gPrefs->Read(wxT("/GUI/Solo"), &mSoloPref, wxT("Standard") );
   gPrefs->Read(wxT("/GUI/TracksFitVerticallyZoomed"), &mTracksFitVerticallyZoomed, false);
   //   gPrefs->Read(wxT("/GUI/UpdateSpectrogram"), &mViewInfo.bUpdateSpectrogram, true);

   gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"), &mRate, AudioIO::GetOptimalSupportedSampleRate());
   mDefaultFormat = (sampleFormat) gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);
}

void AudacityProject::UpdatePrefs()
{
   UpdatePrefsVariables();

   SetProjectTitle();

   if (mTrackPanel) {
      mTrackPanel->UpdatePrefs();
   }
   if (mMixerBoard)
      mMixerBoard->ResizeTrackClusters(); // in case prefs "/GUI/Solo" changed

   if (mToolManager) {
      mToolManager->UpdatePrefs();
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
   if (bForceWaveTracks && mTracks)
   {
      TrackListIterator iter(mTracks);
      Track* pTrack = iter.First();
      while (pTrack)
      {
         if (pTrack->GetKind() == Track::Wave)
         {
            WaveTrack* pWaveTrack = (WaveTrack*)pTrack;
            WaveClipList::compatibility_iterator node = pWaveTrack->GetClipIterator();
            while (node)
            {
               WaveClip *clip = node->GetData();
               clip->MarkChanged();
               node = node->GetNext();
            }
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

   mViewInfo.sel0 = newSel0;
}

void AudacityProject::SetSel1(double newSel1)
{
   //Bound checking should go on here

   mViewInfo.sel1 = newSel1;
}



DirManager *AudacityProject::GetDirManager()
{
   return mDirManager;
}

TrackFactory *AudacityProject::GetTrackFactory()
{
   return mTrackFactory;
}

AdornedRulerPanel *AudacityProject::GetRulerPanel()
{
   return mRuler;
}

int AudacityProject::GetAudioIOToken()
{
   return mAudioIOToken;
}

void AudacityProject::SetAudioIOToken(int token)
{
   mAudioIOToken = token;
}

Tags *AudacityProject::GetTags()
{
   return mTags;
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

void AudacityProject::SetProjectTitle()
{
   wxString name = GetName();
   if( name.IsEmpty() )
   {
      name = wxT("Audacity");
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
}

void AudacityProject::SetSelectionFormat(const wxString & format)
{
   AS_SetSelectionFormat(format);
   if (GetSelectionBar()) {
      GetSelectionBar()->SetSelectionFormat(format);
   }
}

const wxString & AudacityProject::GetSelectionFormat()
{
   return mSelectionFormat;
}


void AudacityProject::AS_ModifySelection(double &start, double &end, bool done)
{
   mViewInfo.sel0 = start;
   mViewInfo.sel1 = end;
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
   wxScrollEvent *dummy = new wxScrollEvent();
   OnScroll(*dummy);
   delete dummy;

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

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      FinishAutoScroll();
   }
}

///
///  This handles the event when the left direction button on the scrollbar is depresssed
///
void AudacityProject::OnScrollLeftButton(wxScrollEvent & event)
{
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   pos -= wxMax((wxInt64)(sbarHjump * mViewInfo.sbarScale), 1);
   pos = wxMax(pos, 0);

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      OnScroll(event);
   }
}

///
///  This handles  the event when the right direction button on the scrollbar is depresssed
///
void AudacityProject::OnScrollRightButton(wxScrollEvent & event)
{
   wxInt64 pos = mHsbar->GetThumbPosition();
   // move at least one scroll increment
   // use wxInt64 for calculation to prevent temporary overflow
   pos += wxMax((wxInt64)(sbarHjump * mViewInfo.sbarScale), 1);
   wxInt64 max = mHsbar->GetRange() - mHsbar->GetThumbSize();
   pos = wxMin(pos, max);

   if (pos != mHsbar->GetThumbPosition()) {
      mHsbar->SetThumbPosition((int)pos);
      OnScroll(event);
   }
}


//
// This method, like the other methods prefaced with TP, handles TrackPanel
// 'callback'.
//
void AudacityProject::TP_ScrollWindow(double scrollto)
{
   int pos = (int) (scrollto * mViewInfo.zoom * mViewInfo.sbarScale);
   int max = mHsbar->GetRange() - mHsbar->GetThumbSize();

   if (pos > max)
      pos = max;
   else if (pos < 0)
      pos = 0;

   mHsbar->SetThumbPosition(pos);

   // Call our Scroll method which updates our ViewInfo variables
   // to reflect the positions of the scrollbars
   wxScrollEvent *dummy = new wxScrollEvent();
   OnScroll(*dummy);
   delete dummy;
}

//
// Scroll vertically. This is called for example by the mouse wheel
// handler in Track Panel. A positive argument makes the window
// scroll down, while a negative argument scrolls up.
//
void AudacityProject::TP_ScrollUpDown(int delta)
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

      wxScrollEvent dummy;
      OnScroll(dummy);
   }
}

void AudacityProject::FixScrollbars()
{
   if(!mTracks)
      return;

   bool refresh = false;
   bool rescroll = false;

   int totalHeight = (mTracks->GetHeight() + 32);

   int panelWidth, panelHeight;
   mTrackPanel->GetTracksUsableArea(&panelWidth, &panelHeight);

   // Add 1/4 of a screen of blank space to the end of the longest track
   mViewInfo.screen = ((double) panelWidth) / mViewInfo.zoom;
   double LastTime = wxMax( mTracks->GetEndTime(), mViewInfo.sel1 );
   mViewInfo.total = LastTime + mViewInfo.screen / 4;

   // Don't remove time from total that's still on the screen
   if (mViewInfo.h > mViewInfo.total - mViewInfo.screen) {
      mViewInfo.total = mViewInfo.h + mViewInfo.screen;
   }
   if (mViewInfo.h < 0.0) {
      mViewInfo.h = 0.0;
      rescroll = true;
   }

   mViewInfo.sbarTotal = (wxInt64) (mViewInfo.total * mViewInfo.zoom);
   mViewInfo.sbarScreen = (wxInt64) (mViewInfo.screen * mViewInfo.zoom);
   mViewInfo.sbarH = (wxInt64) (mViewInfo.h * mViewInfo.zoom);

   int lastv = mViewInfo.vpos;
   mViewInfo.vpos = mVsbar->GetThumbPosition() * mViewInfo.scrollStep;

   if (mViewInfo.vpos >= totalHeight)
      mViewInfo.vpos = totalHeight - 1;
   if (mViewInfo.vpos < 0)
      mViewInfo.vpos = 0;

   bool oldhstate;
   bool oldvstate;
   bool newhstate = mViewInfo.screen < mViewInfo.total;
   bool newvstate = panelHeight < totalHeight;

#ifdef __WXGTK__
   oldhstate = mHsbar->IsShown();
   oldvstate = mVsbar->IsShown();
   mHsbar->Show(mViewInfo.screen < mViewInfo.total);
   mVsbar->Show(panelHeight < totalHeight);
#else
   oldhstate = mHsbar->IsEnabled();
   oldvstate = mVsbar->IsEnabled();
   mHsbar->Enable(mViewInfo.screen < mViewInfo.total);
   mVsbar->Enable(panelHeight < totalHeight);
#endif

   if (panelHeight >= totalHeight && mViewInfo.vpos != 0) {
      mViewInfo.vpos = 0;

      refresh = true;
      rescroll = false;
   }
   if (mViewInfo.screen >= mViewInfo.total && mViewInfo.sbarH != 0) {
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

   int scaledSbarH = (int)(mViewInfo.sbarH * mViewInfo.sbarScale);
   int scaledSbarScreen = (int)(mViewInfo.sbarScreen * mViewInfo.sbarScale);
   int scaledSbarTotal = (int)(mViewInfo.sbarTotal * mViewInfo.sbarScale);

   mHsbar->SetScrollbar(scaledSbarH, scaledSbarScreen, scaledSbarTotal,
                        scaledSbarScreen, TRUE);
   mHsbar->Refresh();

   // Vertical scrollbar
   mVsbar->SetScrollbar(mViewInfo.vpos / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep,
                        totalHeight / mViewInfo.scrollStep,
                        panelHeight / mViewInfo.scrollStep, TRUE);
   mVsbar->Refresh();
   mViewInfo.lastZoom = mViewInfo.zoom;

   if (refresh || (rescroll && mViewInfo.screen < mViewInfo.total)) {
      mTrackPanel->Refresh(false);
   }

   UpdateMenus();

   if (oldhstate != newhstate || oldvstate != newvstate) {
      UpdateLayout();
   }
}

Track *AudacityProject::GetFirstVisible()
{
   if (!mViewInfo.track && mTracks) {
      TrackListIterator iter(mTracks);
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
   if (!mViewInfo.track || !mTracks) {
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

void AudacityProject::OnIconize(wxIconizeEvent &event)
{

   int VisibleProjectCount = 0;

   //JKC: On Iconizing we get called twice.  Don't know
   // why but it does no harm.
   // Should we be returning true/false rather than
   // void return?  I don't know.
   mIconized = event.Iconized();

   unsigned int i;

   for(i=0;i<gAudacityProjects.Count();i++){
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
   HandleResize();
   if (!this->IsMaximized() && !this->IsIconized())
      SetNormalizedWindowState(this->GetRect());
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

///Prevents deletion of projects from outside threads.
void AudacityProject::AllProjectsDeleteLock()
{
   msAllProjectDeleteMutex->Lock();
}

///Reallows deletion of projects from outside threads.
void AudacityProject::AllProjectsDeleteUnlock()
{
   msAllProjectDeleteMutex->Unlock();
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
   wxInt64 hlast = mViewInfo.sbarH;

   mViewInfo.sbarH = (wxInt64)
      (mHsbar->GetThumbPosition() / mViewInfo.sbarScale);

   if (mViewInfo.sbarH != hlast) {
      mViewInfo.h = mViewInfo.sbarH / mViewInfo.zoom;

      if (mViewInfo.h > mViewInfo.total - mViewInfo.screen)
         mViewInfo.h = mViewInfo.total - mViewInfo.screen;
      if (mViewInfo.h < 0.0)
         mViewInfo.h = 0.0;
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

bool AudacityProject::HandleKeyDown(wxKeyEvent & event)
{
   if (event.GetKeyCode() == WXK_ALT)
      mTrackPanel->HandleAltKey(true);

   // Allow the zoom cursor to change to a zoom out cursor
   if (event.GetKeyCode() == WXK_SHIFT)
      mTrackPanel->HandleShiftKey(true);

   if (event.GetKeyCode() == WXK_CONTROL)
      mTrackPanel->HandleControlKey(true);

   // Allow PageUp and PageDown keys to
   //scroll the Track Panel left and right
   if (event.GetKeyCode() == WXK_PAGEUP)
      mTrackPanel->HandlePageUpKey();

   if (event.GetKeyCode() == WXK_PAGEDOWN)
      mTrackPanel->HandlePageDownKey();

   // If a window has captured the keyboard, then allow it
   // first dibs at the event.  If it does an event.Skip(false)
   // then allow the event to process as normal, bypassing the
   // command handler.
   wxWindow *w = HasKeyboardCapture();
   if (w) {
      wxCommandEvent e(EVT_CAPTURE_KEY);
      e.SetEventObject(&event);

      if (w->ProcessEvent(e)) {
         return false;
      }
   }

   return mCommandManager.HandleKey(event, GetUpdateFlags(), 0xFFFFFFFF);
}

bool AudacityProject::HandleChar(wxKeyEvent & WXUNUSED(event))
{
   return false;
}

bool AudacityProject::HandleKeyUp(wxKeyEvent & event)
{
   if (event.GetKeyCode() == WXK_ALT)
      mTrackPanel->HandleAltKey(false);

   // Allow the Zoom Out cursor back to Zoom In
   if (event.GetKeyCode() == WXK_SHIFT)
      mTrackPanel->HandleShiftKey(false);

   if (event.GetKeyCode() == WXK_CONTROL)
      mTrackPanel->HandleControlKey(false);

   return mCommandManager.HandleKey(event, GetUpdateFlags(), 0xFFFFFFFF);
}

void AudacityProject::OnMenuEvent(wxMenuEvent & event)
{
   if (event.GetEventType() == wxEVT_MENU_OPEN) {
      mCommandManager.HandleMenuOpen(event);
   }
   else if (event.GetEventType() == wxEVT_MENU_CLOSE) {
      mCommandManager.HandleMenuClose(event);
   }
}

/// Determines if flags for command are compatible with current state.
/// If not, then try some recovery action to make it so.
/// @return whether compatible or not after any actions taken.
bool AudacityProject::TryToMakeActionAllowed( wxUint32 & flags, wxUint32 flagsRqd, wxUint32 mask )
{
   bool bAllowed;

   bAllowed = ((flags & mask) == (flagsRqd & mask));
   if( bAllowed )
      return true;

   // Action is not allowed
   // IF not set up to select all audio in that case, THEN return with failure.
   if( !mSelectAllOnNone )
      return false;

   wxUint32 MissingFlags = (flags & ~flagsRqd) & mask;

   // IF selecting all audio won't do any good, THEN return with failure.
   if( (flags & WaveTracksExistFlag) == 0 )
      return false;
   if( (MissingFlags & ~( TimeSelectedFlag | WaveTracksSelectedFlag))!=0)
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
                                               0xFFFFFFFF);

   if (handled)
      event.Skip(false);
   else
      event.Skip(true);
}

void AudacityProject::OnUpdateUI(wxUpdateUIEvent & WXUNUSED(event))
{
   UpdateMenus();
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

// LL: All objects that have a reference to the DirManager should
//     be deleted before the final mDirManager->Deref() in this
//     routine.  Failing to do so can cause unwanted recursion
//     and/or attempts to delete objects twice.
void AudacityProject::OnCloseWindow(wxCloseEvent & event)
{
   if (event.CanVeto() && (::wxIsBusy() || mbBusyImporting))
   {
      event.Veto();
      return;
   }

   if (mFreqWindow) {
      mFreqWindow->Destroy();
      mFreqWindow = NULL;
   }

   // Check to see if we were playing or recording
   // audio, and if so, make sure Audio I/O is completely finished.
   // The main point of this is to properly push the state
   // and flush the tracks once we've completely finished
   // recording new state.
   // This code is derived from similar code in
   // AudacityProject::~AudacityProject() and TrackPanel::OnTimer().
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) {

      wxBusyCursor busy;
      gAudioIO->StopStream();
      while(gAudioIO->IsBusy()) {
         wxMilliSleep(100);
      }

      // We were playing or recording audio, but we've stopped the stream.
      wxCommandEvent dummyEvent;
      GetControlToolBar()->OnStop(dummyEvent);

      if (gAudioIO->GetNumCaptureChannels() > 0) {
         // Tracks are buffered during recording.  This flushes
         // them so that there's nothing left in the append
         // buffers.
         TrackListIterator iter(mTracks);
         for (Track * t = iter.First(); t; t = iter.Next()) {
            if (t->GetKind() == Track::Wave) {
               ((WaveTrack *)t)->Flush();
            }
         }
         PushState(_("Recorded Audio"), _("Record"));
         if(IsTimerRecordCancelled())
         {
            OnUndo();
            ResetTimerRecordFlag();
         }
      }

      FixScrollbars();
      SetAudioIOToken(0);
      RedrawProject();
   }
   else if (gAudioIO->IsMonitoring()) {
      gAudioIO->StopStream();
   }

   // These two lines test for an 'empty' project.
   // of course it could still have a history at this stage.
   TrackListIterator iter2(mTracks);
   bool bHasTracks = (iter2.First() != NULL);

   // We may not bother to prompt the user to save, if the
   // project is now empty.
   if (event.CanVeto() && (mEmptyCanBeDirty || bHasTracks)) {
      if (mUndoManager.UnsavedChanges()) {

         wxString Message = _("Save changes before closing?");
         if( !bHasTracks )
         {
          Message += _("\nIf saved, the project will have no tracks.\n\nTo save any previously open tracks:\nCancel, Edit > Undo until all tracks\nare open, then File > Save Project.");
         }
         int result = wxMessageBox( Message,
                                   _("Save changes?"),
                                   wxYES_NO | wxCANCEL | wxICON_QUESTION,
                                   this);

         if (result == wxCANCEL || (result == wxYES && !Save())) {
            event.Veto();
            return;
         }
      }
   }

   ModuleManager::Dispatch(ProjectClosing);

   // Stop the timer since there's no need to update anything anymore
   delete mTimer;
   mTimer = NULL;

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

   // DanH: If we're definitely about to quit, delete the clipboard.
   //       Doing this after Deref'ing the DirManager causes problems.
   if ((gAudacityProjects.GetCount() == 1) && (quitOnClose || gIsQuitting))
      DeleteClipboard();

   // JKC: For Win98 and Linux do not detach the menu bar.
   // We want wxWidgets to clean it up for us.
   // TODO: Is there a Mac issue here??
   // SetMenuBar(NULL);

   // Lock all blocks in all tracks of the last saved version, so that
   // the blockfiles aren't deleted on disk when we delete the blockfiles
   // in memory.  After it's locked, delete the data structure so that
   // there's no memory leak.
   if (mLastSavedTracks) {
      TrackListIterator iter(mLastSavedTracks);
      Track *t = iter.First();
      while (t) {
         if (t->GetKind() == Track::Wave)
            ((WaveTrack *) t)->CloseLock();
         t = iter.Next();
      }

      mLastSavedTracks->Clear(true);
      delete mLastSavedTracks;
      mLastSavedTracks = NULL;
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
   delete mToolManager;
   mToolManager = NULL;

   DestroyChildren();

   delete mTrackFactory;
   mTrackFactory = NULL;

   delete mTags;
   mTags = NULL;

   delete mImportXMLTagHandler;
   mImportXMLTagHandler = NULL;

   // Unregister for tracklist updates
   mTracks->Disconnect(EVT_TRACKLIST_UPDATED,
                       wxCommandEventHandler(AudacityProject::OnTrackListUpdated),
                       NULL,
                       this);

   // Delete all the tracks to free up memory and DirManager references.
   mTracks->Clear(true);
   delete mTracks;
   mTracks = NULL;

   // This must be done before the following Deref() since it holds
   // references to the DirManager.
   mUndoManager.ClearStates();

   // MM: Tell the DirManager it can now delete itself
   // if it finds it is no longer needed. If it is still
   // used (f.e. by the clipboard), it will recognize this
   // and will destroy itself later.
   //
   // LL: All objects with references to the DirManager should
   //     have been deleted before this.
   mDirManager->Deref();

   AllProjectsDeleteLock();
   gAudacityProjects.Remove(this);
   AllProjectsDeleteUnlock();

   if (gActiveProject == this) {
      // Find a new active project
      if (gAudacityProjects.Count() > 0) {
         gActiveProject = gAudacityProjects[0];
      }
      else {
         gActiveProject = NULL;
      }
   }

   // Since we're going to be destroyed, make sure we're not to
   // receive audio notifications anymore.
   if (gAudioIO->GetListener() == this) {
      gAudioIO->SetListener(gActiveProject);
   }

   if (gAudacityProjects.IsEmpty() && !gIsQuitting) {

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

   Destroy();
}

void AudacityProject::OnOpenAudioFile(wxCommandEvent & event)
{
   wxString cmd = event.GetString();

   if (!cmd.IsEmpty()) {
      OpenFile(cmd);
   }

   RequestUserAttention();
}

void AudacityProject::OnCaptureKeyboard(wxCommandEvent & event)
{
   CaptureKeyboard((wxWindow *)event.GetEventObject());
}

void AudacityProject::OnReleaseKeyboard(wxCommandEvent & event)
{
   ReleaseKeyboard((wxWindow *)event.GetEventObject());
}

// static method, can be called outside of a project
wxArrayString AudacityProject::ShowOpenDialog(wxString extraformat, wxString extrafilter)
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
   l.DeleteContents(true);
   wxGetApp().mImporter->GetSupportedImportFormats(&l);

   for (FormatList::compatibility_iterator n = l.GetFirst(); n; n = n->GetNext()) {
      /* this loop runs once per supported _format_ */
      Format *f = n->GetData();

      wxString newfilter = f->formatName + wxT("|");
      // bung format name into string plus | separator
      for (size_t i = 0; i < f->formatExtensions.GetCount(); i++) {
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

#if defined(__WXMSW__reenable_if_new_filtering_does_not_work)
   // Make sure you build wxWidgets with filedlgg.cpp enabled in core/generic
   wxGenericFileDialog dlog(NULL,
#else
   FileDialog dlog(NULL,
#endif
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
bool AudacityProject::IsAlreadyOpen(const wxString projPathName)
{
   wxFileName newProjPathName(projPathName);
   size_t numProjects = gAudacityProjects.Count();
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
   ODManager::Pause();

   for (size_t ff = 0; ff < selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];

      // Make sure it isn't already open.
      if (AudacityProject::IsAlreadyOpen(fileName))
         continue; // Skip ones that are already open.

      gPrefs->Write(wxT("/DefaultOpenPath"), wxPathOnly(fileName));
      gPrefs->Flush();

      // DMM: If the project is dirty, that means it's been touched at
      // all, and it's not safe to open a new project directly in its
      // place.  Only if the project is brand-new clean and the user
      // hasn't done any action at all is it safe for Open to take place
      // inside the current project.
      //
      // If you try to Open a new project inside the current window when
      // there are no tracks, but there's an Undo history, etc, then
      // bad things can happen, including data files moving to the new
      // project directory, etc.
      if (!proj || proj->mDirty || !proj->mTracks->IsEmpty()) {
         // Open in a new window
         proj = CreateNewAudacityProject();
      }
      // This project is clean; it's never been touched.  Therefore
      // all relevant member variables are in their initial state,
      // and it's okay to open a new project inside this window.
      proj->OpenFile(fileName);
   }

   gPrefs->Write(wxT("/LastOpenType"),wxT(""));
   gPrefs->Flush();

   ODManager::Resume();

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


// FIXME? This should return a result that is checked.
//    See comment in AudacityApp::MRUOpen().
void AudacityProject::OpenFile(wxString fileName, bool addtohistory)
{
   // On Win32, we may be given a short (DOS-compatible) file name on rare
   // occassions (e.g. stuff like "C:\PROGRA~1\AUDACI~1\PROJEC~1.AUP"). We
   // convert these to long file name first.
   fileName = PlatformCompatibility::ConvertSlashInFileName(
      PlatformCompatibility::GetLongFileName(fileName));

   // Make sure it isn't already open.
   // Vaughan, 2011-03-25: This was done previously in AudacityProject::OpenFiles()
   //    and AudacityApp::MRUOpen(), but if you open an aup file by double-clicking it
   //    from, e.g., Win Explorer, it would bypass those, get to here with no check,
   //    then open a new project from the same data with no warning.
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

   // We want to open projects using wxTextFile, but if it's NOT a project
   // file (but actually a WAV file, for example), then wxTextFile will spin
   // for a long time searching for line breaks.  So, we look for our
   // signature at the beginning of the file first:

   wxString firstLine = wxT("AudacityProject");

   if (!::wxFileExists(fileName)) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error Opening File"),
                   wxOK | wxCENTRE, this);
      return;
   }

   wxFFile *ff = new wxFFile(fileName, wxT("rb"));
   if (!ff->IsOpened()) {
      wxMessageBox(_("Could not open file: ") + fileName,
                   _("Error opening file"),
                   wxOK | wxCENTRE, this);
   }
   char buf[16];
   int numRead = ff->Read(buf, 15);
   if (numRead != 15) {
      wxMessageBox(wxString::Format(_("File may be invalid or corrupted: \n%s"),
                   (const wxChar*)fileName), _("Error Opening File or Project"),
                   wxOK | wxCENTRE, this);
     ff->Close();
     delete ff;
     return;
   }
   buf[15] = 0;
   ff->Close();
   delete ff;

   wxString temp = LAT1CTOWX(buf);

   if (temp == wxT("AudacityProject")) {
      // It's an Audacity 1.0 (or earlier) project file.
      // If they bail out, return and do no more.
      if( !WarnOfLegacyFile() )
         return;
      // Convert to the new format.
      bool success = ConvertLegacyProjectFile(wxFileName(fileName));
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

   //FIXME: //v Surely we could be smarter about this, like checking much earlier that this is a .aup file.
   if (temp.Mid(0, 6) != wxT("<?xml ")) {
      // If it's not XML, try opening it as any other form of audio
      Import(fileName);
      return;
   }

   ///
   /// Parse project file
   ///

   mFileName = fileName;

   mRecoveryAutoSaveDataDir = wxT("");
   mIsRecovered = false;

   SetProjectTitle();

   // Auto-save files (which are known by the special ending .autosave) do
   // not necessarily have the closing </project> tag, because log data can
   // be added anytime. So before opening an .autosave file, add the necessary
   // closing bracket to make the XML parser happy.
   const wxString autoSaveExt = wxT(".autosave");
   if (mFileName.Length() >= autoSaveExt.Length() &&
       mFileName.Right(autoSaveExt.Length()) == autoSaveExt)
   {
      // This is an auto-save file, add </project> tag, if necessary
      wxFile f(fileName, wxFile::read_write);
      if (f.IsOpened())
      {
         // Read the last 16 bytes of the file and check if they contain
         // "</project>" somewhere.
         const int bufsize = 16;
         char buf[bufsize];
         bool seekOk, readOk;
         seekOk = f.SeekEnd(-bufsize) != wxInvalidOffset;
         if (seekOk)
            readOk = (f.Read(buf, bufsize) == bufsize);
         else
            readOk = false;
         if (readOk && !strstr(buf, "</project>"))
         {
            // End of file does not contain closing </project> tag, so add it
            if (f.Seek(0, wxFromEnd) != wxInvalidOffset)
            {
               strcpy(buf, "</project>\n");
               f.Write(buf, strlen(buf));
            }
         }

         f.Close();
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
      TrackListIterator iter(mTracks);
      mLastSavedTracks = new TrackList();

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
         // This project has been recovered, so write a new auto-save file
         // now and then delete the old one in the auto-save folder. Note that
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
                  for (WaveClipList::compatibility_iterator clipIter = ((WaveTrack*)t)->GetClipIterator();
                        clipIter;
                        clipIter=clipIter->GetNext())
                     clipIter->GetData()->MarkChanged();
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
   if (mRecordingRecoveryHandler)
   {
      delete mRecordingRecoveryHandler;
      mRecordingRecoveryHandler = NULL;
   }

   if (!bParseSuccess)
      return; // No need to do further processing if parse failed.

   GetDirManager()->FillBlockfilesCache();

   //check the ODManager to see if we should add the tracks to the ODManager.
   //this flag would have been set in the HandleXML calls from above, if there were
   //OD***Blocks.
   if(ODManager::HasLoadedODFlag())
   {
      Track *tr;
      TrackListIterator triter(mTracks);
      tr = triter.First();

      std::vector<ODTask*> newTasks;
      ODTask* newTask;
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

            //create whatever new tasks we need to.
            //we want at most one instance of each class for the project
            while((odFlags|createdODTasks) != createdODTasks)
            {
               newTask=NULL;
#ifdef EXPERIMENTAL_OD_FLAC
               if(!(createdODTasks&ODTask::eODFLAC) && odFlags & ODTask::eODFLAC) {
                  newTask= new ODDecodeFlacTask;
                  createdODTasks= createdODTasks | ODTask::eODFLAC;
               }
               else
#endif
               if(!(createdODTasks&ODTask::eODPCMSummary) && odFlags & ODTask::eODPCMSummary) {
                  newTask=new ODComputeSummaryTask;
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
                  newTasks.push_back(newTask);
               }
            }
         }
         tr = triter.Next();
      }
      for(unsigned int i=0;i<newTasks.size();i++)
         ODManager::Instance()->AddNewTask(newTasks[i]);

         //release the flag.
      ODManager::UnmarkLoadedODFlag();
   }
}

bool AudacityProject::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   bool bFileVersionFound = false;
   wxString fileVersion = _("<unrecognized version -- possibly corrupt project file>");
   wxString audacityVersion = _("<unrecognized version -- possibly corrupt project file>");
   int requiredTags = 0;

   // loop through attrs, which is a null-terminated list of
   // attribute-value pairs
   while(*attrs) {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value || !XMLValueChecker::IsGoodString(value))
         break;

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

      if (!wxStrcmp(attr, wxT("version")))
      {
         fileVersion = value;
         bFileVersionFound = true;
         requiredTags++;
      }

      if (!wxStrcmp(attr, wxT("audacityversion"))) {
         audacityVersion = value;
         requiredTags++;
      }

      if (!wxStrcmp(attr, wxT("projname"))) {
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

      if (!wxStrcmp(attr, wxT("sel0")))
         Internat::CompatibleToDouble(value, &mViewInfo.sel0);

      if (!wxStrcmp(attr, wxT("sel1")))
         Internat::CompatibleToDouble(value, &mViewInfo.sel1);

      long longVpos = 0;
      if (!wxStrcmp(attr, wxT("vpos")))
         wxString(value).ToLong(&longVpos);
      mViewInfo.track = NULL;
      mViewInfo.vpos = longVpos;

      if (!wxStrcmp(attr, wxT("h")))
         Internat::CompatibleToDouble(value, &mViewInfo.h);

      if (!wxStrcmp(attr, wxT("zoom")))
         Internat::CompatibleToDouble(value, &mViewInfo.zoom);

      if (!wxStrcmp(attr, wxT("rate"))) {
         Internat::CompatibleToDouble(value, &mRate);
         GetSelectionBar()->SetRate(mRate);
      }

      if (!wxStrcmp(attr, wxT("snapto"))) {
         SetSnapTo(wxString(value) == wxT("on") ? true : false);
      }

      if (!wxStrcmp(attr, wxT("selectionformat"))) {
         SetSelectionFormat(value);
      }
   } // while

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
      // If the tag name is not one of these two (the new name is
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
      return mTags;
   }

   if (!wxStrcmp(tag, wxT("wavetrack"))) {
      WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   #ifdef USE_MIDI
   if (!wxStrcmp(tag, wxT("notetrack"))) {
      NoteTrack *newTrack = mTrackFactory->NewNoteTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }
   #endif // USE_MIDI

   if (!wxStrcmp(tag, wxT("labeltrack"))) {
      LabelTrack *newTrack = mTrackFactory->NewLabelTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!wxStrcmp(tag, wxT("timetrack"))) {
      TimeTrack *newTrack = mTrackFactory->NewTimeTrack();
      mTracks->Add(newTrack);
      return newTrack;
   }

   if (!wxStrcmp(tag, wxT("recordingrecovery"))) {
      if (!mRecordingRecoveryHandler)
         mRecordingRecoveryHandler = new RecordingRecoveryHandler(this);
      return mRecordingRecoveryHandler;
   }

   if (!wxStrcmp(tag, wxT("import"))) {
      if (mImportXMLTagHandler == NULL)
         mImportXMLTagHandler = new ImportXMLTagHandler(this);
      return mImportXMLTagHandler;
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
   xmlFile.WriteAttr(wxT("sel0"), mViewInfo.sel0, 10);
   xmlFile.WriteAttr(wxT("sel1"), mViewInfo.sel1, 10);
   xmlFile.WriteAttr(wxT("vpos"), mViewInfo.vpos);
   xmlFile.WriteAttr(wxT("h"), mViewInfo.h, 10);
   xmlFile.WriteAttr(wxT("zoom"), mViewInfo.zoom, 10);
   xmlFile.WriteAttr(wxT("rate"), mRate);
   xmlFile.WriteAttr(wxT("snapto"), GetSnapTo() ? wxT("on") : wxT("off"));
   xmlFile.WriteAttr(wxT("selectionformat"), GetSelectionFormat());

   mTags->WriteXML(xmlFile);

   Track *t;
   WaveTrack* pWaveTrack;
   TrackListIterator iter(mTracks);
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
      else
         t->WriteXML(xmlFile);

      t = iter.Next();
   }
   mStrOtherNamesArray.Clear();

   if (!mAutoSaving)
   {
      // Only write closing bracket when not auto-saving, since we may add
      // recording log data to the end of the file later
      xmlFile.EndTag(wxT("project"));
   }
}

// Lock all blocks in all tracks of the last saved version
void AudacityProject::LockAllBlocks()
{
   TrackListIterator iter(mLastSavedTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
         ((WaveTrack *) t)->Lock();
      t = iter.Next();
   }
}

// Unlock all blocks in all tracks of the last saved version
void AudacityProject::UnlockAllBlocks()
{
   TrackListIterator iter(mLastSavedTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
         ((WaveTrack *) t)->Unlock();
      t = iter.Next();
   }
}

bool AudacityProject::Save(bool overwrite /* = true */ ,
                           bool fromSaveAs /* = false */,
                           bool bWantSaveCompressed /*= false*/)
{
   if (bWantSaveCompressed)
      wxASSERT(fromSaveAs);
   else
   {
      TrackListIterator iter(mTracks);
      bool bHasTracks = (iter.First() != NULL);
      if (!bHasTracks)
      {
         if (mUndoManager.UnsavedChanges() && mEmptyCanBeDirty) {
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
         // the new directory.  We need to make sure files that belonged
         // to the last saved project don't get erased, so we "lock" them, so that
         // SetProject() copies instead of moves the files.
         // (Otherwise the new project would be fine, but the old one would
         // be empty of all of its files.)

         if (mLastSavedTracks && !overwrite)
            LockAllBlocks();

         // This renames the project directory, and moves or copies
         // all of our block files over.
         success = mDirManager->SetProject(projPath, projName, !overwrite);

         if (mLastSavedTracks && !overwrite)
            UnlockAllBlocks();
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
   catch (XMLFileWriterException* pException)
   {
      wxMessageBox(wxString::Format(
         _("Couldn't write to file \"%s\": %s"),
         mFileName.c_str(), pException->GetMessage().c_str()),
         _("Error Saving Project"), wxICON_ERROR);

      delete pException;

      // When XMLWriter throws an exception, it tries to close it before,
      // so we can at least try to delete the incomplete file and move the
      // backup file over.
      if (safetyFileName != wxT(""))
      {
         wxRemove(mFileName);
         wxRename(safetyFileName, mFileName);
      }

      return false;
   }

#ifdef __WXMAC__
   wxFileName fn(mFileName);
   fn.MacSetTypeAndCreator(AUDACITY_PROJECT_TYPE, AUDACITY_CREATOR);
#endif

   if (bWantSaveCompressed)
      mWantSaveCompressed = false; // Don't want this mode for AudacityProject::WriteXML() any more.
   else
   {
      // Now that we have saved the file, we can delete the auto-saved version
      DeleteCurrentAutoSaveFile();

      if (mIsRecovered)
      {
         // This was a recovered file, that is, we have just overwritten the
         // old, crashed .aup file. There may still be orphaned blockfiles in
         // this directory left over from the crash, so we delete them now
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

      if (mLastSavedTracks) {
         mLastSavedTracks->Clear(true);
         delete mLastSavedTracks;
      }

      mLastSavedTracks = new TrackList();

      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      Track *dupT;
      while (t) {
         dupT = t->Duplicate();
         mLastSavedTracks->Add(dupT);

         //only after the xml has been saved we can mark it saved.
         //thus is because the OD blockfiles change on  background thread while this is going on.
         //         if(dupT->GetKind() == Track::Wave)
         //         ((WaveTrack*)dupT)->MarkSaved();

         t = iter.Next();
      }

      mUndoManager.StateSaved();
   }

   // If we get here, saving the project was successful, so we can delete
   // the .bak file (because it now does not fit our block files anymore
   // anyway).
   if (safetyFileName != wxT(""))
      wxRemoveFile(safetyFileName);

   mStatusBar->SetStatusText(wxString::Format(_("Saved %s"),
                                              mFileName.c_str()));

   return true;
}

#ifdef USE_LIBVORBIS
   bool AudacityProject::SaveCompressedWaveTracks(const wxString strProjectPathName) // full path for aup except extension
   {
      // Some of this is similar to code in ExportMultiple::ExportMultipleByTrack
      // but that code is really tied into the dialogs.

      // Copy the tracks because we're going to do some state changes before exporting.
      Track* pSavedTrack;
      Track* pTrack;
      WaveTrack* pWaveTrack;
      TrackListOfKindIterator iter(Track::Wave, mTracks);
      unsigned int numWaveTracks = 0;
      TrackList* pSavedTrackList = new TrackList();
      for (pTrack = iter.First(); pTrack != NULL; pTrack = iter.Next())
      {
         numWaveTracks++;
         pWaveTrack = (WaveTrack*)pTrack;
         pSavedTrack = mTrackFactory->DuplicateWaveTrack(*pWaveTrack);
         pSavedTrackList->Add(pSavedTrack);
      }

      if (numWaveTracks == 0)
      {
         // Nothing to save compressed => success. Delete the copies and go.
         delete pSavedTrackList;
         return true;
      }

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
      TrackListIterator savedTrackIter(pSavedTrackList);
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

      pSavedTrackList->Clear(true);
      delete pSavedTrackList;

      return bSuccess;
   }
#endif


void AudacityProject::AddImportedTracks(wxString fileName,
                                        Track **newTracks, int numTracks)
{
   SelectNone();

   bool initiallyEmpty = mTracks->IsEmpty();
   double newRate = 0;
   wxString trackNameBase = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast('.');
   bool isLinked = false;
   for (int i = 0; i < numTracks; i++) {
      if (newRate == 0 && newTracks[i]->GetKind() == Track::Wave) {
         newRate = ((WaveTrack *)newTracks[i])->GetRate();
      }
      mTracks->Add(newTracks[i]);
      newTracks[i]->SetSelected(true);
      //we need to check link status based on the first channel only.
      if(0==i)
         isLinked = newTracks[i]->GetLinked();
      if (numTracks > 2 || (numTracks > 1 && !isLinked) ) {
         newTracks[i]->SetName(trackNameBase + wxString::Format(wxT(" %d" ), i + 1));
      }
      else {
         newTracks[i]->SetName(trackNameBase);
      }

      // Check if new track contains aliased blockfiles and if yes,
      // remember this to show a warning later
      if (newTracks[i]->GetKind() == WaveTrack::Wave)
      {
         WaveClip* clip = ((WaveTrack*)newTracks[i])->GetClipByIndex(0);
         if (clip && clip->GetSequence()->GetBlockArray()->GetCount())
         {
            SeqBlock* block = clip->GetSequence()->GetBlockArray()->Item(0);
            if (block->f->IsAlias())
            {
               mImportedDependencies = true;
            }
         }
      }
   }

   delete[]newTracks;

   // Automatically assign rate of imported file to whole project,
   // if this is the first file that is imported
   if (initiallyEmpty && newRate > 0) {
      mRate = newRate;
      GetSelectionBar()->SetRate(mRate);
   }

   PushState(wxString::Format(_("Imported '%s'"), fileName.c_str()),
             _("Import"));

   OnZoomFit();

   mTrackPanel->SetFocus();
   mTrackPanel->EnsureVisible(mTrackPanel->GetFirstSelectedTrack());
   mTrackPanel->Refresh(false);

   if (initiallyEmpty && mDirManager->GetProjectName() == wxT("")) {
      wxString name = fileName.AfterLast(wxFILE_SEP_PATH).BeforeLast(wxT('.'));
      mFileName =::wxPathOnly(fileName) + wxFILE_SEP_PATH + name + wxT(".aup");
      SetProjectTitle();
   }

   // Moved this call to higher levels to prevent flicker redrawing everything on each file.
   //   HandleResize();
}

// If pNewTrackList is passed in non-NULL, it gets filled with the pointers to new tracks.
bool AudacityProject::Import(wxString fileName, WaveTrackArray* pTrackArray /*= NULL*/)
{
   Track **newTracks;
   int numTracks;
   wxString errorMessage=wxT("");

   numTracks = wxGetApp().mImporter->Import(fileName,
                                            mTrackFactory,
                                            &newTracks,
                                            mTags,
                                            errorMessage);

   if (!errorMessage.IsEmpty()) {
// Version that goes to internet...
//      ShowErrorDialog(this, _("Error Importing"),
//                 errorMessage, wxT("http://audacity.sourceforge.net/help/faq?s=files&i=wma-proprietary"));
// Version that looks locally for the text.
      ShowErrorDialog(this, _("Error Importing"),
                 errorMessage, wxT("innerlink:wma-proprietary"));
   }
   if (numTracks <= 0)
      return false;

   wxGetApp().AddFileToHistory(fileName);

   // for LOF ("list of files") files, do not import the file as if it
   // were an audio file itself
   if (fileName.AfterLast('.').IsSameAs(wxT("lof"), false)) {
      return false;
   }

   // Have to set up newTrackList before calling AddImportedTracks,
   // because AddImportedTracks deletes newTracks.
   if (pTrackArray) {
      for (int i = 0; i < numTracks; i++) {
         if (newTracks[i]->GetKind() == Track::Wave) {
            pTrackArray->Add((WaveTrack *)newTracks[i]);
         }
      }
   }

   AddImportedTracks(fileName, newTracks, numTracks);

   int mode = gPrefs->Read(wxT("/AudioFiles/NormalizeOnLoad"), 0L);
   if (mode == 1) {
      //TODO: All we want is a SelectAll()
      SelectNone();
      SelectAllIfNone();
      OnEffect(ALL_EFFECTS | CONFIGURED_EFFECT,
               EffectManager::Get().GetEffectByIdentifier(wxT("Normalize")));
   }

   GetDirManager()->FillBlockfilesCache();
   return true;
}

bool AudacityProject::SaveAs(const wxString newFileName, bool bWantSaveCompressed /*= false*/, bool addToHistory /*= true*/)
{
   wxString oldFileName = mFileName;

   //check to see if the new project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if(mFileName!=newFileName && wxFileExists(newFileName)) {
      wxMessageDialog m(
         NULL,
         _("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   mFileName = newFileName;
   SetProjectTitle();

   bool success = Save(false, true, bWantSaveCompressed);

   if (success && addToHistory) {
      wxGetApp().AddFileToHistory(mFileName);
   }
   if (!success || bWantSaveCompressed) // bWantSaveCompressed doesn't actually change current project.
   {
      // Reset file name on error
      mFileName = oldFileName;
      SetProjectTitle();
   }

   return(success);
}

bool AudacityProject::SaveAs(bool bWantSaveCompressed /*= false*/)
{
   wxString path = wxPathOnly(mFileName);
   wxString fName;

   wxString ext = wxT(".aup");

   fName = GetName().Len()? GetName() + ext : wxString(wxT(""));

   wxString sProjName = this->GetName();
   if (sProjName.IsEmpty())
      sProjName = _("<untitled>");
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
      sDialogTitle.Printf(_("Save Compressed Project \"%s\" As..."), sProjName.c_str());
   }
   else
   {
      if (ShowWarningDialog(this, wxT("FirstProjectSave"),
                           _("\
'Save Project' is for an Audacity project, not an audio file.\n\
For an audio file that will open in other apps, use 'Export'.\n"),
                           true) != wxID_OK)
         return false;
      sDialogTitle.Printf(_("Save Project \"%s\" As..."), sProjName.c_str());
   }

   fName = FileSelector(
      sDialogTitle,
      path, fName, wxT(""),
      _("Audacity projects") + static_cast<wxString>(wxT(" (*.aup)|*.aup")),
      // JKC: I removed 'wxFD_OVERWRITE_PROMPT' because we are checking
      // for overwrite ourselves later, and we disallow it.
      // We disallow overwrite because we would have to delete the many
      // smaller files too, or prompt to move them.
      wxFD_SAVE |  wxRESIZE_BORDER, this);

   if (fName == wxT(""))
      return false;

   size_t len = fName.Len();
   if (len > 4 && fName.Mid(len - 4) == wxT(".aup"))
      fName = fName.Mid(0, len - 4);

   wxString oldFileName = mFileName;

   //check to see if the new project file already exists.
   //We should only overwrite it if this project already has the same name, where the user
   //simply chose to use the save as command although the save command would have the effect.
   if(mFileName!=fName+ext && wxFileExists(fName+ext)) {
      wxMessageDialog m(
         NULL,
         _("The project was not saved because the file name provided would overwrite another project.\nPlease try again and select an original name."),
         _("Error Saving Project"),
         wxOK|wxICON_ERROR);
      m.ShowModal();
      return false;
   }

   mFileName = fName + ext;
   SetProjectTitle();

   bool success = Save(false, true, bWantSaveCompressed);

   if (success) {
      wxGetApp().AddFileToHistory(mFileName);
   }
   if (!success || bWantSaveCompressed) // bWantSaveCompressed doesn't actually change current project.
   {
      // Reset file name on error
      mFileName = oldFileName;
      SetProjectTitle();
   }

   return(success);
}

//
// Undo/History methods
//

void AudacityProject::InitialState()
{
   if (mImportXMLTagHandler != NULL) {
      // We processed an <import> tag, so save it as a normal project, with no <import> tags.
      this->Save();

      // Shouldn't need it any more.
      delete mImportXMLTagHandler;
      mImportXMLTagHandler = NULL;
   }

   mUndoManager.ClearStates();

   mUndoManager.PushState(mTracks, mViewInfo.sel0, mViewInfo.sel1,
                          _("Created new project"), wxT(""));

   mUndoManager.StateSaved();

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenuItems();

   UpdateMenus();
   this->UpdateLyrics();
   this->UpdateMixerBoard();
}

void AudacityProject::PushState(wxString desc,
                                wxString shortDesc,
                                int flags )
{
   mUndoManager.PushState(mTracks, mViewInfo.sel0, mViewInfo.sel1,
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
   if( (flags & PUSH_AUTOSAVE)!= 0)
      AutoSave();
}

void AudacityProject::ModifyState(bool bWantsAutoSave)
{
   mUndoManager.ModifyState(mTracks, mViewInfo.sel0, mViewInfo.sel1);
   if (bWantsAutoSave)
      AutoSave();
}

// LL:  Is there a memory leak here as "l" and "t" are not deleted???
// Vaughan, 2010-08-29: No, as "l" is a TrackList* of an Undo stack state.
//    Need to keep it and its tracks "t" available for Undo/Redo/SetStateTo.
void AudacityProject::PopState(TrackList * l)
{
   mTracks->Clear(true);
   TrackListIterator iter(l);
   Track *t = iter.First();
   bool odUsed = false;
   ODComputeSummaryTask* computeTask = NULL;
   Track* copyTrack;

   while (t)
   {
      copyTrack=t->Duplicate();
      mTracks->Add(copyTrack);

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
               computeTask=new ODComputeSummaryTask;
               odUsed=true;
            }
            computeTask->AddWaveTrack((WaveTrack*)copyTrack);
         }
      }
      t = iter.Next();
   }

   //add the task.
   if(odUsed)
      ODManager::Instance()->AddNewTask(computeTask);

   HandleResize();

   UpdateMenus();
   this->UpdateLyrics();
   this->UpdateMixerBoard();

   AutoSave();
}

void AudacityProject::SetStateTo(unsigned int n)
{
   TrackList *l =
       mUndoManager.SetStateTo(n, &mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

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

   TrackListOfKindIterator iter(Track::Label, mTracks);
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
   for (int i = 0; i < pLabelTrack->GetNumLabels(); i++)
      pLyricsPanel->Add(pLabelTrack->GetLabel(i)->t,
                        pLabelTrack->GetLabel(i)->title);
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
   return msClipboard;
}

//static
void AudacityProject::DeleteClipboard()
{
   if (msClipboard) {
      msClipboard->Clear( true );
      delete msClipboard;
      msClipboard = NULL;
   }
}

//static
void AudacityProject::DeleteAllProjectsDeleteLock()
{
   if(msAllProjectDeleteMutex)
   {
      delete msAllProjectDeleteMutex;
      msAllProjectDeleteMutex=NULL;
   }
}

void AudacityProject::ClearClipboard()
{
   msClipT0 = 0.0;
   msClipT1 = 0.0;
   msClipProject = NULL;
   if (msClipboard) {
      msClipboard->Clear(true);
   }
}

void AudacityProject::Clear()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected() || n->IsSyncLockSelected()) {
         n->Clear(mViewInfo.sel0, mViewInfo.sel1);
      }
      n = iter.Next();
   }

   double seconds = mViewInfo.sel1 - mViewInfo.sel0;

   mViewInfo.sel1 = mViewInfo.sel0;

   PushState(wxString::Format(_("Deleted %.2f seconds at t=%.2f"),
                              seconds,
                              mViewInfo.sel0),
             _("Delete"));

   RedrawProject();
}

void AudacityProject::SelectNone()
{
   TrackListIterator iter(mTracks);
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
   if (level > gMaxZoom)
      level = gMaxZoom;
   if (level <= gMinZoom)
      level = gMinZoom;

   mViewInfo.zoom = level;
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
   mViewInfo.sel0 = 0;
   if (!shift || mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = 0;

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

   mViewInfo.sel1 = len;
   if (!shift || mViewInfo.sel0 > mViewInfo.sel1)
      mViewInfo.sel0 = len;

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

//JKC: same as above *except* this a virtual function that
//can be called from the track panel callback.
//It seems a little crazy doing this but TrackArtist
//needs to get information about the tool bar state and
//I don't currently see a cleaner way.
ControlToolBar * AudacityProject::TP_GetControlToolBar()
{
   return GetControlToolBar();
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

MeterToolBar *AudacityProject::GetMeterToolBar()
{
   return (MeterToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(MeterBarID) :
           NULL);
}

MixerToolBar *AudacityProject::GetMixerToolBar()
{
   return (MixerToolBar *)
          (mToolManager ?
           mToolManager->GetToolBar(MixerBarID) :
           NULL);
}

SelectionBar *AudacityProject::GetSelectionBar()
{
   return (SelectionBar *)
          (mToolManager ?
           mToolManager->GetToolBar(SelectionBarID) :
           NULL);
}

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

void AudacityProject::SetStop(bool bStopped)
{
   mTrackPanel->SetStop(bStopped);
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
   if (gAudioIO->GetNumCaptureChannels() > 0) {
      wxLongLong freeSpace = mDirManager->GetFreeDiskSpace();
      if (freeSpace >= 0) {
         wxString msg;
         double recTime;
         int recMins;

         recTime = freeSpace.GetHi() * 4294967296.0 + freeSpace.GetLo();
         recTime /= SAMPLE_SIZE_DISK(gAudioIO->GetCaptureFormat());
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

         mStatusBar->SetStatusText(msg);
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
            mStatusBar->SetStatusText(msg);

         }
         else if(numTasks>1)
            msg.Printf(_("Import(s) complete. Running %d on-demand waveform calculations. Overall %2.0f%% complete."),
              numTasks,ratioComplete*100.0);
         else
            msg.Printf(_("Import complete. Running an on-demand waveform calculation. %2.0f%% complete."),
             ratioComplete*100.0);


         mStatusBar->SetStatusText(msg);
      }
   }
}

//get regions selected by selected labels
//removes unnecessary regions, overlapping regions are merged
//regions memory need to be deleted by the caller
void AudacityProject::GetRegionsByLabel( Regions &regions )
{
   TrackListIterator iter( mTracks );
   Track *n;

   //determine labelled regions
   for( n = iter.First(); n; n = iter.Next() )
      if( n->GetKind() == Track::Label && n->GetSelected() )
      {
         LabelTrack *lt = ( LabelTrack* )n;
         for( int i = 0; i < lt->GetNumLabels(); i++ )
         {
            const LabelStruct *ls = lt->GetLabel( i );
            if( ls->t >= mViewInfo.sel0 && ls->t1 <= mViewInfo.sel1 )
            {
               Region *region = new Region;
               region->start = ls->t;
               region->end = ls->t1;
               regions.Add( region );
            }
         }
      }

   //anything to do ?
   if( regions.GetCount() == 0 )
      return;

   //sort and remove unnecessary regions
   regions.Sort( Region::cmp );
   unsigned int selected = 1;
   while( selected < regions.GetCount() )
   {
      Region *cur = regions.Item( selected );
      Region *last = regions.Item( selected - 1 );
      if( cur->start < last->end )
      {
         if( cur->end > last->end )
            last->end = cur->end;
         delete cur;
         regions.RemoveAt( selected );
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
void AudacityProject::EditByLabel( WaveTrack::EditFunction action,
                                   bool bSyncLockedTracks )
{
   Regions regions;

   GetRegionsByLabel( regions );
   if( regions.GetCount() == 0 )
      return;

   TrackListIterator iter( mTracks );
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
         for( int i = ( int )regions.GetCount() - 1; i >= 0; i-- )
            ( wt->*action )( regions.Item( i )->start, regions.Item( i )->end );
      }
      n = iter.Next();
   }

   //delete label regions
   for( unsigned int i = 0; i < regions.GetCount(); i++ )
      delete regions.Item( i );
}

//Executes the edit function on all selected wave tracks with
//regions specified by selected labels
//If No tracks selected, function is applied on all tracks
//Functions copy the edited regions to clipboard, possibly in multiple tracks
//This probably should not be called if *action() changes the timeline, because
// the copy needs to happen by track, and the timeline change by group.
void AudacityProject::EditClipboardByLabel( WaveTrack::EditDestFunction action )
{
   Regions regions;

   GetRegionsByLabel( regions );
   if( regions.GetCount() == 0 )
      return;

   TrackListIterator iter( mTracks );
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
         WaveTrack *merged = NULL;
         for( int i = ( int )regions.GetCount() - 1; i >= 0; i-- )
         {
            Track *dest = NULL;
            ( wt->*action )( regions.Item( i )->start, regions.Item( i )->end,
                             &dest );
            if( dest )
            {
               dest->SetChannel( wt->GetChannel() );
               dest->SetLinked( wt->GetLinked() );
               dest->SetName( wt->GetName() );
               if( !merged )
                  merged = ( WaveTrack* )dest;
               else
               {
                  // Paste to the beginning; unless this is the first region,
                  // offset the track to account for time between the regions
                  if (i < (int)regions.GetCount() - 1) {
                     merged->Offset(
                           regions.Item(i+1)->start - regions.Item(i)->end);
                  }

                  bool bResult = merged->Paste( 0.0 , dest );
                  wxASSERT(bResult); // TO DO: Actually handle this.
                  delete dest;
               }
            }
            else  // nothing copied but there is a 'region', so the 'region' must be a 'point label' so offset
               if (i < (int)regions.GetCount() - 1)
                  if( merged )
                     merged->Offset(regions.Item(i+1)->start - regions.Item(i)->end);
         }
         if( merged )
            msClipboard->Add( merged );
      }
   }

   msClipT0 = regions.Item(0)->start;
   msClipT1 = regions.Item(regions.GetCount() - 1)->end;

   //delete label regions
   for( unsigned int i = 0; i < regions.GetCount(); i++ )
      delete regions.Item( i );
}


// TrackPanel callback method
void AudacityProject::TP_DisplayStatusMessage(wxString msg)
{
   mStatusBar->SetStatusText(msg);
   mLastStatusUpdateTime = ::wxGetUTCTime();
}

// Set the status indirectly, using the command system
// (more overhead, but can be used from a non-GUI thread)
void AudacityProject::SafeDisplayStatusMessage(const wxChar *msg)
{
   CommandOutputTarget *target
      = new CommandOutputTarget(TargetFactory::ProgressDefault(),
                                new StatusBarTarget(*mStatusBar),
                                TargetFactory::MessageDefault());
   CommandType *type = CommandDirectory::Get()->LookUp(wxT("Message"));
   wxASSERT_MSG(type != NULL, wxT("Message command not found!"));
   Command *statusCmd = type->Create(target);
   statusCmd->SetParameter(wxT("MessageString"), msg);
   ScriptCommandRelay::PostCommand(this, statusCmd);

   // Although the status hasn't actually been set yet, updating the time now
   // is probably accurate enough
   mLastStatusUpdateTime = ::wxGetUTCTime();
}

void AudacityProject::TP_DisplaySelection()
{
   double audioTime;

   if (gAudioIO->IsBusy())
      audioTime = gAudioIO->GetStreamTime();
   else {
      audioTime = 0;
   }

   GetSelectionBar()->SetTimes(mViewInfo.sel0, mViewInfo.sel1, audioTime);

   if (!gAudioIO->IsBusy() && !mLockPlayRegion)
      mRuler->SetPlayRegion(mViewInfo.sel0, mViewInfo.sel1);
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


// TrackPanel callback methods

int AudacityProject::TP_GetCurrentTool()
{
   //ControlToolBar might be NULL--especially on shutdown.
   //Make sure it isn't and if it is, return a reasonable value
   ToolsToolBar *ctb = GetToolsToolBar();
   if (ctb)
      return GetToolsToolBar()->GetCurrentTool();
   else
      return 0;
}




// TrackPanel callback method
void AudacityProject::TP_OnPlayKey()
{
   OnPlayStop();
}

// TrackPanel callback method
void AudacityProject::TP_PushState(wxString desc, wxString shortDesc,
                                   int flags)
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
   mRuler->GetPlayRegion(playRegionStart, playRegionEnd);
}

wxWindow *AudacityProject::HasKeyboardCapture()
{
   return mKeyboardCaptured;
}

void AudacityProject::CaptureKeyboard(wxWindow *w)
{
   mKeyboardCaptured = w;
}

void AudacityProject::ReleaseKeyboard(wxWindow *w)
{
   if (w == mKeyboardCaptured)
   {
      mKeyboardCaptured = NULL;
   }
}


void AudacityProject::AutoSave()
{
   //    SonifyBeginAutoSave(); // part of RBD's r10680 stuff now backed out

   // To minimize the possibility of race conditions, we first write to a
   // file with the extension ".tmp", then rename the file to .autosave
   wxString projName;

   if (mFileName.IsEmpty())
      projName = _("New Project");
   else
      projName = wxFileName(mFileName).GetName();

   wxString fn = wxFileName(FileNames::AutoSaveDir(),
      projName + wxString(wxT(" - ")) + CreateUniqueName()).GetFullPath();

   XMLFileWriter saveFile;

   try
   {
      saveFile.Open(fn + wxT(".tmp"), wxT("wb"));

      {
         VarSetter<bool> setter(&mAutoSaving, true, false);
         WriteXMLHeader(saveFile);
         WriteXML(saveFile);
      }

      // JKC Calling XMLFileWriter::Close will close the <project> scope.
      // We certainly don't want to do that, if we're doing recordingrecovery,
      // because the recordingrecovery tags need to be inside <project></project>.
      // So instead we do not call Close() but CloseWithoutEndingTags().
      saveFile.CloseWithoutEndingTags();
   }
   catch (XMLFileWriterException* pException)
   {
      wxMessageBox(wxString::Format(
         _("Couldn't write to file \"%s\": %s"),
         (fn + wxT(".tmp")).c_str(), pException->GetMessage().c_str()),
         _("Error Writing Autosave File"), wxICON_ERROR, this);

      delete pException;

      return;
   }

   // Now that we have a new auto-save file, delete the old one
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
   display = wxString::Format(_("Actual Rate: %d"), rate);
   int x, y;
   mStatusBar->GetTextExtent(display, &x, &y);
   int widths[] = {-1, x+50};
   mStatusBar->SetStatusWidths(2, widths);
   mStatusBar->SetStatusText(display, 1);
}

void AudacityProject::OnAudioIOStartRecording()
{
   // Before recording is started, auto-save the file. The file will have
   // empty tracks at the bottom where the recording will be put into
   //
   // When block files are cached, auto recovery is disabled while recording,
   // since no block files are written during recording that could be
   // recovered.
   //
   if (!GetCacheBlockFiles())
      AutoSave();
}

// This is called after recording has stopped and all tracks have flushed.
void AudacityProject::OnAudioIOStopRecording()
{
   // Write all cached files to disk, if any
   mDirManager->WriteCacheToDisk();

   // Now we auto-save again to get the project to a "normal" state again.
   AutoSave();
}

void AudacityProject::OnAudioIONewBlockFiles(const wxString& blockFileLog)
{
   // New blockfiles have been created, so add them to the auto-save file
   if (!GetCacheBlockFiles() &&
       !mAutoSaveFileName.IsEmpty())
   {
      wxFFile f(mAutoSaveFileName, wxT("at"));
      if (!f.IsOpened())
         return; // Keep recording going, there's not much we can do here
      f.Write(blockFileLog);
      f.Close();
   }
}

bool AudacityProject::GetCacheBlockFiles()
{
   bool cacheBlockFiles = false;
#ifdef DEPRECATED_AUDIO_CACHE
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=545.
   gPrefs->Read(wxT("/Directories/CacheBlockFiles"), &cacheBlockFiles);
#endif
   return cacheBlockFiles;
}

void AudacityProject::SetSnapTo(int snap)
{
   AS_SetSnapTo(snap);
   if (GetSelectionBar()) {
      GetSelectionBar()->SetSnapTo(snap);
   }
}

int AudacityProject::GetSnapTo()
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

void AudacityProject::HandleTrackMute(Track *t, const bool exclusive)
{
   // "exclusive" mute means mute the chosen track and unmute all others.
   if (exclusive)
   {
      TrackListIterator iter(mTracks);
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
         TrackListIterator iter(mTracks);
         Track *i = iter.First();
         while (i != t) {  // search for this track
            i = iter.Next();
         }
         i = iter.Next();  // get the next one, since linked
         i->SetMute(muted);   // and mute it as well
      }

      if( IsSoloSimple() )
      {
         TrackListIterator iter(mTracks);
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
         TrackListIterator iter(mTracks);
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
      TrackListIterator iter(mTracks);
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

