/**********************************************************************

  NyqBench.cpp

  Leland Lucius

**********************************************************************/



#include <wx/defs.h>

#include <wx/aboutdlg.h>
#include <wx/filedlg.h>
#include <wx/font.h>
#include <wx/fontdlg.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/splitter.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/toolbar.h>

#include "ActiveProject.h"
#include "AudioIOBase.h"
#include "CommonCommandFlags.h"
#include "ModuleConstants.h"
#include "Prefs.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "effects/EffectManager.h"
#include "effects/EffectUI.h"
#include "effects/nyquist/Nyquist.h"
#include "../images/AudacityLogo.xpm"
#include "../../src/commands/CommandContext.h"
#include "../../src/commands/CommandManager.h"
#include "widgets/AudacityMessageBox.h"

#include "NyqBench.h"

#include <iostream>
#include <ostream>
#include <sstream>

//
// Images are from the Tango Icon Gallery
// http://tango.freedesktop.org/Tango_Icon_Gallery
//
#include "images/document-new-small.xpm"
#include "images/document-open-small.xpm"
#include "images/document-save-as-small.xpm"
#include "images/document-save-small.xpm"
#include "images/edit-clear-small.xpm"
#include "images/edit-copy-small.xpm"
#include "images/edit-cut-small.xpm"
#include "images/edit-delete-small.xpm"
#include "images/edit-find-small.xpm"
#include "images/edit-paste-small.xpm"
#include "images/edit-redo-small.xpm"
#include "images/edit-select-all-small.xpm"
#include "images/edit-undo-small.xpm"
#include "images/go-top-small.xpm"
#include "images/go-up-small.xpm"
#include "images/go-previous-small.xpm"
#include "images/go-next-small.xpm"
#include "images/system-search-small.xpm"
#include "images/media-playback-start-small.xpm"
#include "images/media-playback-stop-small.xpm"

#include "images/document-new-large.xpm"
#include "images/document-open-large.xpm"
#include "images/document-save-as-large.xpm"
#include "images/document-save-large.xpm"
#include "images/edit-clear-large.xpm"
#include "images/edit-copy-large.xpm"
#include "images/edit-cut-large.xpm"
#include "images/edit-delete-large.xpm"
#include "images/edit-find-large.xpm"
#include "images/edit-paste-large.xpm"
#include "images/edit-redo-large.xpm"
#include "images/edit-select-all-large.xpm"
#include "images/edit-undo-large.xpm"
#include "images/go-top-large.xpm"
#include "images/go-up-large.xpm"
#include "images/go-previous-large.xpm"
#include "images/go-next-large.xpm"
#include "images/system-search-large.xpm"
#include "images/media-playback-start-large.xpm"
#include "images/media-playback-stop-large.xpm"

/*
//#define ModuleDispatchName "ModuleDispatch"
See the example in this file.  It has several cases/options in it.
*/

namespace {
CommandHandlerObject &findme(AudacityProject&)
{
   return *NyqBench::GetBench();
}

void RegisterMenuItems()
{
  // Get here only after the module version check passes
   using namespace MenuTable;
   static AttachedItem sAttachment{ wxT("Tools"),
      ( FinderScope( findme ), Section( wxT("NyquistWorkBench"),
         Command( wxT("NyqBench"), XXO("&Nyquist Workbench..."),
            static_cast<CommandFunctorPointer>(&NyqBench::ShowNyqBench),
            AudioIONotBusyFlag())
      ) )
   };
}
}

DEFINE_VERSION_CHECK

extern "C"
{
   static NyqBench *gBench = NULL;

   extern int DLL_API ModuleDispatch(ModuleDispatchTypes type);
   // ModuleDispatch
   // is called by Audacity to initialize/terminate the module
   int ModuleDispatch(ModuleDispatchTypes type){
      switch (type){
         case ModuleInitialize:
            RegisterMenuItems();
            break;
         case AppQuiting: {
            //It is perfectly OK for gBench to be NULL.
            //Can happen if the menu item was never invoked.
            //wxASSERT(gBench != NULL);
            if (gBench) {
               // be sure to do this while gPrefs still exists:
               gBench->SavePrefs();
               gBench->Destroy();
               gBench = NULL;
            }
         }
         break;
         default:
         break;
      }
      return 1;
   }
};

//----------------------------------------------------------------------------
// NyqTextCtrl
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(NyqTextCtrl, wxTextCtrl)
#if defined(__WXMAC__)
   EVT_KEY_DOWN(NyqTextCtrl::OnKeyDown)
#endif
   EVT_KEY_UP(NyqTextCtrl::OnKeyUp)
   EVT_CHAR(NyqTextCtrl::OnChar)
   EVT_UPDATE_UI(wxID_ANY, NyqTextCtrl::OnUpdate)
END_EVENT_TABLE()

NyqTextCtrl::NyqTextCtrl(wxWindow *parent,
                         wxWindowID id,
                         const wxString &value,
                         const wxPoint & pos,
                         const wxSize & size,
                         int style)
:  wxTextCtrl(parent, id, value, pos, size, style)
{
   mLastCaretPos = -1;
   mLeftParen = -1;
   mRightParen = -1;

   mOn.SetTextColour(*wxRED);
   mOff.SetTextColour(*wxBLACK);
}

void NyqTextCtrl::SetFocusFromKbd()
{
#if defined(__WXMSW__)
   // We do this to prevent wxMSW from selecting all text when the
   // user tabs into the text controls.
   wxWindowBase::SetFocusFromKbd();
#else
   wxTextCtrl::SetFocusFromKbd();
#endif
}

void NyqTextCtrl::MarkDirty()
{
   wxTextCtrl::MarkDirty();
   FindParens();
}

void NyqTextCtrl::OnChar(wxKeyEvent & e)
{
	e.Skip();

   // Hide any previously highlighted parens
   if (mLeftParen >= 0) {
#if defined(__WXMSW__)
      Freeze(); // Prevents selection flashing on Windows
#endif

      SetStyle(mLeftParen, mLeftParen + 1, mOff);
      SetStyle(mLeftParens[mLeftParen], mLeftParens[mLeftParen] + 1, mOff);
      mLeftParen = -1;
      mRightParen = -1;

#if defined(__WXMSW__)
      Thaw(); // Prevents selection flashing on Windows
#endif
   }
}

#if defined(__WXMAC__REMOVED_UNTIL_ITS_PROVEN_THAT_IT_IS_STILL_NEEDED)
#include <wx/mac/uma.h>

// This is hackage to correct a problem on Leopard where the
// caret jumps up two lines when the up arrow is pressed and
// the caret is at the beginning of the line.
void NyqTextCtrl::OnKeyDown(wxKeyEvent & e)
{
   e.Skip();
   if (UMAGetSystemVersion() >= 0x1050) {
      if (e.GetKeyCode() == WXK_UP && e.GetModifiers() == 0) {
         long x;
         long y;
   
         PositionToXY(GetInsertionPoint(), &x, &y);
         if (x == 0 && y > 1) {
            y--;
            SetInsertionPoint(XYToPosition(x, y) - 1);
            e.Skip(false);
         }
      }
   }
}
#endif

void NyqTextCtrl::OnKeyUp(wxKeyEvent & e)
{
   e.Skip();

   int pos = GetInsertionPoint();
   int lpos = wxMax(0, pos - 1);

   wxString text = GetRange(lpos, pos);

   if (text[0] == wxT('(')) {
      wxLongToLongHashMap::const_iterator left = mLeftParens.find(lpos);
      if (left != mLeftParens.end()) {
         Colorize(lpos, left->second);
      }
   }
   else if (text[0] == wxT(')')) {
      wxLongToLongHashMap::const_iterator right = mRightParens.find(lpos);
      if (right != mRightParens.end()) {
         Colorize(right->second, lpos);
      }
   }
}

void NyqTextCtrl::OnUpdate(wxUpdateUIEvent & e)
{
   int pos = GetInsertionPoint();

   if (pos != mLastCaretPos) {
      int lpos = wxMax(0, pos - 1);
   
      wxString text = GetRange(lpos, pos);
      if (text.Length() > 0) {
         if (text[0] == wxT('(')) {
            wxLongToLongHashMap::const_iterator left = mLeftParens.find(lpos);
            if (left != mLeftParens.end()) {
               Colorize(lpos, left->second);
            }
         }
         else if (text[0] == wxT(')')) {
            wxLongToLongHashMap::const_iterator right = mRightParens.find(lpos);
            if (right != mRightParens.end()) {
               Colorize(right->second, lpos);
            }
         }
      }

      mLastCaretPos = pos;
   }
}

void NyqTextCtrl::GoMatch()
{
   MoveCursor(mRightParen, mLeftParen);
}

void NyqTextCtrl::GoTop()
{
   wxLongToLongHashMap::const_iterator it;
   long first = -1;
   long second = -1;

   if (mLeftParen != -1) {
      for (it = mLeftParens.begin(); it != mLeftParens.end(); it++) {
         if (mLeftParen > it->first && mLeftParen < it->second) {
            if (first == -1 || it->first < first) {
               first = it->first;
               second = it->second;
            }
         }
      }
   }

   if (first != -1) {
      MoveCursor(first, second);
   }
}

void NyqTextCtrl::GoUp()
{
   wxLongToLongHashMap::const_iterator it;
   long first = -1;
   long second = -1;

   if (mLeftParen != -1) {
      for (it = mLeftParens.begin(); it != mLeftParens.end(); it++) {
         if (mLeftParen > it->first && mLeftParen < it->second) {
            if (first == -1 || it->first > first) {
               first = it->first;
               second = it->second;
            }
         }
      }
   }

   if (first != -1) {
      MoveCursor(first, second);
   }
}

void NyqTextCtrl::GoPrev()
{
   wxLongToLongHashMap::const_iterator it;
   long first = -1;
   long second = -1;

   if (mLeftParen != -1) {
      for (it = mLeftParens.begin(); it != mLeftParens.end(); it++) {
         if (it->first < mLeftParen && it->first >= first) {
            first = it->first;
            second = it->second;
         }
      }
   }

   if (first != -1) {
      MoveCursor(first, second);
   }
}

void NyqTextCtrl::GoNext()
{
   wxLongToLongHashMap::const_iterator it;
   long first = -1;
   long second = -1;

   if (mLeftParen != -1) {
      for (it = mLeftParens.begin(); it != mLeftParens.end(); it++) {
         if (it->first > mLeftParen && (first == -1 || it->first < first)) {
            first = it->first;
            second = it->second;
         }
      }
   }

   if (first != -1) {
      MoveCursor(first, second);
   }
}

void NyqTextCtrl::MoveCursor(long first, long second)
{
   int pos = GetInsertionPoint();
   int lpos = wxMax(0, pos - 1);

   wxString text = GetRange(lpos, pos);

   if (text[0] == wxT('(')) {
      SetInsertionPoint(first + 1);
      Colorize(first, second);
   }
   else if (text[0] == wxT(')')) {
      SetInsertionPoint(second + 1);
      Colorize(first, second);
   }
}

void NyqTextCtrl::Colorize(long left, long right)
{
   // Hide any previously highlighted parens
   if (mLeftParen >= 0) {
#if defined(__WXMSW__)
      Freeze(); // Prevents selection flashing on Windows
#endif

      SetStyle(mLeftParen, mLeftParen + 1, mOff);
      SetStyle(mLeftParens[mLeftParen], mLeftParens[mLeftParen] + 1, mOff);
      mLeftParen = -1;
      mRightParen = -1;

#if defined(__WXMSW__)
      Thaw(); // Prevents selection flashing on Windows
#endif
   }

   mLeftParen = left;
   mRightParen = right;

   if (mLeftParen != -1) {
      SetStyle(mLeftParen, mLeftParen + 1, mOn);
      SetStyle(mRightParen, mRightParen + 1, mOn);

      SetStyle(mLeftParen + 1, mLeftParen + 1, mOff);
      SetStyle(mRightParen + 1, mRightParen + 1, mOff);
   }
}

void NyqTextCtrl::FindParens()
{
   wxString text = GetValue();
   bool inquotes = false;
   wxArrayInt stack;
   long len = (long) text.Length();
   long pos;

   mLeftParens.clear();
   mRightParens.clear();

   for (pos = 0; pos < len; pos++) {
      wxChar c = text[pos];
      switch (c)
      {
         case wxT('"'):
            inquotes = !inquotes;
         break;

         case wxT(';'):
            if (!inquotes) {
               pos = (long)text.find(wxT('\n'), pos);
               if (pos == (long)wxString::npos) {
                  pos = len;
               }
            }
         break;

         case wxT('#'):
            if (!inquotes) {
               long ndx = pos + 1;
               if (ndx < len && text[(int)ndx] == wxT('|')) {
                  // Shamelessly stolen from xlread.c/pcomment()
                  wxChar lastch = -1;
                  int n = 1;
              
                  /* look for the matching delimiter (and handle nesting) */
                  while (n > 0 && ++pos < len) {
                     wxChar ch = text[(int)pos];
                     if (lastch == '|' && ch == '#') {
                        --n;
                        ch = -1;
                     }
                     else if (lastch == '#' && ch == '|') {
                        ++n;
                        ch = -1;
                     }
                     lastch = ch;
                  }
               }
            }
         break;

         case wxT('('):
            if (!inquotes) {
               stack.Add(pos);
            }
         break;

         case wxT(')'):
            if (!inquotes) {
               if (stack.GetCount() > 0) {
                  int left = stack.Last();
                  stack.RemoveAt(stack.GetCount() - 1);

                  mLeftParens[left] = pos;
                  mRightParens[pos] = left;
               }
            }
         break;
      }
   }
}

//----------------------------------------------------------------------------
// NyqRedirector
//----------------------------------------------------------------------------

NyqRedirector::NyqRedirector(NyqTextCtrl *text)
:  mText(text)
{
   mOld = std::cout.rdbuf(this);
}

NyqRedirector::~NyqRedirector()
{
   std::cout.flush();
   std::cout.rdbuf(mOld);
   if (s.length() > 0) {
      AppendText();
   }
}

int NyqRedirector::overflow(int c)
{
   s += (char)c;
   if (c == '\n') {
      AppendText();
   }

   return 0;
}

void NyqRedirector::AppendText()
{
   mText->AppendText(wxString(s.c_str(), wxConvISO8859_1));
   s.clear();
}

//----------------------------------------------------------------------------
// NyqBench
//----------------------------------------------------------------------------

enum
{
   ID_AUTOLOAD = 20000,

   ID_AUTOWRAP,

   ID_FONT,
   ID_SPLITV,
   ID_SPLITH,
   ID_TOGGLECODE,
   ID_TOGGLEOUTPUT,
   ID_SMALLICONS,
   ID_LARGEICONS,
   ID_MATCH,
   ID_TOP,
   ID_UP,
   ID_PREVIOUS,
   ID_NEXT,

   ID_GO,
   ID_STOP,

   ID_SCRIPT,
   ID_OUTPUT
};

BEGIN_EVENT_TABLE(NyqBench, wxFrame)
   EVT_CLOSE(NyqBench::OnClose)
   EVT_MOVE(NyqBench::OnMove)
   EVT_SIZE(NyqBench::OnSize)

   EVT_MENU(wxID_NEW, NyqBench::OnNew)
   EVT_MENU(wxID_OPEN, NyqBench::OnOpen)
   EVT_MENU(wxID_SAVE, NyqBench::OnSave)
   EVT_MENU(wxID_SAVEAS, NyqBench::OnSaveAs)
   EVT_MENU(wxID_REVERT_TO_SAVED, NyqBench::OnRevert)
   EVT_MENU(ID_AUTOLOAD, NyqBench::OnAutoLoad)
   EVT_MENU(wxID_CLOSE, NyqBench::OnCloseWindow)

   EVT_MENU(wxID_UNDO, NyqBench::OnUndo)
   EVT_MENU(wxID_REDO, NyqBench::OnRedo)
   EVT_MENU(wxID_CUT, NyqBench::OnCut)
   EVT_MENU(wxID_COPY, NyqBench::OnCopy)
   EVT_MENU(wxID_PASTE, NyqBench::OnPaste)
   EVT_MENU(wxID_CLEAR, NyqBench::OnClear)
   EVT_MENU(wxID_SELECTALL, NyqBench::OnSelectAll)
   EVT_MENU(wxID_FIND, NyqBench::OnFind)
   EVT_MENU(ID_MATCH, NyqBench::OnGoMatch)
   EVT_MENU(ID_TOP, NyqBench::OnGoTop)
   EVT_MENU(ID_UP, NyqBench::OnGoUp)
   EVT_MENU(ID_PREVIOUS, NyqBench::OnGoPrev)
   EVT_MENU(ID_NEXT, NyqBench::OnGoNext)
   EVT_MENU(ID_AUTOWRAP, NyqBench::OnAutoWrap)

   EVT_MENU(ID_FONT, NyqBench::OnFont)
   EVT_MENU(ID_SPLITV, NyqBench::OnSplitV)
   EVT_MENU(ID_SPLITH, NyqBench::OnSplitH)
   EVT_MENU(ID_TOGGLECODE, NyqBench::OnToggleCode)
   EVT_MENU(ID_TOGGLEOUTPUT, NyqBench::OnToggleOutput)
   EVT_MENU(ID_SMALLICONS, NyqBench::OnSmallIcons)
   EVT_MENU(ID_LARGEICONS, NyqBench::OnLargeIcons)

   EVT_MENU(ID_GO, NyqBench::OnGo)
   EVT_MENU(ID_STOP, NyqBench::OnStop)

   EVT_MENU(wxID_ABOUT, NyqBench::OnAbout)

   EVT_FIND(wxID_ANY, NyqBench::OnFindDialog)
   EVT_FIND_NEXT(wxID_ANY, NyqBench::OnFindDialog)
   EVT_FIND_REPLACE(wxID_ANY, NyqBench::OnFindDialog)
   EVT_FIND_REPLACE_ALL(wxID_ANY, NyqBench::OnFindDialog)
   EVT_FIND_CLOSE(wxID_ANY, NyqBench::OnFindDialog)

   EVT_TEXT(ID_SCRIPT, NyqBench::OnTextUpdate)

   EVT_UPDATE_UI(wxID_SAVE, NyqBench::OnMenuUpdate)
   EVT_UPDATE_UI(wxID_SAVEAS, NyqBench::OnMenuUpdate)
   EVT_UPDATE_UI(wxID_REVERT_TO_SAVED, NyqBench::OnMenuUpdate)

   EVT_UPDATE_UI(wxID_UNDO, NyqBench::OnUndoUpdate)
   EVT_UPDATE_UI(wxID_REDO, NyqBench::OnRedoUpdate)
   EVT_UPDATE_UI(wxID_CUT, NyqBench::OnCutUpdate)
   EVT_UPDATE_UI(wxID_COPY, NyqBench::OnCopyUpdate)
   EVT_UPDATE_UI(wxID_PASTE, NyqBench::OnPasteUpdate)
   EVT_UPDATE_UI(wxID_CLEAR, NyqBench::OnClearUpdate)

   EVT_UPDATE_UI(ID_SPLITH, NyqBench::OnViewUpdate)
   EVT_UPDATE_UI(ID_SPLITV, NyqBench::OnViewUpdate)
   EVT_UPDATE_UI(ID_TOGGLECODE, NyqBench::OnViewUpdate)
   EVT_UPDATE_UI(ID_TOGGLEOUTPUT, NyqBench::OnViewUpdate)

   EVT_UPDATE_UI(ID_GO, NyqBench::OnRunUpdate)

   EVT_UPDATE_UI(ID_SCRIPT, NyqBench::OnScriptUpdate)
   EVT_UPDATE_UI(ID_OUTPUT, NyqBench::OnOutputUpdate)
END_EVENT_TABLE()

/*static*/ NyqBench *NyqBench::GetBench()
{
   if (gBench == nullptr)
   {
      gBench = new NyqBench(NULL);
   }

   return gBench;
}

NyqBench::NyqBench(wxWindow * parent)
:  wxFrame(NULL,
           wxID_ANY,
           wxEmptyString,
           wxDefaultPosition,
           wxDefaultSize,
           wxDEFAULT_FRAME_STYLE |
           wxMINIMIZE_BOX |
           wxMAXIMIZE_BOX |
           wxRESIZE_BORDER)
{
   mFindDlg = NULL;
   mRunning = false;
   mScriptBox = NULL;
   mOutputBox = NULL;
   mScript = NULL;
   mOutput = NULL;

   mPath = gPrefs->Read(wxT("NyqBench/Path"), wxEmptyString);
   mAutoLoad = (gPrefs->Read(wxT("NyqBench/AutoLoad"), 0L) != 0);
   mAutoWrap = (gPrefs->Read(wxT("NyqBench/AutoWrap"), true) != 0);
   mLargeIcons = (gPrefs->Read(wxT("NyqBench/LargeIcons"), 0L) != 0);
   mSplitMode = gPrefs->Read(wxT("NyqBench/SplitMode"), wxSPLIT_VERTICAL);
   mShowCode = (gPrefs->Read(wxT("NyqBench/ShowScript"), true) != 0);
   mShowOutput = (gPrefs->Read(wxT("NyqBench/ShowOutput"), true) != 0);

   SetIcon(wxICON(AudacityLogo));
   SetBackgroundColour(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   wxMenuBar *bar = new wxMenuBar();

   wxMenu *menu = new wxMenu();
   menu->Append(wxID_NEW, wxT("&New\tCtrl+N"));
   menu->Append(wxID_OPEN, wxT("&Open...\tCtrl+O"));
   menu->Append(wxID_SAVE, wxT("&Save...\tCtrl+S"));
   menu->Append(wxID_SAVEAS, wxT("Save &As...\tCtrl+Shift+S"));
   menu->AppendSeparator();
   menu->Append(wxID_REVERT_TO_SAVED, _T("&Revert to Saved"));
   menu->AppendSeparator();
   menu->AppendCheckItem(ID_AUTOLOAD, _T("Auto &Load Last File"))->Check(mAutoLoad);
   menu->AppendSeparator();
   menu->Append(wxID_CLOSE, wxT("&Close Window\tCtrl+W"));
   bar->Append(menu, wxT("&File"));

   menu = new wxMenu();
   menu->Append(wxID_UNDO, _("&Undo\tCtrl+Z"));
   menu->Append(wxID_REDO, _("&Redo\tCtrl+Y"));
   menu->AppendSeparator();
   menu->Append(wxID_CUT, _("Cu&t\tCtrl+X"));
   menu->Append(wxID_COPY, _("&Copy\tCtrl+C"));
   menu->Append(wxID_PASTE, _("&Paste\tCtrl+V"));
   menu->Append(wxID_CLEAR, _("Cle&ar\tCtrl+L"));
   menu->AppendSeparator();
   menu->Append(wxID_SELECTALL, _("Select A&ll\tCtrl+A"));
   menu->AppendSeparator();
   menu->Append(wxID_FIND, _("&Find...\tCtrl+F"));
   menu->AppendSeparator();
   wxMenu *sub = new wxMenu();
   sub->Append(ID_MATCH, _("&Matching Paren\tF8"));
   sub->Append(ID_TOP, _("&Top S-expr\tF9"));
   sub->Append(ID_UP, _("&Higher S-expr\tF10"));
   sub->Append(ID_PREVIOUS, _("&Previous S-expr\tF11"));
   sub->Append(ID_NEXT, _("&Next S-expr\tF12"));
   menu->AppendSubMenu(sub, _("&Go to"));
   menu->AppendSeparator();
   menu->AppendCheckItem(ID_AUTOWRAP, _T("Auto &Wrap"))->Check(mAutoWrap);
   bar->Append(menu, wxT("&Edit"));

   menu = new wxMenu();
   menu->Append(ID_FONT, _("Select &Font..."));
   menu->AppendSeparator();
   menu->Append(ID_SPLITV, _("Split &Vertically"));
   menu->Append(ID_SPLITH, _("Split &Horizontally"));
   menu->AppendSeparator();
   menu->AppendCheckItem(ID_TOGGLECODE, _("Show S&cript"));
   menu->AppendCheckItem(ID_TOGGLEOUTPUT, _("Show &Output"));
   menu->AppendSeparator();
   sub = new wxMenu();
   sub->AppendRadioItem(ID_LARGEICONS, _("&Large Icons"));
   sub->AppendRadioItem(ID_SMALLICONS, _("&Small Icons"));
   menu->AppendSubMenu(sub, _("Toolbar"));
   bar->Append(menu, wxT("&View"));

   menu = new wxMenu();
   menu->Append(ID_GO, _("&Go\tF5"));
   menu->Append(ID_STOP, _("&Stop\tF6"));
   bar->Append(menu, wxT("&Run"));

#if defined(__WXMAC__)
   menu->Append(wxID_ABOUT, _("&About"));
#else
   menu = new wxMenu();
   menu->Append(wxID_ABOUT, _("&About"));
   bar->Append(menu, wxT("Help"));
#endif

   SetMenuBar(bar);

   RecreateToolbar(mLargeIcons);

   wxRect r;
   r.SetX(gPrefs->Read(wxT("NyqBench/Window/X"), -1));
   r.SetY(gPrefs->Read(wxT("NyqBench/Window/Y"), -1));
   r.SetWidth(gPrefs->Read(wxT("NyqBench/Window/Width"), -1));
   r.SetHeight(gPrefs->Read(wxT("NyqBench/Window/Height"), -1));
   if (r == wxRect(-1, -1, -1, -1)) {
      Center();
   }
   else {
      SetSize(r);
   }

   bool maximized = false;
   gPrefs->Read(wxT("NyqBench/Window/Maximized"), maximized);
   if (maximized) {
      Maximize();
   }

   long sashpos;
   sashpos = gPrefs->Read(wxT("NyqBench/SplitX"), 0l);
   if (sashpos > 0) {
      mSplitter->SetSashPosition(sashpos);
   }

   wxString dflt = wxSystemSettings::GetFont(wxSYS_SYSTEM_FONT).GetNativeFontInfoDesc();
   wxString desc;
   wxTextAttr attr;

   desc = gPrefs->Read(wxT("NyqBench/ScriptFont"), dflt);
   mScriptFont.SetNativeFontInfo(desc);
#if defined(__WXMSW__)
   // Force SYSTEM encoding to prevent conversion to Unicode in wxTextCtrl::DoWriteText().
   // I don't know if this is something I'm doing wrong, but I'll have to look at this
   // later if I get bored.
   mScriptFont.SetEncoding(wxFONTENCODING_SYSTEM);
#endif
   attr.SetFont(mScriptFont);
   mScript->SetDefaultStyle(attr);

   desc = gPrefs->Read(wxT("NyqBench/OutputFont"), dflt);
   mOutputFont.SetNativeFontInfo(desc);
#if defined(__WXMSW__)
   // Force SYSTEM encoding to prevent conversion to Unicode in wxTextCtrl::DoWriteText().
   // I don't know if this is something I'm doing wrong, but I'll have to look at this
   // later if I get bored.
   mOutputFont.SetEncoding(wxFONTENCODING_SYSTEM);
#endif
   attr.SetFont(mOutputFont);
   mOutput->SetDefaultStyle(attr);

   if (mAutoLoad && !mPath.GetFullPath().IsEmpty()) {
      LoadFile();
   }

   SetWindowTitle();
}

NyqBench::~NyqBench()
{
}

void NyqBench::SavePrefs()
{
   gPrefs->Write(wxT("NyqBench/Window/Maximized"), IsMaximized());
   if (!IsMaximized()) {
      wxRect r = GetRect();

#if !defined(__WXMAC__)
      if (IsIconized()) {
         r = mLastSize;
      }
#endif

      gPrefs->Write(wxT("NyqBench/Window/X"), r.GetX());
      gPrefs->Write(wxT("NyqBench/Window/Y"), r.GetY());
      gPrefs->Write(wxT("NyqBench/Window/Width"), r.GetWidth());
      gPrefs->Write(wxT("NyqBench/Window/Height"), r.GetHeight());
   }

   gPrefs->Write(wxT("NyqBench/AutoLoad"), mAutoLoad);
   gPrefs->Write(wxT("NyqBench/AutoWrap"), mAutoWrap);
   gPrefs->Write(wxT("NyqBench/ScriptFont"), mScriptFont.GetNativeFontInfoDesc());
   gPrefs->Write(wxT("NyqBench/OutputFont"), mOutputFont.GetNativeFontInfoDesc());
   gPrefs->Write(wxT("NyqBench/LargeIcons"), mLargeIcons);
   gPrefs->Write(wxT("NyqBench/SplitX"), mSplitter->IsSplit() ? mSplitter->GetSashPosition() : 0);
   gPrefs->Write(wxT("NyqBench/SplitMode"), mSplitter->IsSplit() ? mSplitter->GetSplitMode() : 0);
   gPrefs->Write(wxT("NyqBench/ShowCode"), mScript->IsShown());
   gPrefs->Write(wxT("NyqBench/ShowOutput"), mOutput->IsShown());
}

void NyqBench::PopulateOrExchange(ShuttleGui & S)
{
   S.StartHorizontalLay(wxEXPAND, true);
   {
      wxPanel *scriptp;
      wxPanel *outputp;
      wxStaticBoxSizer *scripts;
      wxStaticBoxSizer *outputs;
      wxBoxSizer *bs;

      mSplitter = new wxSplitterWindow(this,
                                       wxID_ANY,
                                       wxDefaultPosition,
                                       wxDefaultSize,
                                       wxSP_LIVE_UPDATE |
                                       wxSP_3DSASH |
                                       wxSP_NOBORDER);
      
      scriptp = new wxPanel(mSplitter,
                            wxID_ANY,
                            wxDefaultPosition,
                            wxDefaultSize,
                            wxNO_FULL_REPAINT_ON_RESIZE |
                            wxCLIP_CHILDREN);
      bs = new wxBoxSizer(wxVERTICAL);
      scriptp->SetSizer(bs);

      mScriptBox = new wxStaticBox(scriptp,
                                   wxID_ANY,
                                   _("Script"));

      scripts = new wxStaticBoxSizer(mScriptBox,
                                     wxVERTICAL);
      bs->Add(scripts, true, wxEXPAND);

      mScript = new NyqTextCtrl(scriptp,
                                ID_SCRIPT,
                                wxEmptyString,
                                wxDefaultPosition,
                                wxDefaultSize,
                                wxTE_RICH2 | wxTE_RICH |
                                (mAutoWrap ? wxTE_BESTWRAP : wxTE_DONTWRAP) |
                                wxTE_NOHIDESEL |
                                wxTE_MULTILINE);
      scripts->Add(mScript, true, wxEXPAND);

      outputp = new wxPanel(mSplitter,
                            wxID_ANY,
                            wxDefaultPosition,
                            wxDefaultSize,
                            wxNO_FULL_REPAINT_ON_RESIZE |
                            wxCLIP_CHILDREN);
      bs = new wxBoxSizer(wxVERTICAL);
      outputp->SetSizer(bs);

      mOutputBox = new wxStaticBox(outputp,
                                   wxID_ANY,
                                   _("Output"));
      outputs = new wxStaticBoxSizer(mOutputBox,
                                     wxVERTICAL);
      bs->Add(outputs, true, wxEXPAND);

      mOutput = new NyqTextCtrl(outputp,
                                ID_OUTPUT,
                                wxEmptyString,
                                wxDefaultPosition,
                                wxDefaultSize,
                                wxTE_READONLY |
#if !defined(__WXMAC__)
// I could not get the bloody horizontal scroll bar to appear on 
// wxMac, so we can't use wxTE_DONTWRAP as you can't easily scroll
// left and right.
                                wxTE_DONTWRAP |
#endif
                                wxTE_NOHIDESEL |
                                wxTE_MULTILINE);
      outputs->Add(mOutput, true, wxEXPAND);

      switch (mSplitMode)
      {
         case wxSPLIT_VERTICAL:
            mSplitter->SplitVertically(scriptp, outputp, 300);
         break;

         case wxSPLIT_HORIZONTAL:
            mSplitter->SplitHorizontally(scriptp, outputp, 300);
         break;

         default:
            mSplitter->Initialize((mShowCode ? scriptp : outputp));
         break;
      }

      mSplitter->SetMinimumPaneSize(50);

      S.AddSpace(5, 1);
      S.Prop(true);
      S.Position(wxEXPAND).AddWindow(mSplitter);
      S.AddSpace(5, 1);

      mSplitter->SetMinSize(wxSize(600, 400));
   }
   S.EndHorizontalLay();

   S.AddSpace(1, 5);

   return;
}

void NyqBench::OnClose(wxCloseEvent & e)
{
   if (!Validate()) {
      e.Veto();
   }
   else {
      Show(false);
   }
}

void NyqBench::OnMove(wxMoveEvent & e)
{
   e.Skip();
   if (!IsIconized() && !IsMaximized()) {
      mLastSize.SetPosition(e.GetPosition());
   }
}

void NyqBench::OnSize(wxSizeEvent & e)
{
   e.Skip();
   if (!IsIconized() && !IsMaximized()) {
      mLastSize.SetSize(e.GetSize());
   }
}

void NyqBench::OnCloseWindow(wxCommandEvent & e)
{
   Close();
}

void NyqBench::OnNew(wxCommandEvent & e)
{
   if (!Validate()) {
      return;
   }

   mPath.SetFullName(wxEmptyString);

   while (mScript->CanUndo()) {
      mScript->Undo();
   }

   mScript->Clear();
   mScript->DiscardEdits();

   SetWindowTitle();
}

void NyqBench::OnOpen(wxCommandEvent & e)
{
   if (mScript->IsModified() && !Validate()) {
      return;
   }

   wxFileDialog dlog(this,
                     _("Load Nyquist script"),
                     mPath.GetPath(),
                     wxEmptyString,
                     _("Nyquist scripts (*.ny)|*.ny|Lisp scripts (*.lsp)|*.lsp|All files|*"),
                     wxFD_OPEN | wxRESIZE_BORDER);
 
   if (dlog.ShowModal() != wxID_OK) {
      return;
   }

   mPath = dlog.GetPath();
   gPrefs->Write(wxT("NyqBench/Path"), mPath.GetFullPath());

   LoadFile();

   SetWindowTitle();
}

void NyqBench::OnSave(wxCommandEvent & e)
{
   if (mScript->GetLastPosition() == 0) {
      return;
   }
 
   if (mPath.GetFullPath().IsEmpty()) {
      OnSaveAs(e);
      return;
   }

   if (!mScript->SaveFile(mPath.GetFullPath()))
   {
      AudacityMessageBox(XO("Script was not saved."),
                   XO("Warning"),
                   wxICON_EXCLAMATION,
                   this);
      return;
   }
}

void NyqBench::OnSaveAs(wxCommandEvent & e)
{
   if (mScript->GetLastPosition() == 0) {
      return;
   }
 
   wxFileDialog dlog(this,
                     _("Save Nyquist script"),
                     mPath.GetFullPath(),
                     wxEmptyString,
                     _("Nyquist scripts (*.ny)|*.ny|Lisp scripts (*.lsp)|*.lsp|All files|*"),
                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER);
 
   if (dlog.ShowModal() != wxID_OK) {
      return;
   }

   mPath = dlog.GetPath();
   gPrefs->Write(wxT("NyqBench/Path"), mPath.GetFullPath());

   if (!mScript->SaveFile(mPath.GetFullPath()))
   {
      AudacityMessageBox(XO("Script was not saved."),
                   XO("Warning"),
                   wxICON_EXCLAMATION,
                   this);
      return;
   }

   SetWindowTitle();
}

void NyqBench::OnAutoLoad(wxCommandEvent & e)
{
   mAutoLoad = e.IsChecked();
}

void NyqBench::OnRevert(wxCommandEvent & e)
{
   if (mPath.GetFullPath().IsEmpty()) {
      return;
   }

   if (!Validate()) {
      return;
   }

   LoadFile();
}

void NyqBench::OnUndo(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->Undo();
}

void NyqBench::OnRedo(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->Redo();
}

void NyqBench::OnCut(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->Cut();
}

void NyqBench::OnCopy(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->Copy();
}

void NyqBench::OnPaste(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->Paste();
}

void NyqBench::OnClear(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->Clear();
}

void NyqBench::OnSelectAll(wxCommandEvent & e)
{
   (FindFocus() == mScript ? mScript : mOutput)->SetSelection(-1, -1);
}

void NyqBench::OnFind(wxCommandEvent & e)
{
   if (mFindDlg ) {
       delete mFindDlg;
       mFindDlg = NULL;
   }
   else {
      NyqTextCtrl *w = (NyqTextCtrl *) FindFocus();
      if (w == mScript || w == mOutput) {
         mFindText = w;

         int flags = 0;

         flags |= (gPrefs->Read(wxT("NyqBench/Find/Down"), 0L) ? wxFR_DOWN : 0);
         flags |= (gPrefs->Read(wxT("NyqBench/Find/Word"), 0L) ? wxFR_WHOLEWORD : 0);
         flags |= (gPrefs->Read(wxT("NyqBench/Find/Case"), 0L) ? wxFR_MATCHCASE : 0);

         mFindData.SetFlags(flags);

         mFindDlg = new wxFindReplaceDialog(this,
                                            &mFindData,
                                            _("Find dialog"),
                                            wxFR_NOWHOLEWORD);
         mFindDlg->Show(true);
      }
   }
}

void NyqBench::OnGoMatch(wxCommandEvent & e)
{
   mScript->GoMatch();
}

void NyqBench::OnGoTop(wxCommandEvent & e)
{
   mScript->GoTop();
}

void NyqBench::OnGoUp(wxCommandEvent & e)
{
   mScript->GoUp();
}

void NyqBench::OnGoPrev(wxCommandEvent & e)
{
   mScript->GoPrev();
}

void NyqBench::OnGoNext(wxCommandEvent & e)
{
   mScript->GoNext();
}

void NyqBench::OnAutoWrap(wxCommandEvent & e)
{
   mAutoWrap = e.IsChecked();

   wxWindow *parent = mScript->GetParent();
   wxString text = mScript->GetValue();
   bool focused = wxWindow::FindFocus() == mScript;
   long pos = mScript->GetInsertionPoint();
   long from;
   long to;
   mScript->GetSelection(&from, &to);

   wxSizer *s = mScript->GetContainingSizer();
   s->Detach(mScript);
   delete mScript;

   mScript = new NyqTextCtrl(parent,
                             ID_SCRIPT,
                             wxEmptyString,
                             wxDefaultPosition,
                             wxDefaultSize,
                             (mAutoWrap ? wxTE_BESTWRAP : wxTE_DONTWRAP) |
                             wxTE_NOHIDESEL | wxTE_RICH2 |
                             wxTE_MULTILINE);
   s->Add(mScript, 1, wxEXPAND);
   s->Layout();

   mScript->ChangeValue(text);
   mScript->SetInsertionPoint(pos);
   mScript->SetSelection(from, to);

   if (focused) {
      mScript->SetFocus();
   }
}

void NyqBench::OnFont(wxCommandEvent & e)
{
   wxWindow *w = FindFocus();
   wxFontData data;
   wxFontDialog dlg(this, data);

   if (w != mScript &&  w != mOutput) {
      return;
   }

   data.SetInitialFont(w == mScript ? mScriptFont : mOutputFont);

   if (dlg.ShowModal() == wxID_OK) {
      wxFontData retData = dlg.GetFontData();
      wxFont font = retData.GetChosenFont();
      wxTextAttr attr;
      attr.SetFont(font);

      if (w == mScript) {
         mScriptFont = font;
      }
      else {
         mOutputFont = font;
      }

      ((wxTextCtrl *)w)->SetDefaultStyle(attr);
      ((wxTextCtrl *)w)->SetStyle(0, ((wxTextCtrl *)w)->GetLastPosition(), attr);
      w->Refresh();
   }
}

void NyqBench::OnSplitV(wxCommandEvent & e)
{
   if (mSplitter->IsSplit()) {
      mSplitter->Unsplit();
   }

   mSplitter->SplitVertically(mScript->GetParent(), mOutput->GetParent());
}

void NyqBench::OnSplitH(wxCommandEvent & e)
{
   if (mSplitter->IsSplit()) {
      mSplitter->Unsplit();
   }

   mSplitter->SplitHorizontally(mScript->GetParent(), mOutput->GetParent());
}

void NyqBench::OnToggleCode(wxCommandEvent & e)
{
   if (e.IsChecked()) {
      if (mSplitter->IsSplit()) {
         // Should never happen
         return;
      }

      if (mSplitMode == wxSPLIT_VERTICAL) {
         mSplitter->SplitVertically(mScript->GetParent(), mOutput->GetParent());
      }
      else {
         mSplitter->SplitHorizontally(mScript->GetParent(), mOutput->GetParent());
      }
   }
   else {
      if (!mSplitter->IsSplit()) {
         // Should never happen
         return;
      }

      mSplitMode = mSplitter->GetSplitMode();
      mSplitter->Unsplit(mScript->GetParent());
   }
}

void NyqBench::OnToggleOutput(wxCommandEvent & e)
{
   if (e.IsChecked()) {
      if (mSplitter->IsSplit()) {
         // Should never happen
         return;
      }

      if (mSplitMode == wxSPLIT_VERTICAL) {
         mSplitter->SplitVertically(mScript->GetParent(), mOutput->GetParent());
      }
      else {
         mSplitter->SplitHorizontally(mScript->GetParent(), mOutput->GetParent());
      }
   }
   else {
      if (!mSplitter->IsSplit()) {
         // Should never happen
         return;
      }

      mSplitMode = mSplitter->GetSplitMode();
      mSplitter->Unsplit(mOutput->GetParent());
   }
}

void NyqBench::OnSmallIcons(wxCommandEvent & e)
{
   RecreateToolbar(false);
}

void NyqBench::OnLargeIcons(wxCommandEvent & e)
{
   RecreateToolbar(true);
}

void NyqBench::OnGo(wxCommandEvent & e)
{
   auto pEffect =
      std::make_unique<NyquistEffect>(L"Nyquist Effect Workbench");
   mEffect = pEffect.get();
   const PluginID & ID =
      EffectManager::Get().RegisterEffect(std::move(pEffect));

   mEffect->SetCommand(mScript->GetValue());
   mEffect->RedirectOutput();

   auto p = GetActiveProject().lock();
   wxASSERT(p);

   if (p) {
      wxWindowDisabler disable(this);
      NyqRedirector redir((NyqTextCtrl *)mOutput);

      mRunning = true;
      UpdateWindowUI();

      EffectUI::DoEffect(ID, CommandContext(*p), 0);

      mRunning = false;
      UpdateWindowUI();
   }

   Raise();

   EffectManager::Get().UnregisterEffect(ID);
}

void NyqBench::OnStop(wxCommandEvent & e)
{
   mRunning = false;
   mEffect->Stop();
}

void NyqBench::OnAbout(wxCommandEvent & e)
{
   wxAboutDialogInfo i;

   i.AddArtist(_("Harvey Lubin (logo)"));
   i.AddArtist(_("Tango Icon Gallery (toolbar icons)"));
   i.AddDeveloper(_("Leland Lucius"));
   i.SetCopyright(_("(C) 2009 by Leland Lucius"));
   i.SetDescription(_("External Audacity module which provides a simple IDE for writing effects."));
   i.SetName(_("Nyquist Effect Workbench"));
   i.SetVersion(__TDATE__);

   wxAboutBox(i);
}

void NyqBench::OnFindDialog(wxFindDialogEvent & e)
{
   wxEventType type = e.GetEventType();

   if (type == wxEVT_COMMAND_FIND_CLOSE) {
      wxFindReplaceDialog *dlg = e.GetDialog();

      dlg->Destroy();

      int flags = mFindData.GetFlags();

      gPrefs->Write(wxT("NyqBench/Find/Down"), (flags & wxFR_DOWN) != 0);
      gPrefs->Write(wxT("NyqBench/Find/Word"), (flags & wxFR_WHOLEWORD) != 0);
      gPrefs->Write(wxT("NyqBench/Find/Case"), (flags & wxFR_MATCHCASE) != 0);

      mFindDlg = NULL;
      mFindText = NULL;

      return;
   }

   wxString text = mFindText->GetValue();

#if defined(__WXMSW__)
   // We cheat on Windows.  We know that the Windows text control
   // uses CRLF for line endings and if we don't account for that,
   // the selection positions will be off.
   //
   // Not sure why I thought I needed this, but it appears not to
   // be.  Leaving just in case.
   //
   // text.Replace(wxT("\n"), wxT("\r\n"));
#endif

   size_t startpos = mFindText->GetInsertionPoint();
   size_t len = mFindText->GetLastPosition();
   size_t pos;

   wxString find = e.GetFindString();
   bool down = (e.GetFlags() & wxFR_DOWN) != 0;
   bool mixed = (e.GetFlags() & wxFR_MATCHCASE) != 0;

   if (!mixed) {
      text.MakeUpper();
      find.MakeUpper();
   }

   if (down) {
      pos = text.find(find, startpos);
      if (type == wxEVT_COMMAND_FIND_NEXT && pos == startpos && pos < len) {
         pos = text.find(find, startpos + 1);
      }
   }
   else {
      pos = text.rfind(find, startpos);
      if (type == wxEVT_COMMAND_FIND_NEXT && pos == startpos && pos > 0) {
         pos = text.rfind(find, startpos - 1);
      }
   }

   if (pos == wxString::npos) {
      AudacityMessageBox(XO("No matches found"),
                   XO("Nyquist Effect Workbench"),
                   wxOK | wxCENTER,
                   e.GetDialog());

      return;
   }

   mFindText->SetInsertionPoint((long)pos);

#if defined(__WXGTK__)
   // GTK's selection and intertion pointer interact where the insertion
   // pointer winds up after the second parameter, so we reverse them to
   // force the pointer at the beginning of the selection.  Doing so
   // allows reverse find to work properly.
   mFindText->SetSelection((long)(pos + find.Length()), (long)pos);
#else
   mFindText->SetSelection((long)pos, (long)(pos + find.Length()));
#endif

#if defined(__WXMAC__)
   // Doing this coaxes the text control to update the selection.  Without
   // it the selection doesn't appear to change if the found string is within
   // the currently displayed text, i.e., no reposition is needed.
   mFindText->Show(false);
   mFindText->Show(true);
#endif
}

void NyqBench::OnTextUpdate(wxCommandEvent & e)
{
   // This really shouldn't be necessary, but Paste()ing doesn't mark the
   // control as dirty...at least on the Mac.
   ((NyqTextCtrl *) e.GetEventObject())->MarkDirty();
}

void NyqBench::OnMenuUpdate(wxUpdateUIEvent & e)
{
   if (e.GetId() != wxID_REVERT_TO_SAVED) {
      e.Enable((mScript->GetLastPosition() > 0) || mScript->IsModified());
   }
   else {
      e.Enable(mScript->IsModified());
   }
}

void NyqBench::OnUndoUpdate(wxUpdateUIEvent & e)
{
   e.Enable((FindFocus() == mScript ? mScript : mOutput)->CanUndo());
}

void NyqBench::OnRedoUpdate(wxUpdateUIEvent & e)
{
   e.Enable((FindFocus() == mScript ? mScript : mOutput)->CanRedo());
}

void NyqBench::OnCutUpdate(wxUpdateUIEvent & e)
{
   e.Enable((FindFocus() == mScript ? mScript : mOutput)->CanCut());
}

void NyqBench::OnCopyUpdate(wxUpdateUIEvent & e)
{
   e.Enable((FindFocus() == mScript ? mScript : mOutput)->CanCopy());
}

void NyqBench::OnPasteUpdate(wxUpdateUIEvent & e)
{
   e.Enable((FindFocus() == mScript ? mScript : mOutput)->CanPaste());
}

void NyqBench::OnClearUpdate(wxUpdateUIEvent & e)
{
   e.Enable(FindFocus() == mOutput ? true : false);
}

void NyqBench::OnViewUpdate(wxUpdateUIEvent & e)
{
   wxMenuBar *bar = GetMenuBar();
   bar->Enable(ID_SPLITV, !mSplitter->IsSplit() || mSplitter->GetSplitMode() != wxSPLIT_VERTICAL);
   bar->Enable(ID_SPLITH, !mSplitter->IsSplit() || mSplitter->GetSplitMode() != wxSPLIT_HORIZONTAL);
   bar->Check(ID_TOGGLECODE, mScript->GetParent()->IsShown());
   bar->Check(ID_TOGGLEOUTPUT, mOutput->GetParent()->IsShown());
   bar->Check(ID_LARGEICONS, mLargeIcons);
   bar->Check(ID_SMALLICONS, !mLargeIcons);
}

void NyqBench::OnRunUpdate(wxUpdateUIEvent & e)
{
   auto p = GetActiveProject().lock();
   wxToolBar *tbar = GetToolBar();
   wxMenuBar *mbar = GetMenuBar();

   auto gAudioIO = AudioIOBase::Get();
   if (p && gAudioIO->IsBusy()) {
      mbar->Enable(ID_GO, false);
      mbar->Enable(ID_STOP, false);

      tbar->EnableTool(ID_GO, false);
      tbar->EnableTool(ID_STOP, false);
   }
   else {
      mbar->Enable(ID_GO, (mScript->GetLastPosition() > 0) && !mRunning);
      mbar->Enable(ID_STOP, (mScript->GetLastPosition() > 0) && mRunning);

      tbar->EnableTool(ID_GO, (mScript->GetLastPosition() > 0) && !mRunning);
      tbar->EnableTool(ID_STOP, (mScript->GetLastPosition() > 0) && mRunning);
   }
}

void NyqBench::OnScriptUpdate(wxUpdateUIEvent & e)
{
   if (mScriptBox && mScript && FindFocus() == mScript) {
      wxString label = mScriptBox->GetLabel();
      if (label == _("Script")) {
         label += wxT("*");
         mScriptBox->SetLabel(label);
         mOutputBox->SetLabel(_("Output"));
      }
   }
}

void NyqBench::OnOutputUpdate(wxUpdateUIEvent & e)
{
   if (mOutputBox && mOutput && FindFocus() == mOutput) {
      wxString label = mOutputBox->GetLabel();
      if (label == _("Output")) {
         label += wxT("*");
         mOutputBox->SetLabel(label);
         mScriptBox->SetLabel(_("Script"));
      }
   }
}

bool NyqBench::Validate()
{
   if (mScript->GetLastPosition() > 0 && mScript->IsModified()) {
      int ans;
      ans = AudacityMessageBox(XO("Code has been modified. Are you sure?"),
                         XO("Warning"),
                         wxYES_NO | wxICON_QUESTION,
                         this);
      if (ans == wxNO) {
         return false;
      }
   }

   return true;
}

void NyqBench::SetWindowTitle()
{
   wxString name = _("Untitled");

   if (!mPath.GetFullPath().IsEmpty()) {
      name = mPath.GetFullName();
   }

   SetTitle(_("Nyquist Effect Workbench - ") + name);
}

void NyqBench::RecreateToolbar(bool large)
{
   mLargeIcons = large;

   wxToolBar *tb = GetToolBar();
   if (tb) {
      delete tb;
   }
   tb = CreateToolBar();

   wxSize sz;

   if (!mLargeIcons) {
      tb->SetToolBitmapSize(wxSize(16, 16));
      mPics[0] = wxBitmap(document_new_small);
      mPics[1] = wxBitmap(document_open_small);
      mPics[2] = wxBitmap(document_save_as_small);
      mPics[3] = wxBitmap(document_save_small);
      mPics[4] = wxBitmap(edit_copy_small);
      mPics[5] = wxBitmap(edit_cut_small);
      mPics[6] = wxBitmap(edit_paste_small);
      mPics[7] = wxBitmap(edit_clear_small);
      mPics[8] = wxBitmap(edit_delete_small);
      mPics[9] = wxBitmap(edit_select_all_small);
      mPics[10] = wxBitmap(edit_undo_small);
      mPics[11] = wxBitmap(edit_redo_small);
      mPics[12] = wxBitmap(edit_find_small);
      mPics[13] = wxBitmap(system_search_small);
      mPics[14] = wxBitmap(go_top_small);
      mPics[15] = wxBitmap(go_up_small);
      mPics[16] = wxBitmap(go_previous_small);
      mPics[17] = wxBitmap(go_next_small);
      mPics[18] = wxBitmap(media_playback_start_small);
      mPics[19] = wxBitmap(media_playback_stop_small);
   }
   else {
      tb->SetToolBitmapSize(wxSize(32, 32));
      mPics[0] = wxBitmap(document_new_large);
      mPics[1] = wxBitmap(document_open_large);
      mPics[2] = wxBitmap(document_save_as_large);
      mPics[3] = wxBitmap(document_save_large);
      mPics[4] = wxBitmap(edit_copy_large);
      mPics[5] = wxBitmap(edit_cut_large);
      mPics[6] = wxBitmap(edit_paste_large);
      mPics[7] = wxBitmap(edit_clear_large);
      mPics[8] = wxBitmap(edit_delete_large);
      mPics[9] = wxBitmap(edit_select_all_large);
      mPics[10] = wxBitmap(edit_undo_large);
      mPics[11] = wxBitmap(edit_redo_large);
      mPics[12] = wxBitmap(edit_find_large);
      mPics[13] = wxBitmap(system_search_large);
      mPics[14] = wxBitmap(go_top_large);
      mPics[15] = wxBitmap(go_up_large);
      mPics[16] = wxBitmap(go_previous_large);
      mPics[17] = wxBitmap(go_next_large);
      mPics[18] = wxBitmap(media_playback_start_large);
      mPics[19] = wxBitmap(media_playback_stop_large);
   }

   tb->SetMargins(2, 2);

   tb->AddTool(wxID_NEW, _("New"), mPics[0], _("New script"));
   tb->AddTool(wxID_OPEN, _("Open"), mPics[1], _("Open script"));
   tb->AddTool(wxID_SAVE, _("Save"), mPics[2], _("Save script"));
   tb->AddTool(wxID_SAVEAS, _("Save As"), mPics[3], _("Save script as..."));
   tb->AddSeparator();
   tb->AddTool(wxID_COPY, _("Copy"), mPics[4], _("Copy to clipboard"));
   tb->AddTool(wxID_CUT, _("Cut"), mPics[5], _("Cut to clipboard"));
   tb->AddTool(wxID_PASTE, _("Paste"), mPics[6], _("Paste from clipboard"));
   tb->AddTool(wxID_CLEAR, _("Clear"), mPics[7], _("Clear selection"));
   tb->AddTool(wxID_SELECTALL, _("Select All"), mPics[9], _("Select all text"));
   tb->AddSeparator();
   tb->AddTool(wxID_UNDO, _("Undo"), mPics[10], _("Undo last change"));
   tb->AddTool(wxID_REDO, _("Redo"), mPics[11], _("Redo previous change"));
   tb->AddSeparator();
   tb->AddTool(wxID_FIND, _("Find"), mPics[12], _("Find text"));
   tb->AddSeparator();
   tb->AddTool(ID_MATCH, _("Match"), mPics[13], _("Go to matching paren"));
   tb->AddTool(ID_TOP, _("Top"), mPics[14], _("Go to top S-expr"));
   tb->AddTool(ID_UP, _("Up"), mPics[15], _("Go to higher S-expr"));
   tb->AddTool(ID_PREVIOUS, _("Previous"), mPics[16], _("Go to previous S-expr"));
   tb->AddTool(ID_NEXT, _("Next"), mPics[17], _("Go to next S-expr"));
   tb->AddSeparator();
   tb->AddTool(ID_GO, _("Start"), mPics[18], _("Start script"));
   tb->AddTool(ID_STOP, _("Stop"), mPics[19], _("Stop script"));
   
   tb->Realize();

   Layout();
   Fit();
   SetMinSize(GetSize());
}

void NyqBench::LoadFile()
{
   wxString path = mPath.GetFullPath();

   if (path.IsEmpty()) {
      return;
   }

   wxFFile f(path);
   if (f.IsOpened()) {
      wxString t;
      if (f.ReadAll(&t)) {
//#if defined(__WXGTK__) || defined(__WXMAC__)
         t.Replace(wxT("\r\n"), wxT("\n"));
         t.Replace(wxT("\r"), wxT("\n"));
//#elif defined(__WXMSW__)
//         t.Replace("\r\n", "\n");
//#endif
         mScript->SetValue(t);
         mScript->DiscardEdits();
      }
   }

//   mScript->LoadFile(mPath.GetFullPath());
}

//----------------------------------------------------------------------------
// Connects Audacity menu item to an action in this dll.
// Only one action implemented so far.
//----------------------------------------------------------------------------
void NyqBench::ShowNyqBench(const CommandContext &)
{
   Show();
}
