/**********************************************************************

  NyqBench.h

  Leland Lucius

**********************************************************************/

#ifndef __NYQUIST_EFFECT_WORKBENCH__
#define __NYQUIST_EFFECT_WORKBENCH__

#include <wx/fdrepdlg.h> // wxFindReplaceData member variable
#include <wx/frame.h> // to inherit
#include <wx/string.h>

#include <iostream>
#include <ostream>
#include <sstream>

#include "commands/CommandManager.h"
#include "effects/nyquist/Nyquist.h"

class wxFileName;

//----------------------------------------------------------------------------
// NyqTextCtrl
//----------------------------------------------------------------------------

class NyqTextCtrl:public wxTextCtrl
{
 public:
   NyqTextCtrl(wxWindow *parent,
               wxWindowID id,
               const wxString & value,
               const wxPoint & pos,
               const wxSize & size,
               int style = 0);

   void SetFocusFromKbd();
   void MarkDirty();

   void GoMatch();
   void GoTop();
   void GoUp();
   void GoPrev();
   void GoNext();

 private:
#if defined(__WXMAC__REMOVED_UNTIL_ITS_PROVEN_THAT_IT_IS_STILL_NEEDED)
   void OnKeyDown(wxKeyEvent & e);
#endif
   void OnKeyUp(wxKeyEvent & e);
   void OnChar(wxKeyEvent & e);
   void OnUpdate(wxUpdateUIEvent & e);

   void MoveCursor(long first, long second);
   void Colorize(long left, long right);
   void FindParens();

 private:
   wxLongToLongHashMap mLeftParens;
   wxLongToLongHashMap mRightParens;

   long mLeftParen;
   long mRightParen;

   long mLastCaretPos;

   wxTextAttr mOn;
   wxTextAttr mOff;

   DECLARE_EVENT_TABLE();
};

//----------------------------------------------------------------------------
// NyqRedirector
//----------------------------------------------------------------------------

class NyqRedirector:wxSTD streambuf
{
 public:
   NyqRedirector(NyqTextCtrl *text);
   virtual ~NyqRedirector();

   int overflow(int c);

 private:
   void AppendText();

   std::string s;
   std::streambuf *mOld;
   NyqTextCtrl *mText;
};

//----------------------------------------------------------------------------
// NyqBench
//----------------------------------------------------------------------------

class NyqBench:public wxFrame
{
 public:
   NyqBench(wxWindow *parent);
   virtual ~NyqBench();

   virtual bool Validate();

   void ShowNyqBench(const CommandContext&);

   static NyqBench *GetBench();
   void SavePrefs();

 private:
   void PopulateOrExchange(ShuttleGui & S);

   void OnClose(wxCloseEvent & e);
   void OnMove(wxMoveEvent & e);
   void OnSize(wxSizeEvent & e);

   void OnNew(wxCommandEvent & e);
   void OnOpen(wxCommandEvent & e);
   void OnSave(wxCommandEvent & e);
   void OnSaveAs(wxCommandEvent & e);
   void OnRevert(wxCommandEvent & e);
   void OnAutoLoad(wxCommandEvent & e);
   void OnCloseWindow(wxCommandEvent & e);

   void OnUndo(wxCommandEvent & e);
   void OnRedo(wxCommandEvent & e);
   void OnCut(wxCommandEvent & e);
   void OnCopy(wxCommandEvent & e);
   void OnPaste(wxCommandEvent & e);
   void OnClear(wxCommandEvent & e);
   void OnSelectAll(wxCommandEvent & e);
   void OnFind(wxCommandEvent & e);
   void OnGoMatch(wxCommandEvent & e);
   void OnGoTop(wxCommandEvent & e);
   void OnGoUp(wxCommandEvent & e);
   void OnGoPrev(wxCommandEvent & e);
   void OnGoNext(wxCommandEvent & e);
   void OnAutoWrap(wxCommandEvent & e);

   void OnFont(wxCommandEvent & e);
   void OnSplitV(wxCommandEvent & e);
   void OnSplitH(wxCommandEvent & e);
   void OnToggleCode(wxCommandEvent & e);
   void OnToggleOutput(wxCommandEvent & e);
   void OnSmallIcons(wxCommandEvent & e);
   void OnLargeIcons(wxCommandEvent & e);

   void OnGo(wxCommandEvent & e);
   void OnStop(wxCommandEvent & e);

   void OnAbout(wxCommandEvent & e);

   void OnFindDialog(wxFindDialogEvent & e);

   void OnTextUpdate(wxCommandEvent & e);

   void OnMenuUpdate(wxUpdateUIEvent & e);
 
   void OnUndoUpdate(wxUpdateUIEvent & e);
   void OnRedoUpdate(wxUpdateUIEvent & e);
   void OnCutUpdate(wxUpdateUIEvent & e);
   void OnCopyUpdate(wxUpdateUIEvent & e);
   void OnPasteUpdate(wxUpdateUIEvent & e);
   void OnClearUpdate(wxUpdateUIEvent & e);

   void OnViewUpdate(wxUpdateUIEvent & e);

   void OnRunUpdate(wxUpdateUIEvent & e);

   void OnScriptUpdate(wxUpdateUIEvent & e);
   void OnOutputUpdate(wxUpdateUIEvent & e);

   void SetWindowTitle();

   void RecreateToolbar(bool large = false);

   void LoadFile();

 private:
   wxStaticBox *mScriptBox;
   wxStaticBox *mOutputBox;
   NyqTextCtrl *mScript;
   NyqTextCtrl *mOutput;
   wxSplitterWindow *mSplitter;

   wxFindReplaceDialog *mFindDlg;
   wxFindReplaceData mFindData;
   NyqTextCtrl *mFindText;

   NyquistEffect *mEffect;

   wxFont mScriptFont;
   wxFont mOutputFont;

   wxBitmap mPics[20];

   int mSplitMode;
   bool mShowCode;
   bool mShowOutput;

   bool mLargeIcons;

   bool mRunning;

   wxFileName mPath;
   bool mAutoLoad;
   bool mAutoWrap;

   wxRect mLastSize;

   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: cad436f5-7c97-40a2-8ee9-3748e8f3e56f
