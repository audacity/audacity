/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 TagsEditor.h
 
 Paul Licameli split from Tags.h
 
 **********************************************************************/

#ifndef __AUDACITY_TAGS_EDITOR__
#define __AUDACITY_TAGS_EDITOR__

#include "Tags.h" // member variable
#include "wxPanelWrapper.h" // to inherit

class ShuttleGui;

class TagsEditorDialog final : public wxDialogWrapper
{
 public:
   // constructors and destructors
   TagsEditorDialog(wxWindow * parent,
              const TranslatableString &title,
              Tags * tags,
              bool editTitle,
              bool editTrack);

   virtual ~TagsEditorDialog();

   AUDACITY_DLL_API static bool ShowEditDialog( Tags &tags,
      wxWindow *parent, const TranslatableString &title, bool force = false);

   //! Invoke ShowEditDialog on project's tags;  commit change in undo history,
   //! return true if not cancelled
   AUDACITY_DLL_API static bool DoEditMetadata(AudacityProject &project,
      const TranslatableString &title,
      const TranslatableString &shortUndoDescription, bool force);

#if !defined(__WXMSW__)
   bool IsEscapeKey(const wxKeyEvent& /*event*/) override { return false; }
#endif

   void PopulateOrExchange(ShuttleGui & S);

   void OnDontShow( wxCommandEvent & Evt);
   void OnHelp(wxCommandEvent & Evt);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

 private:
   void PopulateGenres();
   void SetEditors();

   void OnChange(wxGridEvent & event);

   void OnEdit(wxCommandEvent & event);
   void OnReset(wxCommandEvent & event);

   void OnClear(wxCommandEvent & event);

   void OnLoad(wxCommandEvent & event);
   void OnSave(wxCommandEvent & event);
   void OnSaveDefaults(wxCommandEvent & event);

   void OnAdd(wxCommandEvent & event);
   void OnRemove(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void DoCancel(bool escKey);
   void OnCancel(wxCommandEvent & event);

   void OnKeyDown(wxKeyEvent &event);

   bool IsWindowRectValid(const wxRect *windowRect) const;

 private:
   Tags *mTags;
   bool mEditTitle;
   bool mEditTrack;

   Tags mLocal;

   Grid *mGrid;
   ComboEditor *mComboEditor;
   wxGridCellStringRenderer *mStringRenderer;

   DECLARE_EVENT_TABLE()
};

#endif
