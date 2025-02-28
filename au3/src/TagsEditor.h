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
class wxStaticText;
class wxButton;

class TagsEditorDialog final : public wxDialogWrapper
{
public:
    // constructors and destructors
    TagsEditorDialog(wxWindow* parent, const TranslatableString& title, std::vector<Tags*> tags, std::vector<wxString> names,
                     bool editTitle, bool editTrack);

    virtual ~TagsEditorDialog();

    AUDACITY_DLL_API static bool ShowEditDialog(Tags& tags, wxWindow* parent, const TranslatableString& title);

    //! Invoke ShowEditDialog on project's tags;  commit change in undo history,
    //! return true if not cancelled
    AUDACITY_DLL_API static bool EditProjectMetadata(AudacityProject& project, const TranslatableString& title,
                                                     const TranslatableString& shortUndoDescription);

#if !defined(__WXMSW__)
    bool IsEscapeKey(const wxKeyEvent& /*event*/) override { return false; }
#endif

    void PopulateOrExchange(ShuttleGui& S);

    void OnHelp(wxCommandEvent& Evt);
    bool TransferDataToWindow() override;
    bool TransferDataFromWindow() override;

private:
    void PopulateGenres();
    void SetEditors();

    void OnNext(wxCommandEvent&);
    void OnPrev(wxCommandEvent&);

    void OnChange(wxGridEvent& event);

    void OnEdit(wxCommandEvent& event);
    void OnReset(wxCommandEvent& event);

    void OnClear(wxCommandEvent& event);

    void OnLoad(wxCommandEvent& event);
    void OnSave(wxCommandEvent& event);
    void OnSaveDefaults(wxCommandEvent& event);

    void OnAdd(wxCommandEvent& event);
    void OnRemove(wxCommandEvent& event);

    void OnOk(wxCommandEvent& event);
    void DoCancel(bool escKey);
    void OnCancel(wxCommandEvent& event);

    void OnKeyDown(wxKeyEvent& event);

    bool IsWindowRectValid(const wxRect* windowRect) const;

private:
    std::vector<Tags*> mTags;
    std::vector<wxString> mNames;
    int mSelectedIndex{ 0 };

    bool mEditTitle;
    bool mEditTrack;

    std::vector<std::unique_ptr<Tags> > mEditTags;

    Grid* mGrid;
    wxStaticText* mName{};
    wxButton* mNext{};
    wxButton* mPrev{};
    ComboEditor* mComboEditor;
    wxGridCellStringRenderer* mStringRenderer;

    DECLARE_EVENT_TABLE()
};

#endif
