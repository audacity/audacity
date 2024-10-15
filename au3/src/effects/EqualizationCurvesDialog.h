/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationCurvesDialog.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_EQUALIZATION_CURVES_DIALOG__
#define __AUDACITY_EQUALIZATION_CURVES_DIALOG__

#include "EqualizationCurves.h"
#include "wxPanelWrapper.h"

class wxListCtrl;
class wxListEvent;
class ShuttleGui;

//! EqualizationCurvesDialog manages the available preset curves
class EqualizationCurvesDialog final : public wxDialogWrapper
{
public:
   EqualizationCurvesDialog(wxWindow * parent, const TranslatableString &name,
      int options, EQCurveArray &curves, int position);
   ~EqualizationCurvesDialog();
   int GetItem() const { return mItem; }

private:

   enum EQCurvesDialogControls
   {
      CurvesListID = 11000,
      UpButtonID,
      DownButtonID,
      RenameButtonID,
      DeleteButtonID,
      ImportButtonID,
      ExportButtonID,
      LibraryButtonID,
      DefaultsButtonID
   };

   const TranslatableString &mName;
   const int mOptions;
   EQCurveArray &mCurves;
   wxListCtrl *mList;   // List of curves.
   EQCurveArray mEditCurves;   // Copy of curves to muck about with
   wxWindow *mParent; // the parent EQ Dialog
   int mPosition; // position of current curve in list
   int mItem{ -1 };
   void Populate();
   void PopulateOrExchange(ShuttleGui &S);
   void PopulateList(int position);
   void OnUp(wxCommandEvent &event);
   void OnDown(wxCommandEvent &event);
   long GetPreviousItem(long item);
   void OnRename( wxCommandEvent &event );
   void OnDelete( wxCommandEvent &event );
   void OnImport( wxCommandEvent &event );
   void OnExport( wxCommandEvent &event );
   void OnLibrary( wxCommandEvent &event );
   void OnDefaults( wxCommandEvent &event );
   void OnOK(wxCommandEvent &event);

   void OnListSelectionChange( wxListEvent &event );
   DECLARE_EVENT_TABLE()
};

#endif
