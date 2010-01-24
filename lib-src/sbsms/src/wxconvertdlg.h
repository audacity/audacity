#ifndef CONVERTDLG_H
#define CONVERTDLG_H

#include "wx/wx.h"

class ConvertDialog : public wxDialog 
{
 public:
  ConvertDialog(wxWindow *parent);
  ~ConvertDialog();

  wxStaticText *mQualityText;
  wxSlider *mQuality;
  wxCheckBox *mPreAnalyze;

  int getQuality();
  bool getPreAnalyze();
};

#endif
