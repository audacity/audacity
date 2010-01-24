#include "wxconvertdlg.h"

#define QUALITY 9998
#define QUALITYTEXT 9997
#define PREANALYZE 9996

ConvertDialog :: ConvertDialog(wxWindow *parent) : 
  wxDialog(parent, wxID_ANY, _("Convert"), wxDefaultPosition, wxDefaultSize)
{
  mQualityText = new wxStaticText(this,QUALITYTEXT,_("Quality"));
  mQuality = new wxSlider(this,QUALITY,1,0,2);
  mPreAnalyze = new wxCheckBox(this,PREANALYZE,_("Pre-Analysis"));
  wxBoxSizer *mSizer = new wxBoxSizer(wxVERTICAL);

  wxStdDialogButtonSizer *bs = new wxStdDialogButtonSizer();
  wxButton *b;
  b = new wxButton( this, wxID_OK );
  b->SetDefault();
  bs->AddButton( b );
  bs->AddButton( new wxButton( this, wxID_CANCEL ) );
  bs->AddStretchSpacer();
  bs->Realize();

  mSizer->Add(mQualityText);
  mSizer->Add(mQuality);
  mSizer->Add(mPreAnalyze);
  mSizer->Add(bs);
  SetSizer(mSizer);
  mSizer->Fit(this);
}

ConvertDialog :: ~ConvertDialog()
{
}

int ConvertDialog :: getQuality()
{
  return mQuality->GetValue();
}

bool ConvertDialog :: getPreAnalyze()
{
  return mPreAnalyze->GetValue();
}
 
