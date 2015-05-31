/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class SpectrumPrefs
\brief A PrefsPanel for spectrum settings.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>

#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "SpectrumPrefs.h"
#include "../FFT.h"
#include "../AColor.h"

#define ID_REFRESH_COLOR_SCALE	1
#define ID_CHANGE_COLOR1			2
#define ID_CHANGE_COLOR2			3
#define ID_CHANGE_COLOR3			4
#define ID_CHANGE_COLOR4			5
#define ID_CHANGE_COLOR5			6

BEGIN_EVENT_TABLE(SpectrumPrefs, PrefsPanel)
EVT_CHOICE(ID_REFRESH_COLOR_SCALE, SpectrumPrefs::OnRefresh)
EVT_BUTTON(ID_CHANGE_COLOR1, SpectrumPrefs::OnChangeColor1)
EVT_BUTTON(ID_CHANGE_COLOR2, SpectrumPrefs::OnChangeColor2)
EVT_BUTTON(ID_CHANGE_COLOR3, SpectrumPrefs::OnChangeColor3)
EVT_BUTTON(ID_CHANGE_COLOR4, SpectrumPrefs::OnChangeColor4)
EVT_BUTTON(ID_CHANGE_COLOR5, SpectrumPrefs::OnChangeColor5)
END_EVENT_TABLE()

SpectrumPrefs::SpectrumPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Spectrograms"))
{
   Populate();
}

SpectrumPrefs::~SpectrumPrefs()
{
}

void SpectrumPrefs::Populate()
{
   mSizeChoices.Add(_("8 - most wideband"));
   mSizeChoices.Add(wxT("16"));
   mSizeChoices.Add(wxT("32"));
   mSizeChoices.Add(wxT("64"));
   mSizeChoices.Add(wxT("128"));
   mSizeChoices.Add(_("256 - default"));
   mSizeChoices.Add(wxT("512"));
   mSizeChoices.Add(wxT("1024"));
   mSizeChoices.Add(wxT("2048"));
   mSizeChoices.Add(wxT("4096"));
   mSizeChoices.Add(wxT("8192"));
   mSizeChoices.Add(wxT("16384"));
   mSizeChoices.Add(_("32768 - most narrowband"));

   for (size_t i = 0; i < mSizeChoices.GetCount(); i++) {
      mSizeCodes.Add(1 << (i + 3));
   }

   for (int i = 0; i < NumWindowFuncs(); i++) {
      mTypeChoices.Add(WindowFuncName(i));
      mTypeCodes.Add(i);
   }

	mColorScaleChoices.Add(wxT("Audacity"));
	mColorScaleCodes.Add(0);
	mColorScaleChoices.Add(wxT("Cool"));
	mColorScaleCodes.Add(1);
	mColorScaleChoices.Add(wxT("C2H"));
	mColorScaleCodes.Add(2);
	mColorScaleChoices.Add(wxT("Multicolor"));
	mColorScaleCodes.Add(3);
	mColorScaleChoices.Add(wxT("Fire"));
	mColorScaleCodes.Add(4);
	mColorScaleChoices.Add(wxT("Protanopia"));
	mColorScaleCodes.Add(5);
	mColorScaleChoices.Add(wxT("Deuteranopia"));
	mColorScaleCodes.Add(6);
	mColorScaleChoices.Add(wxT("Tritanopia"));
	mColorScaleCodes.Add(7);
	mColorScaleChoices.Add(wxT("Custom"));
	mColorScaleCodes.Add(8);

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void SpectrumPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("FFT Window"));
   {
      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Window &size:"),
                     wxT("/Spectrum/FFTSize"),
                     256,
                     mSizeChoices,
                     mSizeCodes);
         S.SetSizeHints(mSizeChoices);

         S.TieChoice(_("Window &type:"),
                     wxT("/Spectrum/WindowType"),
                     3,
                     mTypeChoices,
                     mTypeCodes);
         S.SetSizeHints(mTypeChoices);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
// Search and replace with _ if you want translation.
#define TRANSLATABLE( x ) wxT(x)
   wxArrayString wskipn;
   wxArrayInt wskipv;

   for (size_t i = 0; i < 7; i++) {
      wskipn.Add(wxString::Format(wxT("%d"), (1 << i) - 1));
      wskipv.Add((1 << i) - 1);
   }

   /* /////i18n-hint: (noun) Experimental.  Don't know what it does.  Don't translate.*/
   S.StartStatic(TRANSLATABLE("FFT Skip Points"));
   {
      S.StartMultiColumn(2);
      {
         /* /////i18n-hint: (noun) here the user chooses points to skip.*/
         S.TieChoice(TRANSLATABLE("Skip Points:"),
                     wxT("/Spectrum/FFTSkipPoints"),
                     0,
                     wskipn,
                     wskipv);
         S.SetSizeHints(wskipn);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#endif //EXPERIMENTAL_FFT_SKIP_POINTS
	S.StartTwoColumn(); {
   S.StartStatic(_("Display"));
   {
      S.StartTwoColumn();
      {
         mMinFreq =
            S.TieNumericTextBox(_("Mi&nimum Frequency (Hz):"),
                                wxT("/Spectrum/MinFreq"),
                                0,
                                12);

         mMaxFreq =
            S.TieNumericTextBox(_("Ma&ximum Frequency (Hz):"),
                                wxT("/Spectrum/MaxFreq"),
                                8000,
                                12);

         mGain =
            S.TieNumericTextBox(_("&Gain (dB):"),
                                wxT("/Spectrum/Gain"),
                                20,
                                8);

         mRange =
            S.TieNumericTextBox(_("&Range (dB):"),
                                wxT("/Spectrum/Range"),
                                80,
                                8);

         mFrequencyGain =
            S.TieNumericTextBox(_("Frequency g&ain (dB/dec):"),
                                wxT("/Spectrum/FrequencyGain"),
                                0,
                                4);
      }
      S.EndTwoColumn();

      //S.TieCheckBox(_("S&how the spectrum using grayscale colors"),
                    //wxT("/Spectrum/Grayscale"),
                    //false);

#ifdef EXPERIMENTAL_FFT_Y_GRID
      S.TieCheckBox(_("Show a grid along the &Y-axis"),
                    wxT("/Spectrum/FFTYGrid"),
                    false);
#endif //EXPERIMENTAL_FFT_Y_GRID
   }
   S.EndStatic();
	S.StartStatic(_("Colormap Settings")); {
		S.StartTwoColumn(); {
			S.Id(ID_REFRESH_COLOR_SCALE).TieChoice(_("Color &map") + wxString(wxT(":")),
				wxT("/Spectrum/Colormap"),
				0,
				mColorScaleChoices,
				mColorScaleCodes);
			S.SetSizeHints(mColorScaleChoices);
		}
		S.EndTwoColumn();
		S.StartMultiColumn(8); {
			R1 = S.TieNumericTextBox(_("R1:"), wxT("/Spectrum/R1"), -1, 6);
			G1 = S.TieNumericTextBox(_("G1:"), wxT("/Spectrum/G1"), -1, 6);
			B1 = S.TieNumericTextBox(_("B1:"), wxT("/Spectrum/B1"), -1, 6);
			S.Id(ID_CHANGE_COLOR1).AddButton(_("..."));
			S.AddSpace(30, 20);
			R2 = S.TieNumericTextBox(_("R2:"), wxT("/Spectrum/R2"), -1, 6);
			G2 = S.TieNumericTextBox(_("G2:"), wxT("/Spectrum/G2"), -1, 6);
			B2 = S.TieNumericTextBox(_("B2:"), wxT("/Spectrum/B2"), -1, 6);
			S.Id(ID_CHANGE_COLOR2).AddButton(_("..."));
			S.AddSpace(30, 20);
			R3 = S.TieNumericTextBox(_("R3:"), wxT("/Spectrum/R3"), -1, 6);
			G3 = S.TieNumericTextBox(_("G3:"), wxT("/Spectrum/G3"), -1, 6);
			B3 = S.TieNumericTextBox(_("B3:"), wxT("/Spectrum/B3"), -1, 6);
			S.Id(ID_CHANGE_COLOR3).AddButton(_("..."));
			S.AddSpace(30, 20);
			R4 = S.TieNumericTextBox(_("R4:"), wxT("/Spectrum/R4"), -1, 6);
			G4 = S.TieNumericTextBox(_("G4:"), wxT("/Spectrum/G4"), -1, 6);
			B4 = S.TieNumericTextBox(_("B4:"), wxT("/Spectrum/B4"), -1, 6);
			S.Id(ID_CHANGE_COLOR4).AddButton(_("..."));
			S.AddSpace(30, 20);
			R5 = S.TieNumericTextBox(_("R5:"), wxT("/Spectrum/R5"), -1, 6);
			G5 = S.TieNumericTextBox(_("G5:"), wxT("/Spectrum/G5"), -1, 6);
			B5 = S.TieNumericTextBox(_("B5:"), wxT("/Spectrum/B5"), -1, 6);
			S.Id(ID_CHANGE_COLOR5).AddButton(_("..."));
			S.AddSpace(30, 20);
	}
		S.EndMultiColumn();
		S.StartTwoColumn(); {
			S.TieCheckBox(_("S&how the spectrum using grayscale \ncolors"),
				wxT("/Spectrum/Grayscale"),
				false);
		}
		PaintColorScale();
	}
	S.EndStatic();
}
S.EndTwoColumn();
#ifdef EXPERIMENTAL_FIND_NOTES
   /* i18n-hint: FFT stands for Fast Fourier Transform and probably shouldn't be translated*/
   S.StartStatic(_("FFT Find Notes"));
   {
      S.StartTwoColumn();
      {
         mFindNotesMinA =
            S.TieNumericTextBox(_("Minimum Amplitude (dB):"),
                                wxT("/Spectrum/FindNotesMinA"),
                                -30L,
                                8);

         mFindNotesN =
            S.TieNumericTextBox(_("Max. Number of Notes (1..128):"),
                                wxT("/Spectrum/FindNotesN"),
                                5L,
                                8);
      }
      S.EndTwoColumn();

      S.TieCheckBox(_("&Find Notes"),
                    wxT("/Spectrum/FFTFindNotes"),
                    false);

      S.TieCheckBox(_("&Quantize Notes"),
                    wxT("/Spectrum/FindNotesQuantize"),
                    false);
   }
   S.EndStatic();
#endif //EXPERIMENTAL_FIND_NOTES
}

bool SpectrumPrefs::Validate()
{
   long maxFreq;
   if (!mMaxFreq->GetValue().ToLong(&maxFreq)) {
      wxMessageBox(_("The maximum frequency must be an integer"));
      return false;
   }
   if (maxFreq < 100) {
      wxMessageBox(_("Maximum frequency must be 100 Hz or above"));
      return false;
   }

   long minFreq;
   if (!mMinFreq->GetValue().ToLong(&minFreq)) {
      wxMessageBox(_("The minimum frequency must be an integer"));
      return false;
   }
   if (minFreq < 0) {
      wxMessageBox(_("Minimum frequency must be at least 0 Hz"));
      return false;
   }

   if (maxFreq < minFreq) {
      wxMessageBox(_("Minimum frequency must be less than maximum frequency"));
      return false;
   }

   long gain;
   if (!mGain->GetValue().ToLong(&gain)) {
      wxMessageBox(_("The gain must be an integer"));
      return false;
   }
   long range;
   if (!mRange->GetValue().ToLong(&range)) {
      wxMessageBox(_("The range must be a positive integer"));
      return false;
   }
   if (range <= 0) {
      wxMessageBox(_("The range must be at least 1 dB"));
      return false;
   }

   long frequencygain;
   if (!mFrequencyGain->GetValue().ToLong(&frequencygain)) {
      wxMessageBox(_("The frequency gain must be an integer"));
      return false;
   }
   if (frequencygain < 0) {
      wxMessageBox(_("The frequency gain cannot be negative"));
      return false;
   }
   if (frequencygain > 60) {
      wxMessageBox(_("The frequency gain must be no more than 60 dB/dec"));
      return false;
   }
	long r1;
	if (!R1->GetValue().ToLong(&r1)) {
		wxMessageBox(_("The value of R1 must be an integer"));
		return false;
	}
	if (r1 < 0 || r1 > 255) {
		wxMessageBox(_("The value of R1 must be between 0 and 255"));
		return false;
	}
	long r2;
	if (!R2->GetValue().ToLong(&r2)) {
		wxMessageBox(_("The value of RR must be an integer"));
		return false;
	}
	if (r2 < 0 || r2 > 255) {
		wxMessageBox(_("The value of R2 must be between 0 and 255"));
		return false;
}
	long r3;
	if (!R3->GetValue().ToLong(&r3)) {
		wxMessageBox(_("The value of R3 must be an integer"));
		return false;
	}
	if (r3 < 0 || r3 > 255) {
		wxMessageBox(_("The value of R3 must be between 0 and 255"));
		return false;
	}
	long r4;
	if (!R4->GetValue().ToLong(&r4)) {
		wxMessageBox(_("The value of R4 must be an integer"));
		return false;
	}
	if (r4 < 0 || r4 > 255) {
		wxMessageBox(_("The value of R4 must be between 0 and 255"));
		return false;
	}
	long r5;
	if (!R5->GetValue().ToLong(&r5)) {
		wxMessageBox(_("The value of R5 must be an integer"));
		return false;
	}
	if (r5 < 0 || r5 > 255) {
		wxMessageBox(_("The value of R5 must be between 0 and 255"));
		return false;
	}
	long g1;
	if (!G1->GetValue().ToLong(&g1)) {
		wxMessageBox(_("The value of G1 must be an integer"));
		return false;
	}
	if (g1 < 0 || g1 > 255) {
		wxMessageBox(_("The value of G1 must be between 0 and 255"));
		return false;
	}
	long g2;
	if (!G2->GetValue().ToLong(&g2)) {
		wxMessageBox(_("The value of G2 must be an integer"));
		return false;
	}
	if (g2 < 0 || g2 > 255) {
		wxMessageBox(_("The value of G2 must be between 0 and 255"));
		return false;
	}
	long g3;
	if (!G3->GetValue().ToLong(&g3)) {
		wxMessageBox(_("The value of G3 must be an integer"));
		return false;
	}
	if (g3 < 0 || g3 > 255) {
		wxMessageBox(_("The value of G3 must be between 0 and 255"));
		return false;
	}
	long g4;
	if (!G4->GetValue().ToLong(&g4)) {
		wxMessageBox(_("The value of G4 must be an integer"));
		return false;
	}
	if (g4 < 0 || g4 > 255) {
		wxMessageBox(_("The value of G4 must be between 0 and 255"));
		return false;
	}
	long g5;
	if (!G5->GetValue().ToLong(&g5)) {
		wxMessageBox(_("The value of G5 must be an integer"));
		return false;
	}
	if (g5 < 0 || g5 > 255) {
		wxMessageBox(_("The value of G5 must be between 0 and 255"));
		return false;
	}
	long b1;
	if (!B1->GetValue().ToLong(&b1)) {
		wxMessageBox(_("The value of B1 must be an integer"));
		return false;
	}
	if (b1 < 0 || b1 > 255) {
		wxMessageBox(_("The value of B1 must be between 0 and 255"));
		return false;
	}
	long b2;
	if (!B2->GetValue().ToLong(&b2)) {
		wxMessageBox(_("The value of B2 must be an integer"));
		return false;
	}
	if (b2 < 0 || b2 > 255) {
		wxMessageBox(_("The value of B2 must be between 0 and 255"));
		return false;
	}
	long b3;
	if (!B3->GetValue().ToLong(&b3)) {
		wxMessageBox(_("The value of B3 must be an integer"));
		return false;
	}
	if (b3 < 0 || b3 > 255) {
		wxMessageBox(_("The value of B3 must be between 0 and 255"));
		return false;
	}
	long b4;
	if (!B4->GetValue().ToLong(&b4)) {
		wxMessageBox(_("The value of B4 must be an integer"));
		return false;
	}
	if (b4 < 0 || b4 > 255) {
		wxMessageBox(_("The value of B4 must be between 0 and 255"));
		return false;
	}
	long b5;
	if (!B5->GetValue().ToLong(&b5)) {
		wxMessageBox(_("The value of B5 must be an integer"));
		return false;
	}
	if (b5 < 0 || b5 > 255) {
		wxMessageBox(_("The value of B5 must be between 0 and 255"));
		return false;
	}
#ifdef EXPERIMENTAL_FIND_NOTES
   long findNotesMinA;
   if (!mFindNotesMinA->GetValue().ToLong(&findNotesMinA)) {
      wxMessageBox(_("The minimum amplitude (dB) must be an integer"));
      return false;
   }

   long findNotesN;
   if (!mFindNotesN->GetValue().ToLong(&findNotesN)) {
      wxMessageBox(_("The maximum number of notes must be an integer"));
      return false;
   }
   if (findNotesN < 1 || findNotesN > 128) {
      wxMessageBox(_("The maximum number of notes must be in the range 1..128"));
      return false;
   }
#endif //EXPERIMENTAL_FIND_NOTES

   return true;
}

bool SpectrumPrefs::Apply()
{
	ShuttleGui S(this, eIsSavingToPrefs);
	PopulateOrExchange(S);

	return true;
}

void SpectrumPrefs::OnRefresh(wxCommandEvent & e)
{
	ShuttleGui S(this, eIsSavingToPrefs);
	PaintColorScale();
	PopulateOrExchange(S);
	Apply();
}


void SpectrumPrefs::OnChangeColor1(wxCommandEvent & e){
	wxColourData data;
	wxColour color = wxColour(wxAtoi(R1->GetValue()), wxAtoi(G1->GetValue()), wxAtoi(B1->GetValue()));
	wxColour newColor;
	data = wxColourData();
	data.SetColour(color);
	wxColourDialog dialog(this, &data);

	if (R1->IsEditable()){
		if (dialog.ShowModal() == wxID_OK)
		{
			data = dialog.GetColourData();
			newColor = wxColor(data.GetColour().Red(), data.GetColour().Green(), data.GetColour().Blue());
		}
		R1->SetValue(wxString::Format(wxT("%i"), newColor.Red()));
		G1->SetValue(wxString::Format(wxT("%i"), newColor.Green()));
		B1->SetValue(wxString::Format(wxT("%i"), newColor.Blue()));
		C1 = newColor;
	}
	else{
		wxMessageBox(_("You must select the Custom color map to be able to edit a color"));
	}
	PaintColorScale();
	Apply();
}
void SpectrumPrefs::OnChangeColor2(wxCommandEvent & e){
	wxColourData data;
	wxColour color = wxColour(wxAtoi(R2->GetValue()), wxAtoi(G2->GetValue()), wxAtoi(B2->GetValue()));
	wxColour newColor;
	data = wxColourData();
	data.SetColour(color);
	wxColourDialog dialog(this, &data);

	if (R2->IsEditable()){
		if (dialog.ShowModal() == wxID_OK)
		{
			data = dialog.GetColourData();
			newColor = wxColor(data.GetColour().Red(), data.GetColour().Green(), data.GetColour().Blue());
		}
		R2->SetValue(wxString::Format(wxT("%i"), newColor.Red()));
		G2->SetValue(wxString::Format(wxT("%i"), newColor.Green()));
		B2->SetValue(wxString::Format(wxT("%i"), newColor.Blue()));
	}
	else{
		wxMessageBox(_("You must select the Custom color map to be able to edit a color"));
	}
	PaintColorScale();
	Apply();
}
void SpectrumPrefs::OnChangeColor3(wxCommandEvent & e){
	wxColourData data;
	wxColour color = wxColour(wxAtoi(R3->GetValue()), wxAtoi(G3->GetValue()), wxAtoi(B3->GetValue()));
	wxColour newColor;
	data = wxColourData();
	data.SetColour(color);
	wxColourDialog dialog(this, &data);

	if (R3->IsEditable()){
		if (dialog.ShowModal() == wxID_OK)
		{
			data = dialog.GetColourData();
			newColor = wxColor(data.GetColour().Red(), data.GetColour().Green(), data.GetColour().Blue());
		}
		R3->SetValue(wxString::Format(wxT("%i"), newColor.Red()));
		G3->SetValue(wxString::Format(wxT("%i"), newColor.Green()));
		B3->SetValue(wxString::Format(wxT("%i"), newColor.Blue()));
	}
	else{
		wxMessageBox(_("You must select the Custom color map to be able to edit a color"));
	}
	PaintColorScale();
	Apply();
}
void SpectrumPrefs::OnChangeColor4(wxCommandEvent & e){
	wxColourData data;
	wxColour color = wxColour(wxAtoi(R4->GetValue()), wxAtoi(G4->GetValue()), wxAtoi(B4->GetValue()));
	wxColour newColor;
	data = wxColourData();
	data.SetColour(color);
	wxColourDialog dialog(this, &data);

	if (R4->IsEditable()){
		if (dialog.ShowModal() == wxID_OK)
		{
			data = dialog.GetColourData();
			newColor = wxColor(data.GetColour().Red(), data.GetColour().Green(), data.GetColour().Blue());
		}
		R4->SetValue(wxString::Format(wxT("%i"), newColor.Red()));
		G4->SetValue(wxString::Format(wxT("%i"), newColor.Green()));
		B4->SetValue(wxString::Format(wxT("%i"), newColor.Blue()));
	}
	else{
		wxMessageBox(_("You must select the Custom color map to be able to edit a color"));
	}
	PaintColorScale();
	Apply();
}
void SpectrumPrefs::OnChangeColor5(wxCommandEvent & e){
	wxColourData data;
	wxColour color = wxColour(wxAtoi(R5->GetValue()), wxAtoi(G5->GetValue()), wxAtoi(B5->GetValue()));
	wxColour newColor;
	data = wxColourData();
	data.SetColour(color);
	wxColourDialog dialog(this, &data);

	if (R5->IsEditable()){
		if (dialog.ShowModal() == wxID_OK)
		{
			data = dialog.GetColourData();
			newColor = wxColor(data.GetColour().Red(), data.GetColour().Green(), data.GetColour().Blue());
		}
		R5->SetValue(wxString::Format(wxT("%i"), newColor.Red()));
		G5->SetValue(wxString::Format(wxT("%i"), newColor.Green()));
		B5->SetValue(wxString::Format(wxT("%i"), newColor.Blue()));
	}
	else{
		wxMessageBox(_("You must select the Custom color map to be able to edit a color"));
	}
	PaintColorScale();
	Apply();
}

void SpectrumPrefs::PaintColorScale(){

	wxStaticBitmap *ColorMap = new wxStaticBitmap(this, wxID_ANY, wxNullBitmap, wxPoint(460, 122), wxSize(25, 132), wxBORDER_SIMPLE);

	int choice = (gPrefs->Read(wxT("/Spectrum/Colormap"), -1));

	wxBitmap *ColorScale = new wxBitmap(25, 132);
	int Size = ColorScale->GetHeight();
	red = new float[Size];
	blue = new float[Size];
	green = new float[Size];

	float colors[8][5][3] = {
		//Audacity
		{
			{ float(0.75), float(0.75), float(0.75) },    // lt gray
			{ float(0.30), float(0.60), float(1.00) },    // lt blue
			{ float(0.90), float(0.10), float(0.90) },    // violet
			{ float(1.00), float(0.00), float(0.00) },    // red
			{ float(1.00), float(1.00), float(1.00) }     // white
		},
		//Cool
		{
			{ float(0.00), float(0.15), float(0.15) },    // dark gray
			{ float(0.00), float(0.00), float(0.35) },    // dark blue
			{ float(0.65), float(0.10), float(0.65) },    // violet
			{ float(1.00), float(0.00), float(0.00) },    // red
			{ float(1.00), float(1.00), float(0.65) }     // white
		},
		//C2H
		{
			{ float(0.00), float(0.00), float(0.74) },    // dark gray
			{ float(0.00), float(0.74), float(1.00) },    // dark blue
			{ float(0.74), float(1.00), float(0.26) },    // violet
			{ float(1.00), float(0.26), float(0.00) },    // red
			{ float(0.52), float(0.00), float(0.00) }     // white
		},
		//Multicolor
		{
			{ float(1.00), float(0.00), float(0.00) },    // dark gray
			{ float(0.52), float(1.00), float(0.00) },    // dark blue
			{ float(0.00), float(1.00), float(1.00) },    // violet
			{ float(0.48), float(0.00), float(1.00) },    // red
			{ float(1.00), float(0.00), float(0.39) }     // white
		},
		//Fire
		{
			{ float(0.16), float(0.00), float(0.00) },    // dark gray
			{ float(0.84), float(0.00), float(0.00) },    // dark blue
			{ float(1.00), float(0.48), float(0.00) },    // violet
			{ float(1.00), float(1.00), float(0.26) },    // red
			{ float(1.00), float(1.00), float(1.00) }     // white
		},
		//Protanopia
		{
			{ float(0.00), float(0.00), float(0.63) },    // dark gray
			{ float(0.43), float(0.43), float(0.56) },    // dark blue
			{ float(0.43), float(0.43), float(0.24) },    // violet
			{ float(1.00), float(1.00), float(0.24) },    // red
			{ float(1.00), float(1.00), float(1.00) }     // white
		},
			//Deuteranopia
		{
			{ float(0.00), float(0.00), float(0.55) },    // dark gray
			{ float(0.40), float(0.30), float(0.55) },    // dark blue
			{ float(0.35), float(0.30), float(0.30) },    // violet
			{ float(1.00), float(1.00), float(0.30) },    // red
			{ float(1.00), float(1.00), float(1.00) }     // white
		},
			//Tritanopia
		{
			{ float(0.90), float(0.00), float(0.00) },    // dark gray
			{ float(1.00), float(0.43), float(0.47) },    // dark blue
			{ float(0.00), float(0.60), float(0.60) },    // violet
			{ float(0.00), float(1.00), float(1.00) },    // red
			{ float(1.00), float(1.00), float(1.00) }     // white
		}
	};
	if (choice != 8){

		R1->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][0][0] * 255)));
		R2->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][1][0] * 255)));
		R3->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][2][0] * 255)));
		R4->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][3][0] * 255)));
		R5->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][4][0] * 255)));
		G1->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][0][1] * 255)));
		G2->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][1][1] * 255)));
		G3->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][2][1] * 255)));
		G4->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][3][1] * 255)));
		G5->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][4][1] * 255)));
		B1->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][0][2] * 255)));
		B2->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][1][2] * 255)));
		B3->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][2][2] * 255)));
		B4->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][3][2] * 255)));
		B5->SetValue(wxString::Format(wxT("%i"), (int)(colors[choice][4][2] * 255)));

		R1->SetEditable(false);	R2->SetEditable(false);	R3->SetEditable(false);	R4->SetEditable(false);	R5->SetEditable(false);
		G1->SetEditable(false);	G2->SetEditable(false);	G3->SetEditable(false);	G4->SetEditable(false);	G5->SetEditable(false);
		B1->SetEditable(false);	B2->SetEditable(false);	B3->SetEditable(false);	B4->SetEditable(false);	B5->SetEditable(false);
	}
	else{
		R1->SetEditable(true);	R2->SetEditable(true);	R3->SetEditable(true);	R4->SetEditable(true);	R5->SetEditable(true);
		G1->SetEditable(true);	G2->SetEditable(true);	G3->SetEditable(true);	G4->SetEditable(true);	G5->SetEditable(true);
		B1->SetEditable(true);	B2->SetEditable(true);	B3->SetEditable(true);	B4->SetEditable(true);	B5->SetEditable(true);
	}

	C1 = wxColor(wxAtoi(R1->GetValue()), wxAtoi(G1->GetValue()), wxAtoi(B1->GetValue()));
	C2 = wxColor(wxAtoi(R2->GetValue()), wxAtoi(G2->GetValue()), wxAtoi(B2->GetValue()));
	C3 = wxColor(wxAtoi(R3->GetValue()), wxAtoi(G3->GetValue()), wxAtoi(B3->GetValue()));
	C4 = wxColor(wxAtoi(R4->GetValue()), wxAtoi(G4->GetValue()), wxAtoi(B4->GetValue()));
	C5 = wxColor(wxAtoi(R5->GetValue()), wxAtoi(G5->GetValue()), wxAtoi(B5->GetValue()));

	float r[5] = { C1.Red(), C2.Red(), C3.Red(), C4.Red(), C5.Red() };
	float g[5] = { C1.Green(), C2.Green(), C3.Green(), C4.Green(), C5.Green() };
	float b[5] = { C1.Blue(), C2.Blue(), C3.Blue(), C4.Blue(), C5.Blue() };

	for (int j = 0; j < 4; j++){
		for (int i = 0; i < (Size / 4); i++){
			red[i + j * (Size / 4)] = ((r[j] + (r[j + 1] - r[j])*i / (Size / 4)) / 255);
			green[i + j * (Size / 4)] = ((g[j] + (g[j + 1] - g[j])*i / (Size / 4)) / 255);
			blue[i + j * (Size / 4)] = ((b[j] + (b[j + 1] - b[j])*i / (Size / 4)) / 255);
		}
	}
	wxBufferedDC dc;
	dc.Clear();

	dc.SelectObject(*ColorScale);
	for (int i = 0; i < Size; i++)
	{
		dc.SetPen(wxPen(wxColour(red[i] * 255, green[i] * 255, blue[i] * 255, 1), 1));
		dc.DrawLine(1, i, ColorScale->GetWidth(), i);
		dc.DrawLine(1, i, ColorScale->GetWidth(), i);
	}
	dc.SelectObject(wxNullBitmap);
	ColorScale->SetDepth(0);
	ColorMap->SetBitmap(*ColorScale);
	AColor::gradient_inited = 0;
}