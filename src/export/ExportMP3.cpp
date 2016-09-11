/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.cpp

  Joshua Haberman

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

  The difficulty in our approach is that we are attempting to use LAME
  in a way it was not designed to be used. LAME's API is reasonably
  consistant, so if we were linking directly against it we could expect
  this code to work with a variety of different LAME versions. However,
  the data structures change from version to version, and so linking
  with one version of the header and dynamically linking against a
  different version of the dynamic library will not work correctly.

  The solution is to find the lowest common denominator between versions.
  The bare minimum of functionality we must use is this:
      1. Initialize the library.
      2. Set, at minimum, the following global options:
          i.  input sample rate
          ii. input channels
      3. Encode the stream
      4. Call the finishing routine

  Just so that it's clear that we're NOT free to use whatever features
  of LAME we like, I'm not including lame.h, but instead enumerating
  here the extent of functions and structures that we can rely on being
  able to import and use from a dynamic library.

  For the record, we aim to support LAME 3.70 on. Since LAME 3.70 was
  released in April of 2000, that should be plenty.


  Copyright 2002, 2003 Joshua Haberman.
  Some portions may be Copyright 2003 Paolo Patruno.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*******************************************************************//**

\class MP3Exporter
\brief Class used to export MP3 files

*//********************************************************************/

#include "../Audacity.h"
#include "ExportMP3.h"

#include <wx/defs.h>

#include <wx/choice.h>
#include <wx/checkbox.h>
#include <wx/dynlib.h>
#include <wx/ffile.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/mimetype.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/window.h>

#include "../FileNames.h"
#include "../float_cast.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../Tags.h"
#include "../Track.h"
#include "../widgets/LinkingHtmlWindow.h"

#include "FileDialog.h"

#include "Export.h"

#include <lame/lame.h>

#ifdef USE_LIBID3TAG
#include <id3tag.h>
#endif

//----------------------------------------------------------------------------
// ExportMP3Options
//----------------------------------------------------------------------------

#define MODE_SET           0
#define MODE_VBR           1
#define MODE_ABR           2
#define MODE_CBR           3

#define CHANNEL_JOINT      0
#define CHANNEL_STEREO     1
#define CHANNEL_MONO       2

#define QUALITY_0          0
#define QUALITY_1          1
#define QUALITY_2          2
#define QUALITY_3          3
#define QUALITY_4          4
#define QUALITY_5          5
#define QUALITY_6          6
#define QUALITY_7          7
#define QUALITY_8          8
#define QUALITY_9          9

#define ROUTINE_FAST       0
#define ROUTINE_STANDARD   1

#define PRESET_INSANE      0
#define PRESET_EXTREME     1
#define PRESET_STANDARD    2
#define PRESET_MEDIUM      3

// Note: The label field is what will be written to preferences and carries
//       no numerical significance.  It is simply a means to look up a value
//       in a table.
//
//       The entries should be listed in order you want them to appear in the
//       choice dropdown based on the name field.
typedef struct
{
   wxString name;
   int label;
} CHOICES;

static CHOICES fixRates[] =
{
   /* i18n-hint: kbps is the bitrate of the MP3 file, kilobits per second*/
   {wxT(""), 320},
   {wxT(""), 256},
   {wxT(""), 224},
   {wxT(""), 192},
   {wxT(""), 160},
   {wxT(""), 144},
   {wxT(""), 128},
   {wxT(""), 112},
   {wxT(""),  96},
   {wxT(""),  80},
   {wxT(""),  64},
   {wxT(""),  56},
   {wxT(""),  48},
   {wxT(""),  40},
   {wxT(""),  32},
   {wxT(""),  24},
   {wxT(""),  16},
   {wxT(""),   8}
};

static CHOICES varRates[] =
{
   {wxT(""), QUALITY_0},
   {wxT(""), QUALITY_1},
   {wxT(""), QUALITY_2},
   {wxT(""), QUALITY_3},
   {wxT(""), QUALITY_4},
   {wxT(""), QUALITY_5},
   {wxT(""), QUALITY_6},
   {wxT(""), QUALITY_7},
   {wxT(""), QUALITY_8},
   {wxT(""), QUALITY_9}
};

static CHOICES varModes[] =
{
   {wxT(""), ROUTINE_FAST    },
   {wxT(""), ROUTINE_STANDARD}
};

static CHOICES setRates[] =
{
   {wxT(""), PRESET_INSANE  },
   {wxT(""), PRESET_EXTREME },
   {wxT(""), PRESET_STANDARD},
   {wxT(""), PRESET_MEDIUM  }
};

static CHOICES sampRates[] =
{
   {wxT(""),  8000    },
   {wxT(""), 11025    },
   {wxT(""), 12000    },
   {wxT(""), 16000    },
   {wxT(""), 22050    },
   {wxT(""), 24000    },
   {wxT(""), 32000    },
   {wxT(""), 44100    },
   {wxT(""), 48000    }
};

#define ID_SET 7000
#define ID_VBR 7001
#define ID_ABR 7002
#define ID_CBR 7003
#define ID_QUALITY 7004
#define ID_MONO 7005

static void InitMP3_Statics()
{
   for (size_t i=0; i < WXSIZEOF(fixRates); i++)
   {
      fixRates[i].name = wxT("");
      fixRates[i].name << fixRates[i].label << wxT(" ") << _("kbps");
   }
   for (size_t i=0; i < WXSIZEOF(varRates); i++)
   {
      varRates[i].name = wxT("");
      varRates[i].name << i << wxT(", ");
   }
   varRates[0].name << wxT("220-260");
   varRates[1].name << wxT("200-250");
   varRates[2].name << wxT("170-210");
   varRates[3].name << wxT("155-195");
   varRates[4].name << wxT("145-185");
   varRates[5].name << wxT("110-150");
   varRates[6].name << wxT("95-135");
   varRates[7].name << wxT("80-120");
   varRates[8].name << wxT("65-105");
   varRates[9].name << wxT("45-85");
   for (size_t i=0; i < WXSIZEOF(varRates); i++)
      varRates[i].name << wxT(" ") << _("kbps");
   varRates[0].name << wxT(" ") << _("(Best Quality)");
   varRates[9].name << wxT(" ") << _("(Smaller files)");

   varModes[0].name = _("Fast");
   varModes[1].name = _("Standard");

   for (size_t i=0; i < WXSIZEOF(setRates); i++)
      setRates[i].name = wxT("");
   /* i18n-hint: Slightly humorous - as in use an insane precision with MP3.*/
   setRates[0].name << _("Insane"  ) << wxT(", ") << 320;
   setRates[1].name << _("Extreme" ) << wxT(", ") << 220 << wxT("-") << 260;
   setRates[2].name << _("Standard") << wxT(", ") << 170 << wxT("-") << 210;
   setRates[3].name << _("Medium"  ) << wxT(", ") << 145 << wxT("-") << 185;
   for (size_t i=0; i < WXSIZEOF(setRates); i++)
      setRates[i].name << wxT(" ") << _("kbps");

   for (size_t i=0; i < WXSIZEOF(sampRates); i++)
   {
      sampRates[i].name = wxT("");
      sampRates[i].name << sampRates[i].label;
   }
}

class ExportMP3Options final : public wxPanelWrapper
{
public:

   ExportMP3Options(wxWindow *parent, int format);
   virtual ~ExportMP3Options();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   void OnSET(wxCommandEvent& evt);
   void OnVBR(wxCommandEvent& evt);
   void OnABR(wxCommandEvent& evt);
   void OnCBR(wxCommandEvent& evt);
   void OnQuality(wxCommandEvent& evt);
   void OnMono(wxCommandEvent& evt);

   void LoadNames(CHOICES *choices, int count);
   wxArrayString GetNames(CHOICES *choices, int count);
   wxArrayInt GetLabels(CHOICES *choices, int count);
   int FindIndex(CHOICES *choices, int cnt, int needle, int def);

private:

   wxRadioButton *mStereo;
   wxRadioButton *mJoint;
   wxCheckBox    *mMono;
   wxRadioButton *mSET;
   wxRadioButton *mVBR;
   wxRadioButton *mABR;
   wxRadioButton *mCBR;
   wxChoice *mRate;
   wxChoice *mMode;

   long mSetRate;
   long mVbrRate;
   long mAbrRate;
   long mCbrRate;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportMP3Options, wxPanelWrapper)
   EVT_RADIOBUTTON(ID_SET,    ExportMP3Options::OnSET)
   EVT_RADIOBUTTON(ID_VBR,    ExportMP3Options::OnVBR)
   EVT_RADIOBUTTON(ID_ABR,    ExportMP3Options::OnABR)
   EVT_RADIOBUTTON(ID_CBR,    ExportMP3Options::OnCBR)
   EVT_CHOICE(wxID_ANY,       ExportMP3Options::OnQuality)
   EVT_CHECKBOX(ID_MONO,      ExportMP3Options::OnMono)
END_EVENT_TABLE()

///
///
ExportMP3Options::ExportMP3Options(wxWindow *parent, int WXUNUSED(format))
:  wxPanelWrapper(parent, wxID_ANY)
{
   InitMP3_Statics();

   mSetRate = gPrefs->Read(wxT("/FileFormats/MP3SetRate"), PRESET_STANDARD);
   mVbrRate = gPrefs->Read(wxT("/FileFormats/MP3VbrRate"), QUALITY_2);
   mAbrRate = gPrefs->Read(wxT("/FileFormats/MP3AbrRate"), 192);
   mCbrRate = gPrefs->Read(wxT("/FileFormats/MP3CbrRate"), 192);

   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportMP3Options::~ExportMP3Options()
{
   TransferDataFromWindow();
}

///
///
void ExportMP3Options::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxCENTER);
      {
         S.StartMultiColumn(2, wxCENTER);
         {
            S.SetStretchyCol(1);
            S.StartTwoColumn();
            {
               S.AddPrompt(_("Bit Rate Mode:"));
               S.StartHorizontalLay();
               {
                  S.StartRadioButtonGroup(wxT("/FileFormats/MP3RateMode"), MODE_SET);
                  {
                     mSET = S.Id(ID_SET).TieRadioButton(_("Preset"), MODE_SET);
                     mVBR = S.Id(ID_VBR).TieRadioButton(_("Variable"), MODE_VBR);
                     mABR = S.Id(ID_ABR).TieRadioButton(_("Average"), MODE_ABR);
                     mCBR = S.Id(ID_CBR).TieRadioButton(_("Constant"), MODE_CBR);
                  }
                  S.EndRadioButtonGroup();
               }
               S.EndHorizontalLay();
   
               CHOICES *choices;
               int cnt;
               bool enable;
               int defrate;
   
               if (mSET->GetValue()) {
                  choices = setRates;
                  cnt = WXSIZEOF(setRates);
                  enable = true;
                  defrate = mSetRate;
               }
               else if (mVBR->GetValue()) {
                  choices = varRates;
                  cnt = WXSIZEOF(varRates);
                  enable = true;
                  defrate = mVbrRate;
               }
               else if (mABR->GetValue()) {
                  choices = fixRates;
                  cnt = WXSIZEOF(fixRates);
                  enable = false;
                  defrate = mAbrRate;
               }
               else {
                  mCBR->SetValue(true);
                  choices = fixRates;
                  cnt = WXSIZEOF(fixRates);
                  enable = false;
                  defrate = mCbrRate;
               }
   
               mRate = S.Id(ID_QUALITY).TieChoice(_("Quality"),
                                                  wxT("/FileFormats/MP3Bitrate"),
                                                  defrate,
                                                  GetNames(choices, cnt),
                                                  GetLabels(choices, cnt));
   
               mMode = S.TieChoice(_("Variable Speed:"),
                                   wxT("/FileFormats/MP3VarMode"),
                                   ROUTINE_FAST,
                                   GetNames(varModes, WXSIZEOF(varModes)),
                                   GetLabels(varModes, WXSIZEOF(varModes)));
               mMode->Enable(enable);
   
               S.AddPrompt(_("Channel Mode:"));
               S.StartMultiColumn(3, wxEXPAND);
               {
                  bool mono = false;
                  gPrefs->Read(wxT("/FileFormats/MP3ForceMono"), &mono, 0);

                  S.StartRadioButtonGroup(wxT("/FileFormats/MP3ChannelMode"), CHANNEL_JOINT);
                  {
                     mJoint = S.TieRadioButton(_("Joint Stereo"), CHANNEL_JOINT);
                     mStereo = S.TieRadioButton(_("Stereo"), CHANNEL_STEREO);
                     mJoint->Enable(!mono);
                     mStereo->Enable(!mono);
                  }
                  S.EndRadioButtonGroup();

                  mMono = S.Id(ID_MONO).AddCheckBox(_("Force export to mono"), mono? wxT("true") : wxT("false"));
               }
               S.EndTwoColumn();
            }
            S.EndTwoColumn();
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

///
///
bool ExportMP3Options::TransferDataToWindow()
{
   return true;
}

bool ExportMP3Options::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Write(wxT("/FileFormats/MP3SetRate"), mSetRate);
   gPrefs->Write(wxT("/FileFormats/MP3VbrRate"), mVbrRate);
   gPrefs->Write(wxT("/FileFormats/MP3AbrRate"), mAbrRate);
   gPrefs->Write(wxT("/FileFormats/MP3CbrRate"), mCbrRate);
   gPrefs->Flush();

   return true;
}

///
///
void ExportMP3Options::OnSET(wxCommandEvent& WXUNUSED(event))
{
   LoadNames(setRates, WXSIZEOF(setRates));

   mRate->SetSelection(FindIndex(setRates, WXSIZEOF(setRates), mSetRate, 2));
   mRate->Refresh();
   mMode->Enable(true);
}

///
///
void ExportMP3Options::OnVBR(wxCommandEvent& WXUNUSED(event))
{
   LoadNames(varRates, WXSIZEOF(varRates));

   mRate->SetSelection(FindIndex(varRates, WXSIZEOF(varRates), mVbrRate, 2));
   mRate->Refresh();
   mMode->Enable(true);
}

///
///
void ExportMP3Options::OnABR(wxCommandEvent& WXUNUSED(event))
{
   LoadNames(fixRates, WXSIZEOF(fixRates));

   mRate->SetSelection(FindIndex(fixRates, WXSIZEOF(fixRates), mAbrRate, 10));
   mRate->Refresh();
   mMode->Enable(false);
}

///
///
void ExportMP3Options::OnCBR(wxCommandEvent& WXUNUSED(event))
{
   LoadNames(fixRates, WXSIZEOF(fixRates));

   mRate->SetSelection(FindIndex(fixRates, WXSIZEOF(fixRates), mCbrRate, 10));
   mRate->Refresh();
   mMode->Enable(false);
}

void ExportMP3Options::OnQuality(wxCommandEvent& WXUNUSED(event))
{
   int sel = mRate->GetSelection();

   if (mSET->GetValue()) {
      mSetRate = setRates[sel].label;
   }
   else if (mVBR->GetValue()) {
      mVbrRate = varRates[sel].label;
   }
   else if (mABR->GetValue()) {
      mAbrRate = fixRates[sel].label;
   }
   else {
      mCbrRate = fixRates[sel].label;
   }
}

void ExportMP3Options::OnMono(wxCommandEvent& /*evt*/)
{
   bool mono = false;
   mono = mMono->GetValue();
   mJoint->Enable(!mono);
   mStereo->Enable(!mono);

   gPrefs->Write(wxT("/FileFormats/MP3ForceMono"), mono);
   gPrefs->Flush();
}

void ExportMP3Options::LoadNames(CHOICES *choices, int count)
{
   mRate->Clear();

   for (int i = 0; i < count; i++)
   {
      mRate->Append(choices[i].name);
   }
}

wxArrayString ExportMP3Options::GetNames(CHOICES *choices, int count)
{
   wxArrayString names;

   for (int i = 0; i < count; i++) {
      names.Add(choices[i].name);
   }

   return names;
}

wxArrayInt ExportMP3Options::GetLabels(CHOICES *choices, int count)
{
   wxArrayInt labels;

   for (int i = 0; i < count; i++) {
      labels.Add(choices[i].label);
   }

   return labels;
}

int ExportMP3Options::FindIndex(CHOICES *choices, int cnt, int needle, int def)
{
   for (int i = 0; i < cnt; i++) {
      if (choices[i].label == needle) {
         return i;
      }
   }

   return def;
}

//----------------------------------------------------------------------------
// FindDialog
//----------------------------------------------------------------------------

#define ID_BROWSE 5000
#define ID_DLOAD  5001

class FindDialog final : public wxDialogWrapper
{
public:

#ifndef DISABLE_DYNAMIC_LOADING_LAME

   FindDialog(wxWindow *parent, wxString path, wxString name, wxString type)
   :  wxDialogWrapper(parent, wxID_ANY,
   /* i18n-hint: LAME is the name of an MP3 converter and should not be translated*/
   wxString(_("Locate Lame")))
   {
      SetName(GetTitle());
      ShuttleGui S(this, eIsCreating);

      mPath = path;
      mName = name;
      mType = type;

      mLibPath.Assign(mPath, mName);

      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      wxString text;

      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         text.Printf(_("Audacity needs the file %s to create MP3s."), mName.c_str());
         S.AddTitle(text);

         S.SetBorder(3);
         S.StartHorizontalLay(wxALIGN_LEFT, true);
         {
            text.Printf(_("Location of %s:"), mName.c_str());
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(0);
         {
            if (mLibPath.GetFullPath().IsEmpty()) {
               /* i18n-hint: There is a  button to the right of the arrow.*/
               text.Printf(_("To find %s, click here -->"), mName.c_str());
               mPathText = S.AddTextBox(wxT(""), text, 0);
            }
            else {
               mPathText = S.AddTextBox(wxT(""), mLibPath.GetFullPath(), 0);
            }
            S.Id(ID_BROWSE).AddButton(_("Browse..."), wxALIGN_RIGHT);
            /* i18n-hint: There is a  button to the right of the arrow.*/
            S.AddVariableText(_("To get a free copy of Lame, click here -->"), true);
            /* i18n-hint: (verb)*/
            S.Id(ID_DLOAD).AddButton(_("Download"), wxALIGN_RIGHT);
         }
         S.EndMultiColumn();

         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   void OnBrowse(wxCommandEvent & WXUNUSED(event))
   {
      wxString question;
      /* i18n-hint: It's asking for the location of a file, for
       * example, "Where is lame_enc.dll?" - you could translate
       * "Where would I find the file %s" instead if you want. */
      question.Printf(_("Where is %s?"), mName.c_str());

      wxString path = FileSelector(question,
                                   mLibPath.GetPath(),
                                   mLibPath.GetName(),
                                   wxT(""),
                                   mType,
                                   wxFD_OPEN | wxRESIZE_BORDER,
                                   this);
      if (!path.IsEmpty()) {
         mLibPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & WXUNUSED(event))
   {
      wxString page = wxT("http://manual.audacityteam.org/man/faq_installation_and_plug_ins.html#lame");
      ::OpenInDefaultBrowser(page);
   }

   wxString GetLibPath()
   {
      return mLibPath.GetFullPath();
   }

#endif // DISABLE_DYNAMIC_LOADING_LAME

private:

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   wxFileName mLibPath;

   wxString mPath;
   wxString mName;
   wxString mType;
#endif // DISABLE_DYNAMIC_LOADING_LAME

   wxTextCtrl *mPathText;

   DECLARE_EVENT_TABLE()
};

#ifndef DISABLE_DYNAMIC_LOADING_LAME
BEGIN_EVENT_TABLE(FindDialog, wxDialogWrapper)
   EVT_BUTTON(ID_BROWSE, FindDialog::OnBrowse)
   EVT_BUTTON(ID_DLOAD,  FindDialog::OnDownload)
END_EVENT_TABLE()
#endif // DISABLE_DYNAMIC_LOADING_LAME

//----------------------------------------------------------------------------
// MP3Exporter
//----------------------------------------------------------------------------

#ifndef DISABLE_DYNAMIC_LOADING_LAME

typedef lame_global_flags *lame_init_t(void);
typedef int lame_init_params_t(lame_global_flags*);
typedef const char* get_lame_version_t(void);

typedef int lame_encode_buffer_t (
      lame_global_flags* gf,
      const short int    buffer_l [],
      const short int    buffer_r [],
      const int          nsamples,
      unsigned char *    mp3buf,
      const int          mp3buf_size );

typedef int lame_encode_buffer_interleaved_t(
      lame_global_flags* gf,
      short int          pcm[],
      int                num_samples,   /* per channel */
      unsigned char*     mp3buf,
      int                mp3buf_size );

typedef int lame_encode_flush_t(
      lame_global_flags *gf,
      unsigned char*     mp3buf,
      int                size );

typedef int lame_close_t(lame_global_flags*);

typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
typedef int lame_set_out_samplerate_t(lame_global_flags*, int);
typedef int lame_set_num_channels_t(lame_global_flags*, int );
typedef int lame_set_quality_t(lame_global_flags*, int);
typedef int lame_set_brate_t(lame_global_flags*, int);
typedef int lame_set_VBR_t(lame_global_flags *, vbr_mode);
typedef int lame_set_VBR_q_t(lame_global_flags *, int);
typedef int lame_set_VBR_min_bitrate_kbps_t(lame_global_flags *, int);
typedef int lame_set_mode_t(lame_global_flags *, MPEG_mode);
typedef int lame_set_preset_t(lame_global_flags *, int);
typedef int lame_set_error_protection_t(lame_global_flags *, int);
typedef int lame_set_disable_reservoir_t(lame_global_flags *, int);
typedef int lame_set_padding_type_t(lame_global_flags *, Padding_type);
typedef int lame_set_bWriteVbrTag_t(lame_global_flags *, int);
typedef size_t lame_get_lametag_frame_t(const lame_global_flags *, unsigned char* buffer, size_t size);
typedef void lame_mp3_tags_fid_t(lame_global_flags *, FILE *);

#endif // DISABLE_DYNAMIC_LOADING_LAME

#if defined(__WXMSW__)
// An alternative solution to give Windows an additional chance of writing the tag before
// falling bato to lame_mp3_tag_fid().  The latter can have DLL sharing issues when mixing
// Debug/Release builds of Audacity and the lame DLL.
typedef unsigned long beWriteInfoTag_t(lame_global_flags *, char *);

// We use this to determine if the user has selected an older, Blade API only, lame_enc.dll
// so we can be more specific about why their library isn't acceptable.
typedef struct	{

   // BladeEnc DLL Version number

   BYTE	byDLLMajorVersion;
   BYTE	byDLLMinorVersion;

   // BladeEnc Engine Version Number

   BYTE	byMajorVersion;
   BYTE	byMinorVersion;

   // DLL Release date

   BYTE	byDay;
   BYTE	byMonth;
   WORD	wYear;

   // BladeEnc	Homepage URL

   CHAR	zHomepage[129];

   BYTE	byAlphaLevel;
   BYTE	byBetaLevel;
   BYTE	byMMXEnabled;

   BYTE	btReserved[125];
} be_version;
typedef void beVersion_t(be_version *);
#endif

class MP3Exporter
{
public:
   enum AskUser
   {
      No,
      Maybe,
      Yes
   };

   MP3Exporter();
   virtual ~MP3Exporter();

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   bool FindLibrary(wxWindow *parent);
   bool LoadLibrary(wxWindow *parent, AskUser askuser);
   bool ValidLibraryLoaded();
#endif // DISABLE_DYNAMIC_LOADING_LAME

   /* These global settings keep state over the life of the object */
   void SetMode(int mode);
   void SetBitrate(int rate);
   void SetQuality(int q, int r);
   void SetChannel(int mode);

   /* Virtual methods that must be supplied by library interfaces */

   /* initialize the library interface */
   bool InitLibrary(wxString libpath);
   void FreeLibrary();

   /* get library info */
   wxString GetLibraryVersion();
   wxString GetLibraryName();
   wxString GetLibraryPath();
   wxString GetLibraryTypeString();

   /* returns the number of samples PER CHANNEL to send for each call to EncodeBuffer */
   int InitializeStream(unsigned channels, int sampleRate);

   /* In bytes. must be called AFTER InitializeStream */
   int GetOutBufferSize();

   /* returns the number of bytes written. input is interleaved if stereo*/
   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]);
   int EncodeRemainder(short int inbuffer[], int nSamples,
                       unsigned char outbuffer[]);

   int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]);
   int EncodeRemainderMono(short int inbuffer[], int nSamples,
                           unsigned char outbuffer[]);

   int FinishStream(unsigned char outbuffer[]);
   void CancelEncoding();

   void PutInfoTag(wxFFile & f, wxFileOffset off);

private:

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   wxString mLibPath;
   wxDynamicLibrary lame_lib;
   bool mLibraryLoaded;
#endif // DISABLE_DYNAMIC_LOADING_LAME

#if defined(__WXMSW__)
   wxString mBladeVersion;
#endif

   bool mEncoding;
   int mMode;
   int mBitrate;
   int mQuality;
   int mRoutine;
   int mChannel;

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   /* function pointers to the symbols we get from the library */
   lame_init_t* lame_init;
   lame_init_params_t* lame_init_params;
   lame_encode_buffer_t* lame_encode_buffer;
   lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
   lame_encode_flush_t* lame_encode_flush;
   lame_close_t* lame_close;
   get_lame_version_t* get_lame_version;

   lame_set_in_samplerate_t* lame_set_in_samplerate;
   lame_set_out_samplerate_t* lame_set_out_samplerate;
   lame_set_num_channels_t* lame_set_num_channels;
   lame_set_quality_t* lame_set_quality;
   lame_set_brate_t* lame_set_brate;
   lame_set_VBR_t* lame_set_VBR;
   lame_set_VBR_q_t* lame_set_VBR_q;
   lame_set_VBR_min_bitrate_kbps_t* lame_set_VBR_min_bitrate_kbps;
   lame_set_mode_t* lame_set_mode;
   lame_set_preset_t* lame_set_preset;
   lame_set_error_protection_t* lame_set_error_protection;
   lame_set_disable_reservoir_t *lame_set_disable_reservoir;
   lame_set_padding_type_t *lame_set_padding_type;
   lame_set_bWriteVbrTag_t *lame_set_bWriteVbrTag;
   lame_get_lametag_frame_t *lame_get_lametag_frame;
   lame_mp3_tags_fid_t *lame_mp3_tags_fid;
#if defined(__WXMSW__)
   beWriteInfoTag_t *beWriteInfoTag;
   beVersion_t *beVersion;
#endif
#endif // DISABLE_DYNAMIC_LOADING_LAME

   lame_global_flags *mGF;

   static const int mSamplesPerChunk = 220500;
   // See lame.h/lame_encode_buffer() for further explanation
   // As coded here, this should be the worst case.
   static const int mOutBufferSize =
      mSamplesPerChunk * (320 / 8) / 8 + 4 * 1152 * (320 / 8) / 8 + 512;

   // See MAXFRAMESIZE in libmp3lame/VbrTag.c for explanation of 2880.
   unsigned char mInfoTagBuf[2880];
   size_t mInfoTagLen;
};

MP3Exporter::MP3Exporter()
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
   mLibraryLoaded = false;
#endif // DISABLE_DYNAMIC_LOADING_LAME
   mEncoding = false;
   mGF = NULL;

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   if (gPrefs) {
      mLibPath = gPrefs->Read(wxT("/MP3/MP3LibPath"), wxT(""));
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   mBitrate = 128;
   mQuality = QUALITY_2;
   mChannel = CHANNEL_STEREO;
   mMode = MODE_CBR;
   mRoutine = ROUTINE_FAST;
}

MP3Exporter::~MP3Exporter()
{
   FreeLibrary();
}

#ifndef DISABLE_DYNAMIC_LOADING_LAME

bool MP3Exporter::FindLibrary(wxWindow *parent)
{
   wxString path;
   wxString name;

   if (!mLibPath.IsEmpty()) {
      wxFileName fn = mLibPath;
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibraryPath();
      name = GetLibraryName();
   }

   FindDialog fd(parent,
                 path,
                 name,
                 GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      return false;
   }

   path = fd.GetLibPath();

   if (!::wxFileExists(path)) {
      return false;
   }

   mLibPath = path;

   return (gPrefs->Write(wxT("/MP3/MP3LibPath"), mLibPath) && gPrefs->Flush());
}

bool MP3Exporter::LoadLibrary(wxWindow *parent, AskUser askuser)
{
   if (ValidLibraryLoaded()) {
      FreeLibrary();
      mLibraryLoaded = false;
   }

#if defined(__WXMSW__)
   mBladeVersion.Empty();
#endif

   // First try loading it from a previously located path
   if (!mLibPath.IsEmpty()) {
      wxLogMessage(wxT("Attempting to load LAME from previously defined path"));
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, try loading using system search paths
   if (!ValidLibraryLoaded()) {
      wxLogMessage(wxT("Attempting to load LAME from system search paths"));
      mLibPath = GetLibraryName();
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, try loading using compiled in path
   if (!ValidLibraryLoaded()) {
      wxLogMessage(wxT("Attempting to load LAME from builtin path"));
      wxFileName fn(GetLibraryPath(), GetLibraryName());
      mLibPath = fn.GetFullPath();
      mLibraryLoaded = InitLibrary(mLibPath);
   }

   // If not successful, must ask the user
   if (!ValidLibraryLoaded()) {
      wxLogMessage(wxT("(Maybe) ask user for library"));
      if (askuser == MP3Exporter::Maybe && FindLibrary(parent)) {
         mLibraryLoaded = InitLibrary(mLibPath);
      }
   }

   // Oh well, just give up
   if (!ValidLibraryLoaded()) {
#if defined(__WXMSW__)
      if (askuser && !mBladeVersion.IsEmpty()) {
         wxMessageBox(mBladeVersion);
      }
#endif
      wxLogMessage(wxT("Failed to locate LAME library"));

      return false;
   }

   wxLogMessage(wxT("LAME library successfully loaded"));

   return true;
}

bool MP3Exporter::ValidLibraryLoaded()
{
   return mLibraryLoaded;
}

#endif // DISABLE_DYNAMIC_LOADING_LAME

void MP3Exporter::SetMode(int mode)
{
   mMode = mode;
}

void MP3Exporter::SetBitrate(int rate)
{
   mBitrate = rate;
}

void MP3Exporter::SetQuality(int q, int r)
{
   mQuality = q;
   mRoutine = r;
}

void MP3Exporter::SetChannel(int mode)
{
   mChannel = mode;
}

bool MP3Exporter::InitLibrary(wxString libpath)
{
   wxLogMessage(wxT("Loading LAME from %s"), libpath.c_str());

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   if (!lame_lib.Load(libpath, wxDL_LAZY)) {
      wxLogMessage(wxT("load failed"));
      return false;
   }

   wxLogMessage(wxT("Actual LAME path %s"),
              FileNames::PathFromAddr(lame_lib.GetSymbol(wxT("lame_init"))).c_str());

   lame_init = (lame_init_t *)
      lame_lib.GetSymbol(wxT("lame_init"));
   get_lame_version = (get_lame_version_t *)
      lame_lib.GetSymbol(wxT("get_lame_version"));
   lame_init_params = (lame_init_params_t *)
      lame_lib.GetSymbol(wxT("lame_init_params"));
   lame_encode_buffer = (lame_encode_buffer_t *)
      lame_lib.GetSymbol(wxT("lame_encode_buffer"));
   lame_encode_buffer_interleaved = (lame_encode_buffer_interleaved_t *)
      lame_lib.GetSymbol(wxT("lame_encode_buffer_interleaved"));
   lame_encode_flush = (lame_encode_flush_t *)
      lame_lib.GetSymbol(wxT("lame_encode_flush"));
   lame_close = (lame_close_t *)
      lame_lib.GetSymbol(wxT("lame_close"));

   lame_set_in_samplerate = (lame_set_in_samplerate_t *)
       lame_lib.GetSymbol(wxT("lame_set_in_samplerate"));
   lame_set_out_samplerate = (lame_set_out_samplerate_t *)
       lame_lib.GetSymbol(wxT("lame_set_out_samplerate"));
   lame_set_num_channels = (lame_set_num_channels_t *)
       lame_lib.GetSymbol(wxT("lame_set_num_channels"));
   lame_set_quality = (lame_set_quality_t *)
       lame_lib.GetSymbol(wxT("lame_set_quality"));
   lame_set_brate = (lame_set_brate_t *)
       lame_lib.GetSymbol(wxT("lame_set_brate"));
   lame_set_VBR = (lame_set_VBR_t *)
       lame_lib.GetSymbol(wxT("lame_set_VBR"));
   lame_set_VBR_q = (lame_set_VBR_q_t *)
       lame_lib.GetSymbol(wxT("lame_set_VBR_q"));
   lame_set_VBR_min_bitrate_kbps = (lame_set_VBR_min_bitrate_kbps_t *)
       lame_lib.GetSymbol(wxT("lame_set_VBR_min_bitrate_kbps"));
   lame_set_mode = (lame_set_mode_t *)
       lame_lib.GetSymbol(wxT("lame_set_mode"));
   lame_set_preset = (lame_set_preset_t *)
       lame_lib.GetSymbol(wxT("lame_set_preset"));
   lame_set_error_protection = (lame_set_error_protection_t *)
       lame_lib.GetSymbol(wxT("lame_set_error_protection"));
   lame_set_disable_reservoir = (lame_set_disable_reservoir_t *)
       lame_lib.GetSymbol(wxT("lame_set_disable_reservoir"));
   lame_set_padding_type = (lame_set_padding_type_t *)
       lame_lib.GetSymbol(wxT("lame_set_padding_type"));
   lame_set_bWriteVbrTag = (lame_set_bWriteVbrTag_t *)
       lame_lib.GetSymbol(wxT("lame_set_bWriteVbrTag"));

   // These are optional
   lame_get_lametag_frame = (lame_get_lametag_frame_t *)
       lame_lib.GetSymbol(wxT("lame_get_lametag_frame"));
   lame_mp3_tags_fid = (lame_mp3_tags_fid_t *)
       lame_lib.GetSymbol(wxT("lame_mp3_tags_fid"));
#if defined(__WXMSW__)
   beWriteInfoTag = (beWriteInfoTag_t *)
       lame_lib.GetSymbol(wxT("beWriteInfoTag"));
   beVersion = (beVersion_t *)
       lame_lib.GetSymbol(wxT("beVersion"));
#endif

   if (!lame_init ||
      !get_lame_version ||
      !lame_init_params ||
      !lame_encode_buffer ||
      !lame_encode_buffer_interleaved ||
      !lame_encode_flush ||
      !lame_close ||
      !lame_set_in_samplerate ||
      !lame_set_out_samplerate ||
      !lame_set_num_channels ||
      !lame_set_quality ||
      !lame_set_brate ||
      !lame_set_VBR ||
      !lame_set_VBR_q ||
      !lame_set_mode ||
      !lame_set_preset ||
      !lame_set_error_protection ||
      !lame_set_disable_reservoir ||
      !lame_set_padding_type ||
      !lame_set_bWriteVbrTag)
   {
      wxLogMessage(wxT("Failed to find a required symbol in the LAME library."));
#if defined(__WXMSW__)
      if (beVersion) {
         be_version v;
         beVersion(&v);

         mBladeVersion.Printf(_("You are linking to lame_enc.dll v%d.%d. This version is not compatible with Audacity %d.%d.%d.\nPlease download the latest version of the LAME MP3 library."),
                              v.byMajorVersion,
                              v.byMinorVersion,
                              AUDACITY_VERSION,
                              AUDACITY_RELEASE,
                              AUDACITY_REVISION);
      }
#endif

      lame_lib.Unload();
      return false;
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   mGF = lame_init();
   if (mGF == NULL) {
      return false;
   }

   return true;
}

void MP3Exporter::FreeLibrary()
{
   if (mGF) {
      lame_close(mGF);
      mGF = NULL;
   }

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   lame_lib.Unload();
#endif // DISABLE_DYNAMIC_LOADING_LAME

   return;
}

wxString MP3Exporter::GetLibraryVersion()
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
   if (!mLibraryLoaded) {
      return wxT("");
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   return wxString::Format(wxT("LAME %hs"), get_lame_version());
}

int MP3Exporter::InitializeStream(unsigned channels, int sampleRate)
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
   if (!mLibraryLoaded) {
      return -1;
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   if (channels > 2) {
      return -1;
   }

   lame_set_error_protection(mGF, false);
   lame_set_num_channels(mGF, channels);
   lame_set_in_samplerate(mGF, sampleRate);
   lame_set_out_samplerate(mGF, sampleRate);
   lame_set_disable_reservoir(mGF, false);
#ifndef DISABLE_DYNAMIC_LOADING_LAME
// TODO: Make this configurable (detect the existance of this function)
   lame_set_padding_type(mGF, PAD_NO);
#endif // DISABLE_DYNAMIC_LOADING_LAME

   // Add the VbrTag for all types.  For ABR/VBR, a Xing tag will be created.
   // For CBR, it will be a Lame Info tag.
   lame_set_bWriteVbrTag(mGF, true);

   // Set the VBR quality or ABR/CBR bitrate
   switch (mMode) {
      case MODE_SET:
      {
         int preset;

         if (mQuality == PRESET_INSANE) {
            preset = INSANE;
         }
         else if (mRoutine == ROUTINE_FAST) {
            if (mQuality == PRESET_EXTREME) {
               preset = EXTREME_FAST;
            }
            else if (mQuality == PRESET_STANDARD) {
               preset = STANDARD_FAST;
            }
            else {
               preset = 1007;    // Not defined until 3.96
            }
         }
         else {
            if (mQuality == PRESET_EXTREME) {
               preset = EXTREME;
            }
            else if (mQuality == PRESET_STANDARD) {
               preset = STANDARD;
            }
            else {
               preset = 1006;    // Not defined until 3.96
            }
         }

         lame_set_preset(mGF, preset);
      }
      break;

      case MODE_VBR:
         lame_set_VBR(mGF, (mRoutine == ROUTINE_STANDARD ? vbr_rh : vbr_mtrh ));
         lame_set_VBR_q(mGF, mQuality);
      break;

      case MODE_ABR:
         lame_set_preset(mGF, mBitrate );
      break;

      default:
         lame_set_VBR(mGF, vbr_off);
         lame_set_brate(mGF, mBitrate);
      break;
   }

   // Set the channel mode
   MPEG_mode mode;

   if (channels == 1 || mChannel == CHANNEL_MONO) {
      mode = MONO;
   }
   else if (mChannel == CHANNEL_JOINT) {
      mode = JOINT_STEREO;
   }
   else {
      mode = STEREO;
   }
   lame_set_mode(mGF, mode);

   int rc = lame_init_params(mGF);
   if (rc < 0) {
      return rc;
   }

#if 0
   dump_config(mGF);
#endif

   mInfoTagLen = 0;
   mEncoding = true;

   return mSamplesPerChunk;
}

int MP3Exporter::GetOutBufferSize()
{
   if (!mEncoding)
      return -1;

   return mOutBufferSize;
}

int MP3Exporter::EncodeBuffer(short int inbuffer[], unsigned char outbuffer[])
{
   if (!mEncoding) {
      return -1;
   }

   return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
      outbuffer, mOutBufferSize);
}

int MP3Exporter::EncodeRemainder(short int inbuffer[], int nSamples,
                  unsigned char outbuffer[])
{
   if (!mEncoding) {
      return -1;
   }

   return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
      mOutBufferSize);
}

int MP3Exporter::EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[])
{
   if (!mEncoding) {
      return -1;
   }

   return lame_encode_buffer(mGF, inbuffer,inbuffer, mSamplesPerChunk,
      outbuffer, mOutBufferSize);
}

int MP3Exporter::EncodeRemainderMono(short int inbuffer[], int nSamples,
                  unsigned char outbuffer[])
{
   if (!mEncoding) {
      return -1;
   }

   return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
      mOutBufferSize);
}

int MP3Exporter::FinishStream(unsigned char outbuffer[])
{
   if (!mEncoding) {
      return -1;
   }

   mEncoding = false;

   int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);

#if defined(DISABLE_DYNAMIC_LOADING_LAME)
   mInfoTagLen = lame_get_lametag_frame(mGF, mInfoTagBuf, sizeof(mInfoTagBuf));
#else
   if (lame_get_lametag_frame) {
      mInfoTagLen = lame_get_lametag_frame(mGF, mInfoTagBuf, sizeof(mInfoTagBuf));
   }
#endif

   return result;
}

void MP3Exporter::CancelEncoding()
{
   mEncoding = false;
}

void MP3Exporter::PutInfoTag(wxFFile & f, wxFileOffset off)
{
   if (mGF) {
      if (mInfoTagLen > 0) {
         // FIXME: TRAP_ERR Seek and writ ein MP3 exporter could fail.
         f.Seek(off, wxFromStart);
         f.Write(mInfoTagBuf, mInfoTagLen);
      }
#if defined(__WXMSW__)
      else if (beWriteInfoTag) {
         f.Flush();
         beWriteInfoTag(mGF, OSOUTPUT(f.GetName()));
         mGF = NULL;
      }
#endif
      else if (lame_mp3_tags_fid != NULL) {
         lame_mp3_tags_fid(mGF, f.fp());
      }
   }

   f.SeekEnd();
}

#if defined(__WXMSW__)
/* values for Windows */

wxString MP3Exporter::GetLibraryPath()
{
   wxRegKey reg(wxT("HKEY_LOCAL_MACHINE\\Software\\Lame for Audacity"));
   wxString path;

   if (reg.Exists()) {
      wxLogMessage(wxT("LAME registry key exists."));
      reg.QueryValue(wxT("InstallPath"), path);
   }
   else {
      wxLogMessage(wxT("LAME registry key does not exist."));
   }

   wxLogMessage(wxT("Library path is: ") + path);

   return path;
}

wxString MP3Exporter::GetLibraryName()
{
   return wxT("lame_enc.dll");
}

wxString MP3Exporter::GetLibraryTypeString()
{
   return _("Only lame_enc.dll|lame_enc.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files|*");
}

#elif defined(__WXMAC__)
/* values for Mac OS X */

wxString MP3Exporter::GetLibraryPath()
{
   return wxT("/usr/local/lib/audacity");
}

wxString MP3Exporter::GetLibraryName()
{
   return wxT("libmp3lame.dylib");
}

wxString MP3Exporter::GetLibraryTypeString()
{
   return wxString(_("Only libmp3lame.dylib|libmp3lame.dylib|Dynamic Libraries (*.dylib)|*.dylib|All Files (*)|*"));
}

#else //!__WXMAC__
/* Values for Linux / Unix systems */

wxString MP3Exporter::GetLibraryPath()
{
   return wxT(LIBDIR);
}

wxString MP3Exporter::GetLibraryName()
{
   return wxT("libmp3lame.so.0");
}

wxString MP3Exporter::GetLibraryTypeString()
{
   return wxString(_("Only libmp3lame.so.0|libmp3lame.so.0|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
}
#endif

#if 0
// Debug routine from BladeMP3EncDLL.c in the libmp3lame distro
static void dump_config( 	lame_global_flags*	gfp )
{
   wxPrintf(wxT("\n\nLame_enc configuration options:\n"));
   wxPrintf(wxT("==========================================================\n"));

   wxPrintf(wxT("version                =%d\n"),lame_get_version( gfp ) );
   wxPrintf(wxT("Layer                  =3\n"));
   wxPrintf(wxT("mode                   ="));
   switch ( lame_get_mode( gfp ) )
   {
      case STEREO:       wxPrintf(wxT( "Stereo\n" )); break;
      case JOINT_STEREO: wxPrintf(wxT( "Joint-Stereo\n" )); break;
      case DUAL_CHANNEL: wxPrintf(wxT( "Forced Stereo\n" )); break;
      case MONO:         wxPrintf(wxT( "Mono\n" )); break;
      case NOT_SET:      /* FALLTROUGH */
      default:           wxPrintf(wxT( "Error (unknown)\n" )); break;
   }

   wxPrintf(wxT("Input sample rate      =%.1f kHz\n"), lame_get_in_samplerate( gfp ) /1000.0 );
   wxPrintf(wxT("Output sample rate     =%.1f kHz\n"), lame_get_out_samplerate( gfp ) /1000.0 );

   wxPrintf(wxT("bitrate                =%d kbps\n"), lame_get_brate( gfp ) );
   wxPrintf(wxT("Quality Setting        =%d\n"), lame_get_quality( gfp ) );

   wxPrintf(wxT("Low pass frequency     =%d\n"), lame_get_lowpassfreq( gfp ) );
   wxPrintf(wxT("Low pass width         =%d\n"), lame_get_lowpasswidth( gfp ) );

   wxPrintf(wxT("High pass frequency    =%d\n"), lame_get_highpassfreq( gfp ) );
   wxPrintf(wxT("High pass width        =%d\n"), lame_get_highpasswidth( gfp ) );

   wxPrintf(wxT("No short blocks        =%d\n"), lame_get_no_short_blocks( gfp ) );
   wxPrintf(wxT("Force short blocks     =%d\n"), lame_get_force_short_blocks( gfp ) );

   wxPrintf(wxT("de-emphasis            =%d\n"), lame_get_emphasis( gfp ) );
   wxPrintf(wxT("private flag           =%d\n"), lame_get_extension( gfp ) );

   wxPrintf(wxT("copyright flag         =%d\n"), lame_get_copyright( gfp ) );
   wxPrintf(wxT("original flag          =%d\n"),	lame_get_original( gfp ) );
   wxPrintf(wxT("CRC                    =%s\n"), lame_get_error_protection( gfp ) ? wxT("on") : wxT("off") );
   wxPrintf(wxT("Fast mode              =%s\n"), ( lame_get_quality( gfp ) )? wxT("enabled") : wxT("disabled") );
   wxPrintf(wxT("Force mid/side stereo  =%s\n"), ( lame_get_force_ms( gfp ) )?wxT("enabled"):wxT("disabled") );
   wxPrintf(wxT("Padding Type           =%d\n"), (int) lame_get_padding_type( gfp ) );
   wxPrintf(wxT("Disable Reservoir      =%d\n"), lame_get_disable_reservoir( gfp ) );
   wxPrintf(wxT("Allow diff-short       =%d\n"), lame_get_allow_diff_short( gfp ) );
   wxPrintf(wxT("Interchannel masking   =%d\n"), lame_get_interChRatio( gfp ) ); // supposed to be a float, but in lib-src/lame/lame/lame.h it's int
   wxPrintf(wxT("Strict ISO Encoding    =%s\n"), ( lame_get_strict_ISO( gfp ) ) ?wxT("Yes"):wxT("No"));
   wxPrintf(wxT("Scale                  =%5.2f\n"), lame_get_scale( gfp ) );

   wxPrintf(wxT("VBR                    =%s, VBR_q =%d, VBR method ="),
            ( lame_get_VBR( gfp ) !=vbr_off ) ? wxT("enabled"): wxT("disabled"),
            lame_get_VBR_q( gfp ) );

   switch ( lame_get_VBR( gfp ) )
   {
      case vbr_off:	wxPrintf(wxT( "vbr_off\n" ));	break;
      case vbr_mt :	wxPrintf(wxT( "vbr_mt \n" ));	break;
      case vbr_rh :	wxPrintf(wxT( "vbr_rh \n" ));	break;
      case vbr_mtrh:	wxPrintf(wxT( "vbr_mtrh \n" ));	break;
      case vbr_abr:
         wxPrintf(wxT( "vbr_abr (average bitrate %d kbps)\n"), lame_get_VBR_mean_bitrate_kbps( gfp ) );
         break;
      default:
         wxPrintf(wxT("error, unknown VBR setting\n"));
         break;
   }

   wxPrintf(wxT("Vbr Min bitrate        =%d kbps\n"), lame_get_VBR_min_bitrate_kbps( gfp ) );
   wxPrintf(wxT("Vbr Max bitrate        =%d kbps\n"), lame_get_VBR_max_bitrate_kbps( gfp ) );

   wxPrintf(wxT("Write VBR Header       =%s\n"), ( lame_get_bWriteVbrTag( gfp ) ) ?wxT("Yes"):wxT("No"));
   wxPrintf(wxT("VBR Hard min           =%d\n"), lame_get_VBR_hard_min( gfp ) );

   wxPrintf(wxT("ATH Only               =%d\n"), lame_get_ATHonly( gfp ) );
   wxPrintf(wxT("ATH short              =%d\n"), lame_get_ATHshort( gfp ) );
   wxPrintf(wxT("ATH no                 =%d\n"), lame_get_noATH( gfp ) );
   wxPrintf(wxT("ATH type               =%d\n"), lame_get_ATHtype( gfp ) );
   wxPrintf(wxT("ATH lower              =%f\n"), lame_get_ATHlower( gfp ) );
   wxPrintf(wxT("ATH aa                 =%d\n"), lame_get_athaa_type( gfp ) );
   wxPrintf(wxT("ATH aa  loudapprox     =%d\n"), lame_get_athaa_loudapprox( gfp ) );
   wxPrintf(wxT("ATH aa  sensitivity    =%f\n"), lame_get_athaa_sensitivity( gfp ) );

   wxPrintf(wxT("Experimental nspsytune =%d\n"), lame_get_exp_nspsytune( gfp ) );
   wxPrintf(wxT("Experimental X         =%d\n"), lame_get_experimentalX( gfp ) );
   wxPrintf(wxT("Experimental Y         =%d\n"), lame_get_experimentalY( gfp ) );
   wxPrintf(wxT("Experimental Z         =%d\n"), lame_get_experimentalZ( gfp ) );
}
#endif

//----------------------------------------------------------------------------
// ExportMP3
//----------------------------------------------------------------------------

class ExportMP3 final : public ExportPlugin
{
public:

   ExportMP3();
   bool CheckFileName(wxFileName & filename, int format);

   // Required

   wxWindow *OptionsCreate(wxWindow *parent, int format);
   int Export(AudacityProject *project,
               unsigned channels,
               const wxString &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;

private:

   int FindValue(CHOICES *choices, int cnt, int needle, int def);
   wxString FindName(CHOICES *choices, int cnt, int needle);
   int AskResample(int bitrate, int rate, int lowrate, int highrate);
   int AddTags(AudacityProject *project, char **buffer, bool *endOfFile, const Tags *tags);
#ifdef USE_LIBID3TAG
   void AddFrame(struct id3_tag *tp, const wxString & n, const wxString & v, const char *name);
#endif
   int SetNumExportChannels() override;
};

ExportMP3::ExportMP3()
:  ExportPlugin()
{
   InitMP3_Statics();
   AddFormat();
   SetFormat(wxT("MP3"),0);
   AddExtension(wxT("mp3"),0);
   SetMaxChannels(2,0);
   SetCanMetaData(true,0);
   SetDescription(_("MP3 Files"),0);
}

bool ExportMP3::CheckFileName(wxFileName & WXUNUSED(filename), int WXUNUSED(format))
{
#ifndef DISABLE_DYNAMIC_LOADING_LAME
   MP3Exporter exporter;

   if (!exporter.LoadLibrary(wxTheApp->GetTopWindow(), MP3Exporter::Maybe)) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      gPrefs->Flush();

      return false;
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   return true;
}

int ExportMP3::SetNumExportChannels()
{
   bool mono;
   gPrefs->Read(wxT("/FileFormats/MP3ForceMono"), &mono, 0);

   return (mono)? 1 : -1;
}


int ExportMP3::Export(AudacityProject *project,
                       unsigned channels,
                       const wxString &fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       const Tags *metadata,
                       int WXUNUSED(subformat))
{
   int rate = lrint(project->GetRate());
#ifndef DISABLE_DYNAMIC_LOADING_LAME
   wxWindow *parent = project;
#endif // DISABLE_DYNAMIC_LOADING_LAME
   const TrackList *tracks = project->GetTracks();
   MP3Exporter exporter;

#ifdef DISABLE_DYNAMIC_LOADING_LAME
   if (!exporter.InitLibrary(wxT(""))) {
      wxMessageBox(_("Could not initialize MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      gPrefs->Flush();

      return false;
   }
#else
   if (!exporter.LoadLibrary(parent, MP3Exporter::Maybe)) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      gPrefs->Flush();

      return false;
   }

   if (!exporter.ValidLibraryLoaded()) {
      wxMessageBox(_("Not a valid or supported MP3 encoding library!"));
      gPrefs->Write(wxT("/MP3/MP3LibPath"), wxString(wxT("")));
      gPrefs->Flush();

      return false;
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   // Retrieve preferences
   int highrate = 48000;
   int lowrate = 8000;
   int bitrate = 0;
   int brate;
   int rmode;
   int vmode;
   int cmode;
   bool forceMono;

   gPrefs->Read(wxT("/FileFormats/MP3Bitrate"), &brate, 128);
   gPrefs->Read(wxT("/FileFormats/MP3RateMode"), &rmode, MODE_CBR);
   gPrefs->Read(wxT("/FileFormats/MP3VarMode"), &vmode, ROUTINE_FAST);
   gPrefs->Read(wxT("/FileFormats/MP3ChannelMode"), &cmode, CHANNEL_STEREO);
   gPrefs->Read(wxT("/FileFormats/MP3ForceMono"), &forceMono, 0);

   // Set the bitrate/quality and mode
   if (rmode == MODE_SET) {
      int q = FindValue(setRates, WXSIZEOF(setRates), brate, PRESET_STANDARD);
      int r = FindValue(varModes, WXSIZEOF(varModes), vmode, ROUTINE_FAST);
      exporter.SetMode(MODE_SET);
      exporter.SetQuality(q, r);
   }
   else if (rmode == MODE_VBR) {
      int q = FindValue(varRates, WXSIZEOF(varRates), brate, QUALITY_2);
      int r = FindValue(varModes, WXSIZEOF(varModes), vmode, ROUTINE_FAST);
      exporter.SetMode(MODE_VBR);
      exporter.SetQuality(q, r);
   }
   else if (rmode == MODE_ABR) {
      bitrate = FindValue(fixRates, WXSIZEOF(fixRates), brate, 128);
      exporter.SetMode(MODE_ABR);
      exporter.SetBitrate(bitrate);

      if (bitrate > 160) {
         lowrate = 32000;
      }
      else if (bitrate < 32 || bitrate == 144) {
         highrate = 24000;
      }
   }
   else {
      bitrate = FindValue(fixRates, WXSIZEOF(fixRates), brate, 128);
      exporter.SetMode(MODE_CBR);
      exporter.SetBitrate(bitrate);

      if (bitrate > 160) {
         lowrate = 32000;
      }
      else if (bitrate < 32 || bitrate == 144) {
         highrate = 24000;
      }
   }

   // Verify sample rate
   if (FindName(sampRates, WXSIZEOF(sampRates), rate).IsEmpty() ||
      (rate < lowrate) || (rate > highrate)) {
      rate = AskResample(bitrate, rate, lowrate, highrate);
      if (rate == 0) {
         return false;
      }
   }

   // Set the channel mode
   if (forceMono) {
      exporter.SetChannel(CHANNEL_MONO);
   }
   else if (cmode == CHANNEL_JOINT) {
      exporter.SetChannel(CHANNEL_JOINT);
   }
   else {
      exporter.SetChannel(CHANNEL_STEREO);
   }

   auto inSamples = exporter.InitializeStream(channels, rate);
   if (((int)inSamples) < 0) {
      wxMessageBox(_("Unable to initialize MP3 stream"));
      return false;
   }

   // Put ID3 tags at beginning of file
   if (metadata == NULL)
      metadata = project->GetTags();

   // Open file for writing
   wxFFile outFile(fName, wxT("w+b"));
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = AddTags(project, &id3buffer, &endOfFile, metadata);
   if (id3len && !endOfFile) {
     outFile.Write(id3buffer, id3len);
   }

   wxFileOffset pos = outFile.Tell();
   int updateResult = eProgressSuccess;
   long bytes;

   int bufferSize = exporter.GetOutBufferSize();
   unsigned char *buffer = new unsigned char[bufferSize];
   wxASSERT(buffer);

   const WaveTrackConstArray waveTracks =
      tracks->GetWaveTrackConstArray(selectionOnly, false);
   {
      auto mixer = CreateMixer(waveTracks,
         tracks->GetTimeTrack(),
         t0, t1,
         channels, inSamples, true,
         rate, int16Sample, true, mixerSpec);

      wxString title;
      if (rmode == MODE_SET) {
         title.Printf(selectionOnly ?
            _("Exporting selected audio with %s preset") :
            _("Exporting entire file with %s preset"),
            FindName(setRates, WXSIZEOF(setRates), brate).c_str());
      }
      else if (rmode == MODE_VBR) {
         title.Printf(selectionOnly ?
            _("Exporting selected audio with VBR quality %s") :
            _("Exporting entire file with VBR quality %s"),
            FindName(varRates, WXSIZEOF(varRates), brate).c_str());
      }
      else {
         title.Printf(selectionOnly ?
            _("Exporting selected audio at %d Kbps") :
            _("Exporting entire file at %d Kbps"),
            brate);
      }

      ProgressDialog progress(wxFileName(fName).GetName(), title);

      while (updateResult == eProgressSuccess) {
         auto blockLen = mixer->Process(inSamples);

         if (blockLen == 0) {
            break;
         }

         short *mixed = (short *)mixer->GetBuffer();

         if (blockLen < inSamples) {
            if (channels > 1) {
               bytes = exporter.EncodeRemainder(mixed, blockLen, buffer);
            }
            else {
               bytes = exporter.EncodeRemainderMono(mixed, blockLen, buffer);
            }
         }
         else {
            if (channels > 1) {
               bytes = exporter.EncodeBuffer(mixed, buffer);
            }
            else {
               bytes = exporter.EncodeBufferMono(mixed, buffer);
            }
         }

         if (bytes < 0) {
            wxString msg;
            msg.Printf(_("Error %ld returned from MP3 encoder"), bytes);
            wxMessageBox(msg);
            break;
         }

         outFile.Write(buffer, bytes);

         updateResult = progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
   }

   bytes = exporter.FinishStream(buffer);

   if (bytes) {
      outFile.Write(buffer, bytes);
   }

   // Write ID3 tag if it was supposed to be at the end of the file
   if (id3len && endOfFile) {
      outFile.Write(id3buffer, id3len);
   }

   if (id3buffer) {
      free(id3buffer);
   }

   // Always write the info (Xing/Lame) tag.  Until we stop supporting Lame
   // versions before 3.98, we must do this after the MP3 file has been
   // closed.
   //
   // Also, if beWriteInfoTag() is used, mGF will no longer be valid after
   // this call, so do not use it.
   exporter.PutInfoTag(outFile, pos);

   // Close the file
   outFile.Close();

   delete [] buffer;

   return updateResult;
}

wxWindow *ExportMP3::OptionsCreate(wxWindow *parent, int format)
{
   wxASSERT(parent); // to justify safenew
   return safenew ExportMP3Options(parent, format);
}

int ExportMP3::FindValue(CHOICES *choices, int cnt, int needle, int def)
{
   for (int i = 0; i < cnt; i++) {
      if (choices[i].label == needle) {
         return needle;
      }
   }

   return def;
}

wxString ExportMP3::FindName(CHOICES *choices, int cnt, int needle)
{
   for (int i = 0; i < cnt; i++) {
      if (choices[i].label == needle) {
         return choices[i].name.BeforeFirst(wxT(','));
      }
   }

   return wxT("");
}

int ExportMP3::AskResample(int bitrate, int rate, int lowrate, int highrate)
{
   wxDialogWrapper d(nullptr, wxID_ANY, wxString(_("Invalid sample rate")));
   d.SetName(d.GetTitle());
   wxChoice *choice;
   ShuttleGui S(&d, eIsCreating);
   wxString text;

   S.StartVerticalLay();
   {
      S.SetBorder(10);
      S.StartStatic(_("Resample"));
      {
         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            if (bitrate == 0) {
               text.Printf(_("The project sample rate (%d) is not supported by the MP3\nfile format.  "), rate);
            }
            else {
               text.Printf(_("The project sample rate (%d) and bit rate (%d kbps) combination is not\nsupported by the MP3 file format.  "), rate, bitrate);
            }

            text += _("You may resample to one of the rates below.");
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         wxArrayString choices;
         wxString selected = wxT("");
         for (size_t i = 0; i < WXSIZEOF(sampRates); i++) {
            int label = sampRates[i].label;
            if (label >= lowrate && label <= highrate) {
               choices.Add(sampRates[i].name);
               if (label <= rate) {
                  selected = sampRates[i].name;
               }
            }
         }

         if (selected.IsEmpty()) {
            selected = choices[0];
         }

         S.StartHorizontalLay(wxALIGN_CENTER, false);
         {
            choice = S.AddChoice(_("Sample Rates"),
                                 selected,
                                 &choices);
         }
         S.EndHorizontalLay();
      }
      S.EndStatic();

      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   d.Layout();
   d.Fit();
   d.SetMinSize(d.GetSize());
   d.Center();

   if (d.ShowModal() == wxID_CANCEL) {
      return 0;
   }

   return wxAtoi(choice->GetStringSelection());
}

// returns buffer len; caller frees
int ExportMP3::AddTags(AudacityProject *WXUNUSED(project), char **buffer, bool *endOfFile, const Tags *tags)
{
#ifdef USE_LIBID3TAG
   struct id3_tag *tp = id3_tag_new();

   for (const auto &pair : tags->GetRange()) {
      const auto &n = pair.first;
      const auto &v = pair.second;
      const char *name = "TXXX";

      if (n.CmpNoCase(TAG_TITLE) == 0) {
         name = ID3_FRAME_TITLE;
      }
      else if (n.CmpNoCase(TAG_ARTIST) == 0) {
         name = ID3_FRAME_ARTIST;
      }
      else if (n.CmpNoCase(TAG_ALBUM) == 0) {
         name = ID3_FRAME_ALBUM;
      }
      else if (n.CmpNoCase(TAG_YEAR) == 0) {
         // LLL:  Some apps do not like the newer frame ID (ID3_FRAME_YEAR),
         //       so we add old one as well.
         AddFrame(tp, n, v, "TYER");
         name = ID3_FRAME_YEAR;
      }
      else if (n.CmpNoCase(TAG_GENRE) == 0) {
         name = ID3_FRAME_GENRE;
      }
      else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
         name = ID3_FRAME_COMMENT;
      }
      else if (n.CmpNoCase(TAG_TRACK) == 0) {
         name = ID3_FRAME_TRACK;
      }

      AddFrame(tp, n, v, name);
   }

   tp->options &= (~ID3_TAG_OPTION_COMPRESSION); // No compression

   // If this version of libid3tag supports it, use v2.3 ID3
   // tags instead of the newer, but less well supported, v2.4
   // that libid3tag uses by default.
   #ifdef ID3_TAG_HAS_TAG_OPTION_ID3V2_3
   tp->options |= ID3_TAG_OPTION_ID3V2_3;
   #endif

   *endOfFile = false;

   id3_length_t len;

   len = id3_tag_render(tp, 0);
   *buffer = (char *)malloc(len);
   len = id3_tag_render(tp, (id3_byte_t *)*buffer);

   id3_tag_delete(tp);

   return len;
#else //ifdef USE_LIBID3TAG
   return 0;
#endif
}

#ifdef USE_LIBID3TAG
void ExportMP3::AddFrame(struct id3_tag *tp, const wxString & n, const wxString & v, const char *name)
{
   struct id3_frame *frame = id3_frame_new(name);

   if (!n.IsAscii() || !v.IsAscii()) {
      id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_UTF_16);
   }
   else {
      id3_field_settextencoding(id3_frame_field(frame, 0), ID3_FIELD_TEXTENCODING_ISO_8859_1);
   }

   id3_ucs4_t *ucs4 =
      id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) v.mb_str(wxConvUTF8));

   if (strcmp(name, ID3_FRAME_COMMENT) == 0) {
      // A hack to get around iTunes not recognizing the comment.  The
      // language defaults to XXX and, since it's not a valid language,
      // iTunes just ignores the tag.  So, either set it to a valid language
      // (which one???) or just clear it.  Unfortunately, there's no supported
      // way of clearing the field, so do it directly.
      struct id3_frame *frame2 = id3_frame_new(name);
      id3_field_setfullstring(id3_frame_field(frame2, 3), ucs4);
      id3_field *f2 = id3_frame_field(frame2, 1);
      memset(f2->immediate.value, 0, sizeof(f2->immediate.value));
      id3_tag_attachframe(tp, frame2);
      // Now install a second frame with the standard default language = "XXX"
      id3_field_setfullstring(id3_frame_field(frame, 3), ucs4);
   }
   else if (strcmp(name, "TXXX") == 0) {
      id3_field_setstring(id3_frame_field(frame, 2), ucs4);
      free(ucs4);

      ucs4 = id3_utf8_ucs4duplicate((id3_utf8_t *) (const char *) n.mb_str(wxConvUTF8));

      id3_field_setstring(id3_frame_field(frame, 1), ucs4);
   }
   else {
      id3_field_setstrings(id3_frame_field(frame, 1), 1, &ucs4);
   }

   free(ucs4);

   id3_tag_attachframe(tp, frame);
}
#endif

movable_ptr<ExportPlugin> New_ExportMP3()
{
   return make_movable<ExportMP3>();
}

//----------------------------------------------------------------------------
// Return library version
//----------------------------------------------------------------------------

wxString GetMP3Version(wxWindow *parent, bool prompt)
{
   MP3Exporter exporter;
   wxString versionString = _("MP3 export library not found");

#ifndef DISABLE_DYNAMIC_LOADING_LAME
   if (prompt) {
      exporter.FindLibrary(parent);
   }

   if (exporter.LoadLibrary(parent, prompt ? MP3Exporter::Yes : MP3Exporter::No)) {
#endif // DISABLE_DYNAMIC_LOADING_LAME
      versionString = exporter.GetLibraryVersion();
#ifndef DISABLE_DYNAMIC_LOADING_LAME
   }
#endif // DISABLE_DYNAMIC_LOADING_LAME

   return versionString;
}

