/**********************************************************************

Audacity: A Digital Audio Editor

ExportFFmpegDialogs.h

LRN

**********************************************************************/

#if !defined(__EXPORT_FFMPEG_DIALOGS_H__)
#define __EXPORT_FFMPEG_DIALOGS_H__

#if defined(USE_FFMPEG)

#include "wxPanelWrapper.h"

class wxArrayStringEx;

class wxArrayString;
class wxCheckBox;
class wxStaticText;
class wxTextCtrl;
class wxSlider;
class wxChoice;

class ShuttleGui;

/// AC3 export options dialog
class ExportFFmpegAC3Options final : public wxPanelWrapper
{
public:

   ExportFFmpegAC3Options(wxWindow *parent, int format);
   virtual ~ExportFFmpegAC3Options();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   /// Sample Rates supported by AC3 encoder (must end with zero-element)
   /// It is not used in dialog anymore, but will be required later
   static const int iAC3SampleRates[];

private:

   wxChoice *mBitRateChoice;
   int mBitRateFromChoice;
};

class ExportFFmpegAACOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegAACOptions(wxWindow *parent, int format);
   virtual ~ExportFFmpegAACOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;
};

class ExportFFmpegAMRNBOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegAMRNBOptions(wxWindow *parent, int format);
   virtual ~ExportFFmpegAMRNBOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

private:

   wxChoice *mBitRateChoice;
   int mBitRateFromChoice;
};

class ExportFFmpegOPUSOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegOPUSOptions(wxWindow *parent, int format);
   ~ExportFFmpegOPUSOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   static const int iOPUSSampleRates[];

private:

   wxSlider *mBitRateSlider;
   int mBitRateFromSlider;

   wxChoice *mVbrChoice;
   int mVbrFromChoice;

   wxSlider *mComplexitySlider;
   int mComplexityFromSlider;

   wxChoice *mFramesizeChoice;
   int mFramesizeFromChoice;

   wxChoice *mApplicationChoice;
   int mApplicationFromChoice;

   wxChoice *mCuttoffChoice;
   int mCutoffFromChoice;
};

class ExportFFmpegWMAOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegWMAOptions(wxWindow *parent, int format);
   ~ExportFFmpegWMAOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   static const int iWMASampleRates[];

private:

   wxChoice *mBitRateChoice;
   int mBitRateFromChoice;
};

class ExportFFmpegCustomOptions final : public wxPanelWrapper
{
public:

   ExportFFmpegCustomOptions(wxWindow *parent, int format);
   ~ExportFFmpegCustomOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   void OnOpen(wxCommandEvent & evt);

private:
   wxTextCtrl *mFormat;
   wxTextCtrl *mCodec;

   DECLARE_EVENT_TABLE()
};

#endif

#endif //__EXPORT_FFMPEG_DIALOGS_H__
