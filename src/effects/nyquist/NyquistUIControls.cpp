/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistUIControls.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

**********************************************************************/
#include "NyquistUIControls.h"
#include "../Effect.h"
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/slider.h>
#include <wx/tokenzr.h>
#include <wx/valgen.h>
#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "../../widgets/NumericTextCtrl.h"
#include "../../widgets/wxPanelWrapper.h"

enum
{
   ID_Slider = 11000,
   ID_Text = 12000,
   ID_Choice = 13000,
   ID_Time = 14000,
   ID_FILE = 15000
};

NyquistUIControls::~NyquistUIControls() = default;

bool NyquistUIControls::UpdateUI()
{
   const auto &bindings = mBindings;
   auto beginBinding = bindings.cbegin(), pBinding = beginBinding;
   for (const auto &ctrl : mControls) {
      size_t i = pBinding - beginBinding;
      auto &binding = *pBinding++;

      if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         const auto count = ctrl.choices.size();

         int val = (int)binding.val;
         if (val < 0 || val >= (int)count)
         {
            val = 0;
         }

         auto c = static_cast<wxChoice *>(
            mEffect.GetUIParent()->FindWindow(ID_Choice + i));
         c->SetSelection(val);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_FLOAT)
      {
         // wxTextCtrls are handled by the validators
         double range = ctrl.high - ctrl.low;
         int val = (int)(0.5 + ctrl.ticks * (binding.val - ctrl.low) / range);
         auto s = static_cast<wxSlider *>(
            mEffect.GetUIParent()->FindWindow(ID_Slider + i));
         s->SetValue(val);
      }
      else if (ctrl.type == NYQ_CTRL_TIME)
      {
         auto n = static_cast<NumericTextCtrl *>(
            mEffect.GetUIParent()->FindWindow(ID_Time + i));
         n->SetValue(binding.val);
      }
   }

   mEffect.EnablePreview(mEnablePreview);
   return true;
}

bool NyquistUIControls::ValidateUI()
{
   if (mControls.size() == 0)
   {
      return true;
   }

   using namespace NyquistFormatting;
   
   auto &bindings = mBindings;
   auto beginBinding = bindings.begin(), pBinding = beginBinding;
   for (const auto &ctrl : mControls) {
      size_t i = pBinding - beginBinding;
      auto &binding = *pBinding++;

      if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_TEXT)
      {
         continue;
      }

      if (binding.val == UNINITIALIZED_CONTROL)
      {
         binding.val = GetCtrlValue(binding.valStr);
      }

      if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         continue;
      }

      if (ctrl.type == NYQ_CTRL_FILE)
      {
         resolveFilePath(binding.valStr);

         wxString path;
         if (binding.valStr.StartsWith("\"", &path))
         {
            // Validate if a list of quoted paths.
            if (path.EndsWith("\"", &path))
            {
               path.Replace("\"\"", "\"");
               wxStringTokenizer tokenizer(path, "\"");
               while (tokenizer.HasMoreTokens())
               {
                  wxString token = tokenizer.GetNextToken();
                  if(!validatePath(token))
                  {
                     const auto message =
                        XO("\"%s\" is not a valid file path.").Format( token );
                     mEffect.MessageBox(
                        message,
                        wxOK | wxICON_EXCLAMATION | wxCENTRE,
                        XO("Error") );
                     return false;
                  }
               }
               continue;
            }
            else
            {
               const auto message =
                  /* i18n-hint: Warning that there is one quotation mark rather than a pair.*/
                  XO("Mismatched quotes in\n%s").Format( binding.valStr );
               mEffect.MessageBox(
                  message,
                  wxOK | wxICON_EXCLAMATION | wxCENTRE,
                  XO("Error") );
               return false;
            }
         }
         // Validate a single path.
         else if (validatePath(binding.valStr))
         {
            continue;
         }

         // Validation failed
         const auto message =
            XO("\"%s\" is not a valid file path.").Format( binding.valStr );
         mEffect.MessageBox(
            message,
            wxOK | wxICON_EXCLAMATION | wxCENTRE,
            XO("Error") );
         return false;
      }

      if (ctrl.type == NYQ_CTRL_TIME)
      {
         auto n = static_cast<NumericTextCtrl *>(
            mEffect.GetUIParent()->FindWindow(ID_Time + i));
         binding.val = n->GetValue();
      }

      binding.val = std::clamp(binding.val, ctrl.low, ctrl.high);
   }
   
   return true;
}

void NyquistUIControls::Populate(ShuttleGui &S,
   const NumericFormatSymbol &selectionFormat, double projectRate)
{
   auto &bindings = mBindings;
   auto beginBinding = bindings.begin(), pBinding = beginBinding;
   for (const auto &ctrl : mControls) {
      size_t i = pBinding - beginBinding;
      auto &binding = *pBinding++;

      if (ctrl.type == NYQ_CTRL_TEXT)
      {
         S.EndMultiColumn();
         S.StartHorizontalLay(wxALIGN_LEFT, 0);
         {
            S.AddSpace(0, 10);
            S.AddFixedText( Verbatim( ctrl.label ), false );
         }
         S.EndHorizontalLay();
         S.StartMultiColumn(4);
      }
      else
      {
         auto prompt = XXO("%s:").Format( ctrl.name );
         S.AddPrompt( prompt );

         if (ctrl.type == NYQ_CTRL_STRING)
         {
            S.AddSpace(10, 10);

            S.Id(ID_Text + i)
               .Validator<wxGenericValidator>(&binding.valStr)
               .Name( prompt )
               .AddTextBox( {}, wxT(""), 50)
                  ->Bind(wxEVT_COMMAND_TEXT_UPDATED,
                     &NyquistUIControls::OnText, this);
         }
         else if (ctrl.type == NYQ_CTRL_CHOICE)
         {
            S.AddSpace(10, 10);

            S.Id(ID_Choice + i).AddChoice( {},
               Msgids( ctrl.choices.data(), ctrl.choices.size() ) )
                  ->Bind(wxEVT_COMMAND_CHOICE_SELECTED,
                     &NyquistUIControls::OnChoice, this);
         }
         else if (ctrl.type == NYQ_CTRL_TIME)
         {
            S.AddSpace(10, 10);

            const auto options = NumericTextCtrl::Options{}
                                    .AutoPos(true)
                                    .MenuEnabled(true)
                                    .ReadOnly(false);

            NumericTextCtrl *time = safenew
               NumericTextCtrl(S.GetParent(), (ID_Time + i),
                               NumericConverter::TIME,
                               selectionFormat,
                               binding.val,
                               projectRate,
                               options);
            S
               .Name( prompt )
               .Position(wxALIGN_LEFT | wxALL)
               .AddWindow(time)
                  ->Bind(wxEVT_COMMAND_TEXT_UPDATED,
                     &NyquistUIControls::OnTime, this);
         }
         else if (ctrl.type == NYQ_CTRL_FILE)
         {
            S.AddSpace(10, 10);

            // Get default file extension if specified in wildcards
            FileExtension defaultExtension;
            if (!ctrl.fileTypes.empty()) {
               const auto &type = ctrl.fileTypes[0];
               if ( !type.extensions.empty() )
                  defaultExtension = type.extensions[0];
            }
            NyquistFormatting::
            resolveFilePath(binding.valStr, defaultExtension);

            auto item = S.Id(ID_Text+i)
               .Name( prompt )
               .AddTextBox( {}, wxT(""), 40);
            item->Bind(wxEVT_COMMAND_TEXT_UPDATED,
               &NyquistUIControls::OnText, this);
            item->SetValidator(wxGenericValidator(&binding.valStr));

            S.Id(ID_FILE + i).AddButton(
               Verbatim( ctrl.label.empty ()
                  // We'd expect wxFileSelectorPromptStr to already be
                  // translated, but apparently not.
                  ? wxGetTranslation( wxFileSelectorPromptStr )
                  : ctrl.label ),
               wxALIGN_LEFT)
                  ->Bind(wxEVT_COMMAND_BUTTON_CLICKED,
                     &NyquistUIControls::OnFileButton, this);
         }
         else
         {
            // Integer or Real
            if (ctrl.type == NYQ_CTRL_INT_TEXT || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
            {
               S.AddSpace(10, 10);
            }

            S.Id(ID_Text+i);
            if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT)
            {
               double range = ctrl.high - ctrl.low;
               S.Validator<FloatingPointValidator<double>>(
                  // > 12 decimal places can cause rounding errors in display.
                  12, &binding.val,
                  // Set number of decimal places
                  (range < 10
                     ? NumValidatorStyle::THREE_TRAILING_ZEROES
                     : range < 100
                        ? NumValidatorStyle::TWO_TRAILING_ZEROES
                        : NumValidatorStyle::ONE_TRAILING_ZERO),
                  ctrl.low, ctrl.high
               );
            }
            else
            {
               S.Validator<IntegerValidator<double>>(
                  &binding.val, NumValidatorStyle::DEFAULT,
                  (int) ctrl.low, (int) ctrl.high);
            }
            S
               .Name( prompt )
               .AddTextBox( {}, wxT(""),
                  (ctrl.type == NYQ_CTRL_INT_TEXT ||
                   ctrl.type == NYQ_CTRL_FLOAT_TEXT) ? 25 : 12)
                  ->Bind(wxEVT_COMMAND_TEXT_UPDATED,
                     &NyquistUIControls::OnText, this);

            if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_FLOAT)
            {
               S.Id(ID_Slider + i)
                  .Style(wxSL_HORIZONTAL)
                  .MinSize( { 150, -1 } )
                  .AddSlider( {}, 0, ctrl.ticks, 0)
                     ->Bind(wxEVT_COMMAND_SLIDER_UPDATED,
                        &NyquistUIControls::OnSlider, this);
            }
         }

         if (ctrl.type != NYQ_CTRL_FILE)
         {
            if (ctrl.type == NYQ_CTRL_CHOICE || ctrl.label.empty())
            {
               S.AddSpace(10, 10);
            }
            else
            {
               S.AddUnits( Verbatim( ctrl.label ) );
            }
         }
      }
   }
}

void NyquistUIControls::OnSlider(wxCommandEvent & evt)
{
   int i = evt.GetId() - ID_Slider;
   const auto & ctrl = mControls[i];
   auto &binding = mBindings[i];

   int val = evt.GetInt();
   double range = ctrl.high - ctrl.low;
   double newVal = (val / (double)ctrl.ticks) * range + ctrl.low;

   // Determine precision for displayed number
   int precision = range < 1.0 ? 3 :
                   range < 10.0 ? 2 :
                   range < 100.0 ? 1 :
                   0;

   // If the value is at least one tick different from the current value
   // change it (this prevents changes from manually entered values unless
   // the slider actually moved)
   if (fabs(newVal - binding.val) >= (1 / (double)ctrl.ticks) * range &&
       fabs(newVal - binding.val) >= pow(0.1, precision) / 2)
   {
      // First round to the appropriate precision
      newVal *= pow(10.0, precision);
      newVal = floor(newVal + 0.5);
      newVal /= pow(10.0, precision);

      binding.val = newVal;

      mEffect.GetUIParent()->FindWindow(ID_Text + i)->GetValidator()->TransferToWindow();
   }
}

void NyquistUIControls::OnChoice(wxCommandEvent & evt)
{
   mBindings[evt.GetId() - ID_Choice].val = (double) evt.GetInt();
}

void NyquistUIControls::OnTime(wxCommandEvent& evt)
{
   int i = evt.GetId() - ID_Time;
   static double value = 0.0;
   const auto & ctrl = mControls[i];

   auto n = static_cast<NumericTextCtrl *>(
      mEffect.GetUIParent()->FindWindow(ID_Time + i));
   double val = n->GetValue();

   // Observed that two events transmitted on each control change (Linux)
   // so skip if value has not changed.
   if (val != value) {
      if (val < ctrl.low || val > ctrl.high) {
         const auto message = XO("Value range:\n%s to %s")
            .Format( ToTimeFormat(ctrl.low), ToTimeFormat(ctrl.high) );
         mEffect.MessageBox(
            message,
            wxOK | wxCENTRE,
            XO("Value Error") );
      }

      if (val < ctrl.low)
         val = ctrl.low;
      else if (val > ctrl.high)
         val = ctrl.high;

      n->SetValue(val);
      value = val;
   }
}

void NyquistUIControls::OnFileButton(wxCommandEvent& evt)
{
   int i = evt.GetId() - ID_FILE;
   const auto & ctrl = mControls[i];
   auto &binding = mBindings[i];

   // Get style flags:
   // Ensure legal combinations so that wxWidgets does not throw an assert error.
   unsigned int flags = 0;
   if (!ctrl.highStr.empty())
   {
      wxStringTokenizer tokenizer(ctrl.highStr, ",");
      while ( tokenizer.HasMoreTokens() )
      {
         wxString token = tokenizer.GetNextToken().Trim(true).Trim(false);
         if (token.IsSameAs("open", false))
         {
            flags |= wxFD_OPEN;
            flags &= ~wxFD_SAVE;
            flags &= ~wxFD_OVERWRITE_PROMPT;
         }
         else if (token.IsSameAs("save", false))
         {
            flags |= wxFD_SAVE;
            flags &= ~wxFD_OPEN;
            flags &= ~wxFD_MULTIPLE;
            flags &= ~wxFD_FILE_MUST_EXIST;
         }
         else if (token.IsSameAs("overwrite", false) && !(flags & wxFD_OPEN))
         {
            flags |= wxFD_OVERWRITE_PROMPT;
         }
         else if (token.IsSameAs("exists", false) && !(flags & wxFD_SAVE))
         {
            flags |= wxFD_FILE_MUST_EXIST;
         }
         else if (token.IsSameAs("multiple", false) && !(flags & wxFD_SAVE))
         {
            flags |= wxFD_MULTIPLE;
         }
      }
   }

   NyquistFormatting::resolveFilePath(binding.valStr);

   wxFileName fname = binding.valStr;
   wxString defaultDir = fname.GetPath();
   wxString defaultFile = fname.GetName();
   auto message = XO("Select a file");

   if (flags & wxFD_MULTIPLE)
      message = XO("Select one or more files");
   else if (flags & wxFD_SAVE)
      message = XO("Save file as");

   FileDialogWrapper openFileDialog(
      mEffect.GetUIParent()->FindWindow(ID_FILE + i),
      message, defaultDir, defaultFile, ctrl.fileTypes,
      flags);       // styles

   if (openFileDialog.ShowModal() == wxID_CANCEL)
   {
      return;
   }

   wxString path;
   // When multiple files selected, return file paths as a list of quoted strings.
   if (flags & wxFD_MULTIPLE)
   {
      wxArrayString selectedFiles;
      openFileDialog.GetPaths(selectedFiles);

      for (size_t sf = 0; sf < selectedFiles.size(); sf++) {
         path += "\"";
         path += selectedFiles[sf];
         path += "\"";
      }
      binding.valStr = path;
   }
   else
   {
      binding.valStr = openFileDialog.GetPath();
   }

   mEffect.GetUIParent()->FindWindow(ID_Text + i)
      ->GetValidator()->TransferToWindow();
}

bool NyquistUIControls::validatePath(wxString path)
{
   wxFileName fname = path;
   wxString dir = fname.GetPath();

   return (fname.wxFileName::IsOk() &&
           wxFileName::DirExists(dir) &&
           !fname.GetFullName().empty());
}


wxString NyquistUIControls::ToTimeFormat(double t)
{
   int seconds = static_cast<int>(t);
   int hh = seconds / 3600;
   int mm = seconds % 3600;
   mm = mm / 60;
   return wxString::Format("%d:%d:%.3f", hh, mm, t - (hh * 3600 + mm * 60));
}


void NyquistUIControls::OnText(wxCommandEvent & evt)
{
   int i = evt.GetId() - ID_Text;

   const auto & ctrl = mControls[i];
   const auto &binding = mBindings[i];

   if (wxDynamicCast(evt.GetEventObject(), wxWindow)->GetValidator()->TransferFromWindow())
   {
      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_INT)
      {
         int pos = (int)floor((binding.val - ctrl.low) /
                              (ctrl.high - ctrl.low) * ctrl.ticks + 0.5);

         auto slider = static_cast<wxSlider *>(
            mEffect.GetUIParent()->FindWindow(ID_Slider + i));
         slider->SetValue(pos);
      }
   }
}
