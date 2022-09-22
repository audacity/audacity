/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistControls.cpp

  Dominic Mazzoni

  Paul Licameli split from Nyquist.cpp

******************************************************************//**

\class NyqControl
\brief A control on a NyquistDialog.

*//*******************************************************************/
#include "NyquistControls.h"

#include "../../ShuttleAutomation.h"
#include <float.h>

void NyquistControls::Visit(
   const Bindings &bindings, ConstSettingsVisitor &visitor) const
{
   auto pBinding = bindings.cbegin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      double d = binding.val;

      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
         d = NyquistFormatting::GetCtrlValue(binding.valStr);

      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
          ctrl.type == NYQ_CTRL_TIME)
         visitor.Define( d, static_cast<const wxChar*>( ctrl.var.mName.c_str() ),
            (double)0.0, ctrl.low, ctrl.high, 1.0);
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT) {
         int x = d;
         visitor.Define( x,
            static_cast<const wxChar*>( ctrl.var.mName.c_str() ), 0,
            static_cast<int>(ctrl.low), static_cast<int>(ctrl.high), 1);
         //parms.Write(ctrl.var, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE) {
         // untranslated
         int x = d;
         //parms.WriteEnum(ctrl.var, (int) d, choices);
         visitor.DefineEnum( x,
            static_cast<const wxChar*>( ctrl.var.mName.c_str() ),
            0, ctrl.choices.data(), ctrl.choices.size() );
      }
      else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE) {
         visitor.Define( binding.valStr, ctrl.var.mName,
            wxString{}, ctrl.lowStr, ctrl.highStr );
         //parms.Write(ctrl.var, ctrl.valStr);
      }
   }
}

bool NyquistControls::Save(
   const Bindings &bindings, CommandParameters & parms) const
{
   auto pBinding = bindings.cbegin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      double d = binding.val;

      if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
      {
         d = NyquistFormatting::GetCtrlValue(binding.valStr);
      }

      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
          ctrl.type == NYQ_CTRL_TIME)
      {
         parms.Write(ctrl.var.mName, d);
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         parms.Write(ctrl.var.mName, (int) d);
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         // untranslated
         parms.WriteEnum(ctrl.var.mName, (int) d,
                         ctrl.choices.data(), ctrl.choices.size());
      }
      else if (ctrl.type == NYQ_CTRL_STRING)
      {
         parms.Write(ctrl.var.mName, binding.valStr);
      }
      else if (ctrl.type == NYQ_CTRL_FILE)
      {
         // Convert the given path string to platform-dependent equivalent
         NyquistFormatting::
         resolveFilePath(const_cast<NyqValue&>(binding).valStr);
         parms.Write(ctrl.var.mName, binding.valStr);
      }
   }

   return true;
}

int NyquistControls::Load(Bindings &bindings,
   const CommandParameters & parms, bool bTestOnly)
{
   int badCount = 0;
   // First pass verifies values
   auto pBinding = bindings.begin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      bool good = false;

      // This GetCtrlValue code is preserved from former code,
      // but probably is pointless.  The value d isn't used later,
      // and GetCtrlValue does not appear to have important needed
      // side effects.
      if (!bTestOnly) {
         double d = binding.val;
         if (d == UNINITIALIZED_CONTROL && ctrl.type != NYQ_CTRL_STRING)
         {
            d = NyquistFormatting::GetCtrlValue(binding.valStr);
         }
      }

      if (ctrl.type == NYQ_CTRL_FLOAT || ctrl.type == NYQ_CTRL_FLOAT_TEXT ||
         ctrl.type == NYQ_CTRL_TIME)
      {
         double val;
         good = parms.Read(ctrl.var.mName, &val) &&
            val >= ctrl.low &&
            val <= ctrl.high;
         if (good && !bTestOnly)
            binding.val = val;
      }
      else if (ctrl.type == NYQ_CTRL_INT || ctrl.type == NYQ_CTRL_INT_TEXT)
      {
         int val;
         good = parms.Read(ctrl.var.mName, &val) &&
            val >= ctrl.low &&
            val <= ctrl.high;
         if (good && !bTestOnly)
            binding.val = (double)val;
      }
      else if (ctrl.type == NYQ_CTRL_CHOICE)
      {
         int val;
         // untranslated
         good = parms.ReadEnum(ctrl.var.mName, &val,
            ctrl.choices.data(), ctrl.choices.size()) &&
            val != wxNOT_FOUND;
         if (good && !bTestOnly)
            binding.val = (double)val;
      }
      else if (ctrl.type == NYQ_CTRL_STRING || ctrl.type == NYQ_CTRL_FILE)
      {
         wxString val;
         good = parms.Read(ctrl.var.mName, &val);
         if (good && !bTestOnly)
            binding.valStr = val;
      }
      else if (ctrl.type == NYQ_CTRL_TEXT)
      {
         // This "control" is just fixed text (nothing to save or restore),
         // Does not count for good/bad counting.
         good = true;
      }
      badCount += !good ? 1 : 0;
   }
   return badCount;
}

wxString NyquistControls::Expression(const Bindings &bindings) const
{
   NyquistFormatting::Assignments result;
   wxString cmd;
   auto pBinding = bindings.cbegin();
   for (const auto &ctrl : mControls) {
      auto &binding = *pBinding++;
      const auto getValue = [&]() -> NyquistFormatting::Value {
         switch (ctrl.type)  {
         case NYQ_CTRL_FLOAT:
         case NYQ_CTRL_FLOAT_TEXT:
         case NYQ_CTRL_TIME:
            return binding.val;

         case NYQ_CTRL_INT:
         case NYQ_CTRL_INT_TEXT:
         case NYQ_CTRL_CHOICE:
            return int(binding.val);

         case NYQ_CTRL_STRING:
         case NYQ_CTRL_FILE:
            return binding.valStr;

         default:
            assert(false);
            return {}; // make it unbound
         }
      };
      result.Append({ ctrl.var, getValue() });
   }
   return std::move(result);
}
