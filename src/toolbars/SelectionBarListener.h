/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBarListener.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR_LISTENER__
#define __AUDACITY_SELECTION_BAR_LISTENER__

class ComponentInterfaceSymbol;
using NumericFormatId = ComponentInterfaceSymbol;
class SelectedRegion;

class AUDACITY_DLL_API SelectionBarListener /* not final */ {

 public:

   SelectionBarListener(){};
   virtual ~SelectionBarListener(){};

   virtual double AS_GetRate() = 0;
   virtual void AS_SetRate(double rate) = 0;
   virtual int AS_GetSnapTo() = 0;
   virtual void AS_SetSnapTo(int snap) = 0;
   virtual const NumericFormatId & AS_GetSelectionFormat() = 0;
   virtual void AS_SetSelectionFormat(const NumericFormatId & format) = 0;
   virtual void AS_ModifySelection(double &start, double &end, bool done) = 0;
};

#endif
