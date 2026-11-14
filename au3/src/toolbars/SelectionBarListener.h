/**********************************************************************

  Audacity: A Digital Audio Editor

  SelectionBarListener.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SELECTION_BAR_LISTENER__
#define __AUDACITY_SELECTION_BAR_LISTENER__

#include "audacity/Types.h"
#include "ComponentInterfaceSymbol.h"

class SelectedRegion;

class AUDACITY_DLL_API SelectionBarListener /* not final */
{
public:

    SelectionBarListener() {}
    virtual ~SelectionBarListener() {}

    virtual NumericFormatID AS_GetSelectionFormat() = 0;
    virtual void AS_SetSelectionFormat(const NumericFormatID& format) = 0;
    virtual void AS_ModifySelection(double& start, double& end, bool done) = 0;
};

class AUDACITY_DLL_API TimeToolBarListener /* not final */
{
public:

    TimeToolBarListener() {}
    virtual ~TimeToolBarListener() {}

    virtual NumericFormatID TT_GetAudioTimeFormat() = 0;
    virtual void TT_SetAudioTimeFormat(const NumericFormatID& format) = 0;
};

#endif
