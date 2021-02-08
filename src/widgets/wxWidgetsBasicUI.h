/*!********************************************************************

Audacity: A Digital Audio Editor

@file wxWidgetsBasicUI.h
@brief Implementation of BasicUI using wxWidgets

Paul Licameli

**********************************************************************/
#ifndef __WXWIDGETS_BASIC_UI__
#define __WXWIDGETS_BASIC_UI__

#include "BasicUI.h"

//! An implementation of BasicUI::Services in terms of the wxWidgets toolkit
/*! This is a singleton that doesn't need AUDACITY_DLL_API visibility */
class wxWidgetsBasicUI final : public BasicUI::Services {
public:
   ~wxWidgetsBasicUI() override;
};

#endif
