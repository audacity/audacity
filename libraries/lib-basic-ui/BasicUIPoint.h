/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUIPoint.h
@brief A pair of screen coordinates for use in toolkit-neutral UI facades

Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_BASIC_UI_POINT__
#define __AUDACITY_BASIC_UI_POINT__

namespace BasicUI {

//! A pair of screen coordinates, x increasing rightward, y downward
/*! Origin is unspecified */
struct Point{
   Point() = default;
   Point( int x, int y ) : x{x}, y{y} {}

   int x = -1;
   int y = -1;
};

}

#endif
