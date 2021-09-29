/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME__
#define __AUDACITY_THEME__



#include <vector>
#include <wx/defs.h>
#include <wx/window.h> // to inherit
#include "ComponentInterfaceSymbol.h"

#include "Prefs.h"

//! A choice of theme such as "Light", "Dark", ...
using teThemeType = Identifier;

class wxArrayString;
class wxBitmap;
class wxColour;
class wxImage;
class wxPen;

class ChoiceSetting;

// JKC: will probably change name from 'teBmps' to 'tIndexBmp';
using teBmps = int; /// The index of a bitmap resource in Theme Resources.

enum teResourceType
{
   resTypeColour,
   resTypeBitmap,
   resTypeImage = resTypeBitmap,
};

enum teResourceFlags
{
   resFlagNone   =0x00,
   resFlagPaired =0x01,
   resFlagCursor =0x02,
   resFlagNewLine = 0x04,
   resFlagInternal = 0x08,  // For image manipulation.  Don't save or load.
   resFlagSkip = 0x10
};

//! A cursor for iterating the theme bitmap
class AUDACITY_DLL_API FlowPacker
{
public:
   explicit FlowPacker(int width);
   ~FlowPacker() {}
   void GetNextPosition( int xSize, int ySize );
   void SetNewGroup( int iGroupSize );
   void SetColourGroup( );
   wxRect Rect();
   wxRect RectInner();
   void RectMid( int &x, int &y );

   // These 4 should become private again...
   int mFlags = resFlagPaired;
   int mxPos = 0;
   int myPos = 0;
   int myHeight = 0;
   int mBorderWidth = 1;

private:
   int iImageGroupSize = 1;
   int iImageGroupIndex = -1;
   int mOldFlags = resFlagPaired;
   int myPosBase = 0;
   int mxCacheWidth = 0;

   int mComponentWidth = 0;
   int mComponentHeight = 0;

};

class AUDACITY_DLL_API ThemeBase /* not final */
{
public:
   ThemeBase(void);
   ThemeBase ( const ThemeBase & ) = delete;
   ThemeBase &operator =( const ThemeBase & ) = delete;
public:
   virtual ~ThemeBase(void);

public:
   virtual void EnsureInitialised()=0;

   // Typically statically constructed:
   struct AUDACITY_DLL_API RegisteredTheme {
      RegisteredTheme(EnumValueSymbol symbol,
         const std::vector<unsigned char> &data /*!<
            A reference to this vector is stored, not a copy of it! */
      );
      ~RegisteredTheme();
      const EnumValueSymbol symbol;
   };

   void LoadTheme( teThemeType Theme );
   void RegisterImage( int &flags, int &iIndex,char const** pXpm, const wxString & Name);
   void RegisterImage( int &flags, int &iIndex, const wxImage &Image, const wxString & Name );
   void RegisterColour( int &iIndex, const wxColour &Clr, const wxString & Name );

   teThemeType GetFallbackThemeType();
   void CreateImageCache(bool bBinarySave = true);
   bool ReadImageCache( teThemeType type = {}, bool bOkIfNotFound=false);
   void LoadComponents( bool bOkIfNotFound =false);
   void SaveComponents();
   void SaveThemeAsCode();
   void WriteImageDefs( );
   void WriteImageMap( );
   static bool LoadPreferredTheme();
   bool IsUsingSystemTextColour(){ return bIsUsingSystemTextColour; }
   void RecolourBitmap( int iIndex, wxColour From, wxColour To );
   void RecolourTheme();

   int ColourDistance( wxColour & From, wxColour & To );
   wxColour & Colour( int iIndex );
   wxBitmap & Bitmap( int iIndex );
   wxImage  & Image( int iIndex );
   wxSize ImageSize( int iIndex );
   bool bRecolourOnLoad;  // Request to recolour.
   bool bIsUsingSystemTextColour;

   void ReplaceImage( int iIndex, wxImage * pImage );
   void RotateImageInto( int iTo, int iFrom, bool bClockwise );

   void SetBrushColour( wxBrush & Brush, int iIndex );
   void SetPenColour(   wxPen & Pen, int iIndex );

   // Utility function that combines a bitmap and a mask, both in XPM format.
   wxImage MaskedImage( char const ** pXpm, char const ** pMask );
   // Utility function that takes a 32 bit bitmap and makes it into an image.
   wxImage MakeImageWithAlpha( wxBitmap & Bmp );

protected:
   // wxImage, wxBitmap copy cheaply using reference counting
   std::vector<wxImage> mImages;
   std::vector<wxBitmap> mBitmaps;
   wxArrayString mBitmapNames;
   std::vector<int> mBitmapFlags;

   std::vector<wxColour> mColours;
   wxArrayString mColourNames;
};


class AUDACITY_DLL_API Theme final : public ThemeBase
{
public:
   Theme(void);
public:
   ~Theme(void);
public:
   void EnsureInitialised() override;
   void RegisterImages();
   void RegisterColours();
   bool mbInitialised;
};

// A bit cheeky - putting a themable wxStaticText control into
// theme, rather than in a new file.  Saves sorting out makefiles (for now).
class wxWindow;
class wxString;
class wxPaintEvent;

class AUDACITY_DLL_API auStaticText : public wxWindow
{
public:
   auStaticText(wxWindow* parent, wxString text);
   void OnPaint(wxPaintEvent & evt);
   bool AcceptsFocus() const override { return false; }
   void OnErase(wxEraseEvent& event) {
      static_cast<void>(event);
   };
   DECLARE_EVENT_TABLE();
};

extern AUDACITY_DLL_API Theme theTheme;

extern AUDACITY_DLL_API BoolSetting
     GUIBlendThemes
;

extern AUDACITY_DLL_API ChoiceSetting
     &GUITheme()
;

#endif // __AUDACITY_THEME__
