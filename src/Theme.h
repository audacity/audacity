/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME__
#define __AUDACITY_THEME__

#include "Audacity.h"

#include <wx/bitmap.h>
#include <wx/colour.h>
#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/font.h>
#include <wx/image.h>

// JKC: will probably change name from 'teBmps' to 'tIndexBmp';
typedef int teBmps; /// The index of a bitmap resource in Theme Resources.

enum teResourceType
{
   resTypeColour,
   resTypeBitmap,
   resTypeImage = resTypeBitmap,
   resTypeCursor,
   resTypeFont
};

enum teResourceFlags
{
   resFlagNone   =0x00,
   resFlagPaired =0x01,
   resFlagCursor =0x02,
   resFlagNewLine = 0x04,
   resFlagInternal = 0x08  // For image manipulation.  Don't save or load.
};

WX_DECLARE_USER_EXPORTED_OBJARRAY(wxImage,  ArrayOfImages, AUDACITY_DLL_API);
WX_DECLARE_USER_EXPORTED_OBJARRAY(wxBitmap, ArrayOfBitmaps, AUDACITY_DLL_API);
WX_DECLARE_USER_EXPORTED_OBJARRAY(wxColour, ArrayOfColours, AUDACITY_DLL_API);

//WX_DECLARE_OBJARRAY(wxImage,  ArrayOfImages);
//WX_DECLARE_OBJARRAY(wxBitmap, ArrayOfBitmaps);
//WX_DECLARE_OBJARRAY(wxColour, ArrayOfColours);

class AUDACITY_DLL_API FlowPacker
{
public:
   FlowPacker(){;};
   ~FlowPacker(){;};
   void Init(int width);
   void GetNextPosition( int xSize, int ySize );
   void SetNewGroup( int iGroupSize );
   void SetColourGroup( );
   wxRect Rect();
   void RectMid( int &x, int &y );

   // These 4 should become private again...
   int mFlags;
   int mxPos;
   int myPos;
   int myHeight;

private:
   int iImageGroupSize;
   int iImageGroupIndex;
   int mOldFlags;
   int myPosBase;
   int mxWidth;
   int mxCacheWidth;

   int mComponentWidth;
   int mComponentHeight;

};

class AUDACITY_DLL_API ThemeBase /* not final */
{
public:
   ThemeBase(void);
public:
   virtual ~ThemeBase(void);

public:
   virtual void EnsureInitialised()=0;
   virtual void ApplyUpdatedImages()=0;
   void LoadThemeAtStartUp( bool bLookForExternalFiles );
   void RegisterImage( int &iIndex,char const** pXpm, const wxString & Name);
   void RegisterImage( int &iIndex, const wxImage &Image, const wxString & Name );
   void RegisterColour( int &iIndex, const wxColour &Clr, const wxString & Name );

   void CreateImageCache(bool bBinarySave = true);
   bool ReadImageCache( bool bBinaryRead = true, bool bOkIfNotFound=false);
   void LoadComponents( bool bOkIfNotFound =false);
   void SaveComponents();
   void ReadThemeInternal();
   void SaveThemeAsCode();
   void WriteImageDefs( );
   void WriteImageMap( );

   wxColour & Colour( int iIndex );
   wxBitmap & Bitmap( int iIndex );
   wxImage  & Image( int iIndex );
   wxCursor & Cursor( int iIndex );
   wxFont   & Font( int iIndex );
   wxSize ImageSize( int iIndex );

   void ReplaceImage( int iIndex, wxImage * pImage );

   void SetBrushColour( wxBrush & Brush, int iIndex );
   void SetPenColour(   wxPen & Pen, int iIndex );
   void SetFlags( int flags ){ mFlow.mFlags = flags;};

   // Utility function that combines a bitmap and a mask, both in XPM format.
   wxImage MaskedImage( char const ** pXpm, char const ** pMask );
   // Utility functiuon that takes a 32 bit bitmap and makes it into an image.
   wxImage MakeImageWithAlpha( wxBitmap & Bmp );

protected:
   ArrayOfImages mImages;
   ArrayOfBitmaps mBitmaps;
   wxArrayString mBitmapNames;
   wxArrayInt mBitmapFlags;

   ArrayOfColours mColours;
   wxArrayString mColourNames;
   FlowPacker mFlow;
};


class AUDACITY_DLL_API Theme final : public ThemeBase
{
public:
   Theme(void);
public:
   ~Theme(void);
public:
   void EnsureInitialised() override;
   void ApplyUpdatedImages() override;
   void RegisterImages();
   void RegisterColours();
   bool mbInitialised;
};

extern AUDACITY_DLL_API Theme theTheme;

#endif // __AUDACITY_THEME__
