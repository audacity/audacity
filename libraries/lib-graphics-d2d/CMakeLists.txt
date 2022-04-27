#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   graphics/d2d/D2DBitmap.cpp
   graphics/d2d/D2DBitmap.h
   graphics/d2d/D2DFont.cpp
   graphics/d2d/D2DFont.h
   graphics/d2d/D2DFontCollection.cpp
   graphics/d2d/D2DFontCollection.h
   graphics/d2d/D2DPainter.cpp
   graphics/d2d/D2DPainter.h
   graphics/d2d/D2DRenderer.cpp
   graphics/d2d/D2DRenderer.h
   graphics/d2d/D2DRenderTarget.cpp
   graphics/d2d/D2DRenderTarget.h
   graphics/d2d/D2DRenderTargetResource.cpp
   graphics/d2d/D2DRenderTargetResource.h

   graphics/d2d/bitmaps/D2DRenderTargetBitmap.cpp
   graphics/d2d/bitmaps/D2DRenderTargetBitmap.h
   graphics/d2d/bitmaps/D2DSubBitmap.cpp
   graphics/d2d/bitmaps/D2DSubBitmap.h
   graphics/d2d/bitmaps/D2DWICBitmap.cpp
   graphics/d2d/bitmaps/D2DWICBitmap.h

   graphics/d2d/render_targets/D2DWindowRenderTarget.cpp
   graphics/d2d/render_targets/D2DWindowRenderTarget.h
   graphics/d2d/render_targets/D2DDCRenderTarget.cpp
   graphics/d2d/render_targets/D2DDCRenderTarget.h
   graphics/d2d/render_targets/D2DWICRenderTarget.cpp
   graphics/d2d/render_targets/D2DWICRenderTarget.h
)
set( LIBRARIES
   PUBLIC
      lib-graphics-interface
   PRIVATE
      lib-math-interface
      lib-screen-geometry-interface
      lib-track-interface
      wxwidgets::base
      wxwidgets::core
)
audacity_library( lib-graphics-d2d "${SOURCES}" "${LIBRARIES}"
   "" ""
)
