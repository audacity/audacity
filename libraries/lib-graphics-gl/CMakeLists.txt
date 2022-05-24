#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   graphics/gl/Context.cpp
   graphics/gl/Context.h
   graphics/gl/Framebuffer.cpp
   graphics/gl/Framebuffer.h
   graphics/gl/GLFontRenderer.cpp
   graphics/gl/GLFontRenderer.h
   graphics/gl/GLFunctions.cpp
   graphics/gl/GLFunctions.h
   graphics/gl/GLPainter.cpp
   graphics/gl/GLPainter.h
   graphics/gl/GLRenderer.cpp
   graphics/gl/GLRenderer.h
   graphics/gl/PaintTarget.cpp
   graphics/gl/PaintTarget.h
   graphics/gl/PaintTargetsStack.cpp
   graphics/gl/PaintTargetsStack.h
   graphics/gl/Path.cpp
   graphics/gl/Path.h
   graphics/gl/Program.cpp
   graphics/gl/Program.h
   graphics/gl/ProgramLibrary.cpp
   graphics/gl/ProgramLibrary.h
   graphics/gl/StrokeGenerator.cpp
   graphics/gl/StrokeGenerator.h
   graphics/gl/Texture.cpp
   graphics/gl/Texture.h
   graphics/gl/VertexArray.cpp
   graphics/gl/VertexArray.h
   graphics/gl/VertexBuffer.cpp
   graphics/gl/VertexBuffer.h

   $<$<PLATFORM_ID:Windows>:graphics/gl/platforms/windows/WGLRenderer.cpp>
   $<$<PLATFORM_ID:Windows>:graphics/gl/platforms/windows/WGLRenderer.h>
)
set( LIBRARIES
   PUBLIC
      lib-graphics-interface
   PRIVATE
      lib-graphics-fonts
      lib-math-interface
      lib-screen-geometry-interface
      lib-track-interface
      lib-utility-interface
      wxwidgets::base
      wxwidgets::core
)
audacity_library( lib-graphics-gl "${SOURCES}" "${LIBRARIES}"
   "" ""
)
