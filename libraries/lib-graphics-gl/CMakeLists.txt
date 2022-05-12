#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   graphics/gl/Context.cpp
   graphics/gl/Context.h
   graphics/gl/Framebuffer.cpp
   graphics/gl/Framebuffer.h
   graphics/gl/GLFunctions.cpp
   graphics/gl/GLFunctions.h
   graphics/gl/GLRenderer.cpp
   graphics/gl/GLRenderer.h
   graphics/gl/Program.cpp
   graphics/gl/Program.h
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
