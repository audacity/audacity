/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLFunctions.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <cstddef>

namespace graphics::gl
{

#if defined(_WIN32)
#   define GLAPIENTRY __stdcall
#else
#   define GLAPIENTRY
#endif

using GLbitfield = uint32_t;
using GLuint = uint32_t;
using GLint = int32_t;
using GLsizei = int32_t;
using GLboolean = uint8_t;
using GLbyte = int8_t;
using GLshort = int16_t;
using GLubyte = uint8_t;
using GLushort = uint16_t;
using GLfloat = float;
using GLclampf = float;
using GLdouble = double;
using GLclampd = double;
using GLvoid = void;
using GLintptr = intptr_t;
using GLsizeiptr = ptrdiff_t;
using GLchar = char;
using GLuint64 = uint64_t;

struct __GLsync;
using GLsync = __GLsync*;

enum class GLenum : uint32_t
{
   INVALID = 0x0000,
   // Texture formats
   ALPHA = 0x1906,
   RGB = 0x1907,
   RGBA = 0x1908,
   LUMINANCE = 0x1909,
   LUMINANCE_ALPHA = 0x190A,
   BGRA = 0x80E1,

   ALPHA8 = 0x803C,
   LUMINANCE8 = 0x8040,
   LUMINANCE8_ALPHA8 = 0x8045,

   RGB8 = 0x8051,
   RGBA8 = 0x8058,

   RGBA4 = 0x8056,
   RGB5_A1 = 0x8057,
   RGB565 = 0x8D62,

   // Data formats
   BYTE = 0x1400,
   UNSIGNED_BYTE = 0x1401,
   SHORT = 0x1402,
   UNSIGNED_SHORT = 0x1403,
   INT = 0x1404,
   UNSIGNED_INT = 0x1405,
   FLOAT = 0x1406,
   HALF_FLOAT = 0x140B,
   UNSIGNED_INT_8_8_8_8_REV = 0x8367,
   RAW = 0x0000,

   // MISC
   NONE = 0x0000,
   ZERO = 0x0000,
   ONE = 0x0001,

   // Blend
   SRC_COLOR = 0x0300,
   ONE_MINUS_SRC_COLOR = 0x0301,
   SRC_ALPHA = 0x0302,
   ONE_MINUS_SRC_ALPHA = 0x0303,
   DST_ALPHA = 0x0304,
   ONE_MINUS_DST_ALPHA = 0x0305,
   DST_COLOR = 0x0306,
   ONE_MINUS_DST_COLOR = 0x0307,
   SRC_ALPHA_SATURATE = 0x0308,
   FUNC_ADD = 0x8006,
   BLEND_EQUATION = 0x8009,
   BLEND_EQUATION_RGB = 0x8009,
   BLEND_EQUATION_ALPHA = 0x883D,
   FUNC_SUBTRACT = 0x800A,
   FUNC_REVERSE_SUBTRACT = 0x800B,
   BLEND_DST_RGB = 0x80C8,
   BLEND_SRC_RGB = 0x80C9,
   BLEND_DST_ALPHA = 0x80CA,
   BLEND_SRC_ALPHA = 0x80CB,
   CONSTANT_COLOR = 0x8001,
   ONE_MINUS_CONSTANT_COLOR = 0x8002,
   CONSTANT_ALPHA = 0x8003,
   ONE_MINUS_CONSTANT_ALPHA = 0x8004,
   BLEND_COLOR = 0x8005,
   // Compare functions
   NEVER = 0x0200,
   LESS = 0x0201,
   EQUAL = 0x0202,
   LEQUAL = 0x0203,
   GREATER = 0x0204,
   NOTEQUAL = 0x0205,
   GEQUAL = 0x0206,
   ALWAYS = 0x0207,

   // Stencil
   KEEP = 0x1E00,
   REPLACE = 0x1E01,
   INCR = 0x1E02,
   DECR = 0x1E03,
   INVERT = 0x150A,
   INCR_WRAP = 0x8507,
   DECR_WRAP = 0x8508,

   // Strings
   VENDOR = 0x1F00,
   RENDERER = 0x1F01,
   VERSION = 0x1F02,
   EXTENSIONS = 0x1F03,

   // Texture filtering
   NEAREST = 0x2600,
   LINEAR = 0x2601,
   NEAREST_MIPMAP_NEAREST = 0x2700,
   LINEAR_MIPMAP_NEAREST = 0x2701,
   NEAREST_MIPMAP_LINEAR = 0x2702,
   LINEAR_MIPMAP_LINEAR = 0x2703,
   TEXTURE_MAG_FILTER = 0x2800,
   TEXTURE_MIN_FILTER = 0x2801,

   // Texture wrapping
   TEXTURE_WRAP_S = 0x2802,
   TEXTURE_WRAP_T = 0x2803,
   REPEAT = 0x2901,
   CLAMP_TO_EDGE = 0x812F,
   MIRRORED_REPEAT = 0x8370,

   // glClear
   DEPTH_BUFFER_BIT = 0x0100,
   STENCIL_BUFFER_BIT = 0x0400,
   COLOR_BUFFER_BIT = 0x4000,

   // Primitives
   POINTS = 0x0000,
   LINES = 0x0001,
   LINE_LOOP = 0x0002,
   LINE_STRIP = 0x0003,
   TRIANGLES = 0x0004,
   TRIANGLE_STRIP = 0x0005,
   TRIANGLE_FAN = 0x0006,
   LINES_ADJACENCY = 0x000A,
   LINE_STRIP_ADJACENCY = 0x000B,
   TRIANGLES_ADJACENCY = 0x000C,
   TRIANGLE_STRIP_ADJACENCY = 0x000D,
   PRIMITIVE_RESTART = 0x8F9D,

   // Buffers
   ARRAY_BUFFER = 0x8892,
   ELEMENT_ARRAY_BUFFER = 0x8893,
   ARRAY_BUFFER_BINDING = 0x8894,
   ELEMENT_ARRAY_BUFFER_BINDING = 0x8895,
   STREAM_DRAW = 0x88E0,
   STREAM_READ = 0x88E1,
   STATIC_DRAW = 0x88E4,
   DYNAMIC_DRAW = 0x88E8,
   BUFFER_SIZE = 0x8764,
   BUFFER_USAGE = 0x8765,
   CURRENT_VERTEX_ATTRIB = 0x8626,
   // Culling
   FRONT = 0x0404,
   BACK = 0x0405,
   FRONT_AND_BACK = 0x0408,
   CULL_FACE = 0x0B44,
   BLEND = 0x0BE2,
   DITHER = 0x0BD0,
   STENCIL_TEST = 0x0B90,
   DEPTH_TEST = 0x0B71,
   SCISSOR_TEST = 0x0C11,
   POLYGON_OFFSET_FILL = 0x8037,
   SAMPLE_ALPHA_TO_COVERAGE = 0x809E,
   SAMPLE_COVERAGE = 0x80A0,
   NO_ERROR = 0x0000,
   INVALID_ENUM = 0x0500,
   INVALID_VALUE = 0x0501,
   INVALID_OPERATION = 0x0502,
   OUT_OF_MEMORY = 0x0505,
   CW = 0x0900,
   CCW = 0x0901,

   // Shaders
   FRAGMENT_SHADER = 0x8B30,
   VERTEX_SHADER = 0x8B31,
   MAX_VERTEX_ATTRIBS = 0x8869,
   MAX_VERTEX_UNIFORM_VECTORS = 0x8DFB,
   MAX_VARYING_VECTORS = 0x8DFC,
   MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D,
   MAX_VERTEX_TEXTURE_IMAGE_UNITS = 0x8B4C,
   MAX_TEXTURE_IMAGE_UNITS = 0x8872,
   MAX_FRAGMENT_UNIFORM_VECTORS = 0x8DFD,
   SHADER_TYPE = 0x8B4F,
   DELETE_STATUS = 0x8B80,
   LINK_STATUS = 0x8B82,
   VALIDATE_STATUS = 0x8B83,
   ATTACHED_SHADERS = 0x8B85,
   ACTIVE_UNIFORMS = 0x8B86,
   ACTIVE_UNIFORM_MAX_LENGTH = 0x8B87,
   ACTIVE_ATTRIBUTES = 0x8B89,
   ACTIVE_ATTRIBUTE_MAX_LENGTH = 0x8B8A,
   SHADING_LANGUAGE_VERSION = 0x8B8C,
   CURRENT_PROGRAM = 0x8B8D,
   COMPILE_STATUS = 0x8B81,
   INFO_LOG_LENGTH = 0x8B84,
   SHADER_SOURCE_LENGTH = 0x8B88,
   SHADER_COMPILER = 0x8DFA,
   SHADER_BINARY_FORMATS = 0x8DF8,
   NUM_SHADER_BINARY_FORMATS = 0x8DF9,

   // Textures
   TEXTURE_2D = 0x0DE1,
   TEXTURE_RECTANGLE = 0x84F5,
   TEXTURE0 = 0x84C0,
   TEXTURE1 = 0x84C1,

   // Framebuffers
   FRAMEBUFFER = 0x8D40,
   RENDERBUFFER = 0x8D41,
   COLOR_ATTACHMENT0 = 0x8CE0,
   DEPTH_ATTACHMENT = 0x8D00,
   STENCIL_ATTACHMENT = 0x8D20,
   FRAMEBUFFER_COMPLETE = 0x8CD5,
   FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 0x8CD6,
   FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7,
   FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 0x8CD9,
   FRAMEBUFFER_UNSUPPORTED = 0x8CDD,
   INVALID_FRAMEBUFFER_OPERATION = 0x0506,
   READ_FRAMEBUFFER = 0x8CA8,
   DRAW_FRAMEBUFFER = 0x8CA9,

   UNIFORM_BUFFER = 0x8A11,

   // Fill mode
   POINT = 0x1B00,
   LINE = 0x1B01,
   FILL = 0x1B02,

   MULTISAMPLE = 0x809D,

   // Buffer mapping
   READ_ONLY = 0x88B8,
   WRITE_ONLY = 0x88B9,
   READ_WRITE = 0x88BA,

   MAP_READ_BIT = 0x0001,
   MAP_WRITE_BIT = 0x0002,
   MAP_INVALIDATE_RANGE_BIT = 0x0004,
   MAP_INVALIDATE_BUFFER_BIT = 0x0008,
   MAP_FLUSH_EXPLICIT_BIT = 0x0010,
   MAP_UNSYNCHRONIZED_BIT = 0x0020,

   INVALID_INDEX = 0xFFFFFFFFu,
   // glGet
   MAX_TEXTURE_SIZE = 0x0D33,
   MAX_DRAW_BUFFERS = 0x8824,
   TEXTURE_BINDING_2D = 0x8069,

   MAX_VERTEX_UNIFORM_COMPONENTS = 0x8B4A,
   MAX_FRAGMENT_UNIFORM_COMPONENTS = 0x8B49,

   // Pixel buffers
   UNPACK_ALIGNMENT = 0x0CF5,
   PACK_ALIGNMENT = 0x0D05,
   UNPACK_ROW_LENGTH = 0x0CF2,
   PIXEL_PACK_BUFFER = 0x88EB,
   PIXEL_UNPACK_BUFFER = 0x88EC,
   PIXEL_PACK_BUFFER_BINDING = 0x88ED,
   PIXEL_UNPACK_BUFFER_BINDING = 0x88EF,

   NUM_EXTENSIONS = 0x821D,

   R8 = 0x8229,
   RG8 = 0x822B,
   RED = 0x1903,
   RG = 0x8227,

   // Synchronization
   SYNC_FLUSH_COMMANDS_BIT = 0x0001,
   SYNC_GPU_COMMANDS_COMPLETE = 0x9117,
   MAX_SERVER_WAIT_TIMEOUT = 0x9111,

   ALREADY_SIGNALED = 0x911A,
   TIMEOUT_EXPIRED = 0x911B,
   CONDITION_SATISFIED = 0x911C,
   SYNC_WAIT_FAILED = 0x911D,

   // Debug
   DEBUG_OUTPUT_SYNCHRONOUS = 0x8242,
   DEBUG_OUTPUT = 0x92E0,
};

enum class GLDebugSource : uint32_t
{
   API = 0x8246,
   WINDOW_SYSTEM = 0x8247,
   SHADER_COMPILER = 0x8248,
   THIRD_PARTY = 0x8249,
   APPLICATION = 0x824A,
   OTHER = 0x824B,
};

enum class GLDebugType : uint32_t
{
   ERROR = 0x824C,
   DEPRECATED_BEHAVIOR = 0x824D,
   UNDEFINED_BEHAVIOR = 0x824E,
   PORTABILITY = 0x824F,
   PERFORMANCE = 0x8250,
   OTHER = 0x8251,
   MARKER = 0x8268,
   PUSH_GROUP = 0x8269,
   POP_GROUP = 0x826A,
};

enum class GLDebugSeverity : uint32_t
{
   HIGH = 0x9146,
   MEDIUM = 0x9147,
   LOW = 0x9148,
   NOTIFICATION = 0x826B,
};

constexpr GLuint64 TIMEOUT_IGNORED = 0xFFFFFFFFFFFFFFFFULL;

using pfnClear = void(GLAPIENTRY*)(GLbitfield mask);
using pfnClearColor = void(GLAPIENTRY*)(
   GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
using pfnClearDepthf = void(GLAPIENTRY*)(GLclampf depth);
using pfnClearStencil = void(GLAPIENTRY*)(GLint s);
using pfnColorMask = void(GLAPIENTRY*)(
   GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
using pfnCullFace = void(GLAPIENTRY*)(GLenum mode);
using pfnEnable = void(GLAPIENTRY*)(GLenum cap);
using pfnEnablei = void(GLAPIENTRY*)(GLenum cap, GLuint index);
using pfnDisable = void(GLAPIENTRY*)(GLenum cap);
using pfnDisablei = void(GLAPIENTRY*)(GLenum cap, GLuint index);
using pfnFinish = void(GLAPIENTRY*)(void);
using pfnFlush = void(GLAPIENTRY*)(void);
using pfnFrontFace = void(GLAPIENTRY*)(GLenum mode);
using pfnGetError = GLenum(GLAPIENTRY*)(void);
using pfnGetFloatv = void(GLAPIENTRY*)(GLenum pname, GLfloat* params);
using pfnGetIntegerv = void(GLAPIENTRY*)(GLenum pname, GLint* params);
using pfnGetBooleanv = void(GLAPIENTRY*)(GLenum pname, GLboolean* params);
using pfnGetString = const GLubyte*(GLAPIENTRY*)(GLenum name);
using pfnGetStringi = const GLubyte*(GLAPIENTRY*)(GLenum name, GLuint index);
using pfnHint = void(GLAPIENTRY*)(GLenum target, GLenum mode);
using pfnPixelStorei = void(GLAPIENTRY*)(GLenum pname, GLint param);
using pfnPolygonOffset = void(GLAPIENTRY*)(GLfloat factor, GLfloat units);
using pfnReadPixels = void(GLAPIENTRY*)(
   GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
   GLvoid* pixels);
using pfnScissor =
   void(GLAPIENTRY*)(GLint x, GLint y, GLsizei width, GLsizei height);
using pfnViewport =
   void(GLAPIENTRY*)(GLint x, GLint y, GLsizei width, GLsizei height);
using pfnDepthFunc = void(GLAPIENTRY*)(GLenum func);
using pfnDepthMask = void(GLAPIENTRY*)(GLboolean flag);
using pfnDepthRangef = void(GLAPIENTRY*)(GLclampf zNear, GLclampf zFar);
using pfnStencilFunc = void(GLAPIENTRY*)(GLenum func, GLint ref, GLuint mask);
using pfnStencilFuncSeparate =
   void(GLAPIENTRY*)(GLenum face, GLenum func, GLint ref, GLuint mask);
using pfnStencilMask = void(GLAPIENTRY*)(GLuint mask);
using pfnStencilMaskSeparate = void(GLAPIENTRY*)(GLenum face, GLuint mask);
using pfnStencilOp = void(GLAPIENTRY*)(GLenum fail, GLenum zfail, GLenum zpass);
using pfnStencilOpSeparate =
   void(GLAPIENTRY*)(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);
using pfnActiveTexture = void(GLAPIENTRY*)(GLenum texture);
using pfnGenTextures = void(GLAPIENTRY*)(GLsizei n, GLuint* textures);
using pfnBindTexture = void(GLAPIENTRY*)(GLenum target, GLuint texture);
using pfnCompressedTexImage2D = void(GLAPIENTRY*)(
   GLenum target, GLint level, GLenum internalformat, GLsizei width,
   GLsizei height, GLint border, GLsizei imageSize, const GLvoid* data);
using pfnCompressedTexSubImage2D = void(GLAPIENTRY*)(
   GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
   GLsizei height, GLenum format, GLsizei imageSize, const GLvoid* data);
using pfnDeleteTextures = void(GLAPIENTRY*)(GLsizei n, const GLuint* textures);
using pfnGenerateMipmap = void(GLAPIENTRY*)(GLenum target);
using pfnIsTexture = GLboolean(GLAPIENTRY*)(GLuint texture);
using pfnTexParameterf =
   void(GLAPIENTRY*)(GLenum target, GLenum pname, GLfloat param);
using pfnTexParameterfv =
   void(GLAPIENTRY*)(GLenum target, GLenum pname, const GLfloat* params);
using pfnTexParameteri =
   void(GLAPIENTRY*)(GLenum target, GLenum pname, GLint param);
using pfnTexParameteriv =
   void(GLAPIENTRY*)(GLenum target, GLenum pname, const GLint* params);
using pfnTexImage2D = void(GLAPIENTRY*)(
   GLenum target, GLint level, GLenum internalformat, GLsizei width,
   GLsizei height, GLint border, GLenum format, GLenum type,
   const GLvoid* pixels);
using pfnTexSubImage2D = void(GLAPIENTRY*)(
   GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
   GLsizei height, GLenum format, GLenum type, const GLvoid* pixels);
using pfnEnableVertexAttribArray = void(GLAPIENTRY*)(GLuint index);
using pfnDisableVertexAttribArray = void(GLAPIENTRY*)(GLuint index);
using pfnVertexAttribPointer = void(GLAPIENTRY*)(
   GLuint indx, GLint size, GLenum type, GLboolean normalized, GLsizei stride,
   const GLvoid* ptr);
using pfnDrawArrays =
   void(GLAPIENTRY*)(GLenum mode, GLint first, GLsizei count);
using pfnDrawElements = void(GLAPIENTRY*)(
   GLenum mode, GLsizei count, GLenum type, const GLvoid* indices);
using pfnBlendColor = void(GLAPIENTRY*)(
   GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
using pfnBlendEquation = void(GLAPIENTRY*)(GLenum mode);
using pfnBlendEquationSeparate =
   void(GLAPIENTRY*)(GLenum modeRGB, GLenum modeAlpha);
using pfnBlendFunc = void(GLAPIENTRY*)(GLenum sfactor, GLenum dfactor);
using pfnBlendFuncSeparate = void(GLAPIENTRY*)(
   GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha);
using pfnGenBuffers = void(GLAPIENTRY*)(GLsizei n, GLuint* buffers);
using pfnBindBuffer = void(GLAPIENTRY*)(GLenum target, GLuint buffer);
using pfnBufferData = void(GLAPIENTRY*)(
   GLenum target, GLsizeiptr size, const GLvoid* data, GLenum usage);
using pfnBufferSubData = void(GLAPIENTRY*)(
   GLenum target, GLintptr offset, GLsizeiptr size, const GLvoid* data);
using pfnDeleteBuffers = void(GLAPIENTRY*)(GLsizei n, const GLuint* buffers);
using pfnIsBuffer = GLboolean(GLAPIENTRY*)(GLuint buffer);
using pfnMapBufferRange = GLvoid*(
   GLAPIENTRY*)(GLenum target, GLintptr offset, GLsizeiptr length, GLbitfield access);
using pfnMapBuffer = void*(GLAPIENTRY*)(GLenum target, GLenum access);
using pfnUnmapBuffer = GLboolean(GLAPIENTRY*)(GLenum target);
using pfnFlushMappedBufferRange =
   void(GLAPIENTRY*)(GLenum target, GLintptr offset, GLsizeiptr length);
using pfnGenFramebuffers = void(GLAPIENTRY*)(GLsizei n, GLuint* framebuffers);
using pfnGenRenderbuffers = void(GLAPIENTRY*)(GLsizei n, GLuint* renderbuffers);
using pfnBindFramebuffer = void(GLAPIENTRY*)(GLenum target, GLuint framebuffer);
using pfnBindRenderbuffer =
   void(GLAPIENTRY*)(GLenum target, GLuint renderbuffer);
using pfnCheckFramebufferStatus = GLenum(GLAPIENTRY*)(GLenum target);
using pfnDeleteFramebuffers =
   void(GLAPIENTRY*)(GLsizei n, const GLuint* framebuffers);
using pfnDeleteRenderbuffers =
   void(GLAPIENTRY*)(GLsizei n, const GLuint* renderbuffers);
using pfnFramebufferRenderbuffer = void(GLAPIENTRY*)(
   GLenum target, GLenum attachment, GLenum renderbuffertarget,
   GLuint renderbuffer);
using pfnFramebufferTexture2D = void(GLAPIENTRY*)(
   GLenum target, GLenum attachment, GLenum textarget, GLuint texture,
   GLint level);
using pfnIsFramebuffer = GLboolean(GLAPIENTRY*)(GLuint framebuffer);
using pfnIsRenderbuffer = GLboolean(GLAPIENTRY*)(GLuint renderbuffer);
using pfnRenderbufferStorage = void(GLAPIENTRY*)(
   GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
using pfnRenderbufferStorageMultisample = void(GLAPIENTRY*)(
   GLenum target, GLsizei samples, GLenum internalformat, GLsizei width,
   GLsizei height);
using pfnBindVertexArray = void(GLAPIENTRY*)(GLuint array);
using pfnDeleteVertexArrays =
   void(GLAPIENTRY*)(GLsizei n, const GLuint* arrays);
using pfnGenVertexArrays = void(GLAPIENTRY*)(GLsizei n, GLuint* arrays);
using pfnIsVertexArray = GLboolean(GLAPIENTRY*)(GLuint array);
using pfnGenSamplers = void(GLAPIENTRY*)(GLsizei count, GLuint* samplers);
using pfnDeleteSamplers =
   void(GLAPIENTRY*)(GLsizei count, const GLuint* samplers);
using pfnIsSampler = GLboolean(GLAPIENTRY*)(GLuint sampler);
using pfnBindSampler = void(GLAPIENTRY*)(GLuint unit, GLuint sampler);
using pfnSamplerParameteri =
   void(GLAPIENTRY*)(GLuint sampler, GLenum pname, GLint param);
using pfnSamplerParameteriv =
   void(GLAPIENTRY*)(GLuint sampler, GLenum pname, const GLint* param);
using pfnSamplerParameterf =
   void(GLAPIENTRY*)(GLuint sampler, GLenum pname, GLfloat param);
using pfnSamplerParameterfv =
   void(GLAPIENTRY*)(GLuint sampler, GLenum pname, const GLfloat* param);
using pfnAttachShader = void(GLAPIENTRY*)(GLuint program, GLuint shader);
using pfnBindAttribLocation =
   void(GLAPIENTRY*)(GLuint program, GLuint index, const GLchar* name);
using pfnCompileShader = void(GLAPIENTRY*)(GLuint shader);
using pfnCreateProgram = GLuint(GLAPIENTRY*)(void);
using pfnCreateShader = GLuint(GLAPIENTRY*)(GLenum type);
using pfnDeleteProgram = void(GLAPIENTRY*)(GLuint program);
using pfnDeleteShader = void(GLAPIENTRY*)(GLuint shader);
using pfnDetachShader = void(GLAPIENTRY*)(GLuint program, GLuint shader);
using pfnGetAttribLocation =
   GLint(GLAPIENTRY*)(GLuint program, const GLchar* name);
using pfnGetProgramiv =
   void(GLAPIENTRY*)(GLuint program, GLenum pname, GLint* params);
using pfnGetProgramInfoLog = void(GLAPIENTRY*)(
   GLuint program, GLsizei bufsize, GLsizei* length, GLchar* infolog);
using pfnGetShaderiv =
   void(GLAPIENTRY*)(GLuint shader, GLenum pname, GLint* params);
using pfnGetShaderInfoLog = void(GLAPIENTRY*)(
   GLuint shader, GLsizei bufsize, GLsizei* length, GLchar* infolog);
using pfnGetUniformLocation =
   GLint(GLAPIENTRY*)(GLuint program, const GLchar* name);
using pfnIsProgram = GLboolean(GLAPIENTRY*)(GLuint program);
using pfnIsShader = GLboolean(GLAPIENTRY*)(GLuint shader);
using pfnLinkProgram = void(GLAPIENTRY*)(GLuint program);
using pfnShaderBinary = void(GLAPIENTRY*)(
   GLsizei n, const GLuint* shaders, GLenum binaryformat, const GLvoid* binary,
   GLsizei length);
using pfnGetShaderSource = void(GLAPIENTRY*)(GLuint shader, GLsizei bufSize, GLsizei* length, GLchar* source);
using pfnShaderSource = void(GLAPIENTRY*)(GLuint shader, GLsizei count, const GLchar** string, const GLint* length);
using pfnUniform1f = void(GLAPIENTRY*)(GLint location, GLfloat x);
using pfnUniform1fv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLfloat* v);
using pfnUniform1i = void(GLAPIENTRY*)(GLint location, GLint x);
using pfnUniform1iv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLint* v);
using pfnUniform2f = void(GLAPIENTRY*)(GLint location, GLfloat x, GLfloat y);
using pfnUniform2fv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLfloat* v);
using pfnUniform2i = void(GLAPIENTRY*)(GLint location, GLint x, GLint y);
using pfnUniform2iv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLint* v);
using pfnUniform3f = void(GLAPIENTRY*)(GLint location, GLfloat x, GLfloat y, GLfloat z);
using pfnUniform3fv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLfloat* v);
using pfnUniform3i = void(GLAPIENTRY*)(GLint location, GLint x, GLint y, GLint z);
using pfnUniform3iv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLint* v);
using pfnUniform4f = void(GLAPIENTRY*)(
   GLint location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
using pfnUniform4fv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLfloat* v);
using pfnUniform4i = void(GLAPIENTRY*)(GLint location, GLint x, GLint y, GLint z, GLint w);
using pfnUniform4iv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLint* v);
using pfnUniformMatrix2fv = void(GLAPIENTRY*)(
   GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix3fv = void(GLAPIENTRY*)(
   GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix4fv = void(GLAPIENTRY*)(
   GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUseProgram = void(GLAPIENTRY*)(GLuint program);
using pfnValidateProgram = void(GLAPIENTRY*)(GLuint program);
using pfnUniform1ui = void(GLAPIENTRY*)(GLint location, GLuint v0);
using pfnUniform2ui = void(GLAPIENTRY*)(GLint location, GLuint v0, GLuint v1);
using pfnUniform3ui = void(GLAPIENTRY*)(GLint location, GLuint v0, GLuint v1, GLuint v2);
using pfnUniform4ui = void(GLAPIENTRY*)(
   GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3);
using pfnUniform1uiv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLuint* value);
using pfnUniform2uiv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLuint* value);
using pfnUniform3uiv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLuint* value);
using pfnUniform4uiv = void(GLAPIENTRY*)(GLint location, GLsizei count, const GLuint* value);
using pfnGetUniformBlockIndex = GLuint(GLAPIENTRY*)(GLuint program, const GLchar* uniformBlockName);
using pfnUniformBlockBinding = void(GLAPIENTRY*)(GLuint program, GLuint uniformBlockIndex, GLuint uniformBlockBinding);
using pfnBindBufferBase = void(GLAPIENTRY*)(GLenum target, GLuint index, GLuint buffer);
using pfnGetTexImage = void(GLAPIENTRY*)(GLenum target, GLint level, GLenum format, GLenum type, GLvoid* pixels);
using pfnDiscardFramebuffer = void(GLAPIENTRY*)(GLenum target, GLsizei numAttachments, const GLenum* attachments);
using pfnBlitFramebuffer = void(GLAPIENTRY*)(
   GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0,
   GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter);
using pfnResolveMultisampleFramebufferAPPLE = void(GLAPIENTRY*)();
using pfnUniformMatrix2x3fv = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix3x2fv = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix2x4fv = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix4x2fv = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix3x4fv = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);
using pfnUniformMatrix4x3fv = void(GLAPIENTRY*)(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value);

using pfnClientWaitSync = GLenum(GLAPIENTRY*)(GLsync sync, GLbitfield flags, GLuint64 timeout);
using pfnFenceSync = GLsync(GLAPIENTRY*)(GLenum condition, GLbitfield flags);
using pfnDeleteSync = void(GLAPIENTRY*)(GLsync sync);
using pfnWaitSync = GLenum(GLAPIENTRY*)(GLsync sync, GLbitfield flags, GLuint64 timeout);

using pfnPrimitiveRestartIndex = void(GLAPIENTRY*)(GLuint index);

struct GLFunctions /* not final */
{
   virtual ~GLFunctions() = default;

   pfnClear Clear { nullptr };
   pfnClearColor ClearColor { nullptr };
   pfnClearDepthf ClearDepthf { nullptr };
   pfnClearStencil ClearStencil { nullptr };
   pfnColorMask ColorMask { nullptr };
   pfnCullFace CullFace { nullptr };
   pfnEnable Enable { nullptr };
   pfnEnablei Enablei { nullptr };
   pfnDisable Disable { nullptr };
   pfnDisablei Disablei { nullptr };
   pfnFinish Finish { nullptr };
   pfnFlush Flush { nullptr };
   pfnFrontFace FrontFace { nullptr };
   pfnGetError GetError { nullptr };
   pfnGetFloatv GetFloatv { nullptr };
   pfnGetIntegerv GetIntegerv { nullptr };
   pfnGetBooleanv GetBooleanv { nullptr };
   pfnGetString GetString { nullptr };
   pfnGetStringi GetStringi { nullptr };
   pfnHint Hint { nullptr };
   pfnPixelStorei PixelStorei { nullptr };
   pfnPolygonOffset PolygonOffset { nullptr };
   pfnReadPixels ReadPixels { nullptr };
   pfnScissor Scissor { nullptr };
   pfnViewport Viewport { nullptr };
   pfnDepthFunc DepthFunc { nullptr };
   pfnDepthMask DepthMask { nullptr };
   pfnDepthRangef DepthRangef { nullptr };
   pfnStencilFunc StencilFunc { nullptr };
   pfnStencilFuncSeparate StencilFuncSeparate { nullptr };
   pfnStencilMask StencilMask { nullptr };
   pfnStencilMaskSeparate StencilMaskSeparate { nullptr };
   pfnStencilOp StencilOp { nullptr };
   pfnStencilOpSeparate StencilOpSeparate { nullptr };
   pfnActiveTexture ActiveTexture { nullptr };
   pfnGenTextures GenTextures { nullptr };
   pfnBindTexture BindTexture { nullptr };
   pfnCompressedTexImage2D CompressedTexImage2D { nullptr };
   pfnCompressedTexSubImage2D CompressedTexSubImage2D { nullptr };
   pfnDeleteTextures DeleteTextures { nullptr };
   pfnGenerateMipmap GenerateMipmap { nullptr };
   pfnIsTexture IsTexture { nullptr };
   pfnTexParameterf TexParameterf { nullptr };
   pfnTexParameterfv TexParameterfv { nullptr };
   pfnTexParameteri TexParameteri { nullptr };
   pfnTexParameteriv TexParameteriv { nullptr };
   pfnTexImage2D TexImage2D { nullptr };
   pfnTexSubImage2D TexSubImage2D { nullptr };
   pfnEnableVertexAttribArray EnableVertexAttribArray { nullptr };
   pfnDisableVertexAttribArray DisableVertexAttribArray { nullptr };
   pfnVertexAttribPointer VertexAttribPointer { nullptr };
   pfnDrawArrays DrawArrays { nullptr };
   pfnDrawElements DrawElements { nullptr };
   pfnBlendColor BlendColor { nullptr };
   pfnBlendEquation BlendEquation { nullptr };
   pfnBlendEquationSeparate BlendEquationSeparate { nullptr };
   pfnBlendFunc BlendFunc { nullptr };
   pfnBlendFuncSeparate BlendFuncSeparate { nullptr };
   pfnGenBuffers GenBuffers { nullptr };
   pfnBindBuffer BindBuffer { nullptr };
   pfnBufferData BufferData { nullptr };
   pfnBufferSubData BufferSubData { nullptr };
   pfnDeleteBuffers DeleteBuffers { nullptr };
   pfnIsBuffer IsBuffer { nullptr };
   pfnMapBufferRange MapBufferRange { nullptr };
   pfnMapBuffer MapBuffer { nullptr };
   pfnUnmapBuffer UnmapBuffer { nullptr };
   pfnFlushMappedBufferRange FlushMappedBufferRange { nullptr };
   pfnGenFramebuffers GenFramebuffers { nullptr };
   pfnGenRenderbuffers GenRenderbuffers { nullptr };
   pfnBindFramebuffer BindFramebuffer { nullptr };
   pfnBindRenderbuffer BindRenderbuffer { nullptr };
   pfnCheckFramebufferStatus CheckFramebufferStatus { nullptr };
   pfnDeleteFramebuffers DeleteFramebuffers { nullptr };
   pfnDeleteRenderbuffers DeleteRenderbuffers { nullptr };
   pfnFramebufferRenderbuffer FramebufferRenderbuffer { nullptr };
   pfnFramebufferTexture2D FramebufferTexture2D { nullptr };
   pfnIsFramebuffer IsFramebuffer { nullptr };
   pfnIsRenderbuffer IsRenderbuffer { nullptr };
   pfnRenderbufferStorage RenderbufferStorage { nullptr };
   pfnRenderbufferStorageMultisample RenderbufferStorageMultisample { nullptr };
   pfnBindVertexArray BindVertexArray { nullptr };
   pfnDeleteVertexArrays DeleteVertexArrays { nullptr };
   pfnGenVertexArrays GenVertexArrays { nullptr };
   pfnIsVertexArray IsVertexArray { nullptr };
   pfnGenSamplers GenSamplers { nullptr };
   pfnDeleteSamplers DeleteSamplers { nullptr };
   pfnIsSampler IsSampler { nullptr };
   pfnBindSampler BindSampler { nullptr };
   pfnSamplerParameteri SamplerParameteri { nullptr };
   pfnSamplerParameteriv SamplerParameteriv { nullptr };
   pfnSamplerParameterf SamplerParameterf { nullptr };
   pfnSamplerParameterfv SamplerParameterfv { nullptr };
   pfnAttachShader AttachShader { nullptr };
   pfnBindAttribLocation BindAttribLocation { nullptr };
   pfnCompileShader CompileShader { nullptr };
   pfnCreateProgram CreateProgram { nullptr };
   pfnCreateShader CreateShader { nullptr };
   pfnDeleteProgram DeleteProgram { nullptr };
   pfnDeleteShader DeleteShader { nullptr };
   pfnDetachShader DetachShader { nullptr };
   pfnGetAttribLocation GetAttribLocation { nullptr };
   pfnGetProgramiv GetProgramiv { nullptr };
   pfnGetProgramInfoLog GetProgramInfoLog { nullptr };
   pfnGetShaderiv GetShaderiv { nullptr };
   pfnGetShaderInfoLog GetShaderInfoLog { nullptr };
   pfnGetUniformLocation GetUniformLocation { nullptr };
   pfnIsProgram IsProgram { nullptr };
   pfnIsShader IsShader { nullptr };
   pfnLinkProgram LinkProgram { nullptr };
   pfnShaderBinary ShaderBinary { nullptr };
   pfnGetShaderSource GetShaderSource { nullptr };
   pfnShaderSource ShaderSource { nullptr };
   pfnUniform1f Uniform1f { nullptr };
   pfnUniform1fv Uniform1fv { nullptr };
   pfnUniform1i Uniform1i { nullptr };
   pfnUniform1iv Uniform1iv { nullptr };
   pfnUniform2f Uniform2f { nullptr };
   pfnUniform2fv Uniform2fv { nullptr };
   pfnUniform2i Uniform2i { nullptr };
   pfnUniform2iv Uniform2iv { nullptr };
   pfnUniform3f Uniform3f { nullptr };
   pfnUniform3fv Uniform3fv { nullptr };
   pfnUniform3i Uniform3i { nullptr };
   pfnUniform3iv Uniform3iv { nullptr };
   pfnUniform4f Uniform4f { nullptr };
   pfnUniform4fv Uniform4fv { nullptr };
   pfnUniform4i Uniform4i { nullptr };
   pfnUniform4iv Uniform4iv { nullptr };
   pfnUniformMatrix2fv UniformMatrix2fv { nullptr };
   pfnUniformMatrix3fv UniformMatrix3fv { nullptr };
   pfnUniformMatrix4fv UniformMatrix4fv { nullptr };
   pfnUseProgram UseProgram { nullptr };
   pfnValidateProgram ValidateProgram { nullptr };
   pfnUniform1ui Uniform1ui { nullptr };
   pfnUniform2ui Uniform2ui { nullptr };
   pfnUniform3ui Uniform3ui { nullptr };
   pfnUniform4ui Uniform4ui { nullptr };
   pfnUniform1uiv Uniform1uiv { nullptr };
   pfnUniform2uiv Uniform2uiv { nullptr };
   pfnUniform3uiv Uniform3uiv { nullptr };
   pfnUniform4uiv Uniform4uiv { nullptr };
   pfnGetUniformBlockIndex GetUniformBlockIndex { nullptr };
   pfnUniformBlockBinding UniformBlockBinding { nullptr };
   pfnBindBufferBase BindBufferBase { nullptr };
   pfnGetTexImage GetTexImage { nullptr };
   pfnDiscardFramebuffer DiscardFramebuffer { nullptr };
   pfnBlitFramebuffer BlitFramebuffer { nullptr };
   pfnResolveMultisampleFramebufferAPPLE ResolveMultisampleFramebufferAPPLE { nullptr };
   pfnUniformMatrix2x3fv UniformMatrix2x3fv { nullptr };
   pfnUniformMatrix3x2fv UniformMatrix3x2fv { nullptr };
   pfnUniformMatrix2x4fv UniformMatrix2x4fv { nullptr };
   pfnUniformMatrix4x2fv UniformMatrix4x2fv { nullptr };
   pfnUniformMatrix3x4fv UniformMatrix3x4fv { nullptr };
   pfnUniformMatrix4x3fv UniformMatrix4x3fv { nullptr };

   pfnFenceSync FenceSync { nullptr };
   pfnDeleteSync DeleteSync { nullptr };
   pfnClientWaitSync ClientWaitSync { nullptr };
   pfnWaitSync WaitSync { nullptr };

   pfnPrimitiveRestartIndex PrimitiveRestartIndex { nullptr };

protected:
   virtual void* GetFunctionPointer(const char* name) const = 0;

   template<typename Fn>
   bool GetFunction(Fn& fn, const char* name, bool required)
   {
      fn = reinterpret_cast<Fn>(GetFunctionPointer(name));
      return fn != nullptr || !required;
   }

   bool LoadFunctions();
};
}
