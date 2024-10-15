#[[
Various versions of FFmpeg and a facade to hide their binary incompatibilities
]]

if (${_OPT}use_ffmpeg)
   set( SOURCES
      FFmpegTypes.h

      FFmpegFunctions.cpp
      FFmpegFunctions.h

      AVCodecFunctions.h
      AVCodecID.h
      AVFormatFunctions.h
      AVUtilFunctions.h

      FifoBuffer.cpp
      FifoBuffer.h

      wrappers/AVChannelLayoutWrapper.h
      wrappers/AVCodecContextWrapper.cpp
      wrappers/AVCodecContextWrapper.h
      wrappers/AVCodecWrapper.cpp
      wrappers/AVCodecWrapper.h
      wrappers/AVDictionaryWrapper.cpp
      wrappers/AVDictionaryWrapper.h
      wrappers/AVFormatContextWrapper.cpp
      wrappers/AVFormatContextWrapper.h
      wrappers/AVFrameWrapper.cpp
      wrappers/AVFrameWrapper.h
      wrappers/AVInputFormatWrapper.cpp
      wrappers/AVInputFormatWrapper.h
      wrappers/AVIOContextWrapper.cpp
      wrappers/AVIOContextWrapper.h
      wrappers/AVOutputFormatWrapper.cpp
      wrappers/AVOutputFormatWrapper.h
      wrappers/AVPacketWrapper.cpp
      wrappers/AVPacketWrapper.h
      wrappers/AVStreamWrapper.cpp
      wrappers/AVStreamWrapper.h

      impl/DynamicLibraryHelpers.cpp
      impl/DynamicLibraryHelpers.h

      impl/FFmpegAPIResolver.cpp
      impl/FFmpegAPIResolver.h

      impl/FFmpegLog.h

      impl/ffmpeg-2.3.6-single-header.h
      impl/avutil/52/avconfig.h
      impl/avcodec/55/AVCodecIDLookup.cpp
      impl/avcodec/55/AVCodecImpl.cpp
      impl/avformat/55/AVFormatImpl.cpp
      impl/avutil/52/AVUtilImpl.cpp

      impl/ffmpeg-3.4.8-single-header.h
      impl/avutil/55/avconfig.h
      impl/avcodec/57/AVCodecIDLookup.cpp
      impl/avcodec/57/AVCodecImpl.cpp
      impl/avformat/57/AVFormatImpl.cpp
      impl/avutil/55/AVUtilImpl.cpp

      impl/ffmpeg-4.2.4-single-header.h
      impl/avutil/56/avconfig.h
      impl/avcodec/58/AVCodecIDLookup.cpp
      impl/avcodec/58/AVCodecImpl.cpp
      impl/avformat/58/AVFormatImpl.cpp
      impl/avutil/56/AVUtilImpl.cpp

      impl/ffmpeg-5.0.1-single-header.h
      impl/avutil/57/avconfig.h
      impl/avcodec/59/AVCodecIDLookup.cpp
      impl/avcodec/59/AVCodecImpl.cpp
      impl/avformat/59/AVFormatImpl.cpp
      impl/avutil/57/AVUtilImpl.cpp

      impl/ffmpeg-6.0.0-single-header.h
      impl/avutil/58/avconfig.h
      impl/avcodec/60/AVCodecIDLookup.cpp
      impl/avcodec/60/AVCodecImpl.cpp
      impl/avformat/60/AVFormatImpl.cpp
      impl/avutil/58/AVUtilImpl.cpp

      impl/ffmpeg-7.0.0-single-header.h
      impl/avutil/59/avconfig.h
      impl/avcodec/61/AVCodecIDLookup.cpp
      impl/avcodec/61/AVCodecImpl.cpp
      impl/avformat/61/AVFormatImpl.cpp
      impl/avutil/59/AVUtilImpl.cpp

      # Loaders
      impl/avcodec/AVCodecFunctionsLoader.cpp
      impl/avcodec/AVCodecFunctionsLoader.h
      impl/avformat/AVFormatFunctionsLoader.cpp
      impl/avformat/AVFormatFunctionsLoader.h
      impl/avutil/AVUtilFunctionsLoader.cpp
      impl/avutil/AVUtilFunctionsLoader.h

      # not necessary here for the build, but they help browsing in an IDE:
      impl/AVCodecIDLookup.inl
      impl/avcodec/AVCodecContextWrapperImpl.inl
      impl/avcodec/AVCodecWrapperImpl.inl
      impl/avcodec/AVPacketWrapperImpl.inl
      impl/avformat/AVFormatContextWrapperImpl.inl
      impl/avformat/AVIOContextWrapperImpl.inl
      impl/avformat/AVInputFormatWrapperImpl.inl
      impl/avformat/AVOutputFormatWrapperImpl.inl
      impl/avformat/AVStreamWrapperImpl.inl
      impl/avutil/AVChannelLayoutWrapperImpl.inl
      impl/avutil/AVFrameWrapperImpl.inl
      impl/avutil/FFmpegLogImpl.inl
   )

   set( LIBRARIES
      lib-files
      lib-math
   )

   set( DEFINITIONS PUBLIC USE_FFMPEG )

   if(APPLE)
      # Required for RTLD_DEFAULT
      list(APPEND DEFINITIONS PRIVATE _DARWIN_C_SOURCE )
   endif()

   audacity_library( lib-ffmpeg-support "${SOURCES}" "${LIBRARIES}"
      "${DEFINITIONS}" ""
   )

endif()
