include(FetchContent)
set( TFT_VERSION 0.5 )
FetchContent_Declare(
   tft
   GIT_REPOSITORY https://github.com/kgramhans/tft.git
   GIT_TAG 1d96301673ebc05fa09a7bd505eacd82ee0fea33
)
FetchContent_MakeAvailable(tft)


