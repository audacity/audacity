/* WARNING! All changes made to this file will be lost! */

#ifndef W_LILV_CONFIG_H_WAF
#define W_LILV_CONFIG_H_WAF

/* #undef HAVE_GCOV */
#define HAVE_LV2 1
#define HAVE_SERD 1
#define HAVE_SORD 1
#define HAVE_SRATOM 1
#define HAVE_SNDFILE 1
#define HAVE_LSTAT 1
#define HAVE_FLOCK 1
#define HAVE_FILENO 1
#define HAVE_CLOCK_GETTIME 1
#define HAVE_LIBDL 1
#define LILV_VERSION "0.24.4"
#define LILV_PATH_SEP ":"
#define LILV_DIR_SEP "/"
#define LILV_DEFAULT_LV2_PATH "~/Library/Audio/Plug-Ins/LV2:~/.lv2:/usr/local/lib/lv2:/usr/lib/lv2:/Library/Audio/Plug-Ins/LV2"

#endif /* W_LILV_CONFIG_H_WAF */
