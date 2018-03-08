/* SoX Resampler Library      Copyright (c) 2007-13 robs@users.sourceforge.net
 * Licence for this file: LGPL v2.1                  See LICENCE for details. */

typedef void (* fn_t)(void);

#define rdft_forward_setup    (*(void * (*)(int))RDFT_CB[0])
#define rdft_backward_setup   (*(void * (*)(int))RDFT_CB[1])
#define rdft_delete_setup     (*(void (*)(void *))RDFT_CB[2])
#define rdft_forward          (*(void (*)(int, void *, void *, void *))RDFT_CB[3])
#define rdft_oforward         (*(void (*)(int, void *, void *, void *))RDFT_CB[4])
#define rdft_backward         (*(void (*)(int, void *, void *, void *))RDFT_CB[5])
#define rdft_obackward        (*(void (*)(int, void *, void *, void *))RDFT_CB[6])
#define rdft_convolve         (*(void (*)(int, void *, void *, void const *))RDFT_CB[7])
#define rdft_convolve_portion (*(void (*)(int, void *, void const *))RDFT_CB[8])
#define rdft_multiplier       (*(int (*)(void))RDFT_CB[9])
#define rdft_reorder_back     (*(void (*)(int, void *, void *, void *))RDFT_CB[10])
#define rdft_malloc           (*(void * (*)(size_t))RDFT_CB[11])
#define rdft_calloc           (*(void * (*)(size_t, size_t))RDFT_CB[12])
#define rdft_free             (*(void (*)(void *))RDFT_CB[13])
#define rdft_flags            (*(int (*)(void))RDFT_CB[14])

/* Flag templates: */
#define RDFT_IS_SIMD       1
#define RDFT_NEEDS_SCRATCH 2
