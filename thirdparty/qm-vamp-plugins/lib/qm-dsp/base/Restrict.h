/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#ifndef QM_DSP_RESTRICT_H
#define QM_DSP_RESTRICT_H

#ifdef _MSC_VER
#define QM_R__ __restrict
#endif

#ifdef __GNUC__
#define QM_R__ __restrict__
#endif

#ifndef QM_R__
#define QM_R__
#endif

#endif
