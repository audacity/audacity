
#ifndef QM_DSP_NAN_INF_H
#define QM_DSP_NAN_INF_H

#define ISNAN(x) (sizeof(x) == sizeof(double) ? ISNANd(x) : ISNANf(x))
static inline int ISNANf(float x) { return x != x; }
static inline int ISNANd(double x) { return x != x; }
          
#define ISINF(x) (sizeof(x) == sizeof(double) ? ISINFd(x) : ISINFf(x))
static inline int ISINFf(float x) { return !ISNANf(x) && ISNANf(x - x); }
static inline int ISINFd(double x) { return !ISNANd(x) && ISNANd(x - x); }

#endif
