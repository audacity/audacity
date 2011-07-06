#ifndef TRACKPOINT_H
#define TRACKPOINT_H

#include "grain.h"
#include "sbsms.h"

namespace _sbsms_ {

class track;

class tpoint {
 public:
  tpoint() {}
  virtual ~tpoint() {}
  real f;
  real y;
  real ph;
  short h;
  short M;
  real y0;
};

class trackpoint : public tpoint {
 public:
  trackpoint();
  trackpoint(trackpoint *tp);
  trackpoint(grain *g, real x, real y, int N, short M, short h, long time);
  ~trackpoint();

  track *owner;
  long time;

  real contF;
  bool bConnect;
  bool bConnected;
  bool bDelete;

  trackpoint *dupcont;
  trackpoint *cont;
  trackpoint *dup[3];

 protected:
  void init();
};

}

#endif
