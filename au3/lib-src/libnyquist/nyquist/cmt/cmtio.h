#define NOCHAR -2

extern int IOinputfd;
extern int IOnochar;

int IOsetup(int inputfd);
int IOcleanup(void);
int IOgetchar(void);
int IOwaitchar(void);
