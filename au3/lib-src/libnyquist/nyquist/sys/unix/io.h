#define NOCHAR -2

int IOinputfd;
int IOnochar;

int IOsetup(int inputfd);
int IOcleanup(void);
int IOgetchar(void);
int IOwaitchar(void);
