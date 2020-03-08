fmt='
#if !defined(REV_LONG)
#define REV_LONG "%H"
#endif
#if !defined(REV_TIME)
#define REV_TIME "%cd"
#endif
'
git show -s --format="${fmt}" | tee ../src/RevisionIdent.h
touch ../src/AboutDialog.cpp
