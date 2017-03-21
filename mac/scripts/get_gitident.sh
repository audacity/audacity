git show -s --format="#define REV_LONG \"%H\"%n#define REV_TIME \"%cd\"%n" | tee ../src/RevisionIdent.h
touch ../src/AboutDialog.cpp
