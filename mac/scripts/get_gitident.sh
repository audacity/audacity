git show -s --format="wxT(\"[[http://github.com/audacity/audacity/commit/%H|%h]] of %cd\")" | tee ../src/RevisionIdent.h
touch ../src/AboutDialog.cpp
