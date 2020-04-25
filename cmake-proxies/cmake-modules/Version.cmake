# Executed during build (NOT configuration) to create/update the
# RevisionIdent.h header.  It will only update it if there was
# a change in git.

execute_process(
   COMMAND
      ${GIT} show -s "--format=#define REV_LONG \"%H\"%n#define REV_TIME \"%cd\"%n"
   OUTPUT_FILE
      ${_PRVDIR}/RevisionIdent.h.in
   OUTPUT_STRIP_TRAILING_WHITESPACE
   ERROR_QUIET
)

configure_file( ${_PRVDIR}/RevisionIdent.h.in ${_PRVDIR}/RevisionIdent.h COPYONLY )

