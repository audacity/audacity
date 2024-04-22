FROM library/ubuntu:16.04
# For maximum AppImage compatibility, build on the oldest Linux distribution
# that still receives security updates from its manufacturer.
COPY buildscripts/packaging/Linux+BSD/portable/Recipe /Recipe
RUN bash -ex Recipe
# Note that each additional ADD, RUN or COPY command creates a new "layer"
# (i.e. an intermediate Docker image). Any clean-up steps after a RUN command
# must be done as part of that same command, like this:
#
#   RUN command \
#     && clean-up-command   # creates single clean layer
#
# Or if `command` is a script, clean-up could be done as part of that script.
#
# Avoid doing this:
#
#   RUN command             # creates dirty layer
#   RUN clean-up-command    # creates clean layer
#
# This is like reverting a bad commit: you can't see the dirtiness any more,
# but it's still in the history and still takes up space on your hard drive.
