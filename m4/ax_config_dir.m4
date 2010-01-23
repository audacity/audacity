dnl Add Audacity license here?

dnl Function to configure a sub-library now, because we need to know the result
dnl of the configuration now in order to take decisions.
dnl We don't worry about whether the configuration worked or not - it is
dnl assumed that the next thing after this will be a package-specific check to
dnl see if the package is actually available. (Hint: use pkg-config and
dnl -uninstalled.pc files if available).
dnl code based on a simplification of _AC_OUTPUT_SUBDIRS in 
dnl /usr/share/autoconf/autoconf/status.m4 which implements part of 
dnl AC_CONFIG_SUBDIRS

AC_DEFUN([AX_CONFIG_DIR], [
  # Remove --cache-file and --srcdir arguments so they do not pile up.
  ax_sub_configure_args=
  ax_prev=
  eval "set x $ac_configure_args"
  shift
  for ax_arg
  do
    if test -n "$ax_prev"; then
      ax_prev=
      continue
    fi
    case $ax_arg in
    -cache-file | --cache-file | --cache-fil | --cache-fi \
    | --cache-f | --cache- | --cache | --cach | --cac | --ca | --c)
      ax_prev=cache_file ;;
    -cache-file=* | --cache-file=* | --cache-fil=* | --cache-fi=* \
    | --cache-f=* | --cache-=* | --cache=* | --cach=* | --cac=* | --ca=* \
    | --c=*)
      ;;
    --config-cache | -C)
      ;;
    -srcdir | --srcdir | --srcdi | --srcd | --src | --sr)
      ax_prev=srcdir ;;
    -srcdir=* | --srcdir=* | --srcdi=* | --srcd=* | --src=* | --sr=*)
      ;;
    -prefix | --prefix | --prefi | --pref | --pre | --pr | --p)
      ax_prev=prefix ;;
    -prefix=* | --prefix=* | --prefi=* | --pref=* | --pre=* | --pr=* | --p=*)
      ;;
    *)
      case $ax_arg in
      *\'*) ax_arg=`echo "$ax_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
      esac
      ax_sub_configure_args="$ax_sub_configure_args '$ax_arg'" ;;
    esac
  done

  # Always prepend --prefix to ensure using the same prefix
  # in subdir configurations.
  ax_arg="--prefix=$prefix"
  case $ax_arg in
  *\'*) ax_arg=`echo "$ax_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
  esac
  ax_sub_configure_args="'$ax_arg' $ax_sub_configure_args"

  # Pass --silent
  if test "$silent" = yes; then
    ax_sub_configure_args="--silent $ax_sub_configure_args"
  fi

  ax_popdir=`pwd`
  AC_MSG_NOTICE([Configuring sources in $1])
  dnl for out-of-place builds srcdir and builddir will be different, and
  dnl builddir may not exist, so we must create it
  AS_MKDIR_P(["$1"])
  dnl and also set the variables. As this isn't autoconf, the following may be
  dnl risky:
  _AC_SRCDIRS(["$1"])
  cd "$1"

  # Check for guested configure; otherwise get Cygnus style configure.
  if test -f "configure.gnu"; then
    ax_sub_configure=$ac_srcdir/configure.gnu
  elif test -f "$ac_srcdir/configure"; then
    ax_sub_configure=$ac_srcdir/configure
  elif test -f "$ac_srcdir/configure.in"; then
    # This should be Cygnus configure.
	ax_sub_configure=$ac_aux_dir/configure
  else
    AC_MSG_WARN([no configuration information is in $1])
    ax_sub_configure=
  fi

  # The recursion is here.
  if test -n "$ax_sub_configure"; then
    # Make the cache file name correct relative to the subdirectory.
    case $cache_file in
    [[\\/]]* | ?:[[\\/]]* ) ax_sub_cache_file=$cache_file ;;
    *) # Relative name.
	ax_sub_cache_file=$ac_top_build_prefix$cache_file ;;
    esac

    AC_MSG_NOTICE([running $SHELL $ax_sub_configure $ax_sub_configure_args --cache-file=$ax_sub_cache_file --srcdir=$ac_srcdir])
    # The eval makes quoting arguments work.
    eval "\$SHELL \"\$ax_sub_configure\" $ax_sub_configure_args \
	   --cache-file=\"\$ax_sub_cache_file\" --srcdir=\"\$ax_srcdir\""
  fi

  cd "$ax_popdir"
  AC_MSG_NOTICE([Done configuring in $1])
])

