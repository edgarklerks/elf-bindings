#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])
AC_INIT([FULL-PACKAGE-NAME], [VERSION], [BUG-REPORT-ADDRESS])

AC_CONFIG_SRCDIR([elf-bindings.cabal])
# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([elf.h],
	[AC_DEFINE(_HAVE_LIB_ELF, 1, [If defined we don't need the packaged lib elf])], [AC_DEFINE(_HAVE_LIB_ELF, 0, [Using packaged elf file])])
# Checks for typedefs, structures, anid compiler characteristics.

# Generates the hsc and c files needed for the binding 
AC_CONFIG_COMMANDS([GENERATED_ELF_FILES],[], [./buildtools/regenerate_constants.sh])

# Checks for library functions.

AC_OUTPUT
