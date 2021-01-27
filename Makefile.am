include guile.am

SOURCES =                           \
  orgfile/blocks.scm                \
  orgfile/common.scm                \
  orgfile/entities.scm              \
  orgfile/node.scm                  \
  orgfile/parser.scm                \
  orgfile/sxml.scm                  \
  orgfile/utils.scm                 \
  orgfile.scm

SCM_LOG_DRIVER =                               \
  $(top_builddir)/pre-inst-env                 \
  $(GUILE) --no-auto-compile -e main           \
      $(top_srcdir)/build-aux/test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

EXTRA_DIST = README.md                        \
             pre-inst-env.in

