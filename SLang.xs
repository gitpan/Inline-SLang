/*============================================================================
 * SLang.xs
 * Inline::SLang method bindings.
 *==========================================================================*/

#include "util.h"

/*
 * a macro to convert the S-Lang stack to a perl one
 * - should have made it a function but since it messes
 *   around with perl stack commands (eg EXTEND()) I
 *   couldn't be bothered working out how to do that
 *
 * note the minor complication in that we have to reverse
 * the order of the stack when moving from S-Lang to perl
 *
 * The macro requires the following in the PREINIT: section
 *
 *   SV **slist = NULL;
 *   int i, sdepth;
 *
 * and calls the function 'SV * sl2pl()'
 *
 * unlike Inline::Python/Ruby I always check the context
 */

#define CONVERT_SLANG2PERL_STACK \
    sdepth = _SLstack_depth(); \
    Printf( ("    *** stack depth = %d\n", sdepth) ); \
 \
    Printf( ("  checking context:\n") ); \
    Printf( ("    GIMME_V=%i\n", GIMME_V) ); \
    Printf( ("    G_VOID=%i\n", G_VOID) ); \
    Printf( ("    G_ARRAY=%i\n", G_ARRAY) ); \
    Printf( ("    G_SCALAR=%i\n", G_SCALAR) ); \
 \
    /* We can save a little time by checking our context */ \
    switch( GIMME_V ) { \
      case G_VOID: \
        /* let's clear the S-Lang stack */ \
        if ( sdepth ) { \
          Printf( ("clearing the S-Lang stack (%d items) since run in void context\n", sdepth) ); \
          if ( -1 == SLdo_pop_n( sdepth ) ) \
            croak( "Error: unable to clear the S-Lang stack\n" ); \
        } \
        XSRETURN_EMPTY; \
        break; \
 \
      case G_SCALAR: \
        if ( sdepth ) { \
          /* dump everything but the 'first' item */ \
          Printf( ("removing %d items from the stack since run in scalar context\n", \
	    sdepth-1 ) ); \
          if ( sdepth > 1 ) \
            if ( -1 == SLdo_pop_n( sdepth-1 ) ) \
              croak( "Error: unable to clear the S-Lang stack\n" ); \
 \
          Printf( ("trying to set perl stack item 0\n" ) ); \
          PUSHs( sv_2mortal( sl2pl() ) ); \
        } /* if: sdepth */ \
        break; \
 \
      case G_ARRAY: \
        /*  \
         * convert the S-Lang objects on the S-Lang stack into perl objects on  \
         * the perl stack \
         * \
         * note: the order of the S-Lang stack has to be reversed \
         */ \
        if ( sdepth ) { \
          Newz( 0, slist, sdepth, SV * ); \
          if ( slist == NULL ) \
            croak("Error: unable to allocate memory\n" ); /* ott ? */ \
          for ( i = sdepth-1; i >= 0; i-- ) { \
            Printf( ("reading from S-Lang stack item #%d\n", i ) ); \
            slist[i] = sl2pl(); \
          } \
 \
          /* now can stick the objects onto the perl stack */ \
          EXTEND( SP, sdepth ); \
          for ( i = 0; i < sdepth; i++ ) { \
            Printf( ("trying to set perl stack #%d\n", i ) ); \
            PUSHs( sv_2mortal( slist[i] ) ); \
          } \
 \
          Printf( ("freeing up stack-related memory\n") ); \
          Safefree( slist ); \
        } /* if: sdepth */ \
        break; \
 \
      default: \
        /* shouldn't happen with perl <= 5.8.0 */ \
        croak( "Internal error: GIMME_V is a value I don't understand\n" ); \
 \
    } /* switch(GIMME_V) */

static char *_slang_version = SLANG_VERSION_STRING; /* do we really need this ? */

/*
 * Initialize the S-Lang interpreter with all the intrinsic functions
 * - should we just exit on failure or do something a bit cleaner?
 */
void do_slinit() {

  Printf( ( "In do_slinit()\n" ) );

  /* want to allow dynamic linking, hence _init_import() is required */
  if( (-1 == SLang_init_all()) || (-1 == SLang_init_import()) )
    exit (EXIT_FAILURE);

  Printf( ( "  - initialized S-Lang and intrinsic functions\n" ) );

} /* do_slinit() */

MODULE = Inline::SLang	PACKAGE = Inline::SLang

# At boot time:
# - initialise the S-Lang interpreter
#

BOOT:
  do_slinit();

PROTOTYPES: DISABLE

char *
_sl_version( )
  CODE:
    RETVAL = _slang_version;
  OUTPUT:
    RETVAL

# NOTE:
#  the perl routine sl_eval, which calls this, ensures that the
#  string ends in a ';'. you can call this routine directly for a minor
#  speed increase and avoid the check
#
# Shouldn't we just call S-Lang's eval using the perl wrapper
# code ?
#

void
_sl_eval( str )
    char * str

  PREINIT:
    SV **slist = NULL;
    int i, sdepth;

  PPCODE:
    Printf( ("----------------------------------------------------------------------\n") );
    Printf( ("sl_eval: code: %s\n", str ) );
    Printf( ("----------------------------------------------------------------------\n") );
    if ( -1 == SLang_load_string(str) ) {
        /* do we really want to restart the S-Lang interpreter? */
        /***
        SLang_restart (1);
        SLang_Error = 0;
         ***/
	croak( "Error -- sl_eval failed to parse input" );
	// XSRETURN_EMPTY;
    }
    /* stick any return values on the stack */
    CONVERT_SLANG2PERL_STACK

# support for reference handling -- used by DESTROY method
# of Inline::SLang::reference object.
# THis should be considered a hack for now
#
void
_sl_free_ref( ptr )
    SV * ptr
  PREINIT:
    SLang_Ref_Type *ref;
  PPCODE:
    /* assume we're called correctly */
    ref = INT2PTR( SLang_Ref_Type *, SvIV(ptr) );
    Printf( ( "About to delete S-Lang ref pointer %p\n", ref) );
    SLang_free_ref( ref );

#undef  NUM_FIXED_ARGS
#define NUM_FIXED_ARGS 1

void
sl_call_function( qualname, ... )
    char * qualname

  PREINIT:
    SV **slist = NULL;
    int i, sdepth;

  PPCODE:
    Printf( ("sl_call_function called:\n  function: %s\n", qualname ) );

    /*
     * We could remove this check - ie assume that if we've got this
     * far then eveything 'should' be fine
     */
    if ( 1 > SLang_is_defined(qualname) ) {
      croak( "'%s' is not a S-Lang function", qualname );
      XSRETURN_EMPTY;
    }
    Printf( ("  and it is a S-Lang function\n" ) );

    /*
     * convert the perl arguments into S-Lang arguments and
     * stick them onto the S-Lang stack
     */
    Printf( ("  converting %d arguments to S-Lang\n", items-NUM_FIXED_ARGS) );
    SLang_start_arg_list ();
    for ( i = NUM_FIXED_ARGS; i < items; i++ ) {
	pl2sl( ST(i) );
    }
    SLang_end_arg_list ();

    /* perhaps should use SLexecute_function() instead */
    if ( -1 == SLang_execute_function( qualname ) ) {
      croak( "Error: unable to execute S-lang function '%s'\n", qualname );
      XSRETURN_EMPTY;
    }
    Printf( ("  and executed the function\n") );

    CONVERT_SLANG2PERL_STACK

