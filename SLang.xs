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
        croak( "Internal error: GIMME_V is set to a value I don't understand\n" ); \
 \
    } /* switch(GIMME_V) */

static char *_slang_version = SLANG_VERSION_STRING; /* do we really need this ? */

/*
 * Error handler: we call croak on the supplied string
 * after clearing the S-Lang error and restarting the
 * interpreter
 *
 * This means that SLang_load_string() will no longer return
 * a -1 on error: we'll get the croak called instead.
 *
 * Perhaps we should just append the error message onto $@
 * instead?
 *
 * - I'm not sure if we need to do something like
 *      errsv = get_sv("@", TRUE);
 *      sv_setsv(errsv, exception_object);
 *      croak(Nullch);
 */
void _sl_error_handler( char *emsg ) {
  SLang_restart (1);
  SLang_Error = 0;
  /*
   * add a trailing '\n' to stop line number being included in 
   * $@ since the location isn't much use to people
   */
  croak( "%s\n", emsg );
}

MODULE = Inline::SLang	PACKAGE = Inline::SLang

# At boot time:
# - initialise the S-Lang interpreter
# - set up the S-Lang error handler
#

BOOT:
  Printf( ( "In Perl's BOOT section\n" ) );
  /* want to allow dynamic linking, hence _init_import() is required */
  if( (-1 == SLang_init_all()) || (-1 == SLang_init_import()) )
    croak("Internal error: unable to initialize the S-Lang library\n");
  /* set up error hook */
  SLang_Error_Hook = _sl_error_handler;
  Printf( ( "  - initialized S-Lang and intrinsic functions\n" ) );

PROTOTYPES: DISABLE

# return the S-Lang version as a string

char *
_sl_version( )
  CODE:
    RETVAL = _slang_version;
  OUTPUT:
    RETVAL

# return, as an associative array reference, the 
# names of defined types (key) and a 2-element
# array containing the class number and a boolean flag
# indicating whether the type is a "named" struct.
# Note that Struct_Type objects will have 0, confusingly enough
#
# Now, this latter piece of information is hard to find out
# - for now I'm going to use a S-Lang library routine that 
#   is not in slang.h but is not marked static
# - this is not the best thing
#
# this is partly hacking at the internals of S-Lang,
# even if the routines are public
#
void
_sl_defined_types( )
  PREINIT:
    SLang_Class_Type *_SLclass_get_class( unsigned char ); /* from slclass.c */
    HV *hashref;
    AV *arrayref;
    char *name;
    int i, sflag;

  PPCODE:
    /* create the hash array to store the results in */
    hashref = (HV *) sv_2mortal( (SV *) newHV() );

    /*
     * assume that the max number of types is 256
     * - this is not a great thing to do
     * and the use of _SLclass_get_class() et al 
     * is even worse
     */
    for ( i = 0; i < 256; i++ ) {
      if ( SLclass_is_class_defined(i) ) {
        name = SLclass_get_datatype_name( (SLtype) i );
	sflag = _SLclass_get_class( (unsigned char) i )->cl_struct_def != NULL;

	arrayref = newAV();
	av_extend( arrayref, (I32) 2 );
	av_store( arrayref, 0, newSViv(i) );
	av_store( arrayref, 1, newSViv(sflag) );

        Printf( ("class number %d has a name of %s and struct flag = %d\n", i, name, sflag) );
	(void) hv_store( hashref, name, strlen(name),
		newRV_inc( (SV *) arrayref ), 0 );
      }
    } /* for: i */

    /* return the associative array reference */
    PUSHs( newRV_inc( (SV *) hashref) );

#
# returns a flag to say whether the input value is
# the name of a DataType_Type and the name of it [after converting
# synonyms]. See the DataType_Type object code in SLang.pm
#
void
_sl_isa_datatype( inname )
    char *inname
  PREINIT:
    char *outname;
    char *slbuffer;
    size_t blen;
    int flag;

  PPCODE:
    blen = 34+2*strlen(inname);
    Newz( "", slbuffer, blen, char );
    snprintf( slbuffer, blen,
              "string(%s);typeof(%s)==DataType_Type;", inname, inname );

    Printf( ("Checking if %s is a valid DataType_Type name\n",inname) );
    Printf( ("S-Lang buffer= [%s]\n", slbuffer) );

    (void) SLang_load_string( slbuffer  );

    if ( -1 == SLang_pop_integer(&flag) )
      croak("Internal error: unable to pop an integer from the S-Lang stack" );
    Printf( ("  flag [ie is datatype?] = [%d]\n", flag) );
    PUSHs( sv_2mortal(newSVuv(flag)) );

    if ( -1 == SLang_pop_slstring(&outname) )
      croak("Internal error: unable to pop a string from the S-Lang stack" );
    Printf( ("  and 'base' type name = [%s]\n", outname) );
    PUSHs( sv_2mortal(newSVpv(outname,0)) );

    /* don't forget to free up memory */
    SLang_free_slstring(outname);
    Safefree(slbuffer);

# try and guess the datatype of a Perl variable to try and get
# around Perl's permiscuous datatypes
#
# -- test for - in order
#       float
#       integer
#       string
#     or object
#       Math::Compex
#       s/thing derived from Inline::SLang::_Type
#     or currently die
#
# see also util.c/pl2sl()
#

void
_guess_sltype( item )
    SV * item
  PREINIT:
    SV * out;
  PPCODE:
    /* don't call return here since can;t be bothered to read about it */
    if ( !SvOK(item) )      { out = newSVpv( "Null_Type", 0 ); }
    else if ( SvIOK(item) ) { out = newSVpv( "Integer_Type", 0 ); }
    else if ( SvNOK(item) ) { out = newSVpv( "Double_Type", 0 ); }
    else if ( SvPOK(item) ) { out = newSVpv( "String_Type", 0 ); }
    else if ( sv_isobject(item) ) {
      if ( sv_derived_from( item, "Math::Complex" ) ) {
        out = newSVpv( "Complex_Type", 0 );
      } else if ( sv_derived_from( item, "Inline::SLang::_Type" ) ) {
	SV *type;
	/* prob leaks mem here */
	fixme( "memleak?" );
	CALL_METHOD_SCALAR_SV( item, "typeof", , type );
        CALL_METHOD_SCALAR_SV( type, "stringify", , out );
	SvREFCNT_dec( type );
      } else {
      croak( "Internal error: unable to understand Perl object" );
      }
    } else {
      croak( "Internal error: unable to understand Perl datatype" );
    }
    PUSHs( sv_2mortal( out ) );

# NOTE:
#  the perl routine sl_eval, which calls this, ensures that the
#  string ends in a ';'. you can call this routine directly for a minor
#  speed increase and avoid the check
#
# Since we installed an error handler that calls Perl's croak on
# an error, rather than printing the messages to STDERR,
# SLang_load_string() no longer returns on error
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

    /*
     * since we have installed an error handler which croak's, 
     * we do not need to check the return value of this function
     */
    (void) SLang_load_string(str);

    /* stick any return values on the stack */
    CONVERT_SLANG2PERL_STACK

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

    /*
     * perhaps should use SLexecute_function() instead
     * - also, not clear if need to check for the return value given
     *   the error handler
     */
    if ( -1 == SLang_execute_function( qualname ) ) {
      croak( "Error: unable to execute S-lang function '%s'\n", qualname );
      XSRETURN_EMPTY;
    }
    Printf( ("  and executed the function\n") );

    CONVERT_SLANG2PERL_STACK

