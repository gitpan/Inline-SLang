/****************************************************************************
 *
 * $Id: SLang.xs,v 1.30 2004/04/16 04:11:27 dburke Exp $
 *
 * SLang.xs
 *   Inline::SLang method bindings.
 *
 ****************************************************************************/

#include "util.h"
#include "pdl.h"
#include "sl2pl.h"
#include "pl2sl.h"

/*
 * How are S-Lang arrays converted to Perl?
 *  non-numeric types are as array references
 *  numeric arrays are piddles [if PDL support is available]
 *
 * Cannot be static since used in sl2pl.c
 */
int _slang_array_format = I_SL_ARRAY2AREF | I_SL_ARRAY2PDL;

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
   * $@ since the location of the error isn't much use to people
   */
  croak( "%s\n", emsg );
}

/*
 * a simple random-number generator used to store "opaque" objects
 * in the _inline namespace. Obtained from 
 *   http://www.taygeta.com/random.html
 * and removed the set/get seed parts
 *
 * Linear Congruential Method, the "minimal standard generator"
 * Park & Miller, 1988, Comm of the ACM, 31(10), pp. 1192-1201
 * static char rcsid[] = "@(#)randlcg.c	1.1 15:48:15 11/21/94   EFC";
 *
 * I don't know why I didn't just use a system random-number
 * generator (I'm not that bothered if they're not really random)
 */

#include <math.h>
#include <limits.h>

static long int my_quotient  = LONG_MAX / 16807L;
static long int my_remainder = LONG_MAX % 16807L;
static long int my_seed_val  = 0; /* is set at BOOT time */

/*
 * returns a random number between 0 and nmax-1 inclusive
 * - could it return the value of nmax?
 */
static int quick_random( int *nmax ) {
  double scale = (double) *nmax;
  if ( my_seed_val <= my_quotient )
    my_seed_val = (my_seed_val * 16807L) % LONG_MAX;
  else {
    long int high_part = my_seed_val / my_quotient;
    long int low_part  = my_seed_val % my_quotient;
    long int test = 16807L * low_part - my_remainder * high_part;
    if ( test > 0 ) my_seed_val = test;
    else            my_seed_val = test + LONG_MAX;
  }
  /* being cavalier about overflows and rounding here */
  return (int) floor( my_seed_val * scale / LONG_MAX );
}


MODULE = Inline::SLang	PACKAGE = Inline::SLang

# At boot time:
# - initialise the S-Lang interpreter
# - set up the S-Lang error handler
#

BOOT:
  {
    SLang_NameSpace_Type *ns;

    Printf( ( "In Perl's BOOT section\n" ) );

    /*
     * want to allow dynamic linking, hence _init_import() is required
     * and for my needs I want the extra array routines provided by
     * the _init_array_extra routines
     */
    if( (-1 == SLang_init_all()) ||
	(-1 == SLang_init_array_extra()) ||
	(-1 == SLang_init_import()) )
      croak("Internal error: unable to initialize the S-Lang library\n");
    /* set up error hook */
    SLang_Error_Hook = _sl_error_handler;
    Printf( ( "  - initialized S-Lang and intrinsic functions\n" ) );

    /*
     * now create the _inline namespace and add a simple random-number
     * generator to it
     */
    ns = SLns_create_namespace( "_inline" );
    if( NULL == ns )
      croak( "Error: Unable to create S-Lang namespace (_inline) during initialization\n" );
    if( -1 == SLns_add_intrinsic_function(
                ns, "_qrandom",(FVOID_STAR) quick_random,
                SLANG_INT_TYPE, 1, SLANG_INT_TYPE ) )
      croak( "Error: Unable to create ran. num generator in _inline during initialization\n" );

    my_seed_val = (long int) time(NULL);

    /* set up PDL - if support is compiled in */
    INIT_PDL_CORE;

  }

PROTOTYPES: DISABLE

# return the S-Lang version as a string

char *
sl_version( )
  CODE:
    RETVAL = SLANG_VERSION_STRING;
  OUTPUT:
    RETVAL

int
sl_have_pdl( )
  CODE:
    RETVAL = I_SL_HAVE_PDL;
  OUTPUT:
    RETVAL

# how are S-Lang arrays converted to Perl?
#
int
sl_array2perl( ... )
  PREINIT:
    int newtype;
  CODE:
    if ( items > 1 ) croak( "Usage: sl_array2perl( [$flag] )" );
    if ( 1 == items ) {
      newtype = (int) SvIV( ST(0) );
      if ( newtype < 0 || newtype > 1+(I_SL_HAVE_PDL<<1) )
        croak( "Error: sl_array2perl() can only be sent an integer between 0 and %d (inclusive)", 1+(I_SL_HAVE_PDL<<1) );
      _slang_array_format = newtype;
    }
    RETVAL = _slang_array_format;
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
# - this is not ideal since I'm making assumptions about the
#   internals of S-Lang that aren't obviously "public" (ie
#   things may change)
#
# Also, have shoe-horned into this routine a list of type
# "synonyms" recognised by the S-Lang interpreter. This
# information is generated at 'perl Makefile.PL' time,
# and we just include it here. In this case the hash key is the
# synonym, the first element of the array is the 'base' type,
# and the second element is set to 2 (this assumes that there aren't
# any type synonyms of structures)
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

    /* add in the type synonyms */
    /*** NOTE: are we over-writing info here ??
     ***   [or do the synonymns not have a class]  
     ***   {is everything in stf.h a synonyn?}
     ***/
#include "stf.h"

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
    const char *slformat = "string(%s);typeof(%s)==DataType_Type;";
    char *outname;
    char *slbuffer;
    size_t blen;
    int flag;

  PPCODE:
    /* not bothered anout 2 extra chars due to %s %s in format */
    blen = strlen(slformat)+2*strlen(inname);
    Newz( "", slbuffer, blen, char );
    snprintf( slbuffer, blen, slformat, inname, inname );

    Printf( ("Checking if %s is a valid DataType_Type name\n",inname) );
    Printf( ("S-Lang buffer= [%s]\n", slbuffer) );

    (void) SLang_load_string( slbuffer );

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
# - would be nice if didn't need to treat things using a switch
#   statement (since it's another bit of code that needs updating
#   whenever the type code changes)
#

void
_guess_sltype( item )
    SV * item
  PREINIT:
    SLtype sltype;
    int slflag;
    SV * out;
  PPCODE:
    sltype = pltype( item, &slflag );

    switch( sltype ) {
      case SLANG_NULL_TYPE:     out = newSVpv( "Null_Type", 0 ); break;
      case SLANG_INT_TYPE:      out = newSVpv( "Integer_Type", 0 ); break;
      case SLANG_DOUBLE_TYPE:   out = newSVpv( "Double_Type", 0 ); break;
      case SLANG_STRING_TYPE:   out = newSVpv( "String_Type", 0 ); break;
      case SLANG_COMPLEX_TYPE:  out = newSVpv( "Complex_Type", 0 ); break;
      case SLANG_DATATYPE_TYPE: out = newSVpv( "DataType_Type", 0 ); break;
      case SLANG_STRUCT_TYPE:   out = newSVpv( "Struct_Type", 0 ); break;
      case SLANG_ASSOC_TYPE:    out = newSVpv( "Assoc_Type", 0 ); break;
      case SLANG_ARRAY_TYPE:    out = newSVpv( "Array_Type", 0 ); break;
      case SLANG_UNDEFINED_TYPE:
        if ( 0 == slflag ) out = newSVpv( "Undefined_Type", 0 );
        else {
	  /* handle a type treated as an 'opaque' type */
	  SV *type;
	  /* prob leaks mem here */
	  fixme( "memleak?" );
	  CALL_METHOD_SCALAR_SV( item, "typeof", , type );
          CALL_METHOD_SCALAR_SV( type, "stringify", , out );
	  SvREFCNT_dec( type );
       }
       break;

      default:
        croak( "Internal error: unable to understand Perl datatype" );
    }
    PUSHs( sv_2mortal( out ) );

# create an empty nD array
# - function is defined in util.c, here we just convert a perl array into
#   a C one
# - note no error checking
#
void
_create_empty_array( SV *in )
  PREINIT:
    int dims[SLARRAY_MAX_DIMS];
    AV *aref;
    SV * out;
    int i, ndim;
  PPCODE:
    aref = (AV *) SvRV( in );
    ndim = 1 + av_len(aref);
    Printf( ("Creating an empty array (%d dim) with size ", ndim) );
    for( i = 0; i < ndim; i++ ) {
      int dsize = SvIV( *av_fetch( aref, i, 0 ) );
      dims[i] = dsize;
      Printf( ("%d ", dsize) );
    }
    Printf( ("\n") );
    out = _create_empty_array( ndim, dims );
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

