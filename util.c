/****************************************************************************
 *
 * $Id: util.c,v 1.51 2003/08/19 03:44:09 dburke Exp $
 *
 * util.c
 *   Conversion routines between S-Lang and Perl data types.
 *
 * Need to relook at Astro::CFITSIO to see how it handles arrays
 *
 * A note on error handling:
 *  We ignore the return value of SLang_load_string() [*] because
 *  we have installed (see BOOT: code in SLang.xs) an error
 *  handler that croak's on error with the S-Lang error message.
 *  For the code in this file it might be beneficial to use a different
 *  error handler - since if there is an error message it will not make
 *  sense to the casual user [as it will come from the mucking around
 *  we do whilst converting between S-Lang and Perl representation].
 *  However, we don't do this at the moment.
 *
 *  [* unfortunately I don't think this means we can ignore the
 *     return value of things like SLang_push_complex() ? ]
 *
 ****************************************************************************/

#include "util.h"

/* used by the CALL_xxx macros: can not be static since use in SLang.xs */
char *
_get_object_type( SV *obj ) {
  HV *stash = SvSTASH( obj ); /* assume obj really is an object */
  return ( stash ? HvNAME(stash) : "<none>" );
} /* _get_object_type() */

/*
 * since we use $1..$n to create/manipulate variables
 * it can lead to mis-leading results (ie what you think a routine
 * is doing is only happening because $1 happens to be set
 * correctly from a previous call [can happen if test set/get
 * routines next to each other].
 *
 * so, we try and clean things out (setting them to null)
 * could also free up some memory quicker
 */
void
_clean_slang_vars( int n ) {
  char stxt[12]; /* assuming n <= 99 */
  int i;
  for ( i = 1; i <= n; i++ ) {
    (void) sprintf( stxt, "$%d = NULL;", i );
    (void) SLang_load_string( stxt );
  }
} /* _clean_slang_vars() */

/* called from XS and from sl2pl.c */
SV *
_create_empty_array( int ndims, int dims[] ) {
  AV *array;
  int dimsize = dims[0] - 1;
  long i;

  /* create the array */
  array = (AV *) sv_2mortal( (SV *) newAV() );

  /* fill it in */
  if ( ndims > 0 ) {
    av_extend( array, (I32) dimsize );

    if ( ndims > 1 ) {
      for ( i = 0; i <= dimsize; i++ ) {
	av_store( array, i, _create_empty_array( ndims-1, dims+1 ) );
      }
    }
  }

  return newRV_inc( (SV *) array );

} /* _create_empty_array */
		    
/* util.c */
