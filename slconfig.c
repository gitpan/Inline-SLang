/*
 * Usage:
 *  slconfig min-version
 *
 * Aim:
 *  Find out some things about S-Lang.
 *    a) is the S-Lang version >= min-version
 *       min-version must be specified as 10407 *not* 1.4.7
 *    b) support for float and complex types
 *    c) what do the fixed-size types map to
 *    d) other common type synonyms
 *
 *  Run during the initial 'perl Makefile.PL' step used to set-up
 *  the module for compilation.
 *
 * Error codes:
 *   The exit status is used to indicate what went wrong
 *   (the numerical order does not indicate what order they
 *    are tested)
 *      0 - success
 *
 *      1 - general error code
 *     10 - S-Lang version < min-version
 *     11 - unable to initialize the S-Lang library
 *     12 - Unable to create slconfig.dat (output file)
 *     13 - Unable to close slconfig.dat (output file)
 *     20 - No support for Float_Type variables
 *     21 - No support for Complex_Type variables
 *
 * Output:
 *   Writes to slconfig.dat a list, to stdout, of synonyms
 *   and bae types: For example:
 *      1 Int16_Type Short_Type
 *    
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slang.h"

#define NCHECKS 2
static char *inchecks[NCHECKS] = {
  "Float_Type", "Complex_Type"
};

#define NTYPES 14
static char *intypes[NTYPES] = {
  /*** fixed-size types ***/
  "Int16_Type", "UInt16_Type",
  "Int32_Type", "UInt32_Type",
  "Float32_Type", "Float64_Type",
  "Int64_Type", "UInt64_Type",
  /*** other possible type synonyms ***/
  "Short_Type", "UShort_Type",
  "Long_Type", "ULong_Type",
  "Int_Type", "UInt_Type"  /* we know what these map to but add them here */
};

/* stop loads of messages to the screen */
void _sl_error_handler( char *emsg ) { return; }

static char outname[] = "slconfig.dat";

int main( int argc, char *argv[] ) {
  static char slstring[30]; /* not bothered about buffer overflows ;) */
  FILE *fp;
  char *outtype;
  int minver;
  int i;

  if ( argc != 2 ) {
    printf( "Usage: %s min-version\n", argv[0] );
    exit(1);
  }
  /* should really check for a conversion error */
  minver = atoi( argv[1] );

  /*
   * could check against SLANG_VERSION from slang.h
   * but am having "fun" with this on my gen too box, so
   * we use the version from the library itself
   */
  if ( minver > SLang_Version ) {
    printf( "ERROR: S-Lang version (%d) is < the minimum supported (%d)\n",
	    SLang_Version, minver );
    exit( 10 );
  }

  /* although don't need import here, just check it works */
  if( (-1 == SLang_init_all()) || (-1 == SLang_init_import()) ) {
    printf( "ERROR: Unable to initialize the S-Lang library.\n" );
    exit( 11 );
  }

  /* install an error handler (move before SLang_init* calls?) */
  SLang_Error_Hook = _sl_error_handler;

  /* does S-Lang support for floats and complex numbers ? */
  for ( i = 0; i < NCHECKS; i++ ) {
    sprintf( slstring, "%s;", inchecks[i] );
    if ( -1 == SLang_load_string( slstring ) ) {
      printf( "ERROR: Your S-Lang does not support %s type variables.\n",
	      inchecks[i] );
      exit( 20 + i );
    }
  }  /* for: i < NCHECKS */

  /* create the output file */
  if ( (fp = fopen( outname, "w" )) == NULL ) {
    printf( "ERROR: Unable to create the output file '%s'\n", outname );
    exit( 12 );
  }

  /* find the type conversions */
  for ( i = 0; i < NTYPES; i++ ) {
    sprintf( slstring, "string(%s);", intypes[i] );
    if ( -1 != SLang_load_string( slstring ) ) {
      (void) SLang_pop_slstring( &outtype );
      if ( strcmp( outtype, intypes[i] ) )
	fprintf( fp, "%d %s %s\n", i+1, intypes[i], outtype );
      SLang_free_slstring( outtype );
    } else {
      SLang_restart(1);
      SLang_Error = 0;
    }
  } /* for: i < NTYPES */

  if ( 0 != fclose( fp ) ) {
    printf( "ERROR: Unable to close output file '%s'\n", outname );
    exit( 13 );
  }

  exit( 0 );
    
} /* main */
