/****************************************************************************
 *
 * $Id: pdl.c,v 1.3 2003/08/19 03:36:35 dburke Exp $
 *
 * pdl.c
 *   PDL support for Inline::SLang (at least the utility functions,
 *   since some PDL-specific code will appear in other files)
 *
 ****************************************************************************/

#include "util.h"
#include "pdl.h"

/* Should only ever be compiled if I_SL_HAVE_PDL is 1 so do not need to check */

/*
 * access the PDL internals
 * - this is essentially the output of
 *     use PDL::Core::Dev; print &PDL_AUTO_INCLUDE();
 */
Core* PDL;   /* Structure holds core C functions */
SV* CoreSV;  /* Gets pointer to perl var holding core structure */

/* temporary variables whilst the conversion of nD arrays <-> piddles is a mess */
static int needto_print_nd_warning_slang = 1;
static int needto_print_nd_warning_pdl   = 1;

/*
 * initialize the pointers that will allow us to call PDL functions 
 * - this is called from the BOOT section of the XS code
 *   we do it this way so that the PDL-related variables can be
 *   localised to this file (which now seems a bit pointless
 *   as they're no longer static)
 */
void initialize_pdl_core( void ) {

  /* a 'use PDL::LiteF;' in SLang.pm would be simpler... */
  load_module( 0, newSVpv("PDL::Lite",0), NULL );

  /* 
   * this code fragment is essentially the output of:
   *    use PDL::Core::Dev; print &PDL_BOOT();
   * minus the require_pv line and the aTHX_ defines
   */
  CoreSV = perl_get_sv("PDL::SHARE",FALSE);
  if( NULL == CoreSV )
    Perl_croak(aTHX_ "The Inline::SLang module requires the PDL::Core module, which was not found");
  PDL = INT2PTR(Core*,SvIV( CoreSV ));
  if ( PDL_CORE_VERSION != PDL->Version )
    Perl_croak(aTHX_ "The Inline::SLang module needs to be recompiled against the latest installed PDL");

} /* initialize_pdl_core() */

/* Used by sl2pl.c - convert a S-Lang array into a piddle */

/*
 * see 'pdldoc API' for explanation of what's going on here
 *
 * SV *sl2pl_array_pdl( SLang_Array_Type * )
 *   convert the S-Lang array into a piddle
 *
 * void pl2sl_array_pdl( SV * )
 *   convert the piddle into a S-Lang array and
 *   push this array onto the S-Lang stack
 */

SV *
sl2pl_array_pdl( SLang_Array_Type *at ) {

  PDL_Long dims[SLARRAY_MAX_DIMS];
  pdl  *out;
  SV *sv;
  size_t dsize;
  int i;

  /* temp warning */
  if ( at->num_dims > 1 && needto_print_nd_warning_slang ) {
    fprintf( stderr, "WARNING: converting a >1d array to a piddle.\n" );
    fprintf( stderr, "         This is not well handled as yet.\n" );
    fprintf( stderr, " (this warning will not appear again this run)\n" );
    needto_print_nd_warning_slang = 0;
  }

  /*
   * copy over the dims
   * - for now we reverse the dims
   */
  for ( i = 0; i < at->num_dims; i++ )
    dims[at->num_dims-1-i] = at->dims[i];

  /* should we check for failure? */
  out = PDL->pdlnew();
  PDL->setdims( out, dims, at->num_dims );
#include "topdl.h"

  /*
   * copy the memory from the array since I don't know when S-Lang may delete it
   * (would be quicker to just point to it but that leads to memory-managment issues)
   */
  PDL->allocdata( out );
  (void) memcpy( out->data, at->data, (size_t) at->num_elements * dsize );

  /* covert the piddle into a 'SV *' */
  sv = sv_newmortal();
  PDL->SetSV_PDL( sv, out );
  SvREFCNT_inc( sv );
  return sv;

} /* sl2pl_array_pdl() */

void
pl2sl_array_pdl( SV *item ) {

  int dims[SLARRAY_MAX_DIMS];
  SLang_Array_Type *at;
  pdl *pdl;
  SLtype otype;
  size_t dsize;
  int i;

  fixme("XXX to do XXX");

  /* have a piddle: do I need to call pdl_make_physical on it??? */
  pdl = PDL->SvPDLV(item);
  if ( pdl->ndims > SLARRAY_MAX_DIMS )
    croak( "Error: max number of dimensions for a S-Lang array is %d",
	   SLARRAY_MAX_DIMS );
  
  if ( pdl->ndims == 0 )
    croak( "Error: S-Lang does not allow a 0d array - perhaps should promote to 1d or convert to a scalar?" );
  
  /* temp warning */
  if ( pdl->ndims > 1 && needto_print_nd_warning_pdl ) {
    fprintf( stderr, "WARNING: converting a >1d piddle to S-Lang.\n" );
    fprintf( stderr, "         This is not well handled as yet.\n" );
    fprintf( stderr, " (this warning will not appear again this run)\n" );
    needto_print_nd_warning_pdl = 0;
  }

  /* note: we swap the dims for now */
  Printf( ("*** converting Piddle to S-Lang: ndims=%d [", pdl->ndims) );
  for ( i = 0; i < pdl->ndims; i++ ) {
    dims[pdl->ndims-1-i] = pdl->dims[i];
    Printf( (" %d", dims[i]) );
  }
  Printf( ("] dtype=%d", pdl->datatype) );

#include "toslang.h"

  Printf( (" -> %s\n", SLclass_get_datatype_name(otype)) );
  at = SLang_create_array( otype, 0, NULL, dims, pdl->ndims );
  if ( at == NULL )
    croak( "Error: Unable to create a S-Lang array of %ld elements",
	   pdl->nvals );

  /* copy over the data */
  (void) memcpy( at->data, pdl->data, (size_t) pdl->nvals * dsize );

  /* stick array on the stack */
  (void) SLang_push_array (at, 1);

} /* pl2sl_array_pdl() */

/* pdl.c */
