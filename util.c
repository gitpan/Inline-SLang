/****************************************************************************
 * util.c
 * Conversion routines between S-Lang and Perl data types.
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
 */

#include "util.h"

void pl2sl_type( SV *item, SLtype item_type, int item_flag );

/*
 * a badly-named macro
 * This is used when calling a S-Lang function whose error code we
 * should check but I'm not sure whether the error handler catches
 * the error or not. So, I've wrapped the code in a define which
 * we can easily change if the error handler works
 */
#define UTIL_SLERR( slfunc, emsg ) if ( -1 == slfunc ) croak( emsg )

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
static void
_clean_slang_vars( int n ) {
  char stxt[12]; /* assuming n <= 99 */
  int i;
  for ( i = 1; i <= n; i++ ) {
    (void) sprintf( stxt, "$%d = NULL;", i );
    (void) SLang_load_string( stxt );
  }
} /* _clean_slang_vars() */

/*
 * utility functions for pl2sl()
 */

/*
 * Usage:
 *   SLtype = pltype( SV *val, int *flag )
 *
 * Aim:
 *   Given a Perl object (as a SV *), return the approproiate
 *   S-Lang type (as a SLtype value) for it. flag is an output
 *   variable -
 *     if 1 then the SLtype should be considered to mean just that,
 *     if 0 then it indicates a "special" meaning
 *     (used by assoc/array types)
 *
 * Notes:
 *   Initial version - needs thinking/work
 *
 *   - probably important to do integer/double/string check in
 *     that order due to Perl's DWIM-ery wrt types
 *
 * used by _guess_type in SLang.xs so can't be static
 */
SLtype
pltype( SV *plval, int *flag ) {

  *flag = 1;

  if ( SvROK(plval) ) {

    /*
     * assume that if an object we either know what to do
     * or it can't be converted
     */
    if ( sv_isobject(plval) ) {

      if ( sv_derived_from(plval,"Math::Complex") ) return SLANG_COMPLEX_TYPE;
      if ( sv_derived_from(plval,"DataType_Type") ) return SLANG_DATATYPE_TYPE;
      if ( sv_derived_from(plval,"Struct_Type") )   return SLANG_STRUCT_TYPE;
      if ( sv_derived_from(plval,"Assoc_Type") )    return SLANG_ASSOC_TYPE;
      if ( sv_derived_from(plval,"Array_Type") )    return SLANG_ARRAY_TYPE;

      /*
       * run out of specific types
       *  - indicate this by returning SLANG_UNDEFINED_TYPE 
       *    but with a flag of 0
       */
      if ( sv_derived_from(plval,"Inline::SLang::_Type") ) {
	*flag = 0;
	return SLANG_UNDEFINED_TYPE;
      }
      
    } else {
      SV *ref = SvRV(plval);

      if ( SvTYPE(ref) == SVt_PVHV ) { *flag = 0; return SLANG_ASSOC_TYPE; }
      if ( SvTYPE(ref) == SVt_PVAV ) { *flag = 0; return SLANG_ARRAY_TYPE; }
  
    }

  } else {
    /* not a reference */
  
    if ( !SvOK(plval) ) return SLANG_NULL_TYPE;
    if ( SvIOK(plval) ) return SLANG_INT_TYPE;
    if ( SvNOK(plval) ) return SLANG_DOUBLE_TYPE;
    if ( SvPOK(plval) ) return SLANG_STRING_TYPE;

  }

  croak( "Sent a perl type that can not be converted to S-Lang." );

} /* pltype() */

/*
 * pl2sl_assoc()
 * must be called with the S-Lang assoc array in $1
 */

static void
pl2sl_assoc( HV *hash ) {
  I32 nfields, i;

  /*
   * loop through the keys in the Perl hash and set the corresponding 
   * value in the S-Lang Assoc_Type array
   */
  nfields = hv_iterinit( hash );
  Printf( ("  hash ref contains %d fields\n",nfields) );
  for ( i = 0; i < nfields; i++ ) {
    HE *next;
    SV *value;
    char *fieldname;
    I32 ignore;

    /* get the next key/value pair from the hash */
    value = hv_iternextsv( hash, &fieldname, &ignore );
    Printf( ("  - field %d/%d name=[%s]\n",i,nfields-1,fieldname) );

    /* 
     * push $1 [in case pl2sl() trashes it], the field name,
     * and then the Perl value (converted to S-Lang) onto the
     * S-Lang stack
     *
     * TODO: [low priority enhancement]
     *   we know the type of the variable we are converting to
     *   so we could save some time by calling the correct part
     *   of pl2sl(). Although not sure about Any_Type arrays
     *   in this scheme.
     */
    (void) SLang_load_string( "$1;" );
    UTIL_SLERR(
      SLang_push_string( fieldname ),
      "Unable to push a string onto the stack"
    );
    pl2sl( value );

    /*
     * this sort of a call can leak mem prior to S-Lang < 1.4.9 but I think we're
     * okay with this version. Any mem leaks in the struct code should first
     * check that S-Lang lib >= 1.4.9
     */
    (void) SLang_load_string( "$3=(); $2=(); $1=(); $1[$2] = $3;" );
    
  }

  SL_PUSH_ELEM1_ONTO_STACK(3);
  return;

} /* pl2sl_assoc() */

/*
 * pl2sl_array()
 * must be called with the S-Lang array in $1
 * - originally had hard-coded 1/2D routines and a generic
 *   support system for up to 7D data structure.
 *   Have moved to just using the generic system.
 *   The plan is to add support for arrays of particular
 *   types - ie those with a C API - and it's easier if
 *   we only have to code them once.
 *
 * Note:
 *   this is being written in such a way as to force users to use
 *   piddles for arrays wherever possible!
 *
 *   Need to update to take advantage of pl2sl_type()
 */
static void
pl2sl_array( AV *array, AV *dims ) {
  long dimsize[SLARRAY_MAX_DIMS], coord[SLARRAY_MAX_DIMS];
  AV *aref[SLARRAY_MAX_DIMS];

  SV *set_array_elem_sv;
  char *set_array_elem_str;
  SV **dval;

  long nelem;
  I32 maxdim, i, j;

  SLtype sl_type;
  int sl_flag;

  maxdim = av_len( dims ); /* count from 0 */

  /*
   * I think S-Lang arrays are limited to <= 7 [SLARRAY_MAX_DIMS]
   * - left check in in case this changes (the reason why we are
   *   limited to 7 is that we need to use 2 $x (ie temp) vars
   *   for the array and the value, which leaves a max of 7 for
   *   coordinates
   */
  Printf( ("  * converting %dD Array_Type array to S_Lang\n",maxdim+1) );

  if ( maxdim > 6 )
    croak( "Error: unable to convert an array of dimensionality %d\n", maxdim+1 );

  if ( maxdim == -1 )     {
    /* not a very useful array */
    SL_PUSH_ELEM1_ONTO_STACK(2);
    return;
  }

  /*
   * set up arrays for looping through the array
   */
  nelem = 1;
  for ( i = 0; i <= maxdim; i++ ) {
    SV **numsv = av_fetch( dims, i, 0 );
    long num = SvIV( *numsv );
    Printf( ("  *** dimension %d has size %d\n",i,num) );
    nelem *= num;
    dimsize[i] = num-1; /* want to start counting at 0 */
    coord[i] = 0;
    if ( i == 0 )
      aref[i] = array;
    else
      aref[i] = (AV *) SvRV( *av_fetch( aref[i-1], 0, 0 ) );
  }

  /*
   * this is truly not wonderful: set up the string that
   * pops the array, coordinates, and data value off the
   * S-Lang stack and fills in the array element
   * - *and* I'm too lazy to do this in C!
   */
  Printf( ("Calling Array_Type::_private_get_assign_string(%d)\n",maxdim) );
  {
    int count;
    dSP; ENTER; SAVETMPS; PUSHMARK(SP);
    XPUSHs( sv_2mortal(newSViv(maxdim)) );
    PUTBACK;
    count = call_pv( "Array_Type::_private_get_assign_string", G_SCALAR );
    SPAGAIN;
    if ( count != 1 )
      croak( "Internal error: unable to call _private_get_assign_string()\n" );
    set_array_elem_sv = SvREFCNT_inc( POPs );
    PUTBACK; FREETMPS; LEAVE;
  }
  set_array_elem_str = SvPV_nolen(set_array_elem_sv);
  Printf( ("set str = [%s]\n",set_array_elem_str) );

  /*
   * loop i=1 to nelem
   *   - from coord/aref arrays can get the data value from Perl
   *     and set the S-Lang value
   *   - increase coord/aref arrays to point to the next value
   *     [a recursive loop
   *      if last elem of coord array < ndims[last element]
   *        add 1 to it; update aref[last element]
   *      else
   *        reset last element to 0, repeat with previous
   *        coord element [possibly repeat]
   *        update the necessary aref elements
   *
   */
  dval = av_fetch( aref[maxdim], coord[maxdim], 0 );
  sl_type = pltype( *dval, &sl_flag );
  for ( i = 1; i < nelem; i++ ) {
    Printf( ("  **** Setting %dD array elem %d coord=[",maxdim+1,i) );

    /*
     * since we are about to call pl2l() we push $1 onto the stack
     * to protect it. Then we push the coordinates, and then the
     * current data value
     */
    (void) SLang_load_string( "$1;" );
    for( j = 0; j <= maxdim; j++ ) {
      Printf( (" %d",coord[j]) );
      UTIL_SLERR(
	SLang_push_integer(coord[j]),
        "Internal error: unable to push onto the stack"
      );
    }
    Printf( (" ] and coord[maxdim] = %d\n",coord[maxdim]) );
    dval = av_fetch( aref[maxdim], coord[maxdim], 0 );
    pl2sl_type( *dval, sl_type, sl_flag );

    /* now set the value (also resets $1 to be the array) */
    (void) SLang_load_string( set_array_elem_str );

    /* update the pointer */
    if ( coord[maxdim] < dimsize[maxdim] ) coord[maxdim]++;
    else {
      Printf( ("+++ start: loop to upate coords/array refs\n") );
      /*
       * loop through each previous coord until we find
       * one with 'coord[j] < dimsize[j]', increase it
       * and then reset the 'higher dim' coord/aref values
       */
      j = maxdim - 1;
      while ( coord[j] == dimsize[j] ) { j--; }
      Printf( ("++++++++ got to dim #%d with coord=[%d]\n",j,coord[j]) );
      coord[j]++;
      if ( j )
        aref[j] = (AV *) SvRV( *av_fetch( aref[j-1], coord[j-1], 0 ) );
      j++;
      while ( j <= maxdim ) {
	Printf( ("++++++ resetting dim #%d to 0\n",j) );
	coord[j] = 0;
	aref[j] = (AV *) SvRV( *av_fetch( aref[j-1], coord[j-1], 0 ) );
	j++;
      }
      Printf( ("+++ finished coords/array refs update\n") );
    } /* if: coord[maxdim] == dimsize[maxdim] */

  } /* for: i=1 .. nelem-1

  /* handle the last element */
  Printf( ("  **** Setting %dD array elem %d coord=[",maxdim+1,nelem) );
  (void) SLang_load_string( "$1;" );
  for( j = 0; j <= maxdim; j++ ) {
    Printf( (" %d",coord[j]) );
    UTIL_SLERR(
      SLang_push_integer(coord[j]),
      "Internal error: unable to push onto the stack"
    );
  }
  Printf( (" ] [[last element]]\n") );
  dval = av_fetch( aref[maxdim], coord[maxdim], 0 );
  pl2sl_type( *dval, sl_type, sl_flag );
  (void) SLang_load_string( set_array_elem_str );

  SL_PUSH_ELEM1_ONTO_STACK(maxdim+3);
  SvREFCNT_dec( set_array_elem_sv ); /* free up mem */
  return;

} /* pl2sl_array() */

void
pl2sl_type( SV *item, SLtype item_type, int item_flag ) {

  if ( item_type == SLANG_NULL_TYPE ) {
    Printf( ("item=undef\n") );
    UTIL_SLERR(
      SLang_push_null(),
      "Error: unable to push a null onto the S-Lang stack"
    );
    return;
  } /* undef */

  if ( item_type == SLANG_INT_TYPE ) {
    Printf( ("item=integer %d\n", SvIV(item)) );
    UTIL_SLERR(
      SLang_push_integer( SvIV(item) ),
      "Error: unable to push an integer onto the S-Lang stack"
    );
    return;
  } /* integer */

  if ( item_type == SLANG_DOUBLE_TYPE ) {
    Printf( ("item=float %f\n", SvNV(item)) );
    UTIL_SLERR(
      SLang_push_double( SvNV(item) ),
      "Error: unable to push a floating-point number onto the S-Lang stack"
    );
    return;
  } /* floating-point */

  if ( item_type == SLANG_STRING_TYPE ) {
    STRLEN len;
    char *ptr = SvPV(item, len);
    Printf(("string: %s\n", ptr));
    UTIL_SLERR(
      SLang_push_string( ptr ),
      "Error: unable to push a string onto the S-Lang stack"
    );
    return;
  } /* string */

  if ( item_type == SLANG_COMPLEX_TYPE ) {
    double real, imag;

    Printf( ("*** converting Perl's Math::Complex to S-Lang Complex_Type\n") );

    /* call the Re and Im methods */
    CALL_METHOD_SCALAR_DOUBLE( item, "Re", , real );
    CALL_METHOD_SCALAR_DOUBLE( item, "Im", , imag );

    /* push the complex number onto the S-Lang stack */
    UTIL_SLERR(
      SLang_push_complex( real, imag ),
      "Error: unable to push a complex number onto the S-Lang stack"
    );
    return;
  } /* Math::Complex */

  if ( item_type == SLANG_DATATYPE_TYPE ) {
    char *name;

    Printf( ("*** converting DataType_Type to S-Lang Datatype_Type\n") );

    /* de-reference the object (we can do this since it's our class) */
    name = SvPV_nolen( SvRV(item) );

    /*
     * now, we have the "printed" name of the datatype which we need to
     * convert to the S-Lang datatype. All we do is push the name
     * onto the stack and let S-Lang do the conversion to an
     * actual DataType_Type
     * - not the most efficient implementation but saves messing
     *   with the internals of S-Lang
     */
    (void) SLang_load_string( name );
    return;
  } /* DataType_Type */

  if ( item_type == SLANG_STRUCT_TYPE ) {
    SV *dstruct;
    SV *object;
    HV *hash;
    I32 nfields, i;

    Printf( ("*** converting Perl struct to S-Lang\n") );

    /*
     * create a structure in $1 with the correct fields
     * - once the string has been used we can decrease the
     *   reference count to ensure it is freed
     */
    CALL_METHOD_SCALAR_SV( item, "_define_struct", , dstruct );
    Printf( ("struct definition =\n[%s]\n", SvPV_nolen(dstruct)) );
    (void) SLang_load_string( SvPV_nolen(dstruct) );
    SvREFCNT_dec( dstruct );

    /*
     * get the hash used to store the actual data
     */
    CALL_METHOD_SCALAR_SV( item, "_private_get_hashref", , object );
    object = sv_2mortal( object );
    hash = (HV *) SvRV( object );

    /*
     * loop through the keys in the Perl hash and set the corresponding 
     * value in the S-Lang struct
     */
    nfields = hv_iterinit( hash );
    Printf( ("  struct contains %d fields\n",nfields) );
    for ( i = 0; i < nfields; i++ ) {
      HE *next;
      SV *value;
      char *fieldname;
      I32 ignore;

      /* get the next key/value pair from the hash */
      value = hv_iternextsv( hash, &fieldname, &ignore );
      Printf( ("  - field %d/%d name=[%s]\n",i,nfields-1,fieldname) );

      /* 
       * push $1 [in case pl2sl() trashes it], the field name,
       * and then the Perl value (converted to S-Lang) onto the
       * S-Lang stack
       */
      (void) SLang_load_string( "$1;" );
      UTIL_SLERR(
	SLang_push_string( fieldname ),
	"Unable to push a string onto the stack"
      );
      pl2sl( value );

      /*
       * this sort of a call can leak mem prior to S-Lang < 1.4.9 but I think we're
       * okay with this version. Any mem leaks in the struct code should first
       * check that S-Lang lib >= 1.4.9
       */
      (void) SLang_load_string(
	 "$3=(); $2=(); $1=(); set_struct_field( $1, $2, $3 );"
      );

    }

    SL_PUSH_ELEM1_ONTO_STACK(3);
    return;

  } /* Struct_Type */

  if ( item_type == SLANG_ASSOC_TYPE ) {
    HV *hash;

    if ( item_flag ) {
      SV *typename;
      SV *object;
      Printf( ("*** converting Perl Assoc_Type object to S-Lang\n") );

      /*
       * create the array with the correct type
       *
       * TODO: [low priority]
       *   Newz() to create a char * large enough to contain
       *     '$1 = Assoc_Type[%s];', typename
       *   and then SLang_load_string() that
       */
      CALL_METHOD_SCALAR_SV( item, "_private_get_typeof", , typename );
      Printf( ("  assoc type = [%s]\n", SvPV_nolen(typename)) );
      (void) SLang_load_string( SvPV_nolen(typename) );
      (void) SLang_load_string( "$2=(); $1 = Assoc_Type [$2];" );
      SvREFCNT_dec( typename );

      /*
       * get the hash used to store the actual data
       */
      CALL_METHOD_SCALAR_SV( item, "_private_get_hashref", , object );
      object = sv_2mortal( object );
      hash = (HV *) SvRV( object );

    } else {
      /*
       * hash ref: follow Assoc_Type object handling above - we convert
       * to an 'Assoc_Type [Any_Type];' array since we can't be sure
       * about the type without looping through all the keys
       */
      Printf( ("*** converting Perl {...} object to S-Lang\n") );

      /* create the assoc array in $1 */
      (void) SLang_load_string( "$1 = Assoc_Type [Any_Type];" );

      /* iterate through the hash, filling in the values */
      hash = (HV*) SvRV( item ); // sv_2mortal ???

    }

    /* and delegate all the complicated stuff */
    pl2sl_assoc( hash );
    return;

  } /* hash */

  /* since the array handling needs work on we split up the two for now */
  if ( item_type == SLANG_ARRAY_TYPE && item_flag ) {
    SV *arraystr;
    SV *arrayref, *dimsref;
    AV *array, *dims;

    Printf( ("*** converting Perl Array_Type object to S-Lang\n") );

    /*
     * create the array with the correct type & dims in $1
     */
    CALL_METHOD_SCALAR_SV( item, "_private_define_array", , arraystr );
    Printf( ("  array definition = [%s]\n", SvPV_nolen(arraystr)) );
    (void) SLang_load_string( SvPV_nolen(arraystr) );
    SvREFCNT_dec( arraystr );

    /*
     * get the array reference used to store the actual data
     * and the array dimensions [could do in one call]
     */
    CALL_METHOD_SCALAR_SV( item, "_private_get_arrayref", , arrayref );
    arrayref = sv_2mortal( arrayref );
    array = (AV *) SvRV( arrayref );

    CALL_METHOD_SCALAR_SV( item, "_private_get_dims", , dimsref );
    dimsref = sv_2mortal( dimsref );
    dims = (AV *) SvRV( dimsref );

    /*
     * and delegate all the complicated stuff, including pushing
     * the array back onto the S-Lang stack and clearing $1..$n
     */
    pl2sl_array( array, dims );
    return;

  } /* Array_Type */

  if ( item_type == SLANG_ARRAY_TYPE ) {
    int dimsize[SLARRAY_MAX_DIMS];

    AV *array = (AV*) SvRV(item);
    AV *temp;
    AV *dims;

    SLang_Array_Type *sl_dims;
    SLtype dtype;
    int i, ndims, nelem, dtype_flag;

    /*
     * an array reference
     * - we have to guess the array dimensions and data type
     *   the current algorithm is LESS THAN OPTIMAL
     *   eg given [ [ 1, 2 ], "foo" ] it should return Any_Type [1]
     *   but it will assume Integer_Type [2]
     *   also  something like [ 1, 2.3, "foo" ] is prob best
     *   converted as a String_Type array - this code selects Integer_Type
     * >>>> will change at some point but not a high priority just now <<<<
     *
     * - see Array_Type
     *
     */

    for ( i = 0; i < SLARRAY_MAX_DIMS; i++ ) dimsize[i] = 0;

    array = (AV*) SvRV(item);
    Printf( ("*** converting Perl array ref to ") );

    /*
     * what is the data type and array size?
     * ALGORITHM SHOULD BE MORE CLEVERERER
     */
    ndims = 0;
    dimsize[ndims] = av_len(array) + 1;
    nelem = dimsize[ndims];
    temp  = array;
    Printf( ("[%d]",dimsize[ndims]) );
    ndims++;

    fixme( "think dimension handling is wrong" );

    while ( 1 ) {
      SV *val = *av_fetch( temp, 0, 0 );
      if ( SvROK(val) && SvTYPE(SvRV(val)) == SVt_PVAV ) {
	if ( ndims == SLARRAY_MAX_DIMS )
	  croak( "Error: Max array dimension for S-Lang is %d.\n", SLARRAY_MAX_DIMS );
	temp = (AV *) SvRV(val);
	dimsize[ndims] = av_len(temp) + 1;
	nelem *= dimsize[ndims];
	Printf( ("[%d]", dimsize[ndims]) );
	ndims++;
      } else {
	/* found a non-array element: guess its data type */
	dtype = pltype( val, &dtype_flag );
	break;
      }
    }

    /*
     * create a Perl array containing the array dimensions
     * - I think I need to re-work pl2sl_array()!
     */
    dims = (AV *) sv_2mortal( (SV *) newAV() );
    av_extend( dims, ndims );
    for ( i=0; i<ndims; i++ ) {
      Printf( (" Hack: setting dimsize[%d] = %d\n",i,dimsize[i]) );
      av_store( dims, i, newSViv(dimsize[i]) );
    }

    Printf( (" %s [%d dim] array - nelem=%d\n",
	     SLclass_get_datatype_name(dtype), ndims, nelem) );

    /*
     * create the array in $1; $2 = datatype and $3 = array dims
     */
    UTIL_SLERR(
      SLang_push_datatype(dtype),
      "Internal error: unable to push datatype name onto the S-Lang stack"
    );
    sl_dims = SLang_create_array( SLANG_INT_TYPE, 0, NULL, &ndims, 1 );
    if ( sl_dims == NULL )
      croak("Internal error: unable to make S-Lang int array.");
    for ( i = 0; i < ndims; i++ ) {
      if ( -1 == SLang_set_array_element( sl_dims, &i, &dimsize[i] ) )
	croak("Internal error: unable to set element of S-Lang int array.");
    }
    UTIL_SLERR(
      SLang_push_array( sl_dims, 1 ),
      "Internal error: unable to push array onto the S-Lang stack."
    );
    (void) SLang_load_string( "$3=();$2=(); $1 = @Array_Type($2,$3);" );

    /*
     * and delegate all the complicated stuff, including pushing
     * the array back onto the S-Lang stack and clearing $1..$n
     */
    pl2sl_array( array, dims );
    return;

  } /* array reference */

  /*
   * if we've got this far then assume we're a type that Perl can't handle
   * directly (ie the S-Lang data is actually stored in_inline->_store[])
   *
   * Perhaps we need to add a routine to the _Type class to indicate
   * this condition?
   */
  Printf( ("*** converting Perl _Type object to S-Lang\n") );
  pl2sl( SvRV(item) );
  (void) SLang_load_string( "$1 = (); _inline->_push_data( $1 );" );
  _clean_slang_vars(1);

} /* pl2sl() */

/*
 * convert perl variables to S-Lang variables
 *
 * note: we automatically push each variable onto the S-Lang stack
 * - this will probably turn out to be a bad idea; for instance it
 *   means it can't be called recursively when converting
 *   array/associative arrays.
 *
 * - we croak for those types we do not recognise [in pltype]
 */

void
pl2sl( SV *item ) {
  SLtype item_type;
  int    item_flag;

  item_type = pltype( item, &item_flag );
  pl2sl_type( item, item_type, item_flag );

}

#ifdef OLDCODE

/* kept for reference for the moment */

/*
 * utility routines for sl2pl()
 *
 * sl2pl1D() - convert a 1D array into a perl array reference
 * sl2pl2D() - convert a 2D array into a perl array reference
 * sl2plND() - convert a nD array into a perl array reference
 *
 */

static AV *
sl2pl1D( SLang_Array_Type *at ) {
  AV *parray;
  int i;

  Printf( ("*** in sl2pl1D\n*" ) );

  /* create the perl array */
  parray = (AV *) sv_2mortal( (SV *) newAV() );
  av_extend( parray, (I32) at->num_elements );

  /*
   * fill in the data from the array
   * Q: should we be checking the return value of SLang_get_array_element() ?
   */
  switch( at->data_type ) {

    SL2PL_ARRAY1D_ITYPE( CHAR,  char )
    SL2PL_ARRAY1D_ITYPE( SHORT, short )
    SL2PL_ARRAY1D_ITYPE( INT,   int )
    SL2PL_ARRAY1D_ITYPE( LONG,  long )

    SL2PL_ARRAY1D_FTYPE( FLOAT,  float )
    SL2PL_ARRAY1D_FTYPE( DOUBLE, double )

  case SLANG_COMPLEX_TYPE:
    {
      SV *elem;
      double data[2];

      /* need to check this */
      fixme( "assuming Complex_Type elements of 1D arrays returned as foo[2] arrays." );

      for ( i = 0; i < at->num_elements; i++ ) {

        (void) SLang_get_array_element( at, &i, data );
	Printf( ("  complex array1D[%d] = %g + %g i\n", i, data[0], data[1] ) );

	CALL_METHOD_SCALAR_SV(
	     sv_2mortal(newSVpv("Math::Complex",0)),
             "make",
	     C2PL_MARG_D( data[0] ); C2PL_MARG_D( data[1] );,
             elem );

	/* do we need to bother with memory management here? */
	av_store( parray, i, elem );
      }
      break;
    } /* complex */

  case SLANG_STRING_TYPE:
    {
      Printf( ("* array of strings\n") );

      /*
       * thanks to John Davis for pointing out my incorrect pointer
       * use in get_array_element()
       */
      for ( i = 0; i < at->num_elements; i++ ) {
	char *string;
        (void) SLang_get_array_element( at, &i, &string );
	Printf( ( "array string %d = [%s]\n", i, string ) );
	av_store( parray, i, newSVpv( string, 0 ) );
	SLang_free_slstring( string );
      }

      break;
    } /* string */

  case SLANG_DATATYPE_TYPE:
    {
      int dtype; /* datatypes stored as integers not as SLtype */

      Printf( ("* array of datatypes\n") );

      for ( i = 0; i < at->num_elements; i++ ) {

      	(void) SLang_get_array_element( at, &i, &dtype );

      	Printf( ("array datatype %d = [%d] [name=%s]\n",
		 i, dtype, SLclass_get_datatype_name(dtype)) );

	/* leaks if use newRV [==newRV_inc] */
      	av_store( parray, i,
      		  sv_bless(
			   newRV_noinc( newSVpv( SLclass_get_datatype_name(dtype), 0 ) ),
			   gv_stashpv("DataType_Type",1) )
      		  );

      } /* for: i */

      break;
    } /* datatype */

  default:
    croak( "Error: 1D arrays of type %s are currently not supported\n", 
	   SLclass_get_datatype_name(at->data_type) );

  } /* switch: at->data_type */

  /* return the array */
  return parray;

} /* sl2pl1D */

static AV *
sl2pl2D( SLang_Array_Type *at ) { 

  AV *xarray, *yarray;
  int dims[2], nx, ny, i, j;

  Printf( ("*** in sl2pl2D\n" ) );
  nx = at->dims[0];
  ny = at->dims[1];

  /* create the perl array */
  xarray = (AV *) sv_2mortal( (SV *) newAV() );
  av_extend( xarray, (I32) nx );

  /*
   * fill in the data from the array
   * Q: should we be checking the return value of SLang_get_array_element() ?
   */
  switch( at->data_type ) {

    SL2PL_ARRAY2D_ITYPE( CHAR,  char )
    SL2PL_ARRAY2D_ITYPE( SHORT, short )
    SL2PL_ARRAY2D_ITYPE( INT,   int )
    SL2PL_ARRAY2D_ITYPE( LONG,  long )

    SL2PL_ARRAY2D_FTYPE( FLOAT,  float )
    SL2PL_ARRAY2D_FTYPE( DOUBLE, double )

  case SLANG_STRING_TYPE:
    {
      char *string;
      Printf( ("* 2D array of strings\n") );

      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;

	  (void) SLang_get_array_element( at, dims, &string );
	  Printf( ("  string array2D[%d,%d] = %s\n", i, j, string ) );
	  av_store( yarray, j, newSVpv( string, 0 ) );
	  SLang_free_slstring( string );

	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* STRINGS-2D */

  case SLANG_COMPLEX_TYPE:
    {
      double data[2];
      SV *elem;

      /* need to check this */
      fixme( "assuming Complex_Type elements of 2D arrays returned as foo[2] arrays." );

      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;
	  
	  (void) SLang_get_array_element( at, dims, data );
	  Printf( ("  complex array2D[%d,%d] = %g + %g i\n", i, j, data[0], data[1] ) );

	  /* do we need to bother with memory management here? */
          CALL_METHOD_SCALAR_SV(
	     sv_2mortal(newSVpv("Math::Complex",0)),
             "make",
	     C2PL_MARG_D( data[0] ); C2PL_MARG_D( data[1] );,
             elem );
	  av_store( yarray, j, elem );
	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* COMPLEX-2D */

  case SLANG_DATATYPE_TYPE:
    {
      int dtype; /* datatypes stored as integers not as SLtype */

      Printf( ("* 2D array of datatypes\n") );

      for ( i = 0; i < nx; i++ ) {
	yarray = (AV *) sv_2mortal( (SV *) newAV() );
	av_extend( yarray, (I32) ny );

	dims[0] = i;
	for ( j = 0; j < ny; j++ ) {
	  dims[1] = j;

	  (void) SLang_get_array_element( at, dims, &dtype );

	  Printf( ("  datatype array2D[%d,%d] = %d [%s]\n",
		   i, j, dtype, SLclass_get_datatype_name(dtype) ) );

	  av_store( yarray, j,
		    sv_bless(
			     newRV_inc( newSVpv( SLclass_get_datatype_name(dtype), 0 ) ),
			     gv_stashpv("DataType_Type",1) )
		    );

	} /* for: j */

	av_store( xarray, i, newRV_inc( (SV *) yarray ) );

      } /* for: i */

      break;
    } /* DATATYPE-2D */

  default:
    croak( "Error: 2D arrays of type %s are currently not supported\n", 
	   SLclass_get_datatype_name(at->data_type) );

  } /* switch: at->data_type */

  /* return the array */
  return xarray;

} /* sl2pl2D() */

#endif /* OLDCODE */

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
		    
/*
 * implement support for ND arrays using a generic interface
 * - ie do not use the C API as but use S-Lang itself -
 * which is not as efficient but handles everything.
 * Support for specific types can be added later
 *
 * NOTE:
 *   calling pl2sl() is a bit of a waste since we
 *   know the input type (ie the array type). A future
 *   improvement would be to allow the conversion to
 *   have a specified type [or some such thang; similar to
 *   the way we do the perl to slang conversion]
 *
 * NOTE:
 *   the following algorithm is a mess since we have the array
 *   both on the stack and in C scope
 *
 * S-Lang's dimensions are stored in int arrays (at least in 1.4.9)
 *
 */
static SV *
sl2pl_array_aref( SLang_Array_Type *at ) {

  AV *aref[SLARRAY_MAX_DIMS];
  int dimsize[SLARRAY_MAX_DIMS], coord[SLARRAY_MAX_DIMS];

  SV *arrayref = NULL;

  SV *get_array_elem_sv;
  char *get_array_elem_str;
  SV *dval;

  SLtype dtype = at->data_type;
  int    nelem = at->num_elements;
  int    ndims = at->num_dims;
  int   *dims  = at->dims;

  int maxdim = ndims - 1;
  int i, j;

  /*
   * set up the arrays for the loop
   *  1 - the actual data array
   *  2 - the arrays used to loop through it
   */
  arrayref = _create_empty_array( ndims, dims );

  for ( i = 0; i < ndims; i++ ) {
    Printf( ("  *** dimension %d has size %d\n",i,dims[i]) );
    coord[i]   = 0;
    dimsize[i] = dims[i] - 1;
    if ( i )
      aref[i] = (AV *) SvRV( *av_fetch( aref[i-1], 0, 0 ) );
    else
      aref[i] = (AV *) SvRV( arrayref );
  } 

  /*
   * this is truly not wonderful: set up the string that
   * pops the array and coordinates off the
   * S-Lang stack and returns the value of the corresponding array element
   * - *and* I'm too lazy to do this in C!
   */
  Printf( ("Calling Array_Type::_private_get_read_string(%d)\n",maxdim) );
  {
    int count;
    dSP; ENTER; SAVETMPS; PUSHMARK(SP);
    XPUSHs( sv_2mortal(newSViv(maxdim)) );
    PUTBACK;
    count = call_pv( "Array_Type::_private_get_read_string", G_SCALAR );
    SPAGAIN;
    if ( count != 1 )
      croak( "Internal error: unable to call _private_get_read_string()\n" );
    get_array_elem_sv = SvREFCNT_inc( POPs );
    PUTBACK; FREETMPS; LEAVE;
  }
  get_array_elem_str = SvPV_nolen(get_array_elem_sv);
  Printf( ("get str = [%s]\n",get_array_elem_str) );

  /*
   * We need the array in $1 with the current code,
   * so we have to push it back onto the stack but we do NOT
   * free at
   */
  UTIL_SLERR(
    SLang_push_array( at, 0 ),
    "Internal error - unable to push array onto the S-Lang stack"
  );
  (void) SLang_load_string( "$1=();" );

  /*
   * loop i=1 to nelem - see pl2sl_array() for more details
   */
  for ( i = 1; i < nelem; i++ ) {
    Printf( ("  **** Setting %dD array elem %d coord=[",ndims,i) );

    /*
     * since we are about to call sl2pl() we push $1 onto the stack
     * to protect it. Then we push the coordinates, and then the
     * current data value
     */
    for( j = 0; j < ndims; j++ ) {
      Printf( (" %d",coord[j]) );
      UTIL_SLERR(
	SLang_push_integer(coord[j]),
        "Internal error: unable to push onto the stack"
      );
    }
    Printf( (" ] and coord[maxdim] = %d\n",coord[maxdim]) );

    /* now get the value, convert to Perl, and store [also pushes array onto stack */
    (void) SLang_load_string( get_array_elem_str );
    dval = sl2pl();
    av_store( aref[maxdim], coord[maxdim], dval );

    /* restore $1 to be the array */
    (void) SLang_load_string( "$1=();" );

    /* update the pointer */
    if ( coord[maxdim] < dimsize[maxdim] ) coord[maxdim]++;
    else {
      Printf( ("+++ start: loop to upate coords/array refs\n") );
      /*
       * loop through each previous coord until we find
       * one with 'coord[j] < dimsize[j]', increase it
       * and then reset the 'higher dim' coord/aref values
       */
      j = maxdim - 1;
      while ( coord[j] == dimsize[j] ) { j--; }
      Printf( ("++++++++ got to dim #%d with coord=[%d]\n",j,coord[j]) );
      coord[j]++;
      if ( j )
        aref[j] = (AV *) SvRV( *av_fetch( aref[j-1], coord[j-1], 0 ) );
      j++;
      while ( j <= maxdim ) {
	Printf( ("++++++ resetting dim #%d from %d to 0 (prev dimension val=%d)\n",
		 j,coord[j],coord[j-1]) );
	coord[j] = 0;
	aref[j] = (AV *) SvRV( *av_fetch( aref[j-1], coord[j-1], 0 ) );
	j++;
      }
      Printf( ("+++ finished coords/array refs update\n") );
    } /* if: coord[maxdim] == dimsize[maxdim] */

  } /* for: i=1 .. nelem-1

  /* handle the last element */
  Printf( ("  **** Setting %dD array elem %d coord=[",ndims,nelem) );
  for( j = 0; j <= maxdim; j++ ) {
    Printf( (" %d",coord[j]) );
    UTIL_SLERR(
      SLang_push_integer(coord[j]),
      "Internal error: unable to push onto the stack"
    );
  }
  Printf( (" ] [[last element]]\n") );

  /* now get the value, convert to Perl, and store */
  (void) SLang_load_string( get_array_elem_str );
  dval = sl2pl();
  av_store( aref[maxdim], coord[maxdim], dval );
  (void) SLang_load_string("$1=();"); /* clean up the stack */

  _clean_slang_vars(maxdim+2);
  SvREFCNT_dec( get_array_elem_sv );
  return arrayref;

} /* sl2pl_array_aref() */

/*
 * to reduce replicated code we delegate most of the conversion
 * to sl2pl_array_aref() and then convert the array reference into
 * an Array_Type object. It's somewhat wasteful since we have
 * to find the dimensions and datatype again (especially as I'm
 * just relying on sl_array() to do this in Perl).
 */
static SV *
sl2pl_array_atype( SLang_Array_Type *at ) {
  SV *aref = sl2pl_array_aref(at);
  SV *obj;

  /***
      Should create the dims and datatype values and send to sl_array
      so that things are converted properly (otherwise
      UChar_Type -> Integer_Type etc)
  ***/

  Printf( ("Calling Inline::SLang::sl_array() to convert to Array_Type\n") );
  {
    int count;
    dSP; ENTER; SAVETMPS; PUSHMARK(SP);
    fixme( "memleaks?" );
    //    XPUSHs( sv_2mortal(newSViv(maxdim)) );
    XPUSHs( aref );
    XPUSHs( sv_2mortal(newSVpv(SLclass_get_datatype_name(at->data_type),0)) );
    PUTBACK;
    count = call_pv( "Inline::SLang::sl_array", G_SCALAR );
    SPAGAIN;
    if ( count != 1 )
      croak( "Internal error: unable to call Inline::SLang::sl_array()\n" );
    fixme( "memleak?" );
    obj = SvREFCNT_inc( POPs );
    PUTBACK; FREETMPS; LEAVE;
  }

  return obj;
} /* sl2pl_array_atype() */

static SV *
sl2pl_array_pdl( SLang_Array_Type *at ) {
  croak( "ERROR: PDL support is not yet available, whatever the docs might say!" );
} /* sl2pl_array_pdl() */

/*
 * Convert S-Lang structs - including type-deffed ones
 * to Perl <> objects
 *
 * If we were just bothered about S-lang structs - ie not the
 * type-deffed ones - then we could just have this code directly
 * in sl2pl() within a "case SLANG_STRUCT_TYPE: {}".
 * However, as I don't know how we can easily tell whether an
 * item on the S_Lang stack is a type-deffed structure we go
 * with this method
 *
 * We are called with a S-Lang structure in $1
 */
SV *
sl2pl_struct(void) {
  char *stype;
  SV *tied_object, *object;
  HV *hash;
  SV *fieldsref;
  AV *fields;
  int i, nfields;
  int a2p_flag;

  Printf( ("  stack contains: structure - ") );

  /*
   * get the Perl class name for this structure
   * (let S-Lang bother with the string handling)
   */
  (void) SLang_load_string( "string(typeof($1));" );
  UTIL_SLERR(
    SLang_pop_slstring(&stype), 
    "Error: unable to get datatype of a structure\n"
  );
  Printf( ("it's type is %s\n",stype) );

  /*
   * - handle similarly to associative arrays, in that
   *   we take advantage of the S-Lang stack
   * - can't guarantee that $1 isn't going to get trashed when
   *   converting the array of strings, so we push it on
   *
   */
  (void) SLang_load_string( "$1;get_struct_field_names($1);" );

  /*
   * convert the item on the stack (ie the field names) to a perl array
   *
   * NOTE:
   *   since we are going to convert an array from S-Lang to
   *   Perl we need to ensure that we do it as a array ref
   *   whatever the user actually wants (and set it back later)
   */
  a2p_flag = _slang_array_format;
  _slang_array_format = 0;
  fieldsref = sv_2mortal( sl2pl() );
  _slang_array_format = a2p_flag;
  fields = (AV *) SvRV( fieldsref );
  nfields = 1 + av_len( fields );
  Printf( ("Number of fields in the structure = %d\n", nfields ) );

  (void) SLang_load_string( "$1=();" );

  /*
   * create the <XXX> object and then get the underlying structure
   * used to implement the tied hash. object is a reference to the
   * hash that stores the data [ie it's not the full Struct_Type
   * implementation 'object' which is an array reference]
   */
  CALL_METHOD_SCALAR_SV( sv_2mortal(newSVpv(stype,0)), 
			 "new", XPUSHs(fieldsref);, tied_object );
  CALL_METHOD_SCALAR_SV( tied_object, "_private_get_hashref", , object );
  object = sv_2mortal( object );
  hash = (HV *) SvRV( object );

  /*
   * loop through each field: push its value onto the S-Lang stack, convert
   * it to a Perl SV *, and store in the Perl hash
   *
   * Since we call sl2pl() - which may trash $1 - we need to protect the value
   * in $1 by pushing it onto the stack prior to the sl2pl() call and then
   * popping it back again afterwards. Not really memory/time efficient
   */
  for ( i = 0; i<nfields; i++ ) {
    SV **name;
    SV *value;
    char *fieldname;
    
    /* get the field name */
    name = av_fetch( fields, (I32) i, 0 );
    fieldname = SvPV_nolen( *name );
    Printf( ("struct field name %d/%d = [%s]\n", i, nfields-1, fieldname) );

    UTIL_SLERR(
      SLang_push_string( fieldname ),
      "Internal error - Unable to push name of struct field onto stack"
    );
    (void) SLang_load_string( "$2=(); $1; get_struct_field($1,$2);" );
    value = sl2pl();

    /*
     * if value = undef [ie S-Lang value == NULL] then leave alone
     * since calling hv_store with an undef value seems to delete
     * the key from the hash
     *
     * should we check for failure/NULL from hv_store?
     */
    if ( SvOK(value) )
      hv_store( hash, fieldname, strlen(fieldname), value, 0 );
    else
      SvREFCNT_dec( value );

    (void) SLang_load_string( "$1=();" );

    Printf( ("  and finished with struct field %d/%d [%s]\n", i, nfields-1,
	     fieldname) );

  } /* for: i */
  
  /* free up memory/clean-up vars */
  SLang_free_slstring( stype );
  _clean_slang_vars(2);
  return tied_object;

} /* sl2pl_struct() */

/*
 * Handle S-Lang variables for which we 
 * consider the type to be "opaque" in Perl scope - ie
 * you can assign it to a variable and send it back to S-Lang
 * but there's not a lot else you can do with it.
 * To do this we store the variable in the _inline namespace
 * and return the index string for that variable. This
 * variable gets converted to a Perl object of class
 * <typeof S-Lang variable>, which inherits
 * from Inline::SLang::_Type.
 * See the definition of the _inline namespace in SLang.pm
 * (created during the load phase of processing)
 *
 * We are called with a S-Lang variable in $1
 */
SV *
sl2pl_opaque(void) {
  char *sltype;
  char *slkey;
  SV *perlobj;

  (void) SLang_load_string( "_inline->_store_data( $1 );" );
  UTIL_SLERR(
     SLang_pop_slstring(&slkey),
    "Error: unable to store S-Lang data"
  );
  UTIL_SLERR(
     SLang_pop_slstring(&sltype),
    "Error: unable to store S-Lang data"
  );
  _clean_slang_vars(1);
  Printf( ("Storing S-Lang type %s using key %s\n", sltype, slkey) );

  /*
   * Now create an object of the right type
   */
  CALL_METHOD_SCALAR_SV(
			sv_2mortal(newSVpv(sltype,0)),
			"new",
			C2PL_MARG_S( slkey ),
			perlobj );
  return perlobj;

} /* sl2pl_opaque() */

/*
 * Is the S-Lang datatype something that we can convert
 * to a piddle?
 *
 * - need to sort out datatype sizes for piddles and
 *   S-Lang numeric types
 * - and perhaps we should return the type (rather than just
 *   a yes/no here)?
 */
static int
_convertable_to_pdl( SLtype type ) {
  fixme( "need to write this!!!!!" );
  return 0;
}

/*
 * convert S-Lang variables to perl variables
 */

SV *
sl2pl_type( SLtype type ) {

  /*
   * handle the various types
   * - having separate items for all the "integer" types is
   *   probably OTT
   */
  switch( type ) {

  case SLANG_NULL_TYPE:
    {
      /* return an undef */
      Printf( ("  stack contains: NULL\n") );
      /* clear out the stack, assume it works */
      (void) SLdo_pop_n(1);
      return &PL_sv_undef;
    } /* NULL */

    /* integers */
    SL2PL_ITYPE( CHAR,  char,    char )
    SL2PL_ITYPE( SHORT, short,   short )
    SL2PL_ITYPE( INT,   integer, int )
    SL2PL_ITYPE( LONG,  long,    long )

  case SLANG_FLOAT_TYPE:
    {
      float fval;
      UTIL_SLERR(
	SLang_pop_float( &fval ),
	"Error: unable to read float value from the stack\n"
      );
      Printf( ("  stack contains: float = %g\n", fval ) );
      return newSVnv(fval);
    }

  case SLANG_DOUBLE_TYPE:
    {
      double dval;
      UTIL_SLERR(
	SLang_pop_double( &dval, NULL, NULL ),
	"Error: unable to read double value from the stack\n"
      );
      Printf( ("  stack contains: double = %g\n", dval ) );
      return newSVnv(dval);
    }

  case SLANG_STRING_TYPE:
    {
      SV *out;
      char *sval;
      UTIL_SLERR(
        SLang_pop_slstring(&sval),
	"Error: unable to read a string from the stack\n"
      );
      Printf( ("  stack contains: string = %s\n", sval ) );
      out = newSVpv( sval, 0 );
      SLang_free_slstring( sval );
      return out;
    }

  case SLANG_COMPLEX_TYPE:
    {
      /*
       * store as a Math::Complex object
       */
      SV *object;
      double real, imag;

      UTIL_SLERR(
        SLang_pop_complex( &real, &imag ),
	"Error: unable to read complex value from the stack\n"
      );
      Printf( ("  stack contains: complex %g + %g i\n", real, imag ) );

      CALL_METHOD_SCALAR_SV(
       	 sv_2mortal(newSVpv("Math::Complex",0)),
         "make",
	 C2PL_MARG_D( real ); C2PL_MARG_D( imag );,
         object );

      return object;

    } /* COMPLEX */

  case SLANG_ARRAY_TYPE:
    {
      SV *out;
      SLang_Array_Type *at = NULL;
      int is_numeric;

      Printf( ("  S-Lang stack contains: array  ") );

      UTIL_SLERR(
	SLang_pop_array( &at, 0 ),
	"Internal error - unable to pop duplicated array off the stack"
      );
      Printf( (" num dims=%d  nelem=%d  type=%s\n",
	       at->num_dims, at->num_elements,
	       SLclass_get_datatype_name(at->data_type)) );

      /*
       * Output is one of the following - determined by the
       * value of the variable _slang_array_format:
       *
       *       Non-numeric        Numeric
       *   0 - array ref          -
       *   1 - Array_Type         -
       *   2 - array ref          piddle
       *   3 - Array_Type         piddle
       *
       * could do comparison by bit manipulation
       */

      switch ( _slang_array_format ) {
        case 0:
	  out = sl2pl_array_aref( at );
          break;
        case 1:
	  out = sl2pl_array_atype( at );
          break;
        case 2:
          is_numeric = _convertable_to_pdl( at->data_type );
          if ( is_numeric )
	    out = sl2pl_array_pdl( at );
          else
	    out = sl2pl_array_aref( at );
          break;
        case 3:
          is_numeric = _convertable_to_pdl( at->data_type );
          if ( is_numeric )
	    out = sl2pl_array_pdl( at );
          else
	    out = sl2pl_array_atype( at );
          break;
      }

      /*
       * can free up the array now (although will want to keep it around
       * once we re-implement the type-specific routines)
       */
      SLang_free_array( at );

      return out;

    } /* SLANG_ARRAY_TYPE */

    /* use a tied hash: see also Struct_Type */
  case SLANG_ASSOC_TYPE:
    {
      SLang_Array_Type *keys = NULL;
      SV *tied_object, *object;
      HV *hash;
      char *typename, *keyname;
      int i;

      Printf( ("  stack contains an associative array\n") );

      /*
       * use S-Lang to parse the Associative array
       * (approach suggested by John Davis) since there isn't
       * a public C API for them (ie internals liable to change)
       */
      (void) SLang_load_string( "$1=();assoc_get_keys($1);" );
      UTIL_SLERR(
        SLang_pop_array( &keys, 0 ),
        "Internal error: unable to pop keys array off the stack\n"
      );

      (void) SLang_load_string( "string(_typeof(assoc_get_values($1)));" );
      UTIL_SLERR(
        SLang_pop_slstring( &typename ),
        "Internal error: unable to pop string off the S-Lang stack\n"
      );
      Printf( (">> Assoc_Array has type = [%s]\n",typename) );
      CALL_METHOD_SCALAR_SV( sv_2mortal(newSVpv("Assoc_Type",0)), 
			     "new", C2PL_MARG_S(typename);, tied_object );
      SLang_free_slstring(typename);

      /*
       * get a reference to the hash which is actually storing the data
       */
      CALL_METHOD_SCALAR_SV( tied_object, "_private_get_hashref", , object );
      object = sv_2mortal( object );
      hash = (HV *) SvRV( object );

      /*
       * loop through each element, converting the values to Perl types
       * NOTE:
       *   previously converted all the field values to Perl in one go
       *   but as I'm planning to change the array handling to use
       *   tied arrays and I don't understand how to access them from
       *   C I'm doing them one at a time
       */
      Printf( ("About to loop through the Assoc array keys [nelem=%d]\n",
	       keys->num_elements) );
      for ( i = 0; i < keys->num_elements; i++ ) {
	SV *value;

	/* get the key */
        (void) SLang_get_array_element( keys, &i, &keyname );
	Printf( ( "assoc array key = [%s]\n", keyname ) );

	/* convert the value from the S-Lang array - leave on stack */
	UTIL_SLERR(
	  SLang_push_string(keyname),
	  "Internal error during conversion of S-Lang Assoc_Array to Perl\n"
        );

	/*
	 * since the sl2pl() call may invalidate the value of $1
	 * we cheat and stick $1 onto the S-Lang stack as well as
         * the value of the key we're interested in so that we can
         * reset $1 after the call to sl2pl()
	 * THIS IS NOT MEMORY/TIME EFFICIENT !
	 */
	(void) SLang_load_string( "$2 = (); $1; $1[$2];" );
	value = sl2pl();
	(void) SLang_load_string( "$1 = ();" );

	/* store in the hash */
	hv_store( hash, keyname, strlen(keyname), value, 0 );

	SLang_free_slstring( keyname ); // is this necessary?
      }

      /* free up memory */
      _clean_slang_vars(2);
      SLang_free_array( keys );
      Printf( ("freed up keys array (S-Lang)\n") );

      return tied_object;
      break;

    } /* ASSOC */

  case SLANG_DATATYPE_TYPE:
    {
      /*
       * store the datatype value as a string of the name,
       * into an DataType_Type object
       * we let S-Lang do the conversion to a string
       *
       * ideally would pop the datatype off the stack and then
       * call SLclass_get_datatype_name() on it, but
       * the pop datatype routine is only present in >= v1.4.5
       * and can't be bothered to have 2 code pathways depending
       * on the library version
       */
      Printf( ("  stack contains: a S-Lang datatype object\n") );

      /* convert the object on the stack to a string */
      (void) SLang_load_string( "string();" );

      /* if use newRV [== newRV_inc] then this leaks memory */
      return
	sv_bless(
		 newRV_noinc( sl2pl() ),
		 gv_stashpv("DataType_Type",1)
		 );
      break;

    } /* DATATYPE */

  default:
    {
      /*
       * There are 2 cases:
       *  - a struct, including type-deffed ones
       *  - everything else
       *
       * Important that $1 left as value since needed by sl2pl_struct|opaque
       * routines
       */
      int is_struct;
      (void) SLang_load_string( "$1 = (); is_struct_type($1);" );
      UTIL_SLERR(
        SLang_pop_integer( &is_struct ),
	"Error: unable to pop an item from the S-Lang stack"
      );

      if ( is_struct ) return sl2pl_struct();
      else             return sl2pl_opaque();

    } /* default */
  }

} /* sl2pl_type() */

/*
 * convert the object on the S-Lang stack to
 * a perl object.
 *
 * The use of the S-Lang stack may limit recursion,
 * but it's easy to stick values back onto the S-Lang
 * stack. In fact, we make use of this when processing
 * certain types.
 */

SV *
sl2pl( void ) {

  /* should we really be using SLtype instead of int? */
  int type = SLang_peek_at_stack();
  return sl2pl_type( type );

} /* sl2pl() */
