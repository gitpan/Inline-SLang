#ifndef SL2PL_SL2PL_H
#define SL2PL_SL2PL_H

SV * sl2pl( void );

#define C2PL_MARG(x)   XPUSHs( sv_2mortal( x ) )
#define C2PL_MARG_D(x) C2PL_MARG( newSVnv( x ) )
#define C2PL_MARG_S(x) C2PL_MARG( newSVpv( x, 0 ) )

/*
 * SL2PL_ITYPE( INT, integer, int ) 
 * will create code to handle scalar values of integer type
 *   SLANG_INT_TYPE and SLANG_UINT_TYPE
 * the second argument is the name of the SLang_pop_xxx
 * routine, and the third the c type (it's only for int/integer
 * that the second and third args are different)
 */

#define SL2PL_ITYPE(stypeu,stypel,ctype) \
  case SLANG_##stypeu##_TYPE: \
    { \
      ctype ival; \
      if ( -1 == SLang_pop_##stypel ( &ival ) ) \
	croak( "Error: unable to read stypel value from the stack\n" ); \
      Printf( ("  stack contains: ctype = %i\n", ival ) ); \
      return newSViv(ival); \
    } \
  \
  case SLANG_U##stypeu##_TYPE: \
    { \
      unsigned ctype ival; \
      if ( -1 == SLang_pop_u##stypel ( &ival ) ) \
	croak( "Error: unable to read stypel value from the stack\n" ); \
      Printf( ("  stack contains: unsigned ctype = %i\n", ival ) ); \
      return newSVuv(ival); \
    }

#endif /* SL2PL_SL2PL_H */

