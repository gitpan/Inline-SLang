#ifndef SL2PL_PL2SL_H
#define SL2PL_PL2SL_H

/* functions that are visible outside of util.c */
void pl2sl( SV *item );
SLtype pltype( SV *plval, int *flag );

/* 
 * need to pop item off S-Lang's internal stack and push
 * it onto S-Lang's main stack (or I've confused myself)
 */
#define SL_PUSH_ELEM1_ONTO_STACK(nelem) \
  (void) SLang_load_string( \
    "$2=struct {value};set_struct_field($2,\"value\",$1);__push_args($2);" \
  ); \
  _clean_slang_vars(nelem);

#endif /* SL2PL_PL2SL_H */

