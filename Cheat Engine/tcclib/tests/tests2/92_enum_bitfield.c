/* This checks if enums needing 8 bit but only having positive
   values are correctly zero extended (instead of sign extended)
   when stored into/loaded from a 8 bit bit-field of enum type (which
   itself is implementation defined, so isn't necessarily supported by all
   other compilers).  */
enum tree_code {
  SOME_CODE = 148, /* has bit 7 set, and hence all further enum values as well */
  LAST_AND_UNUSED_TREE_CODE
};
typedef union tree_node *tree;
struct tree_common
{
  union tree_node *chain;
  union tree_node *type;
  enum tree_code code : 8;
  unsigned side_effects_flag : 1;
};
union tree_node
{
  struct tree_common common;
 };
enum c_tree_code {
  C_DUMMY_TREE_CODE = LAST_AND_UNUSED_TREE_CODE,
  STMT_EXPR,
  LAST_C_TREE_CODE
};
enum cplus_tree_code {
  CP_DUMMY_TREE_CODE = LAST_C_TREE_CODE,
  AMBIG_CONV,
  LAST_CPLUS_TREE_CODE
};

extern int printf(const char *, ...);
int blah(){return 0;}

int convert_like_real (tree convs)
{
  switch (((enum tree_code) (convs)->common.code))
    {
    case AMBIG_CONV: /* This has bit 7 set, which must not be the sign
			bit in tree_common.code, i.e. the bitfield must
			be somehow marked unsigned.  */
      return blah();
    default:
      break;
    };
   printf("unsigned enum bit-fields broken\n");
   return 0;
}

int main()
{
  union tree_node convs;

  convs.common.code = AMBIG_CONV;
  convert_like_real (&convs);
  return 0;
}
