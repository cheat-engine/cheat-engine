#if defined test_56_btype_excess_1
struct A {} int i;

#elif defined test_57_btype_excess_2
char int i;

#elif defined test_58_function_redefinition
int f(void) { return 0; }
int f(void) { return 1; }

#elif defined test_global_redefinition
int xxx = 1;
int xxx;
int xxx = 2;

#elif defined test_59_function_array
int (*fct)[42](int x);

#elif defined test_60_enum_redefinition
enum color { RED, GREEN, BLUE };
enum color { R, G, B };
enum color c;

#elif defined test_62_enumerator_redefinition
enum color { RED, GREEN, BLUE };
enum rgb { RED, G, B};
enum color c = RED;

#elif defined test_63_local_enumerator_redefinition
enum {
    FOO,
    BAR
};

int main(void)
{
    enum {
        FOO = 2,
        BAR
    };

    return BAR - FOO;
}

#elif defined test_61_undefined_enum
enum rgb3 c = 42;

#elif defined test_74_non_const_init
int i = i++;

#endif
