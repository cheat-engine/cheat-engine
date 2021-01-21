#define SRC(y...)                       \
        9999: y;                        \
        .section __ex_table, "a";       \
        .long 9999b, 6001f      ;       \
        // .previous

    SRC(1: movw (%esi), %bx)
6001:
