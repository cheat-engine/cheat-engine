typedef unsigned short uint16_t;
typedef unsigned char uint8_t;

typedef union Unaligned16a {
  uint16_t u;
  uint8_t b[2];
} __attribute__((packed)) Unaligned16a;

typedef union __attribute__((packed)) Unaligned16b {
  uint16_t u;
  uint8_t b[2];
} Unaligned16b;

extern void foo (void) __attribute__((stdcall));
void __attribute__((stdcall)) foo (void)
{
}

int main () { return 0; }
