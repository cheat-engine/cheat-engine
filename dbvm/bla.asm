00030000  FA                cli
00030001  6689D6            mov si,dx
00030004  0FA2              cpuid
00030006  6689F2            mov dx,si
00030009  B800008ED8        mov eax,0xd88e0000
0003000E  66833E00          cmp word [esi],byte +0x0
00030012  7C00              jl 0x30014
00030014  7404              jz 0x3001a
00030016  F390              pause
00030018  EBE7              jmp short 0x30001
0003001A  66B80100          mov ax,0x1
0003001E  0000              add [eax],al
00030020  668706            xchg ax,[esi]
00030023  007C6683          add [esi-0x7d],bh
00030027  F8                clc
00030028  0075D6            add [ebp-0x2a],dh
0003002B  FB                sti
0003002C  E9C5010000        jmp dword 0x301f6
00030031  50                push eax
00030032  52                push edx
00030033  3E8A9622FE3E8A    mov dl,[ds:esi-0x75c101de]
0003003A  B623              mov dh,0x23
0003003C  FE                db 0xfe
0003003D  B800B88EC0        mov eax,0xc08eb800
00030042  88F0              mov al,dh
00030044  B4A0              mov ah,0xa0
00030046  F6EC              imul ah
00030048  52                push edx
00030049  81E2FF00D1E2      and edx,0xe2d100ff
0003004F  01D0              add eax,edx
00030051  5A                pop edx
00030052  96                xchg eax,esi
00030053  26880C26          mov [es:esi],cl
00030057  C644010F96        mov byte [ecx+eax+0xf],0x96
0003005C  FEC2              inc dl
0003005E  80FA50            cmp dl,0x50
00030061  7302              jnc 0x30065
00030063  EB0B              jmp short 0x30070
00030065  B200              mov dl,0x0
00030067  FEC6              inc dh
00030069  80FE19            cmp dh,0x19
0003006C  7202              jc 0x30070
0003006E  B600              mov dh,0x0
00030070  3E889622FE3E88    mov [ds:esi-0x77c101de],dl
00030077  B623              mov dh,0x23
00030079  FE                db 0xfe
0003007A  5A                pop edx
0003007B  58                pop eax
0003007C  C3                ret
0003007D  8A0CE8            mov cl,[eax+ebp*8]
00030080  07                pop es
00030081  004680            add [esi-0x80],al
00030084  3C24              cmp al,0x24
00030086  75F5              jnz 0x3007d
00030088  C3                ret
00030089  50                push eax
0003008A  52                push edx
0003008B  5A                pop edx
0003008C  58                pop eax
0003008D  C3                ret
0003008E  50                push eax
0003008F  53                push ebx
00030090  51                push ecx
00030091  57                push edi
00030092  89FE              mov esi,edi
00030094  01CE              add esi,ecx
00030096  4E                dec esi
00030097  C604244E          mov byte [esp],0x4e
0003009B  39FE              cmp esi,edi
0003009D  721F              jc 0x300be
0003009F  31D2              xor edx,edx
000300A1  BB0A00F7F3        mov ebx,0xf3f7000a
000300A6  80C230            add dl,0x30
000300A9  88144E            mov [esi+ecx*2],dl
000300AC  83F800            cmp eax,byte +0x0
000300AF  75EA              jnz 0x3009b
000300B1  89F1              mov ecx,esi
000300B3  41                inc ecx
000300B4  39FE              cmp esi,edi
000300B6  7206              jc 0x300be
000300B8  C604204E          mov byte [eax],0x4e
000300BC  EBF6              jmp short 0x300b4
000300BE  89CE              mov esi,ecx
000300C0  5F                pop edi
000300C1  59                pop ecx
000300C2  5B                pop ebx
000300C3  58                pop eax
000300C4  C3                ret
000300C5  50                push eax
000300C6  53                push ebx
000300C7  51                push ecx
000300C8  57                push edi
000300C9  89FE              mov esi,edi
000300CB  01CE              add esi,ecx
000300CD  4E                dec esi
000300CE  C604244E          mov byte [esp],0x4e
000300D2  39FE              cmp esi,edi
000300D4  722C              jc 0x30102
000300D6  31D2              xor edx,edx
000300D8  BB1000F7F3        mov ebx,0xf3f70010
000300DD  80FA09            cmp dl,0x9
000300E0  7705              ja 0x300e7
000300E2  80C230            add dl,0x30
000300E5  EB06              jmp short 0x300ed
000300E7  80EA0A            sub dl,0xa
000300EA  80C261            add dl,0x61
000300ED  88144E            mov [esi+ecx*2],dl
000300F0  83F800            cmp eax,byte +0x0
000300F3  75DD              jnz 0x300d2
000300F5  89F1              mov ecx,esi
000300F7  41                inc ecx
000300F8  39FE              cmp esi,edi
000300FA  7206              jc 0x30102
000300FC  C604204E          mov byte [eax],0x4e
00030100  EBF6              jmp short 0x300f8
00030102  89CE              mov esi,ecx
00030104  5F                pop edi
00030105  59                pop ecx
00030106  5B                pop ebx
00030107  58                pop eax
00030108  C3                ret
00030109  3838              cmp [eax],bh
0003010B  3D24653830        cmp eax,0x30386524
00030110  313A              xor [edx],edi
00030112  2465              and al,0x65
00030114  3832              cmp [edx],dh
00030116  303A              xor [edx],bh
00030118  0D0A246178        or eax,0x7861240a
0003011D  3D2462783D        cmp eax,0x3d786224
00030122  2463              and al,0x63
00030124  783D              js 0x30163
00030126  2464              and al,0x64
00030128  783D              js 0x30167
0003012A  2453              and al,0x53
0003012C  4D                dec ebp
0003012D  41                inc ecx
0003012E  50                push eax
0003012F  204552            and [ebp+0x52],al
00030132  52                push edx
00030133  4F                dec edi
00030134  52                push edx
00030135  210D0A240000      and [dword 0x240a],ecx
0003013B  0000              add [eax],al
0003013D  0000              add [eax],al
0003013F  0000              add [eax],al
00030141  0000              add [eax],al
00030143  0000              add [eax],al
00030145  0000              add [eax],al
00030147  0000              add [eax],al
00030149  0D0A240000        or eax,0x240a
0003014E  0000              add [eax],al
00030150  0000              add [eax],al
00030152  0000              add [eax],al
00030154  0000              add [eax],al
00030156  0000              add [eax],al
00030158  0000              add [eax],al
0003015A  0000              add [eax],al
0003015C  0000              add [eax],al
0003015E  0000              add [eax],al
00030160  0000              add [eax],al
00030162  0000              add [eax],al
00030164  0000              add [eax],al
00030166  0000              add [eax],al
00030168  0000              add [eax],al
0003016A  0000              add [eax],al
0003016C  67657420          gs jz 0x30190
00030170  56                push esi
00030171  45                inc ebp
00030172  53                push ebx
00030173  41                inc ecx
00030174  20696E            and [ecx+0x6e],ch
00030177  666F              outsw
00030179  207375            and [ebx+0x75],dh
0003017C  636365            arpl [ebx+0x65],sp
0003017F  7373              jnc 0x301f4
00030181  0D0A246765        or eax,0x6567240a
00030186  7420              jz 0x301a8
00030188  56                push esi
00030189  45                inc ebp
0003018A  53                push ebx
0003018B  41                inc ecx
0003018C  20696E            and [ecx+0x6e],ch
0003018F  666F              outsw
00030191  206661            and [esi+0x61],ah
00030194  696C65640D0A2420  imul ebp,[ebp+0x64],dword 0x20240a0d
0003019C  2020              and [eax],ah
0003019E  46                inc esi
0003019F  61                popad
000301A0  696C757265766964  imul ebp,[ebp+esi*2+0x72],dword 0x64697665
000301A8  656F              gs outsd
000301AA  6D                insd
000301AB  6F                outsd
000301AC  64652030          and [gs:eax],dh
000301B0  7824              js 0x301d6
000301B2  1E                push ds
000301B3  06                push es
000301B4  31C0              xor eax,eax
000301B6  8ED8              mov ds,eax
000301B8  48                dec eax
000301B9  8EC0              mov es,eax
000301BB  26A11000F7D0      mov eax,[es:0xd0f70010]
000301C1  FF36              push dword [esi]
000301C3  0000              add [eax],al
000301C5  A30000A100        mov [0xa10000],eax
000301CA  0026              add [esi],ah
000301CC  3B06              cmp eax,[esi]
000301CE  1000              adc [eax],al
000301D0  8F06              pop dword [esi]
000301D2  0000              add [eax],al
000301D4  07                pop es
000301D5  1F                pop ds
000301D6  7404              jz 0x301dc
000301D8  B80100C3B8        mov eax,0xb8c30001
000301DD  0000              add [eax],al
000301DF  C3                ret
000301E0  7265              jc 0x30247
000301E2  7472              jz 0x30256
000301E4  7920              jns 0x30206
000301E6  7265              jc 0x3024d
000301E8  61                popad
000301E9  64696E6720646973  imul ebp,[fs:esi+0x67],dword 0x73696420
000301F1  6B0DC3B80080FA    imul ecx,[dword 0x8000b8c3],byte -0x6
000301F8  8ED0              mov ss,eax
000301FA  BCF0FFFB66        mov esp,0x66fbfff0
000301FF  31ED              xor ebp,ebp
00030201  6631F6            xor si,si
00030204  8CC8              mov eax,cs
00030206  8ED8              mov ds,eax
00030208  8EC0              mov es,eax
0003020A  E800005D3E        call dword 0x3e60020f
0003020F  8DBE3FFF3E80      lea edi,[esi-0x7fc100c1]
00030215  3D000F859F        cmp eax,0x9f850f00
0003021A  003E              add [esi],bh
0003021C  88964FFF80BE      mov [esi-0x417f00b1],dl
00030222  3F                aas
00030223  FF00              inc dword [eax]
00030225  0F859100B800      jnz dword 0xbb02bc
0003022B  60                pushad
0003022C  8EC0              mov es,eax
0003022E  31FF              xor edi,edi
00030230  B800083E8A        mov eax,0x8a3e0800
00030235  96                xchg eax,esi
00030236  4F                dec edi
00030237  FFCD              dec ebp
00030239  13FE              adc edi,esi
0003023B  C6                db 0xc6
0003023C  3E88B657FF89C8    mov [ds:esi-0x377600a9],dh
00030243  83E03F            and eax,byte +0x3f
00030246  40                inc eax
00030247  3E898653FFC1E9    mov [ds:esi-0x163e00ad],eax
0003024E  06                push es
0003024F  41                inc ecx
00030250  3E898E5BFFB800    mov [ds:esi+0xb8ff5b],ecx
00030257  808EC0BF000066    or byte [esi+0xbfc0],0x66
0003025E  BB00000000        mov ebx,0x0
00030263  66B820E8          mov ax,0xe820
00030267  0000              add [eax],al
00030269  66B91400          mov cx,0x14
0003026D  0000              add [eax],al
0003026F  66BA5041          mov dx,0x4150
00030273  4D                dec ebp
00030274  53                push ebx
00030275  CD15              int 0x15
00030277  7222              jc 0x3029b
00030279  663D5041          cmp ax,0x4150
0003027D  4D                dec ebp
0003027E  53                push ebx
0003027F  751A              jnz 0x3029b
00030281  6683F914          cmp cx,byte +0x14
00030285  7514              jnz 0x3029b
00030287  57                push edi
00030288  3E8DBE3FFF3E66    lea edi,[ds:esi+0x663eff3f]
0003028F  FF055F83C714      inc dword [dword 0x14c7835f]
00030295  6683FB00          cmp bx,byte +0x0
00030299  75C8              jnz 0x30263
0003029B  3E8DBE3FFF8B05    lea edi,[ds:esi+0x58bff3f]
000302A2  3E8DBE2CFFB910    lea edi,[ds:esi+0x10b9ff2c]
000302A9  00E8              add al,ch
000302AB  18FE              sbb dh,bh
000302AD  3E8DB61F02E8C8    lea esi,[ds:esi-0x3717fde1]
000302B4  FD                std
000302B5  B13B              mov cl,0x3b
000302B7  E8CFFDE492        call dword 0x92e8008b
000302BC  A802              test al,0x2
000302BE  7504              jnz 0x302c4
000302C0  0C02              or al,0x2
000302C2  E692              out 0x92,al
000302C4  E8EBFE8DBE        call dword 0xbe9101b4
000302C9  4B                dec ebx
000302CA  FF8905B80040      dec dword [ecx+0x4000b805]
000302D0  8EC0              mov es,eax
000302D2  26C70600006F00    mov dword [es:esi],0x6f0000
000302D9  2666C7060200      mov word [es:esi],0x2
000302DF  0000              add [eax],al
000302E1  0500B80050        add eax,0x5000b800
000302E6  8EC0              mov es,eax
000302E8  2666C7060000      mov word [es:esi],0x0
000302EE  0000              add [eax],al
000302F0  0000              add [eax],al
000302F2  2666C7060400      mov word [es:esi],0x4
000302F8  0000              add [eax],al
000302FA  0000              add [eax],al
000302FC  2666C7060800      mov word [es:esi],0x8
00030302  FF                db 0xff
00030303  FF00              inc dword [eax]
00030305  0026              add [esi],ah
00030307  66C7060C00        mov word [esi],0xc
0003030C  0092CF002666      add [edx+0x662600cf],dl
00030312  C7061000FFFF      mov dword [esi],0xffff0010
00030318  0000              add [eax],al
0003031A  2666C7061400      mov word [es:esi],0x14
00030320  0096CF002666      add [esi+0x662600cf],dl
00030326  C7061800FFFF      mov dword [esi],0xffff0018
0003032C  0000              add [eax],al
0003032E  2666C7061C00      mov word [es:esi],0x1c
00030334  009BCF002666      add [ebx+0x662600cf],bl
0003033A  C7062000FFFF      mov dword [esi],0xffff0020
00030340  0000              add [eax],al
00030342  2666C7062400      mov word [es:esi],0x24
00030348  009A00002666      add [edx+0x66260000],bl
0003034E  C7062800FFFF      mov dword [esi],0xffff0028
00030354  0000              add [eax],al
00030356  2666C7062C00      mov word [es:esi],0x2c
0003035C  009200002666      add [edx+0x66260000],dl
00030362  C7063000FFFF      mov dword [esi],0xffff0030
00030368  0000              add [eax],al
0003036A  2666C7063400      mov word [es:esi],0x34
00030370  039A00002666      add ebx,[edx+0x66260000]
00030376  C70638000000      mov dword [esi],0x38
0003037C  0000              add [eax],al
0003037E  2666C7063C00      mov word [es:esi],0x3c
00030384  0000              add [eax],al
00030386  0000              add [eax],al
00030388  2666C7064000      mov word [es:esi],0x40
0003038E  0000              add [eax],al
00030390  0000              add [eax],al
00030392  2666C7064400      mov word [es:esi],0x44
00030398  0000              add [eax],al
0003039A  0000              add [eax],al
0003039C  2666C7064800      mov word [es:esi],0x48
000303A2  0000              add [eax],al
000303A4  0000              add [eax],al
000303A6  2666C7064C00      mov word [es:esi],0x4c
000303AC  0000              add [eax],al
000303AE  0000              add [eax],al
000303B0  2666C7065000      mov word [es:esi],0x50
000303B6  0000              add [eax],al
000303B8  0000              add [eax],al
000303BA  2666C7065400      mov word [es:esi],0x54
000303C0  009AA0002666      add [edx+0x662600a0],bl
000303C6  C70658000000      mov dword [esi],0x58
000303CC  0000              add [eax],al
000303CE  2666C7065C00      mov word [es:esi],0x5c
000303D4  0000              add [eax],al
000303D6  0000              add [eax],al
000303D8  2666C7066000      mov word [es:esi],0x60
000303DE  0000              add [eax],al
000303E0  0000              add [eax],al
000303E2  2666C7066400      mov word [es:esi],0x64
000303E8  0000              add [eax],al
000303EA  0000              add [eax],al
000303EC  2666C7066800      mov word [es:esi],0x68
000303F2  0000              add [eax],al
000303F4  0000              add [eax],al
000303F6  2666C7066C00      mov word [es:esi],0x6c
000303FC  0000              add [eax],al
000303FE  0000              add [eax],al
00030400  B800408EE0        mov eax,0xe08e4000
00030405  66640F0116        o16 lgdt [fs:esi]
0003040A  0000              add [eax],al
0003040C  B8FFFF8ED8        mov eax,0xd88effff
00030411  FA                cli
00030412  0F20C3            mov ebx,cr0
00030415  6683CB01          or bx,byte +0x1
00030419  0F22C3            mov cr0,ebx
0003041C  EB00              jmp short 0x3041e
0003041E  90                nop
0003041F  90                nop
00030420  8CCB              mov ebx,cs
00030422  66EA58040300      jmp word 0x3:0x458
00030428  1800              sbb [eax],al
0003042A  EB2C              jmp short 0x30458
0003042C  6E                outsb
0003042D  6F                outsd
0003042E  726D              jc 0x3049d
00030430  61                popad
00030431  6C                insb
00030432  0D0A244361        or eax,0x6143240a
00030437  6C                insb
00030438  6C                insb
00030439  6564204C4744      and [fs:edi+eax*2+0x44],cl
0003043F  54                push esp
00030440  2028              and [eax],ch
00030442  6E                outsb
00030443  6F                outsd
00030444  7429              jz 0x3046f
00030446  0D0A248A0E        or eax,0xe8a240a
0003044B  E807000000        call dword 0x30457
00030450  46                inc esi
00030451  803E00            cmp byte [esi],0x0
00030454  75F3              jnz 0x30449
00030456  C3                ret
00030457  C3                ret
00030458  C1E310            shl ebx,0x10
0003045B  668CDB            mov bx,ds
0003045E  668CC1            mov cx,es
00030461  C1E110            shl ecx,0x10
00030464  668CE1            mov cx,fs
00030467  66B80800          mov ax,0x8
0003046B  8ED8              mov ds,eax
0003046D  8EC0              mov es,eax
0003046F  8EE0              mov fs,eax
00030471  8EE8              mov gs,eax
00030473  66B80800          mov ax,0x8
00030477  8ED0              mov ss,eax
00030479  BCFCFF3F00        mov esp,0x3ffffc
0003047E  891D50010300      mov [dword 0x30150],ebx
00030484  890D54010300      mov [dword 0x30154],ecx
0003048A  31C0              xor eax,eax
0003048C  BF00003F00        mov edi,0x3f0000
00030491  B9FFFF0000        mov ecx,0xffff
00030496  F3AA              rep stosb
00030498  E8DD020000        call dword 0x3077a
0003049D  B001              mov al,0x1
0003049F  C60500800B006C    mov byte [dword 0xb8000],0x6c
000304A6  A201800B00        mov [0xb8001],al
000304AB  FEC0              inc al
000304AD  C60502800B006F    mov byte [dword 0xb8002],0x6f
000304B4  A203800B00        mov [0xb8003],al
000304B9  FEC0              inc al
000304BB  C60504800B0073    mov byte [dword 0xb8004],0x73
000304C2  A205800B00        mov [0xb8005],al
000304C7  FEC0              inc al
000304C9  C60506800B0065    mov byte [dword 0xb8006],0x65
000304D0  A207800B00        mov [0xb8007],al
000304D5  FEC0              inc al
000304D7  C60508800B0072    mov byte [dword 0xb8008],0x72
000304DE  A209800B00        mov [0xb8009],al
000304E3  FEC0              inc al
000304E5  EBB8              jmp short 0x3049f
000304E7  49                dec ecx
000304E8  6E                outsb
000304E9  7369              jnc 0x30554
000304EB  646520676F        and [gs:edi+0x6f],ah
000304F0  746F              jz 0x30561
000304F2  56                push esi
000304F3  4D                dec ebp
000304F4  4D                dec ebp
000304F5  0D0A004861        or eax,0x6148000a
000304FA  7665              jna 0x30561
000304FC  207365            and [ebx+0x65],dh
000304FF  7420              jz 0x30521
00030501  7468              jz 0x3056b
00030503  65204744          and [gs:edi+0x44],al
00030507  54                push esp
00030508  0D0A000000        or eax,0xa
0003050D  0000              add [eax],al
0003050F  0000              add [eax],al
00030511  0000              add [eax],al
00030513  0000              add [eax],al
00030515  0000              add [eax],al
00030517  8B1D13050300      mov ebx,[dword 0x30513]
0003051D  BEE7040300        mov esi,0x304e7
00030522  E822FFFFFF        call dword 0x30449
00030527  A10F050300        mov eax,[0x3050f]
0003052C  0F22D8            mov cr3,eax
0003052F  66C705000004006F  mov word [dword 0x40000],0x6f
         -00
00030538  C7050200040000F0  mov dword [dword 0x40002],0x43f000
         -4300
00030542  0F011500000400    lgdt [dword 0x40000]
00030549  BEF8040300        mov esi,0x304f8
0003054E  E8F6FEFFFF        call dword 0x30449
00030553  8A155C010300      mov dl,[dword 0x3015c]
00030559  B830000000        mov eax,0x30
0003055E  0F22E0            mov cr4,eax
00030561  B9800000C0        mov ecx,0xc0000080
00030566  0F32              rdmsr
00030568  0D00010000        or eax,0x100
0003056D  0F30              wrmsr
0003056F  0F20C0            mov eax,cr0
00030572  0D20000080        or eax,0x80000020
00030577  0F22C0            mov cr0,eax
0003057A  31C0              xor eax,eax
0003057C  EA000040005000    jmp dword 0x50:0x400000
00030583  F4                hlt
00030584  EBFD              jmp short 0x30583
00030586  A300800700        mov [0x78000],eax
0003058B  891D04800700      mov [dword 0x78004],ebx
00030591  890D08800700      mov [dword 0x78008],ecx
00030597  89150C800700      mov [dword 0x7800c],edx
0003059D  893510800700      mov [dword 0x78010],esi
000305A3  893D14800700      mov [dword 0x78014],edi
000305A9  892D18800700      mov [dword 0x78018],ebp
000305AF  89251C800700      mov [dword 0x7801c],esp
000305B5  B8D7050300        mov eax,0x305d7
000305BA  25FFFF0000        and eax,0xffff
000305BF  A320800700        mov [0x78020],eax
000305C4  C705248007003000  mov dword [dword 0x78024],0x30
         -0000
000305CE  90                nop
000305CF  90                nop
000305D0  90                nop
000305D1  FF2D20800700      jmp dword far [dword 0x78020]
000305D7  B828008ED8        mov eax,0xd88e0028
000305DC  8EC0              mov es,eax
000305DE  8EE0              mov fs,eax
000305E0  8EE8              mov gs,eax
000305E2  8ED0              mov ss,eax
000305E4  6631C0            xor ax,ax
000305E7  0F22C0            mov cr0,eax
000305EA  B800608ED8        mov eax,0xd88e6000
000305EF  66B80206          mov ax,0x602
000305F3  0300              add eax,[eax]
000305F5  A30000C706        mov [0x6c70000],eax
000305FA  0200              add al,[eax]
000305FC  0030              add [eax],dh
000305FE  FF2E              jmp dword far [esi]
00030600  0000              add [eax],al
00030602  B800708ED8        mov eax,0xd88e7000
00030607  B800608EC0        mov eax,0xc08e6000
0003060C  8ED0              mov ss,eax
0003060E  BCFEFFFBB8        mov esp,0xb8fbfffe
00030613  0102              add [edx],eax
00030615  31DB              xor ebx,ebx
00030617  8A0E              mov cl,[esi]
00030619  0000              add [eax],al
0003061B  8A36              mov dh,[esi]
0003061D  0100              add [eax],eax
0003061F  8A2E              mov ch,[esi]
00030621  0200              add al,[eax]
00030623  8A16              mov dl,[esi]
00030625  0300              add eax,[eax]
00030627  CD13              int 0x13
00030629  7208              jc 0x30633
0003062B  66B80100          mov ax,0x1
0003062F  0000              add [eax],al
00030631  EB03              jmp short 0x30636
00030633  6631C0            xor ax,ax
00030636  BB00708EDB        mov ebx,0xdb8e7000
0003063B  66A30080FA0F      mov [0xffa8000],ax
00030641  20C3              and bl,al
00030643  6683CB01          or bx,byte +0x1
00030647  0F22C3            mov cr0,ebx
0003064A  EB00              jmp short 0x3064c
0003064C  90                nop
0003064D  90                nop
0003064E  66EA56060300      jmp word 0x3:0x656
00030654  1800              sbb [eax],al
00030656  66B80800          mov ax,0x8
0003065A  8ED8              mov ds,eax
0003065C  8EC0              mov es,eax
0003065E  8EE0              mov fs,eax
00030660  8EE8              mov gs,eax
00030662  8ED0              mov ss,eax
00030664  A100800700        mov eax,[0x78000]
00030669  8B1D04800700      mov ebx,[dword 0x78004]
0003066F  8B0D08800700      mov ecx,[dword 0x78008]
00030675  8B150C800700      mov edx,[dword 0x7800c]
0003067B  8B3510800700      mov esi,[dword 0x78010]
00030681  8B3D14800700      mov edi,[dword 0x78014]
00030687  8B2D18800700      mov ebp,[dword 0x78018]
0003068D  8B251C800700      mov esp,[dword 0x7801c]
00030693  C3                ret
00030694  55                push ebp
00030695  89E5              mov ebp,esp
00030697  83EC38            sub esp,byte +0x38
0003069A  C745F400000700    mov dword [ebp-0xc],0x70000
000306A1  A160010300        mov eax,[0x30160]
000306A6  8D48FF            lea ecx,[eax-0x1]
000306A9  8B4508            mov eax,[ebp+0x8]
000306AC  89C2              mov edx,eax
000306AE  C1FA1F            sar edx,0x1f
000306B1  F7F9              idiv ecx
000306B3  89D0              mov eax,edx
000306B5  89C2              mov edx,eax
000306B7  8B45F4            mov eax,[ebp-0xc]
000306BA  8810              mov [eax],dl
000306BC  A160010300        mov eax,[0x30160]
000306C1  83E801            sub eax,byte +0x1
000306C4  8945E4            mov [ebp-0x1c],eax
000306C7  8B4508            mov eax,[ebp+0x8]
000306CA  89C2              mov edx,eax
000306CC  C1FA1F            sar edx,0x1f
000306CF  F77DE4            idiv dword [ebp-0x1c]
000306D2  8B0D64010300      mov ecx,[dword 0x30164]
000306D8  89C2              mov edx,eax
000306DA  C1FA1F            sar edx,0x1f
000306DD  F7F9              idiv ecx
000306DF  89D0              mov eax,edx
000306E1  89C2              mov edx,eax
000306E3  8B45F4            mov eax,[ebp-0xc]
000306E6  885001            mov [eax+0x1],dl
000306E9  A160010300        mov eax,[0x30160]
000306EE  83E801            sub eax,byte +0x1
000306F1  8945E4            mov [ebp-0x1c],eax
000306F4  8B4508            mov eax,[ebp+0x8]
000306F7  89C2              mov edx,eax
000306F9  C1FA1F            sar edx,0x1f
000306FC  F77DE4            idiv dword [ebp-0x1c]
000306FF  8B1564010300      mov edx,[dword 0x30164]
00030705  8955E4            mov [ebp-0x1c],edx
00030708  89C2              mov edx,eax
0003070A  C1FA1F            sar edx,0x1f
0003070D  F77DE4            idiv dword [ebp-0x1c]
00030710  89C2              mov edx,eax
00030712  8B45F4            mov eax,[ebp-0xc]
00030715  885002            mov [eax+0x2],dl
00030718  A15C010300        mov eax,[0x3015c]
0003071D  89C2              mov edx,eax
0003071F  8B45F4            mov eax,[ebp-0xc]
00030722  885003            mov [eax+0x3],dl
00030725  8B45F4            mov eax,[ebp-0xc]
00030728  0FB600            movzx eax,byte [eax]
0003072B  8D5001            lea edx,[eax+0x1]
0003072E  8B45F4            mov eax,[ebp-0xc]
00030731  8810              mov [eax],dl
00030733  C744240400020000  mov dword [esp+0x4],0x200
0003073B  C7042400000600    mov dword [esp],0x60000
00030742  E8AD1B0000        call dword 0x322f4
00030747  E83AFEFFFF        call dword 0x30586
0003074C  83F801            cmp eax,byte +0x1
0003074F  7522              jnz 0x30773
00030751  C744240800020000  mov dword [esp+0x8],0x200
00030759  C744240400000600  mov dword [esp+0x4],0x60000
00030761  8B450C            mov eax,[ebp+0xc]
00030764  890424            mov [esp],eax
00030767  E8B61B0000        call dword 0x32322
0003076C  B801000000        mov eax,0x1
00030771  EB05              jmp short 0x30778
00030773  B800000000        mov eax,0x0
00030778  C9                leave
00030779  C3                ret
0003077A  55                push ebp
0003077B  89E5              mov ebp,esp
0003077D  53                push ebx
0003077E  81EC84020000      sub esp,0x284
00030784  C745E800000000    mov dword [ebp-0x18],0x0
0003078B  C745EC00000000    mov dword [ebp-0x14],0x0
00030792  C745E4FFFFFFFF    mov dword [ebp-0x1c],0xffffffff
00030799  E8721A0000        call dword 0x32210
0003079E  C680803B030000    mov byte [eax+0x33b80],0x0
000307A5  C70424F42F0300    mov dword [esp],0x32ff4
000307AC  E8C3190000        call dword 0x32174
000307B1  C704241C300300    mov dword [esp],0x3301c
000307B8  E8B7190000        call dword 0x32174
000307BD  C7042440300300    mov dword [esp],0x33040
000307C4  E8AB190000        call dword 0x32174
000307C9  A150010300        mov eax,[0x30150]
000307CE  89442404          mov [esp+0x4],eax
000307D2  C7042463300300    mov dword [esp],0x33063
000307D9  E896190000        call dword 0x32174
000307DE  A154010300        mov eax,[0x30154]
000307E3  89442404          mov [esp+0x4],eax
000307E7  C704246A300300    mov dword [esp],0x3306a
000307EE  E881190000        call dword 0x32174
000307F3  B87A070300        mov eax,0x3077a
000307F8  89442404          mov [esp+0x4],eax
000307FC  C7042474300300    mov dword [esp],0x33074
00030803  E86C190000        call dword 0x32174
00030808  A14C010300        mov eax,[0x3014c]
0003080D  C74424084C010300  mov dword [esp+0x8],0x3014c
00030815  89442404          mov [esp+0x4],eax
00030819  C70424A0300300    mov dword [esp],0x330a0
00030820  E84F190000        call dword 0x32174
00030825  C70424D4300300    mov dword [esp],0x330d4
0003082C  E843190000        call dword 0x32174
00030831  BA603B0300        mov edx,0x33b60
00030836  A1603B0300        mov eax,[0x33b60]
0003083B  89542408          mov [esp+0x8],edx
0003083F  89442404          mov [esp+0x4],eax
00030843  C70424FB300300    mov dword [esp],0x330fb
0003084A  E825190000        call dword 0x32174
0003084F  A1603B0300        mov eax,[0x33b60]
00030854  85C0              test eax,eax
00030856  0F8514100000      jnz dword 0x31870
0003085C  C744241001000000  mov dword [esp+0x10],0x1
00030864  C744240C0F000000  mov dword [esp+0xc],0xf
0003086C  C744240800000000  mov dword [esp+0x8],0x0
00030874  C744240400000000  mov dword [esp+0x4],0x0
0003087C  C704241C310300    mov dword [esp],0x3311c
00030883  E85A220000        call dword 0x32ae2
00030888  C744241001000000  mov dword [esp+0x10],0x1
00030890  C744240C0F000000  mov dword [esp+0xc],0xf
00030898  C744240801000000  mov dword [esp+0x8],0x1
000308A0  C744240400000000  mov dword [esp+0x4],0x0
000308A8  C704246C310300    mov dword [esp],0x3316c
000308AF  E82E220000        call dword 0x32ae2
000308B4  C744241001000000  mov dword [esp+0x10],0x1
000308BC  C744240C0F000000  mov dword [esp+0xc],0xf
000308C4  C744240802000000  mov dword [esp+0x8],0x2
000308CC  C744240400000000  mov dword [esp+0x4],0x0
000308D4  C704246C310300    mov dword [esp],0x3316c
000308DB  E802220000        call dword 0x32ae2
000308E0  C744241001000000  mov dword [esp+0x10],0x1
000308E8  C744240C0F000000  mov dword [esp+0xc],0xf
000308F0  C744240803000000  mov dword [esp+0x8],0x3
000308F8  C744240400000000  mov dword [esp+0x4],0x0
00030900  C704246C310300    mov dword [esp],0x3316c
00030907  E8D6210000        call dword 0x32ae2
0003090C  C744241001000000  mov dword [esp+0x10],0x1
00030914  C744240C0F000000  mov dword [esp+0xc],0xf
0003091C  C744240804000000  mov dword [esp+0x8],0x4
00030924  C744240400000000  mov dword [esp+0x4],0x0
0003092C  C704246C310300    mov dword [esp],0x3316c
00030933  E8AA210000        call dword 0x32ae2
00030938  C744241001000000  mov dword [esp+0x10],0x1
00030940  C744240C0F000000  mov dword [esp+0xc],0xf
00030948  C744240805000000  mov dword [esp+0x8],0x5
00030950  C744240400000000  mov dword [esp+0x4],0x0
00030958  C70424BC310300    mov dword [esp],0x331bc
0003095F  E87E210000        call dword 0x32ae2
00030964  C744241001000000  mov dword [esp+0x10],0x1
0003096C  C744240C0A000000  mov dword [esp+0xc],0xa
00030974  C744240802000000  mov dword [esp+0x8],0x2
0003097C  C74424041D000000  mov dword [esp+0x4],0x1d
00030984  C704240C320300    mov dword [esp],0x3320c
0003098B  E852210000        call dword 0x32ae2
00030990  C744241001000000  mov dword [esp+0x10],0x1
00030998  C744240C0C000000  mov dword [esp+0xc],0xc
000309A0  C744240803000000  mov dword [esp+0x8],0x3
000309A8  C744240420000000  mov dword [esp+0x4],0x20
000309B0  C7042424320300    mov dword [esp],0x33224
000309B7  E826210000        call dword 0x32ae2
000309BC  C7058C3C03000700  mov dword [dword 0x33c8c],0x7
         -0000
000309C6  A158010300        mov eax,[0x30158]
000309CB  89442404          mov [esp+0x4],eax
000309CF  C7042438320300    mov dword [esp],0x33238
000309D6  E83D230000        call dword 0x32d18
000309DB  A15C010300        mov eax,[0x3015c]
000309E0  C74424085C010300  mov dword [esp+0x8],0x3015c
000309E8  89442404          mov [esp+0x4],eax
000309EC  C7042453320300    mov dword [esp],0x33253
000309F3  E820230000        call dword 0x32d18
000309F8  A160010300        mov eax,[0x30160]
000309FD  89442404          mov [esp+0x4],eax
00030A01  C7042470320300    mov dword [esp],0x33270
00030A08  E80B230000        call dword 0x32d18
00030A0D  A164010300        mov eax,[0x30164]
00030A12  89442404          mov [esp+0x4],eax
00030A16  C7042484320300    mov dword [esp],0x33284
00030A1D  E8F6220000        call dword 0x32d18
00030A22  A168010300        mov eax,[0x30168]
00030A27  89442404          mov [esp+0x4],eax
00030A2B  C7042496320300    mov dword [esp],0x33296
00030A32  E8E1220000        call dword 0x32d18
00030A37  8D85A0FDFFFF      lea eax,[ebp-0x260]
00030A3D  89442404          mov [esp+0x4],eax
00030A41  C70424AC320300    mov dword [esp],0x332ac
00030A48  E8CB220000        call dword 0x32d18
00030A4D  C70424BC320300    mov dword [esp],0x332bc
00030A54  E8BF220000        call dword 0x32d18
00030A59  E85E1E0000        call dword 0x328bc
00030A5E  E8591E0000        call dword 0x328bc
00030A63  8D85A0FDFFFF      lea eax,[ebp-0x260]
00030A69  89442404          mov [esp+0x4],eax
00030A6D  C7042400000000    mov dword [esp],0x0
00030A74  E81BFCFFFF        call dword 0x30694
00030A79  85C0              test eax,eax
00030A7B  750E              jnz 0x30a8b
00030A7D  C70424DC320300    mov dword [esp],0x332dc
00030A84  E88F220000        call dword 0x32d18
00030A89  EBFE              jmp short 0x30a89
00030A8B  C70424FC320300    mov dword [esp],0x332fc
00030A92  E881220000        call dword 0x32d18
00030A97  E8201E0000        call dword 0x328bc
00030A9C  E81B1E0000        call dword 0x328bc
00030AA1  8D85A0FDFFFF      lea eax,[ebp-0x260]
00030AA7  83C008            add eax,byte +0x8
00030AAA  0FB700            movzx eax,word [eax]
00030AAD  66A3883C0300      mov [0x33c88],ax
00030AB3  8D85A0FDFFFF      lea eax,[ebp-0x260]
00030AB9  83C010            add eax,byte +0x10
00030ABC  0FB700            movzx eax,word [eax]
00030ABF  66A3803C0300      mov [0x33c80],ax
00030AC5  0FB705883C0300    movzx eax,word [dword 0x33c88]
00030ACC  0FB7C0            movzx eax,ax
00030ACF  89442404          mov [esp+0x4],eax
00030AD3  C7042420330300    mov dword [esp],0x33320
00030ADA  E895160000        call dword 0x32174
00030ADF  0FB705803C0300    movzx eax,word [dword 0x33c80]
00030AE6  0FB7C0            movzx eax,ax
00030AE9  89442404          mov [esp+0x4],eax
00030AED  C704243A330300    mov dword [esp],0x3333a
00030AF4  E87B160000        call dword 0x32174
00030AF9  0FB705883C0300    movzx eax,word [dword 0x33c88]
00030B00  0FB7C0            movzx eax,ax
00030B03  89442404          mov [esp+0x4],eax
00030B07  C7042455330300    mov dword [esp],0x33355
00030B0E  E805220000        call dword 0x32d18
00030B13  C705603B03000100  mov dword [dword 0x33b60],0x1
         -0000
00030B1D  C745D000000800    mov dword [ebp-0x30],0x80000
00030B24  C745F400000000    mov dword [ebp-0xc],0x0
00030B2B  E9B2010000        jmp dword 0x30ce2
00030B30  8B55F4            mov edx,[ebp-0xc]
00030B33  89D0              mov eax,edx
00030B35  C1E002            shl eax,0x2
00030B38  01D0              add eax,edx
00030B3A  C1E002            shl eax,0x2
00030B3D  89C2              mov edx,eax
00030B3F  8B45D0            mov eax,[ebp-0x30]
00030B42  01D0              add eax,edx
00030B44  8B4004            mov eax,[eax+0x4]
00030B47  BA00000000        mov edx,0x0
00030B4C  89C1              mov ecx,eax
00030B4E  89D3              mov ebx,edx
00030B50  89CB              mov ebx,ecx
00030B52  B900000000        mov ecx,0x0
00030B57  8B55F4            mov edx,[ebp-0xc]
00030B5A  89D0              mov eax,edx
00030B5C  C1E002            shl eax,0x2
00030B5F  01D0              add eax,edx
00030B61  C1E002            shl eax,0x2
00030B64  89C2              mov edx,eax
00030B66  8B45D0            mov eax,[ebp-0x30]
00030B69  01D0              add eax,edx
00030B6B  8B00              mov eax,[eax]
00030B6D  BA00000000        mov edx,0x0
00030B72  01C8              add eax,ecx
00030B74  11DA              adc edx,ebx
00030B76  8945C8            mov [ebp-0x38],eax
00030B79  8955CC            mov [ebp-0x34],edx
00030B7C  8B55F4            mov edx,[ebp-0xc]
00030B7F  89D0              mov eax,edx
00030B81  C1E002            shl eax,0x2
00030B84  01D0              add eax,edx
00030B86  C1E002            shl eax,0x2
00030B89  89C2              mov edx,eax
00030B8B  8B45D0            mov eax,[ebp-0x30]
00030B8E  01D0              add eax,edx
00030B90  8B400C            mov eax,[eax+0xc]
00030B93  BA00000000        mov edx,0x0
00030B98  89C1              mov ecx,eax
00030B9A  89D3              mov ebx,edx
00030B9C  89CB              mov ebx,ecx
00030B9E  B900000000        mov ecx,0x0
00030BA3  8B55F4            mov edx,[ebp-0xc]
00030BA6  89D0              mov eax,edx
00030BA8  C1E002            shl eax,0x2
00030BAB  01D0              add eax,edx
00030BAD  C1E002            shl eax,0x2
00030BB0  89C2              mov edx,eax
00030BB2  8B45D0            mov eax,[ebp-0x30]
00030BB5  01D0              add eax,edx
00030BB7  8B4008            mov eax,[eax+0x8]
00030BBA  BA00000000        mov edx,0x0
00030BBF  01C8              add eax,ecx
00030BC1  11DA              adc edx,ebx
00030BC3  8945C0            mov [ebp-0x40],eax
00030BC6  8955C4            mov [ebp-0x3c],edx
00030BC9  8B45C0            mov eax,[ebp-0x40]
00030BCC  8B55C4            mov edx,[ebp-0x3c]
00030BCF  8B4DC8            mov ecx,[ebp-0x38]
00030BD2  8B5DCC            mov ebx,[ebp-0x34]
00030BD5  01C8              add eax,ecx
00030BD7  11DA              adc edx,ebx
00030BD9  83FA00            cmp edx,byte +0x0
00030BDC  776C              ja 0x30c4a
00030BDE  837DC400          cmp dword [ebp-0x3c],byte +0x0
00030BE2  7266              jc 0x30c4a
00030BE4  837DC400          cmp dword [ebp-0x3c],byte +0x0
00030BE8  7709              ja 0x30bf3
00030BEA  817DC0FFFF3F00    cmp dword [ebp-0x40],0x3fffff
00030BF1  7657              jna 0x30c4a
00030BF3  8B55F4            mov edx,[ebp-0xc]
00030BF6  89D0              mov eax,edx
00030BF8  C1E002            shl eax,0x2
00030BFB  01D0              add eax,edx
00030BFD  C1E002            shl eax,0x2
00030C00  89C2              mov edx,eax
00030C02  8B45D0            mov eax,[ebp-0x30]
00030C05  01D0              add eax,edx
00030C07  8B4010            mov eax,[eax+0x10]
00030C0A  83F801            cmp eax,byte +0x1
00030C0D  753B              jnz 0x30c4a
00030C0F  8B45C0            mov eax,[ebp-0x40]
00030C12  8B55C4            mov edx,[ebp-0x3c]
00030C15  8B4DC8            mov ecx,[ebp-0x38]
00030C18  8B5DCC            mov ebx,[ebp-0x34]
00030C1B  01C8              add eax,ecx
00030C1D  11DA              adc edx,ebx
00030C1F  3B55EC            cmp edx,[ebp-0x14]
00030C22  7226              jc 0x30c4a
00030C24  3B55EC            cmp edx,[ebp-0x14]
00030C27  7705              ja 0x30c2e
00030C29  3B45E8            cmp eax,[ebp-0x18]
00030C2C  761C              jna 0x30c4a
00030C2E  8B45C0            mov eax,[ebp-0x40]
00030C31  8B55C4            mov edx,[ebp-0x3c]
00030C34  8B4DC8            mov ecx,[ebp-0x38]
00030C37  8B5DCC            mov ebx,[ebp-0x34]
00030C3A  01C8              add eax,ecx
00030C3C  11DA              adc edx,ebx
00030C3E  8945E8            mov [ebp-0x18],eax
00030C41  8955EC            mov [ebp-0x14],edx
00030C44  8B45F4            mov eax,[ebp-0xc]
00030C47  8945E4            mov [ebp-0x1c],eax
00030C4A  8B55F4            mov edx,[ebp-0xc]
00030C4D  89D0              mov eax,edx
00030C4F  C1E002            shl eax,0x2
00030C52  01D0              add eax,edx
00030C54  C1E002            shl eax,0x2
00030C57  89C2              mov edx,eax
00030C59  8B45D0            mov eax,[ebp-0x30]
00030C5C  01D0              add eax,edx
00030C5E  8B4010            mov eax,[eax+0x10]
00030C61  89442418          mov [esp+0x18],eax
00030C65  8B45C0            mov eax,[ebp-0x40]
00030C68  8B55C4            mov edx,[ebp-0x3c]
00030C6B  89442410          mov [esp+0x10],eax
00030C6F  89542414          mov [esp+0x14],edx
00030C73  8B45C8            mov eax,[ebp-0x38]
00030C76  8B55CC            mov edx,[ebp-0x34]
00030C79  89442408          mov [esp+0x8],eax
00030C7D  8954240C          mov [esp+0xc],edx
00030C81  8B45F4            mov eax,[ebp-0xc]
00030C84  89442404          mov [esp+0x4],eax
00030C88  C7042470330300    mov dword [esp],0x33370
00030C8F  E8E0140000        call dword 0x32174
00030C94  8B55F4            mov edx,[ebp-0xc]
00030C97  89D0              mov eax,edx
00030C99  C1E002            shl eax,0x2
00030C9C  01D0              add eax,edx
00030C9E  C1E002            shl eax,0x2
00030CA1  89C2              mov edx,eax
00030CA3  8B45D0            mov eax,[ebp-0x30]
00030CA6  01D0              add eax,edx
00030CA8  8B4010            mov eax,[eax+0x10]
00030CAB  89442418          mov [esp+0x18],eax
00030CAF  8B45C0            mov eax,[ebp-0x40]
00030CB2  8B55C4            mov edx,[ebp-0x3c]
00030CB5  89442410          mov [esp+0x10],eax
00030CB9  89542414          mov [esp+0x14],edx
00030CBD  8B45C8            mov eax,[ebp-0x38]
00030CC0  8B55CC            mov edx,[ebp-0x34]
00030CC3  89442408          mov [esp+0x8],eax
00030CC7  8954240C          mov [esp+0xc],edx
00030CCB  8B45F4            mov eax,[ebp-0xc]
00030CCE  89442404          mov [esp+0x4],eax
00030CD2  C7042470330300    mov dword [esp],0x33370
00030CD9  E83A200000        call dword 0x32d18
00030CDE  8345F401          add dword [ebp-0xc],byte +0x1
00030CE2  A14C010300        mov eax,[0x3014c]
00030CE7  3945F4            cmp [ebp-0xc],eax
00030CEA  0F8C40FEFFFF      jl dword 0x30b30
00030CF0  E8C71B0000        call dword 0x328bc
00030CF5  E8C21B0000        call dword 0x328bc
00030CFA  837DE4FF          cmp dword [ebp-0x1c],byte -0x1
00030CFE  7511              jnz 0x30d11
00030D00  C70424A0330300    mov dword [esp],0x333a0
00030D07  E80C200000        call dword 0x32d18
00030D0C  E872F8FFFF        call dword 0x30583
00030D11  A14C010300        mov eax,[0x3014c]
00030D16  89C2              mov edx,eax
00030D18  89D0              mov eax,edx
00030D1A  C1E002            shl eax,0x2
00030D1D  01D0              add eax,edx
00030D1F  C1E002            shl eax,0x2
00030D22  89C2              mov edx,eax
00030D24  8B45D0            mov eax,[ebp-0x30]
00030D27  01D0              add eax,edx
00030D29  C7400400000000    mov dword [eax+0x4],0x0
00030D30  A14C010300        mov eax,[0x3014c]
00030D35  89C2              mov edx,eax
00030D37  89D0              mov eax,edx
00030D39  C1E002            shl eax,0x2
00030D3C  01D0              add eax,edx
00030D3E  C1E002            shl eax,0x2
00030D41  89C2              mov edx,eax
00030D43  8B45D0            mov eax,[ebp-0x30]
00030D46  01D0              add eax,edx
00030D48  C70000000000      mov dword [eax],0x0
00030D4E  A14C010300        mov eax,[0x3014c]
00030D53  89C2              mov edx,eax
00030D55  89D0              mov eax,edx
00030D57  C1E002            shl eax,0x2
00030D5A  01D0              add eax,edx
00030D5C  C1E002            shl eax,0x2
00030D5F  89C2              mov edx,eax
00030D61  8B45D0            mov eax,[ebp-0x30]
00030D64  01D0              add eax,edx
00030D66  C7400C00000000    mov dword [eax+0xc],0x0
00030D6D  A14C010300        mov eax,[0x3014c]
00030D72  89C2              mov edx,eax
00030D74  89D0              mov eax,edx
00030D76  C1E002            shl eax,0x2
00030D79  01D0              add eax,edx
00030D7B  C1E002            shl eax,0x2
00030D7E  89C2              mov edx,eax
00030D80  8B45D0            mov eax,[ebp-0x30]
00030D83  01D0              add eax,edx
00030D85  C7400800000000    mov dword [eax+0x8],0x0
00030D8C  A14C010300        mov eax,[0x3014c]
00030D91  89C2              mov edx,eax
00030D93  89D0              mov eax,edx
00030D95  C1E002            shl eax,0x2
00030D98  01D0              add eax,edx
00030D9A  C1E002            shl eax,0x2
00030D9D  89C2              mov edx,eax
00030D9F  8B45D0            mov eax,[ebp-0x30]
00030DA2  01D0              add eax,edx
00030DA4  C74010FF000000    mov dword [eax+0x10],0xff
00030DAB  8B45E8            mov eax,[ebp-0x18]
00030DAE  8B55EC            mov edx,[ebp-0x14]
00030DB1  09D0              or eax,edx
00030DB3  85C0              test eax,eax
00030DB5  751D              jnz 0x30dd4
00030DB7  C70424C8330300    mov dword [esp],0x333c8
00030DBE  E8B1130000        call dword 0x32174
00030DC3  C70424C8330300    mov dword [esp],0x333c8
00030DCA  E8491F0000        call dword 0x32d18
00030DCF  E9AD0A0000        jmp dword 0x31881
00030DD4  8B45E8            mov eax,[ebp-0x18]
00030DD7  89442404          mov [esp+0x4],eax
00030DDB  C70424EC330300    mov dword [esp],0x333ec
00030DE2  E88D130000        call dword 0x32174
00030DE7  8B45E8            mov eax,[ebp-0x18]
00030DEA  2D00004000        sub eax,0x400000
00030DEF  8945BC            mov [ebp-0x44],eax
00030DF2  8B45BC            mov eax,[ebp-0x44]
00030DF5  89442404          mov [esp+0x4],eax
00030DF9  C70424FD330300    mov dword [esp],0x333fd
00030E00  E8131F0000        call dword 0x32d18
00030E05  8B45BC            mov eax,[ebp-0x44]
00030E08  25FFFF3F00        and eax,0x3fffff
00030E0D  89442404          mov [esp+0x4],eax
00030E11  C704240C340300    mov dword [esp],0x3340c
00030E18  E8FB1E0000        call dword 0x32d18
00030E1D  8165BC0000C0FF    and dword [ebp-0x44],0xffc00000
00030E24  8B45BC            mov eax,[ebp-0x44]
00030E27  89442404          mov [esp+0x4],eax
00030E2B  C704242B340300    mov dword [esp],0x3342b
00030E32  E8E11E0000        call dword 0x32d18
00030E37  8B45E4            mov eax,[ebp-0x1c]
00030E3A  89442404          mov [esp+0x4],eax
00030E3E  C7042438340300    mov dword [esp],0x33438
00030E45  E8CE1E0000        call dword 0x32d18
00030E4A  C704244C340300    mov dword [esp],0x3344c
00030E51  E8C21E0000        call dword 0x32d18
00030E56  8B55E4            mov edx,[ebp-0x1c]
00030E59  89D0              mov eax,edx
00030E5B  C1E002            shl eax,0x2
00030E5E  01D0              add eax,edx
00030E60  C1E002            shl eax,0x2
00030E63  89C2              mov edx,eax
00030E65  8B45D0            mov eax,[ebp-0x30]
00030E68  01D0              add eax,edx
00030E6A  8B08              mov ecx,[eax]
00030E6C  8B55E4            mov edx,[ebp-0x1c]
00030E6F  89D0              mov eax,edx
00030E71  C1E002            shl eax,0x2
00030E74  01D0              add eax,edx
00030E76  C1E002            shl eax,0x2
00030E79  89C2              mov edx,eax
00030E7B  8B45D0            mov eax,[ebp-0x30]
00030E7E  01D0              add eax,edx
00030E80  8B4008            mov eax,[eax+0x8]
00030E83  01C8              add eax,ecx
00030E85  8945B8            mov [ebp-0x48],eax
00030E88  8B55E4            mov edx,[ebp-0x1c]
00030E8B  89D0              mov eax,edx
00030E8D  C1E002            shl eax,0x2
00030E90  01D0              add eax,edx
00030E92  C1E002            shl eax,0x2
00030E95  89C2              mov edx,eax
00030E97  8B45D0            mov eax,[ebp-0x30]
00030E9A  8D0C02            lea ecx,[edx+eax]
00030E9D  8B55E4            mov edx,[ebp-0x1c]
00030EA0  89D0              mov eax,edx
00030EA2  C1E002            shl eax,0x2
00030EA5  01D0              add eax,edx
00030EA7  C1E002            shl eax,0x2
00030EAA  89C2              mov edx,eax
00030EAC  8B45D0            mov eax,[ebp-0x30]
00030EAF  01D0              add eax,edx
00030EB1  8B00              mov eax,[eax]
00030EB3  8B55BC            mov edx,[ebp-0x44]
00030EB6  89D3              mov ebx,edx
00030EB8  29C3              sub ebx,eax
00030EBA  89D8              mov eax,ebx
00030EBC  894108            mov [ecx+0x8],eax
00030EBF  8B45E4            mov eax,[ebp-0x1c]
00030EC2  8D5001            lea edx,[eax+0x1]
00030EC5  89D0              mov eax,edx
00030EC7  C1E002            shl eax,0x2
00030ECA  01D0              add eax,edx
00030ECC  C1E002            shl eax,0x2
00030ECF  89C2              mov edx,eax
00030ED1  8B45D0            mov eax,[ebp-0x30]
00030ED4  01D0              add eax,edx
00030ED6  8B4010            mov eax,[eax+0x10]
00030ED9  83F802            cmp eax,byte +0x2
00030EDC  0F858B000000      jnz dword 0x30f6d
00030EE2  8B45E4            mov eax,[ebp-0x1c]
00030EE5  8D5001            lea edx,[eax+0x1]
00030EE8  89D0              mov eax,edx
00030EEA  C1E002            shl eax,0x2
00030EED  01D0              add eax,edx
00030EEF  C1E002            shl eax,0x2
00030EF2  89C2              mov edx,eax
00030EF4  8B45D0            mov eax,[ebp-0x30]
00030EF7  01D0              add eax,edx
00030EF9  8B08              mov ecx,[eax]
00030EFB  8B45E4            mov eax,[ebp-0x1c]
00030EFE  8D5001            lea edx,[eax+0x1]
00030F01  89D0              mov eax,edx
00030F03  C1E002            shl eax,0x2
00030F06  01D0              add eax,edx
00030F08  C1E002            shl eax,0x2
00030F0B  89C2              mov edx,eax
00030F0D  8B45D0            mov eax,[ebp-0x30]
00030F10  01D0              add eax,edx
00030F12  8B4008            mov eax,[eax+0x8]
00030F15  01C8              add eax,ecx
00030F17  8945B8            mov [ebp-0x48],eax
00030F1A  8B45E4            mov eax,[ebp-0x1c]
00030F1D  8D5001            lea edx,[eax+0x1]
00030F20  89D0              mov eax,edx
00030F22  C1E002            shl eax,0x2
00030F25  01D0              add eax,edx
00030F27  C1E002            shl eax,0x2
00030F2A  89C2              mov edx,eax
00030F2C  8B45D0            mov eax,[ebp-0x30]
00030F2F  01C2              add edx,eax
00030F31  8B45BC            mov eax,[ebp-0x44]
00030F34  8902              mov [edx],eax
00030F36  8B45E4            mov eax,[ebp-0x1c]
00030F39  8D5001            lea edx,[eax+0x1]
00030F3C  89D0              mov eax,edx
00030F3E  C1E002            shl eax,0x2
00030F41  01D0              add eax,edx
00030F43  C1E002            shl eax,0x2
00030F46  89C2              mov edx,eax
00030F48  8B45D0            mov eax,[ebp-0x30]
00030F4B  01D0              add eax,edx
00030F4D  8B55BC            mov edx,[ebp-0x44]
00030F50  8B4DB8            mov ecx,[ebp-0x48]
00030F53  89CB              mov ebx,ecx
00030F55  29D3              sub ebx,edx
00030F57  89DA              mov edx,ebx
00030F59  895008            mov [eax+0x8],edx
00030F5C  C7042490340300    mov dword [esp],0x33490
00030F63  E8B01D0000        call dword 0x32d18
00030F68  E912010000        jmp dword 0x3107f
00030F6D  A14C010300        mov eax,[0x3014c]
00030F72  8945F4            mov [ebp-0xc],eax
00030F75  EB4B              jmp short 0x30fc2
00030F77  8B55F4            mov edx,[ebp-0xc]
00030F7A  89D0              mov eax,edx
00030F7C  C1E002            shl eax,0x2
00030F7F  01D0              add eax,edx
00030F81  C1E002            shl eax,0x2
00030F84  89C2              mov edx,eax
00030F86  8B45D0            mov eax,[ebp-0x30]
00030F89  01C2              add edx,eax
00030F8B  8B45F4            mov eax,[ebp-0xc]
00030F8E  8D48FF            lea ecx,[eax-0x1]
00030F91  89C8              mov eax,ecx
00030F93  C1E002            shl eax,0x2
00030F96  01C8              add eax,ecx
00030F98  C1E002            shl eax,0x2
00030F9B  89C1              mov ecx,eax
00030F9D  8B45D0            mov eax,[ebp-0x30]
00030FA0  01C8              add eax,ecx
00030FA2  8B08              mov ecx,[eax]
00030FA4  890A              mov [edx],ecx
00030FA6  8B4804            mov ecx,[eax+0x4]
00030FA9  894A04            mov [edx+0x4],ecx
00030FAC  8B4808            mov ecx,[eax+0x8]
00030FAF  894A08            mov [edx+0x8],ecx
00030FB2  8B480C            mov ecx,[eax+0xc]
00030FB5  894A0C            mov [edx+0xc],ecx
00030FB8  8B4010            mov eax,[eax+0x10]
00030FBB  894210            mov [edx+0x10],eax
00030FBE  836DF401          sub dword [ebp-0xc],byte +0x1
00030FC2  8B45F4            mov eax,[ebp-0xc]
00030FC5  3B45E4            cmp eax,[ebp-0x1c]
00030FC8  7FAD              jg 0x30f77
00030FCA  8B45E4            mov eax,[ebp-0x1c]
00030FCD  8D5001            lea edx,[eax+0x1]
00030FD0  89D0              mov eax,edx
00030FD2  C1E002            shl eax,0x2
00030FD5  01D0              add eax,edx
00030FD7  C1E002            shl eax,0x2
00030FDA  89C2              mov edx,eax
00030FDC  8B45D0            mov eax,[ebp-0x30]
00030FDF  01D0              add eax,edx
00030FE1  C7400400000000    mov dword [eax+0x4],0x0
00030FE8  8B45E4            mov eax,[ebp-0x1c]
00030FEB  8D5001            lea edx,[eax+0x1]
00030FEE  89D0              mov eax,edx
00030FF0  C1E002            shl eax,0x2
00030FF3  01D0              add eax,edx
00030FF5  C1E002            shl eax,0x2
00030FF8  89C2              mov edx,eax
00030FFA  8B45D0            mov eax,[ebp-0x30]
00030FFD  01C2              add edx,eax
00030FFF  8B45BC            mov eax,[ebp-0x44]
00031002  8902              mov [edx],eax
00031004  8B45E4            mov eax,[ebp-0x1c]
00031007  8D5001            lea edx,[eax+0x1]
0003100A  89D0              mov eax,edx
0003100C  C1E002            shl eax,0x2
0003100F  01D0              add eax,edx
00031011  C1E002            shl eax,0x2
00031014  89C2              mov edx,eax
00031016  8B45D0            mov eax,[ebp-0x30]
00031019  01D0              add eax,edx
0003101B  C7400C00000000    mov dword [eax+0xc],0x0
00031022  8B45E4            mov eax,[ebp-0x1c]
00031025  8D5001            lea edx,[eax+0x1]
00031028  89D0              mov eax,edx
0003102A  C1E002            shl eax,0x2
0003102D  01D0              add eax,edx
0003102F  C1E002            shl eax,0x2
00031032  89C2              mov edx,eax
00031034  8B45D0            mov eax,[ebp-0x30]
00031037  01D0              add eax,edx
00031039  8B55BC            mov edx,[ebp-0x44]
0003103C  8B4DB8            mov ecx,[ebp-0x48]
0003103F  89CB              mov ebx,ecx
00031041  29D3              sub ebx,edx
00031043  89DA              mov edx,ebx
00031045  895008            mov [eax+0x8],edx
00031048  8B45E4            mov eax,[ebp-0x1c]
0003104B  8D5001            lea edx,[eax+0x1]
0003104E  89D0              mov eax,edx
00031050  C1E002            shl eax,0x2
00031053  01D0              add eax,edx
00031055  C1E002            shl eax,0x2
00031058  89C2              mov edx,eax
0003105A  8B45D0            mov eax,[ebp-0x30]
0003105D  01D0              add eax,edx
0003105F  C7401002000000    mov dword [eax+0x10],0x2
00031066  A14C010300        mov eax,[0x3014c]
0003106B  83C001            add eax,byte +0x1
0003106E  A34C010300        mov [0x3014c],eax
00031073  C70424B8340300    mov dword [esp],0x334b8
0003107A  E8991C0000        call dword 0x32d18
0003107F  E838180000        call dword 0x328bc
00031084  E833180000        call dword 0x328bc
00031089  C70424CE340300    mov dword [esp],0x334ce
00031090  E8831C0000        call dword 0x32d18
00031095  C745F400000000    mov dword [ebp-0xc],0x0
0003109C  E9E7000000        jmp dword 0x31188
000310A1  8B55F4            mov edx,[ebp-0xc]
000310A4  89D0              mov eax,edx
000310A6  C1E002            shl eax,0x2
000310A9  01D0              add eax,edx
000310AB  C1E002            shl eax,0x2
000310AE  89C2              mov edx,eax
000310B0  8B45D0            mov eax,[ebp-0x30]
000310B3  01D0              add eax,edx
000310B5  8B4004            mov eax,[eax+0x4]
000310B8  BA00000000        mov edx,0x0
000310BD  89C1              mov ecx,eax
000310BF  89D3              mov ebx,edx
000310C1  89CB              mov ebx,ecx
000310C3  B900000000        mov ecx,0x0
000310C8  8B55F4            mov edx,[ebp-0xc]
000310CB  89D0              mov eax,edx
000310CD  C1E002            shl eax,0x2
000310D0  01D0              add eax,edx
000310D2  C1E002            shl eax,0x2
000310D5  89C2              mov edx,eax
000310D7  8B45D0            mov eax,[ebp-0x30]
000310DA  01D0              add eax,edx
000310DC  8B00              mov eax,[eax]
000310DE  BA00000000        mov edx,0x0
000310E3  01C8              add eax,ecx
000310E5  11DA              adc edx,ebx
000310E7  8945C8            mov [ebp-0x38],eax
000310EA  8955CC            mov [ebp-0x34],edx
000310ED  8B55F4            mov edx,[ebp-0xc]
000310F0  89D0              mov eax,edx
000310F2  C1E002            shl eax,0x2
000310F5  01D0              add eax,edx
000310F7  C1E002            shl eax,0x2
000310FA  89C2              mov edx,eax
000310FC  8B45D0            mov eax,[ebp-0x30]
000310FF  01D0              add eax,edx
00031101  8B400C            mov eax,[eax+0xc]
00031104  BA00000000        mov edx,0x0
00031109  89C1              mov ecx,eax
0003110B  89D3              mov ebx,edx
0003110D  89CB              mov ebx,ecx
0003110F  B900000000        mov ecx,0x0
00031114  8B55F4            mov edx,[ebp-0xc]
00031117  89D0              mov eax,edx
00031119  C1E002            shl eax,0x2
0003111C  01D0              add eax,edx
0003111E  C1E002            shl eax,0x2
00031121  89C2              mov edx,eax
00031123  8B45D0            mov eax,[ebp-0x30]
00031126  01D0              add eax,edx
00031128  8B4008            mov eax,[eax+0x8]
0003112B  BA00000000        mov edx,0x0
00031130  01C8              add eax,ecx
00031132  11DA              adc edx,ebx
00031134  8945C0            mov [ebp-0x40],eax
00031137  8955C4            mov [ebp-0x3c],edx
0003113A  8B55F4            mov edx,[ebp-0xc]
0003113D  89D0              mov eax,edx
0003113F  C1E002            shl eax,0x2
00031142  01D0              add eax,edx
00031144  C1E002            shl eax,0x2
00031147  89C2              mov edx,eax
00031149  8B45D0            mov eax,[ebp-0x30]
0003114C  01D0              add eax,edx
0003114E  8B4010            mov eax,[eax+0x10]
00031151  89442418          mov [esp+0x18],eax
00031155  8B45C0            mov eax,[ebp-0x40]
00031158  8B55C4            mov edx,[ebp-0x3c]
0003115B  89442410          mov [esp+0x10],eax
0003115F  89542414          mov [esp+0x14],edx
00031163  8B45C8            mov eax,[ebp-0x38]
00031166  8B55CC            mov edx,[ebp-0x34]
00031169  89442408          mov [esp+0x8],eax
0003116D  8954240C          mov [esp+0xc],edx
00031171  8B45F4            mov eax,[ebp-0xc]
00031174  89442404          mov [esp+0x4],eax
00031178  C7042470330300    mov dword [esp],0x33370
0003117F  E8941B0000        call dword 0x32d18
00031184  8345F401          add dword [ebp-0xc],byte +0x1
00031188  A14C010300        mov eax,[0x3014c]
0003118D  3945F4            cmp [ebp-0xc],eax
00031190  0F8C0BFFFFFF      jl dword 0x310a1
00031196  A14C010300        mov eax,[0x3014c]
0003119B  89442404          mov [esp+0x4],eax
0003119F  C70424D8340300    mov dword [esp],0x334d8
000311A6  E86D1B0000        call dword 0x32d18
000311AB  E80C170000        call dword 0x328bc
000311B0  E807170000        call dword 0x328bc
000311B5  A14C010300        mov eax,[0x3014c]
000311BA  89C2              mov edx,eax
000311BC  89D0              mov eax,edx
000311BE  C1E002            shl eax,0x2
000311C1  01D0              add eax,edx
000311C3  C1E002            shl eax,0x2
000311C6  89C2              mov edx,eax
000311C8  8B45D0            mov eax,[ebp-0x30]
000311CB  01D0              add eax,edx
000311CD  C7400400000000    mov dword [eax+0x4],0x0
000311D4  A14C010300        mov eax,[0x3014c]
000311D9  89C2              mov edx,eax
000311DB  89D0              mov eax,edx
000311DD  C1E002            shl eax,0x2
000311E0  01D0              add eax,edx
000311E2  C1E002            shl eax,0x2
000311E5  89C2              mov edx,eax
000311E7  8B45D0            mov eax,[ebp-0x30]
000311EA  01D0              add eax,edx
000311EC  C70000000000      mov dword [eax],0x0
000311F2  A14C010300        mov eax,[0x3014c]
000311F7  89C2              mov edx,eax
000311F9  89D0              mov eax,edx
000311FB  C1E002            shl eax,0x2
000311FE  01D0              add eax,edx
00031200  C1E002            shl eax,0x2
00031203  89C2              mov edx,eax
00031205  8B45D0            mov eax,[ebp-0x30]
00031208  01D0              add eax,edx
0003120A  C7400C00000000    mov dword [eax+0xc],0x0
00031211  A14C010300        mov eax,[0x3014c]
00031216  89C2              mov edx,eax
00031218  89D0              mov eax,edx
0003121A  C1E002            shl eax,0x2
0003121D  01D0              add eax,edx
0003121F  C1E002            shl eax,0x2
00031222  89C2              mov edx,eax
00031224  8B45D0            mov eax,[ebp-0x30]
00031227  01D0              add eax,edx
00031229  C7400800000000    mov dword [eax+0x8],0x0
00031230  A14C010300        mov eax,[0x3014c]
00031235  89C2              mov edx,eax
00031237  89D0              mov eax,edx
00031239  C1E002            shl eax,0x2
0003123C  01D0              add eax,edx
0003123E  C1E002            shl eax,0x2
00031241  89C2              mov edx,eax
00031243  8B45D0            mov eax,[ebp-0x30]
00031246  01D0              add eax,edx
00031248  C74010FF000000    mov dword [eax+0x10],0xff
0003124F  8B45BC            mov eax,[ebp-0x44]
00031252  89442404          mov [esp+0x4],eax
00031256  C70424F4340300    mov dword [esp],0x334f4
0003125D  E8B61A0000        call dword 0x32d18
00031262  E855160000        call dword 0x328bc
00031267  E850160000        call dword 0x328bc
0003126C  8B45BC            mov eax,[ebp-0x44]
0003126F  8945B4            mov [ebp-0x4c],eax
00031272  C745F400000000    mov dword [ebp-0xc],0x0
00031279  EB0F              jmp short 0x3128a
0003127B  8B55F4            mov edx,[ebp-0xc]
0003127E  8B45B4            mov eax,[ebp-0x4c]
00031281  01D0              add eax,edx
00031283  C60000            mov byte [eax],0x0
00031286  8345F401          add dword [ebp-0xc],byte +0x1
0003128A  817DF4FFFF3F00    cmp dword [ebp-0xc],0x3fffff
00031291  7EE8              jng 0x3127b
00031293  C704241A350300    mov dword [esp],0x3351a
0003129A  E8791A0000        call dword 0x32d18
0003129F  E818160000        call dword 0x328bc
000312A4  E813160000        call dword 0x328bc
000312A9  8B45BC            mov eax,[ebp-0x44]
000312AC  89442408          mov [esp+0x8],eax
000312B0  C744240400E00300  mov dword [esp+0x4],0x3e000
000312B8  C704242C350300    mov dword [esp],0x3352c
000312BF  E8541A0000        call dword 0x32d18
000312C4  0FB705883C0300    movzx eax,word [dword 0x33c88]
000312CB  0FB7C0            movzx eax,ax
000312CE  89442404          mov [esp+0x4],eax
000312D2  C7042453350300    mov dword [esp],0x33553
000312D9  E83A1A0000        call dword 0x32d18
000312DE  8B45BC            mov eax,[ebp-0x44]
000312E1  89442404          mov [esp+0x4],eax
000312E5  C7042463350300    mov dword [esp],0x33563
000312EC  E8271A0000        call dword 0x32d18
000312F1  C744240400E00300  mov dword [esp+0x4],0x3e000
000312F9  C704246D350300    mov dword [esp],0x3356d
00031300  E8131A0000        call dword 0x32d18
00031305  E8B2150000        call dword 0x328bc
0003130A  E8AD150000        call dword 0x328bc
0003130F  C745E000000000    mov dword [ebp-0x20],0x0
00031316  EB6D              jmp short 0x31385
00031318  8B55E0            mov edx,[ebp-0x20]
0003131B  8B45BC            mov eax,[ebp-0x44]
0003131E  01D0              add eax,edx
00031320  89C2              mov edx,eax
00031322  0FB705883C0300    movzx eax,word [dword 0x33c88]
00031329  0FB7C8            movzx ecx,ax
0003132C  8B45E0            mov eax,[ebp-0x20]
0003132F  8D98FF010000      lea ebx,[eax+0x1ff]
00031335  85C0              test eax,eax
00031337  0F48C3            cmovs eax,ebx
0003133A  C1F809            sar eax,0x9
0003133D  01C8              add eax,ecx
0003133F  89542404          mov [esp+0x4],edx
00031343  890424            mov [esp],eax
00031346  E849F3FFFF        call dword 0x30694
0003134B  85C0              test eax,eax
0003134D  7415              jz 0x31364
0003134F  C7042479350300    mov dword [esp],0x33579
00031356  E8BD190000        call dword 0x32d18
0003135B  8145E000020000    add dword [ebp-0x20],0x200
00031362  EB21              jmp short 0x31385
00031364  8B45E0            mov eax,[ebp-0x20]
00031367  89442404          mov [esp+0x4],eax
0003136B  C704247B350300    mov dword [esp],0x3357b
00031372  E8A1190000        call dword 0x32d18
00031377  C7042496350300    mov dword [esp],0x33596
0003137E  E895190000        call dword 0x32d18
00031383  EBF2              jmp short 0x31377
00031385  817DE0FFDF0300    cmp dword [ebp-0x20],0x3dfff
0003138C  7E8A              jng 0x31318
0003138E  C704249E350300    mov dword [esp],0x3359e
00031395  E87E190000        call dword 0x32d18
0003139A  C70424A0350300    mov dword [esp],0x335a0
000313A1  E872190000        call dword 0x32d18
000313A6  E811150000        call dword 0x328bc
000313AB  E80C150000        call dword 0x328bc
000313B0  8B45BC            mov eax,[ebp-0x44]
000313B3  0500F00300        add eax,0x3f000
000313B8  2500F0FFFF        and eax,0xfffff000
000313BD  8945B0            mov [ebp-0x50],eax
000313C0  8B45B0            mov eax,[ebp-0x50]
000313C3  89442404          mov [esp+0x4],eax
000313C7  C70424B1350300    mov dword [esp],0x335b1
000313CE  E8A10D0000        call dword 0x32174
000313D3  8B45B0            mov eax,[ebp-0x50]
000313D6  C74424086F000000  mov dword [esp+0x8],0x6f
000313DE  C744240400000500  mov dword [esp+0x4],0x50000
000313E6  890424            mov [esp],eax
000313E9  E8340F0000        call dword 0x32322
000313EE  8B45BC            mov eax,[ebp-0x44]
000313F1  0500000400        add eax,0x40000
000313F6  2500F0FFFF        and eax,0xfffff000
000313FB  A30F050300        mov [0x3050f],eax
00031400  A10F050300        mov eax,[0x3050f]
00031405  89442404          mov [esp+0x4],eax
00031409  C70424BE350300    mov dword [esp],0x335be
00031410  E803190000        call dword 0x32d18
00031415  A10F050300        mov eax,[0x3050f]
0003141A  8945AC            mov [ebp-0x54],eax
0003141D  C744240400100000  mov dword [esp+0x4],0x1000
00031425  8B45AC            mov eax,[ebp-0x54]
00031428  890424            mov [esp],eax
0003142B  E8C40E0000        call dword 0x322f4
00031430  A10F050300        mov eax,[0x3050f]
00031435  0500100000        add eax,0x1000
0003143A  8945A8            mov [ebp-0x58],eax
0003143D  C744240400100000  mov dword [esp+0x4],0x1000
00031445  8B45A8            mov eax,[ebp-0x58]
00031448  890424            mov [esp],eax
0003144B  E8A40E0000        call dword 0x322f4
00031450  A10F050300        mov eax,[0x3050f]
00031455  0500200000        add eax,0x2000
0003145A  8945A4            mov [ebp-0x5c],eax
0003145D  C744240400100000  mov dword [esp+0x4],0x1000
00031465  8B45A4            mov eax,[ebp-0x5c]
00031468  890424            mov [esp],eax
0003146B  E8840E0000        call dword 0x322f4
00031470  8B45AC            mov eax,[ebp-0x54]
00031473  0FB610            movzx edx,byte [eax]
00031476  83CA01            or edx,byte +0x1
00031479  8810              mov [eax],dl
0003147B  8B45AC            mov eax,[ebp-0x54]
0003147E  0FB610            movzx edx,byte [eax]
00031481  83CA02            or edx,byte +0x2
00031484  8810              mov [eax],dl
00031486  8B45A8            mov eax,[ebp-0x58]
00031489  C1E80C            shr eax,0xc
0003148C  89C2              mov edx,eax
0003148E  81E2FFFFFF00      and edx,0xffffff
00031494  8B45AC            mov eax,[ebp-0x54]
00031497  89D1              mov ecx,edx
00031499  83E10F            and ecx,byte +0xf
0003149C  89CB              mov ebx,ecx
0003149E  C1E304            shl ebx,0x4
000314A1  0FB64801          movzx ecx,byte [eax+0x1]
000314A5  83E10F            and ecx,byte +0xf
000314A8  09D9              or ecx,ebx
000314AA  884801            mov [eax+0x1],cl
000314AD  89D1              mov ecx,edx
000314AF  C1E904            shr ecx,0x4
000314B2  0FB6D9            movzx ebx,cl
000314B5  0FB64802          movzx ecx,byte [eax+0x2]
000314B9  83E100            and ecx,byte +0x0
000314BC  09D9              or ecx,ebx
000314BE  884802            mov [eax+0x2],cl
000314C1  89D1              mov ecx,edx
000314C3  C1E90C            shr ecx,0xc
000314C6  0FB6D9            movzx ebx,cl
000314C9  0FB64803          movzx ecx,byte [eax+0x3]
000314CD  83E100            and ecx,byte +0x0
000314D0  09D9              or ecx,ebx
000314D2  884803            mov [eax+0x3],cl
000314D5  C1EA14            shr edx,0x14
000314D8  83E20F            and edx,byte +0xf
000314DB  89D1              mov ecx,edx
000314DD  83E10F            and ecx,byte +0xf
000314E0  0FB65004          movzx edx,byte [eax+0x4]
000314E4  83E2F0            and edx,byte -0x10
000314E7  09CA              or edx,ecx
000314E9  885004            mov [eax+0x4],dl
000314EC  8B45A8            mov eax,[ebp-0x58]
000314EF  0FB610            movzx edx,byte [eax]
000314F2  83CA01            or edx,byte +0x1
000314F5  8810              mov [eax],dl
000314F7  8B45A8            mov eax,[ebp-0x58]
000314FA  0FB610            movzx edx,byte [eax]
000314FD  83CA02            or edx,byte +0x2
00031500  8810              mov [eax],dl
00031502  8B45A4            mov eax,[ebp-0x5c]
00031505  C1E80C            shr eax,0xc
00031508  89C2              mov edx,eax
0003150A  81E2FFFFFF00      and edx,0xffffff
00031510  8B45A8            mov eax,[ebp-0x58]
00031513  89D1              mov ecx,edx
00031515  83E10F            and ecx,byte +0xf
00031518  89CB              mov ebx,ecx
0003151A  C1E304            shl ebx,0x4
0003151D  0FB64801          movzx ecx,byte [eax+0x1]
00031521  83E10F            and ecx,byte +0xf
00031524  09D9              or ecx,ebx
00031526  884801            mov [eax+0x1],cl
00031529  89D1              mov ecx,edx
0003152B  C1E904            shr ecx,0x4
0003152E  0FB6D9            movzx ebx,cl
00031531  0FB64802          movzx ecx,byte [eax+0x2]
00031535  83E100            and ecx,byte +0x0
00031538  09D9              or ecx,ebx
0003153A  884802            mov [eax+0x2],cl
0003153D  89D1              mov ecx,edx
0003153F  C1E90C            shr ecx,0xc
00031542  0FB6D9            movzx ebx,cl
00031545  0FB64803          movzx ecx,byte [eax+0x3]
00031549  83E100            and ecx,byte +0x0
0003154C  09D9              or ecx,ebx
0003154E  884803            mov [eax+0x3],cl
00031551  C1EA14            shr edx,0x14
00031554  83E20F            and edx,byte +0xf
00031557  89D1              mov ecx,edx
00031559  83E10F            and ecx,byte +0xf
0003155C  0FB65004          movzx edx,byte [eax+0x4]
00031560  83E2F0            and edx,byte -0x10
00031563  09CA              or edx,ecx
00031565  885004            mov [eax+0x4],dl
00031568  8B45A4            mov eax,[ebp-0x5c]
0003156B  0FB610            movzx edx,byte [eax]
0003156E  83CA01            or edx,byte +0x1
00031571  8810              mov [eax],dl
00031573  8B45A4            mov eax,[ebp-0x5c]
00031576  0FB610            movzx edx,byte [eax]
00031579  83CA02            or edx,byte +0x2
0003157C  8810              mov [eax],dl
0003157E  8B45A4            mov eax,[ebp-0x5c]
00031581  0FB610            movzx edx,byte [eax]
00031584  83CA80            or edx,byte -0x80
00031587  8810              mov [eax],dl
00031589  8B45A4            mov eax,[ebp-0x5c]
0003158C  0FB65001          movzx edx,byte [eax+0x1]
00031590  83E21F            and edx,byte +0x1f
00031593  885001            mov [eax+0x1],dl
00031596  0FB65002          movzx edx,byte [eax+0x2]
0003159A  83E200            and edx,byte +0x0
0003159D  885002            mov [eax+0x2],dl
000315A0  0FB65003          movzx edx,byte [eax+0x3]
000315A4  83E200            and edx,byte +0x0
000315A7  885003            mov [eax+0x3],dl
000315AA  0FB65004          movzx edx,byte [eax+0x4]
000315AE  83E2F0            and edx,byte -0x10
000315B1  885004            mov [eax+0x4],dl
000315B4  8B45A4            mov eax,[ebp-0x5c]
000315B7  83C008            add eax,byte +0x8
000315BA  0FB610            movzx edx,byte [eax]
000315BD  83CA01            or edx,byte +0x1
000315C0  8810              mov [eax],dl
000315C2  8B45A4            mov eax,[ebp-0x5c]
000315C5  83C008            add eax,byte +0x8
000315C8  0FB610            movzx edx,byte [eax]
000315CB  83CA02            or edx,byte +0x2
000315CE  8810              mov [eax],dl
000315D0  8B45A4            mov eax,[ebp-0x5c]
000315D3  83C008            add eax,byte +0x8
000315D6  0FB610            movzx edx,byte [eax]
000315D9  83CA80            or edx,byte -0x80
000315DC  8810              mov [eax],dl
000315DE  8B45A4            mov eax,[ebp-0x5c]
000315E1  83C008            add eax,byte +0x8
000315E4  0FB65001          movzx edx,byte [eax+0x1]
000315E8  83E21F            and edx,byte +0x1f
000315EB  885001            mov [eax+0x1],dl
000315EE  0FB65002          movzx edx,byte [eax+0x2]
000315F2  83E200            and edx,byte +0x0
000315F5  83CA20            or edx,byte +0x20
000315F8  885002            mov [eax+0x2],dl
000315FB  0FB65003          movzx edx,byte [eax+0x3]
000315FF  83E200            and edx,byte +0x0
00031602  885003            mov [eax+0x3],dl
00031605  0FB65004          movzx edx,byte [eax+0x4]
00031609  83E2F0            and edx,byte -0x10
0003160C  885004            mov [eax+0x4],dl
0003160F  8B45A4            mov eax,[ebp-0x5c]
00031612  83C010            add eax,byte +0x10
00031615  0FB610            movzx edx,byte [eax]
00031618  83CA01            or edx,byte +0x1
0003161B  8810              mov [eax],dl
0003161D  8B45A4            mov eax,[ebp-0x5c]
00031620  83C010            add eax,byte +0x10
00031623  0FB610            movzx edx,byte [eax]
00031626  83CA02            or edx,byte +0x2
00031629  8810              mov [eax],dl
0003162B  8B45A4            mov eax,[ebp-0x5c]
0003162E  83C010            add eax,byte +0x10
00031631  0FB610            movzx edx,byte [eax]
00031634  83CA80            or edx,byte -0x80
00031637  8810              mov [eax],dl
00031639  8B45A4            mov eax,[ebp-0x5c]
0003163C  83C010            add eax,byte +0x10
0003163F  8B55BC            mov edx,[ebp-0x44]
00031642  C1EA0D            shr edx,0xd
00031645  81E2FFFF7F00      and edx,0x7fffff
0003164B  89D1              mov ecx,edx
0003164D  83E107            and ecx,byte +0x7
00031650  89CB              mov ebx,ecx
00031652  C1E305            shl ebx,0x5
00031655  0FB64801          movzx ecx,byte [eax+0x1]
00031659  83E11F            and ecx,byte +0x1f
0003165C  09D9              or ecx,ebx
0003165E  884801            mov [eax+0x1],cl
00031661  89D1              mov ecx,edx
00031663  C1E903            shr ecx,0x3
00031666  0FB6D9            movzx ebx,cl
00031669  0FB64802          movzx ecx,byte [eax+0x2]
0003166D  83E100            and ecx,byte +0x0
00031670  09D9              or ecx,ebx
00031672  884802            mov [eax+0x2],cl
00031675  89D1              mov ecx,edx
00031677  C1E90B            shr ecx,0xb
0003167A  0FB6D9            movzx ebx,cl
0003167D  0FB64803          movzx ecx,byte [eax+0x3]
00031681  83E100            and ecx,byte +0x0
00031684  09D9              or ecx,ebx
00031686  884803            mov [eax+0x3],cl
00031689  C1EA13            shr edx,0x13
0003168C  83E20F            and edx,byte +0xf
0003168F  89D1              mov ecx,edx
00031691  83E10F            and ecx,byte +0xf
00031694  0FB65004          movzx edx,byte [eax+0x4]
00031698  83E2F0            and edx,byte -0x10
0003169B  09CA              or edx,ecx
0003169D  885004            mov [eax+0x4],dl
000316A0  8B45BC            mov eax,[ebp-0x44]
000316A3  A313050300        mov [0x30513],eax
000316A8  8B45A4            mov eax,[ebp-0x5c]
000316AB  83C018            add eax,byte +0x18
000316AE  0FB610            movzx edx,byte [eax]
000316B1  83CA01            or edx,byte +0x1
000316B4  8810              mov [eax],dl
000316B6  8B45A4            mov eax,[ebp-0x5c]
000316B9  83C018            add eax,byte +0x18
000316BC  0FB610            movzx edx,byte [eax]
000316BF  83CA02            or edx,byte +0x2
000316C2  8810              mov [eax],dl
000316C4  8B45A4            mov eax,[ebp-0x5c]
000316C7  83C018            add eax,byte +0x18
000316CA  0FB610            movzx edx,byte [eax]
000316CD  83CA80            or edx,byte -0x80
000316D0  8810              mov [eax],dl
000316D2  8B45A4            mov eax,[ebp-0x5c]
000316D5  83C018            add eax,byte +0x18
000316D8  8B55BC            mov edx,[ebp-0x44]
000316DB  81C200002000      add edx,0x200000
000316E1  C1EA0D            shr edx,0xd
000316E4  81E2FFFF7F00      and edx,0x7fffff
000316EA  89D1              mov ecx,edx
000316EC  83E107            and ecx,byte +0x7
000316EF  89CB              mov ebx,ecx
000316F1  C1E305            shl ebx,0x5
000316F4  0FB64801          movzx ecx,byte [eax+0x1]
000316F8  83E11F            and ecx,byte +0x1f
000316FB  09D9              or ecx,ebx
000316FD  884801            mov [eax+0x1],cl
00031700  89D1              mov ecx,edx
00031702  C1E903            shr ecx,0x3
00031705  0FB6D9            movzx ebx,cl
00031708  0FB64802          movzx ecx,byte [eax+0x2]
0003170C  83E100            and ecx,byte +0x0
0003170F  09D9              or ecx,ebx
00031711  884802            mov [eax+0x2],cl
00031714  89D1              mov ecx,edx
00031716  C1E90B            shr ecx,0xb
00031719  0FB6D9            movzx ebx,cl
0003171C  0FB64803          movzx ecx,byte [eax+0x3]
00031720  83E100            and ecx,byte +0x0
00031723  09D9              or ecx,ebx
00031725  884803            mov [eax+0x3],cl
00031728  C1EA13            shr edx,0x13
0003172B  83E20F            and edx,byte +0xf
0003172E  89D1              mov ecx,edx
00031730  83E10F            and ecx,byte +0xf
00031733  0FB65004          movzx edx,byte [eax+0x4]
00031737  83E2F0            and edx,byte -0x10
0003173A  09CA              or edx,ecx
0003173C  885004            mov [eax+0x4],dl
0003173F  8B45A8            mov eax,[ebp-0x58]
00031742  0FB65001          movzx edx,byte [eax+0x1]
00031746  C0EA04            shr dl,0x4
00031749  0FB6D2            movzx edx,dl
0003174C  0FB64802          movzx ecx,byte [eax+0x2]
00031750  C1E104            shl ecx,0x4
00031753  09D1              or ecx,edx
00031755  0FB65003          movzx edx,byte [eax+0x3]
00031759  C1E20C            shl edx,0xc
0003175C  09CA              or edx,ecx
0003175E  0FB64004          movzx eax,byte [eax+0x4]
00031762  83E00F            and eax,byte +0xf
00031765  C1E014            shl eax,0x14
00031768  09D0              or eax,edx
0003176A  89442404          mov [esp+0x4],eax
0003176E  C70424D2350300    mov dword [esp],0x335d2
00031775  E8FA090000        call dword 0x32174
0003177A  8B45BC            mov eax,[ebp-0x44]
0003177D  89442404          mov [esp+0x4],eax
00031781  C70424E9350300    mov dword [esp],0x335e9
00031788  E8E7090000        call dword 0x32174
0003178D  8B45BC            mov eax,[ebp-0x44]
00031790  8945D4            mov [ebp-0x2c],eax
00031793  C745D800000000    mov dword [ebp-0x28],0x0
0003179A  EB5D              jmp short 0x317f9
0003179C  8B45D8            mov eax,[ebp-0x28]
0003179F  C1E004            shl eax,0x4
000317A2  89C2              mov edx,eax
000317A4  8B45BC            mov eax,[ebp-0x44]
000317A7  01D0              add eax,edx
000317A9  89442404          mov [esp+0x4],eax
000317AD  C70424F3350300    mov dword [esp],0x335f3
000317B4  E8BB090000        call dword 0x32174
000317B9  C745DC00000000    mov dword [ebp-0x24],0x0
000317C0  EB21              jmp short 0x317e3
000317C2  8B45D4            mov eax,[ebp-0x2c]
000317C5  0FB600            movzx eax,byte [eax]
000317C8  0FB6C0            movzx eax,al
000317CB  89442404          mov [esp+0x4],eax
000317CF  C70424F9350300    mov dword [esp],0x335f9
000317D6  E899090000        call dword 0x32174
000317DB  8345DC01          add dword [ebp-0x24],byte +0x1
000317DF  8345D401          add dword [ebp-0x2c],byte +0x1
000317E3  837DDC0F          cmp dword [ebp-0x24],byte +0xf
000317E7  7ED9              jng 0x317c2
000317E9  C70424FD350300    mov dword [esp],0x335fd
000317F0  E82D090000        call dword 0x32122
000317F5  8345D801          add dword [ebp-0x28],byte +0x1
000317F9  837DD807          cmp dword [ebp-0x28],byte +0x7
000317FD  7E9D              jng 0x3179c
000317FF  8B45BC            mov eax,[ebp-0x44]
00031802  8945A0            mov [ebp-0x60],eax
00031805  8B45A0            mov eax,[ebp-0x60]
00031808  83C010            add eax,byte +0x10
0003180B  C70000000000      mov dword [eax],0x0
00031811  C7400400000000    mov dword [eax+0x4],0x0
00031818  8B45A0            mov eax,[ebp-0x60]
0003181B  8D4818            lea ecx,[eax+0x18]
0003181E  8B45BC            mov eax,[ebp-0x44]
00031821  BA00000000        mov edx,0x0
00031826  8901              mov [ecx],eax
00031828  895104            mov [ecx+0x4],edx
0003182B  8B45A0            mov eax,[ebp-0x60]
0003182E  83C020            add eax,byte +0x20
00031831  C70000004400      mov dword [eax],0x440000
00031837  C7400400000000    mov dword [eax+0x4],0x0
0003183E  C7042400360300    mov dword [esp],0x33600
00031845  E8CE140000        call dword 0x32d18
0003184A  C745F401000000    mov dword [ebp-0xc],0x1
00031851  EB10              jmp short 0x31863
00031853  C704249E350300    mov dword [esp],0x3359e
0003185A  E8B9140000        call dword 0x32d18
0003185F  8345F401          add dword [ebp-0xc],byte +0x1
00031863  837DF418          cmp dword [ebp-0xc],byte +0x18
00031867  7EEA              jng 0x31853
00031869  E8A9ECFFFF        call dword 0x30517
0003186E  EB11              jmp short 0x31881
00031870  C7042448360300    mov dword [esp],0x33648
00031877  E8F8080000        call dword 0x32174
0003187C  E896ECFFFF        call dword 0x30517
00031881  C704246C360300    mov dword [esp],0x3366c
00031888  E8E7080000        call dword 0x32174
0003188D  B800000000        mov eax,0x0
00031892  81C484020000      add esp,0x284
00031898  5B                pop ebx
00031899  5D                pop ebp
0003189A  C3                ret
0003189B  90                nop
0003189C  55                push ebp
0003189D  89E5              mov ebp,esp
0003189F  56                push esi
000318A0  53                push ebx
000318A1  81EC20010000      sub esp,0x120
000318A7  89E0              mov eax,esp
000318A9  89C3              mov ebx,eax
000318AB  8B4510            mov eax,[ebp+0x10]
000318AE  890424            mov [esp],eax
000318B1  E8AA0A0000        call dword 0x32360
000318B6  89C2              mov edx,eax
000318B8  83EA01            sub edx,byte +0x1
000318BB  8955E4            mov [ebp-0x1c],edx
000318BE  BA10000000        mov edx,0x10
000318C3  83EA01            sub edx,byte +0x1
000318C6  01D0              add eax,edx
000318C8  C78504FFFFFF1000  mov dword [ebp-0xfc],0x10
         -0000
000318D2  BA00000000        mov edx,0x0
000318D7  F7B504FFFFFF      div dword [ebp-0xfc]
000318DD  6BC010            imul eax,eax,byte +0x10
000318E0  29C4              sub esp,eax
000318E2  8D442414          lea eax,[esp+0x14]
000318E6  83C000            add eax,byte +0x0
000318E9  8945E0            mov [ebp-0x20],eax
000318EC  8B4510            mov eax,[ebp+0x10]
000318EF  890424            mov [esp],eax
000318F2  E8690A0000        call dword 0x32360
000318F7  8945DC            mov [ebp-0x24],eax
000318FA  C745E800000000    mov dword [ebp-0x18],0x0
00031901  837D0C00          cmp dword [ebp+0xc],byte +0x0
00031905  750A              jnz 0x31911
00031907  B800000000        mov eax,0x0
0003190C  E908080000        jmp dword 0x32119
00031911  C745EC00000000    mov dword [ebp-0x14],0x0
00031918  C745F400000000    mov dword [ebp-0xc],0x0
0003191F  EB19              jmp short 0x3193a
00031921  8B55F4            mov edx,[ebp-0xc]
00031924  8B4510            mov eax,[ebp+0x10]
00031927  01D0              add eax,edx
00031929  0FB600            movzx eax,byte [eax]
0003192C  8B4DE0            mov ecx,[ebp-0x20]
0003192F  8B55F4            mov edx,[ebp-0xc]
00031932  01CA              add edx,ecx
00031934  8802              mov [edx],al
00031936  8345F401          add dword [ebp-0xc],byte +0x1
0003193A  8B75F4            mov esi,[ebp-0xc]
0003193D  8B4510            mov eax,[ebp+0x10]
00031940  890424            mov [esp],eax
00031943  E8180A0000        call dword 0x32360
00031948  39C6              cmp esi,eax
0003194A  72D5              jc 0x31921
0003194C  C744240440000000  mov dword [esp+0x4],0x40
00031954  8D8574FFFFFF      lea eax,[ebp-0x8c]
0003195A  890424            mov [esp],eax
0003195D  E892090000        call dword 0x322f4
00031962  C745F400000000    mov dword [ebp-0xc],0x0
00031969  EB12              jmp short 0x3197d
0003196B  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031971  8B45F4            mov eax,[ebp-0xc]
00031974  01D0              add eax,edx
00031976  C600FF            mov byte [eax],0xff
00031979  8345F401          add dword [ebp-0xc],byte +0x1
0003197D  837DF43F          cmp dword [ebp-0xc],byte +0x3f
00031981  7EE8              jng 0x3196b
00031983  C745F400000000    mov dword [ebp-0xc],0x0
0003198A  E945010000        jmp dword 0x31ad4
0003198F  8B55E0            mov edx,[ebp-0x20]
00031992  8B45F4            mov eax,[ebp-0xc]
00031995  01D0              add eax,edx
00031997  0FB600            movzx eax,byte [eax]
0003199A  3C25              cmp al,0x25
0003199C  0F852E010000      jnz dword 0x31ad0
000319A2  8B55E0            mov edx,[ebp-0x20]
000319A5  8B45F4            mov eax,[ebp-0xc]
000319A8  01D0              add eax,edx
000319AA  C60000            mov byte [eax],0x0
000319AD  8B45F4            mov eax,[ebp-0xc]
000319B0  8D5001            lea edx,[eax+0x1]
000319B3  8B45E0            mov eax,[ebp-0x20]
000319B6  0FB60410          movzx eax,byte [eax+edx]
000319BA  3C64              cmp al,0x64
000319BC  7513              jnz 0x319d1
000319BE  8D9574FFFFFF      lea edx,[ebp-0x8c]
000319C4  8B45E8            mov eax,[ebp-0x18]
000319C7  01D0              add eax,edx
000319C9  C60000            mov byte [eax],0x0
000319CC  E9EE000000        jmp dword 0x31abf
000319D1  8B45F4            mov eax,[ebp-0xc]
000319D4  8D5001            lea edx,[eax+0x1]
000319D7  8B45E0            mov eax,[ebp-0x20]
000319DA  0FB60410          movzx eax,byte [eax+edx]
000319DE  3C78              cmp al,0x78
000319E0  7513              jnz 0x319f5
000319E2  8D9574FFFFFF      lea edx,[ebp-0x8c]
000319E8  8B45E8            mov eax,[ebp-0x18]
000319EB  01D0              add eax,edx
000319ED  C60001            mov byte [eax],0x1
000319F0  E9CA000000        jmp dword 0x31abf
000319F5  8B45F4            mov eax,[ebp-0xc]
000319F8  8D5001            lea edx,[eax+0x1]
000319FB  8B45E0            mov eax,[ebp-0x20]
000319FE  0FB60410          movzx eax,byte [eax+edx]
00031A02  3C38              cmp al,0x38
00031A04  7513              jnz 0x31a19
00031A06  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031A0C  8B45E8            mov eax,[ebp-0x18]
00031A0F  01D0              add eax,edx
00031A11  C60003            mov byte [eax],0x3
00031A14  E9A6000000        jmp dword 0x31abf
00031A19  8B45F4            mov eax,[ebp-0xc]
00031A1C  8D5001            lea edx,[eax+0x1]
00031A1F  8B45E0            mov eax,[ebp-0x20]
00031A22  0FB60410          movzx eax,byte [eax+edx]
00031A26  3C70              cmp al,0x70
00031A28  7513              jnz 0x31a3d
00031A2A  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031A30  8B45E8            mov eax,[ebp-0x18]
00031A33  01D0              add eax,edx
00031A35  C60003            mov byte [eax],0x3
00031A38  E982000000        jmp dword 0x31abf
00031A3D  8B45F4            mov eax,[ebp-0xc]
00031A40  8D5001            lea edx,[eax+0x1]
00031A43  8B45E0            mov eax,[ebp-0x20]
00031A46  0FB60410          movzx eax,byte [eax+edx]
00031A4A  3C36              cmp al,0x36
00031A4C  7510              jnz 0x31a5e
00031A4E  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031A54  8B45E8            mov eax,[ebp-0x18]
00031A57  01D0              add eax,edx
00031A59  C60004            mov byte [eax],0x4
00031A5C  EB61              jmp short 0x31abf
00031A5E  8B45F4            mov eax,[ebp-0xc]
00031A61  8D5001            lea edx,[eax+0x1]
00031A64  8B45E0            mov eax,[ebp-0x20]
00031A67  0FB60410          movzx eax,byte [eax+edx]
00031A6B  3C32              cmp al,0x32
00031A6D  7510              jnz 0x31a7f
00031A6F  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031A75  8B45E8            mov eax,[ebp-0x18]
00031A78  01D0              add eax,edx
00031A7A  C60006            mov byte [eax],0x6
00031A7D  EB40              jmp short 0x31abf
00031A7F  8B45F4            mov eax,[ebp-0xc]
00031A82  8D5001            lea edx,[eax+0x1]
00031A85  8B45E0            mov eax,[ebp-0x20]
00031A88  0FB60410          movzx eax,byte [eax+edx]
00031A8C  3C73              cmp al,0x73
00031A8E  7510              jnz 0x31aa0
00031A90  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031A96  8B45E8            mov eax,[ebp-0x18]
00031A99  01D0              add eax,edx
00031A9B  C60002            mov byte [eax],0x2
00031A9E  EB1F              jmp short 0x31abf
00031AA0  8B45F4            mov eax,[ebp-0xc]
00031AA3  8D5001            lea edx,[eax+0x1]
00031AA6  8B45E0            mov eax,[ebp-0x20]
00031AA9  0FB60410          movzx eax,byte [eax+edx]
00031AAD  3C63              cmp al,0x63
00031AAF  750E              jnz 0x31abf
00031AB1  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031AB7  8B45E8            mov eax,[ebp-0x18]
00031ABA  01D0              add eax,edx
00031ABC  C60005            mov byte [eax],0x5
00031ABF  8B45F4            mov eax,[ebp-0xc]
00031AC2  8D5001            lea edx,[eax+0x1]
00031AC5  8B45E0            mov eax,[ebp-0x20]
00031AC8  C6041000          mov byte [eax+edx],0x0
00031ACC  8345E801          add dword [ebp-0x18],byte +0x1
00031AD0  8345F401          add dword [ebp-0xc],byte +0x1
00031AD4  8B45F4            mov eax,[ebp-0xc]
00031AD7  3B45DC            cmp eax,[ebp-0x24]
00031ADA  0F8CAFFEFFFF      jl dword 0x3198f
00031AE0  C745F400000000    mov dword [ebp-0xc],0x0
00031AE7  C745E800000000    mov dword [ebp-0x18],0x0
00031AEE  E9F3050000        jmp dword 0x320e6
00031AF3  8B55E0            mov edx,[ebp-0x20]
00031AF6  8B45F4            mov eax,[ebp-0xc]
00031AF9  01D0              add eax,edx
00031AFB  0FB600            movzx eax,byte [eax]
00031AFE  84C0              test al,al
00031B00  0F85A8050000      jnz dword 0x320ae
00031B06  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031B0C  8B45E8            mov eax,[ebp-0x18]
00031B0F  01D0              add eax,edx
00031B11  0FB600            movzx eax,byte [eax]
00031B14  3CFF              cmp al,0xff
00031B16  752E              jnz 0x31b46
00031B18  C744241004000000  mov dword [esp+0x10],0x4
00031B20  C744240C02000000  mov dword [esp+0xc],0x2
00031B28  C744240816000000  mov dword [esp+0x8],0x16
00031B30  C74424043C000000  mov dword [esp+0x4],0x3c
00031B38  C70424A4360300    mov dword [esp],0x336a4
00031B3F  E89E0F0000        call dword 0x32ae2
00031B44  EBFE              jmp short 0x31b44
00031B46  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031B4C  8B45E8            mov eax,[ebp-0x18]
00031B4F  01D0              add eax,edx
00031B51  0FB600            movzx eax,byte [eax]
00031B54  0FB6C0            movzx eax,al
00031B57  83F803            cmp eax,byte +0x3
00031B5A  0F845C010000      jz dword 0x31cbc
00031B60  83F803            cmp eax,byte +0x3
00031B63  7F12              jg 0x31b77
00031B65  85C0              test eax,eax
00031B67  7427              jz 0x31b90
00031B69  83F801            cmp eax,byte +0x1
00031B6C  0F84B4000000      jz dword 0x31c26
00031B72  E9FB020000        jmp dword 0x31e72
00031B77  83F806            cmp eax,byte +0x6
00031B7A  0F84F0010000      jz dword 0x31d70
00031B80  3DFF000000        cmp eax,0xff
00031B85  0F8497020000      jz dword 0x31e22
00031B8B  E9E2020000        jmp dword 0x31e72
00031B90  8B4514            mov eax,[ebp+0x14]
00031B93  8D5004            lea edx,[eax+0x4]
00031B96  895514            mov [ebp+0x14],edx
00031B99  8B00              mov eax,[eax]
00031B9B  8945D8            mov [ebp-0x28],eax
00031B9E  C744240C64000000  mov dword [esp+0xc],0x64
00031BA6  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031BAC  89442408          mov [esp+0x8],eax
00031BB0  C74424040A000000  mov dword [esp+0x4],0xa
00031BB8  8B45D8            mov eax,[ebp-0x28]
00031BBB  890424            mov [esp],eax
00031BBE  E8A00B0000        call dword 0x32763
00031BC3  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031BC9  890424            mov [esp],eax
00031BCC  E88F070000        call dword 0x32360
00031BD1  8945F0            mov [ebp-0x10],eax
00031BD4  8B45F0            mov eax,[ebp-0x10]
00031BD7  8B55EC            mov edx,[ebp-0x14]
00031BDA  01D0              add eax,edx
00031BDC  3B450C            cmp eax,[ebp+0xc]
00031BDF  7C19              jl 0x31bfa
00031BE1  8B45F0            mov eax,[ebp-0x10]
00031BE4  8B55EC            mov edx,[ebp-0x14]
00031BE7  01C2              add edx,eax
00031BE9  8B450C            mov eax,[ebp+0xc]
00031BEC  89C1              mov ecx,eax
00031BEE  29D1              sub ecx,edx
00031BF0  89CA              mov edx,ecx
00031BF2  8B450C            mov eax,[ebp+0xc]
00031BF5  01D0              add eax,edx
00031BF7  8945F0            mov [ebp-0x10],eax
00031BFA  8B55EC            mov edx,[ebp-0x14]
00031BFD  8B4508            mov eax,[ebp+0x8]
00031C00  01C2              add edx,eax
00031C02  8B45F0            mov eax,[ebp-0x10]
00031C05  89442408          mov [esp+0x8],eax
00031C09  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031C0F  89442404          mov [esp+0x4],eax
00031C13  891424            mov [esp],edx
00031C16  E807070000        call dword 0x32322
00031C1B  8B45F0            mov eax,[ebp-0x10]
00031C1E  0145EC            add [ebp-0x14],eax
00031C21  E94C020000        jmp dword 0x31e72
00031C26  8B4514            mov eax,[ebp+0x14]
00031C29  8D5004            lea edx,[eax+0x4]
00031C2C  895514            mov [ebp+0x14],edx
00031C2F  8B00              mov eax,[eax]
00031C31  8945D4            mov [ebp-0x2c],eax
00031C34  8B45F0            mov eax,[ebp-0x10]
00031C37  C744240C64000000  mov dword [esp+0xc],0x64
00031C3F  8D9510FFFFFF      lea edx,[ebp-0xf0]
00031C45  89542408          mov [esp+0x8],edx
00031C49  C744240410000000  mov dword [esp+0x4],0x10
00031C51  890424            mov [esp],eax
00031C54  E80A0B0000        call dword 0x32763
00031C59  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031C5F  890424            mov [esp],eax
00031C62  E8F9060000        call dword 0x32360
00031C67  8945F0            mov [ebp-0x10],eax
00031C6A  8B45F0            mov eax,[ebp-0x10]
00031C6D  8B55EC            mov edx,[ebp-0x14]
00031C70  01D0              add eax,edx
00031C72  3B450C            cmp eax,[ebp+0xc]
00031C75  7C19              jl 0x31c90
00031C77  8B45F0            mov eax,[ebp-0x10]
00031C7A  8B55EC            mov edx,[ebp-0x14]
00031C7D  01C2              add edx,eax
00031C7F  8B450C            mov eax,[ebp+0xc]
00031C82  89C1              mov ecx,eax
00031C84  29D1              sub ecx,edx
00031C86  89CA              mov edx,ecx
00031C88  8B450C            mov eax,[ebp+0xc]
00031C8B  01D0              add eax,edx
00031C8D  8945F0            mov [ebp-0x10],eax
00031C90  8B55EC            mov edx,[ebp-0x14]
00031C93  8B4508            mov eax,[ebp+0x8]
00031C96  01C2              add edx,eax
00031C98  8B45F0            mov eax,[ebp-0x10]
00031C9B  89442408          mov [esp+0x8],eax
00031C9F  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031CA5  89442404          mov [esp+0x4],eax
00031CA9  891424            mov [esp],edx
00031CAC  E871060000        call dword 0x32322
00031CB1  8B45F0            mov eax,[ebp-0x10]
00031CB4  0145EC            add [ebp-0x14],eax
00031CB7  E9B6010000        jmp dword 0x31e72
00031CBC  8B4514            mov eax,[ebp+0x14]
00031CBF  8D5004            lea edx,[eax+0x4]
00031CC2  895514            mov [ebp+0x14],edx
00031CC5  8B00              mov eax,[eax]
00031CC7  8945D0            mov [ebp-0x30],eax
00031CCA  C744240C64000000  mov dword [esp+0xc],0x64
00031CD2  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031CD8  89442408          mov [esp+0x8],eax
00031CDC  C744240410000000  mov dword [esp+0x4],0x10
00031CE4  8B45D0            mov eax,[ebp-0x30]
00031CE7  890424            mov [esp],eax
00031CEA  E8740A0000        call dword 0x32763
00031CEF  C744240864000000  mov dword [esp+0x8],0x64
00031CF7  C744240408000000  mov dword [esp+0x4],0x8
00031CFF  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031D05  890424            mov [esp],eax
00031D08  E828070000        call dword 0x32435
00031D0D  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031D13  890424            mov [esp],eax
00031D16  E845060000        call dword 0x32360
00031D1B  8945F0            mov [ebp-0x10],eax
00031D1E  8B45F0            mov eax,[ebp-0x10]
00031D21  8B55EC            mov edx,[ebp-0x14]
00031D24  01D0              add eax,edx
00031D26  3B450C            cmp eax,[ebp+0xc]
00031D29  7C19              jl 0x31d44
00031D2B  8B45F0            mov eax,[ebp-0x10]
00031D2E  8B55EC            mov edx,[ebp-0x14]
00031D31  01C2              add edx,eax
00031D33  8B450C            mov eax,[ebp+0xc]
00031D36  89C1              mov ecx,eax
00031D38  29D1              sub ecx,edx
00031D3A  89CA              mov edx,ecx
00031D3C  8B450C            mov eax,[ebp+0xc]
00031D3F  01D0              add eax,edx
00031D41  8945F0            mov [ebp-0x10],eax
00031D44  8B55EC            mov edx,[ebp-0x14]
00031D47  8B4508            mov eax,[ebp+0x8]
00031D4A  01C2              add edx,eax
00031D4C  8B45F0            mov eax,[ebp-0x10]
00031D4F  89442408          mov [esp+0x8],eax
00031D53  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031D59  89442404          mov [esp+0x4],eax
00031D5D  891424            mov [esp],edx
00031D60  E8BD050000        call dword 0x32322
00031D65  8B45F0            mov eax,[ebp-0x10]
00031D68  0145EC            add [ebp-0x14],eax
00031D6B  E902010000        jmp dword 0x31e72
00031D70  8B4514            mov eax,[ebp+0x14]
00031D73  8D5004            lea edx,[eax+0x4]
00031D76  895514            mov [ebp+0x14],edx
00031D79  8B00              mov eax,[eax]
00031D7B  8845CF            mov [ebp-0x31],al
00031D7E  0FB645CF          movzx eax,byte [ebp-0x31]
00031D82  C744240C64000000  mov dword [esp+0xc],0x64
00031D8A  8D9510FFFFFF      lea edx,[ebp-0xf0]
00031D90  89542408          mov [esp+0x8],edx
00031D94  C744240410000000  mov dword [esp+0x4],0x10
00031D9C  890424            mov [esp],eax
00031D9F  E8BF090000        call dword 0x32763
00031DA4  C744240864000000  mov dword [esp+0x8],0x64
00031DAC  C744240402000000  mov dword [esp+0x4],0x2
00031DB4  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031DBA  890424            mov [esp],eax
00031DBD  E873060000        call dword 0x32435
00031DC2  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031DC8  890424            mov [esp],eax
00031DCB  E890050000        call dword 0x32360
00031DD0  8945F0            mov [ebp-0x10],eax
00031DD3  8B45F0            mov eax,[ebp-0x10]
00031DD6  8B55EC            mov edx,[ebp-0x14]
00031DD9  01D0              add eax,edx
00031DDB  3B450C            cmp eax,[ebp+0xc]
00031DDE  7C19              jl 0x31df9
00031DE0  8B45F0            mov eax,[ebp-0x10]
00031DE3  8B55EC            mov edx,[ebp-0x14]
00031DE6  01C2              add edx,eax
00031DE8  8B450C            mov eax,[ebp+0xc]
00031DEB  89C1              mov ecx,eax
00031DED  29D1              sub ecx,edx
00031DEF  89CA              mov edx,ecx
00031DF1  8B450C            mov eax,[ebp+0xc]
00031DF4  01D0              add eax,edx
00031DF6  8945F0            mov [ebp-0x10],eax
00031DF9  8B55EC            mov edx,[ebp-0x14]
00031DFC  8B4508            mov eax,[ebp+0x8]
00031DFF  01C2              add edx,eax
00031E01  8B45F0            mov eax,[ebp-0x10]
00031E04  89442408          mov [esp+0x8],eax
00031E08  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031E0E  89442404          mov [esp+0x4],eax
00031E12  891424            mov [esp],edx
00031E15  E808050000        call dword 0x32322
00031E1A  8B45F0            mov eax,[ebp-0x10]
00031E1D  0145EC            add [ebp-0x14],eax
00031E20  EB50              jmp short 0x31e72
00031E22  C744241004000000  mov dword [esp+0x10],0x4
00031E2A  C744240C02000000  mov dword [esp+0xc],0x2
00031E32  C744240815000000  mov dword [esp+0x8],0x15
00031E3A  C744240428000000  mov dword [esp+0x4],0x28
00031E42  C70424A4360300    mov dword [esp],0x336a4
00031E49  E8940C0000        call dword 0x32ae2
00031E4E  8B45EC            mov eax,[ebp-0x14]
00031E51  3B450C            cmp eax,[ebp+0xc]
00031E54  7C09              jl 0x31e5f
00031E56  8B450C            mov eax,[ebp+0xc]
00031E59  83E801            sub eax,byte +0x1
00031E5C  8945EC            mov [ebp-0x14],eax
00031E5F  8B55EC            mov edx,[ebp-0x14]
00031E62  8B4508            mov eax,[ebp+0x8]
00031E65  01D0              add eax,edx
00031E67  C60000            mov byte [eax],0x0
00031E6A  8B45EC            mov eax,[ebp-0x14]
00031E6D  E9A7020000        jmp dword 0x32119
00031E72  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031E78  8B45E8            mov eax,[ebp-0x18]
00031E7B  01D0              add eax,edx
00031E7D  0FB600            movzx eax,byte [eax]
00031E80  3C02              cmp al,0x2
00031E82  7566              jnz 0x31eea
00031E84  8B4514            mov eax,[ebp+0x14]
00031E87  8D5004            lea edx,[eax+0x4]
00031E8A  895514            mov [ebp+0x14],edx
00031E8D  8B00              mov eax,[eax]
00031E8F  8945C8            mov [ebp-0x38],eax
00031E92  8B45C8            mov eax,[ebp-0x38]
00031E95  890424            mov [esp],eax
00031E98  E8C3040000        call dword 0x32360
00031E9D  8945F0            mov [ebp-0x10],eax
00031EA0  8B45F0            mov eax,[ebp-0x10]
00031EA3  8B55EC            mov edx,[ebp-0x14]
00031EA6  01D0              add eax,edx
00031EA8  3B450C            cmp eax,[ebp+0xc]
00031EAB  7E19              jng 0x31ec6
00031EAD  8B45F0            mov eax,[ebp-0x10]
00031EB0  8B55EC            mov edx,[ebp-0x14]
00031EB3  01C2              add edx,eax
00031EB5  8B450C            mov eax,[ebp+0xc]
00031EB8  89C1              mov ecx,eax
00031EBA  29D1              sub ecx,edx
00031EBC  89CA              mov edx,ecx
00031EBE  8B450C            mov eax,[ebp+0xc]
00031EC1  01D0              add eax,edx
00031EC3  8945F0            mov [ebp-0x10],eax
00031EC6  8B55EC            mov edx,[ebp-0x14]
00031EC9  8B4508            mov eax,[ebp+0x8]
00031ECC  01C2              add edx,eax
00031ECE  8B45F0            mov eax,[ebp-0x10]
00031ED1  89442408          mov [esp+0x8],eax
00031ED5  8B45C8            mov eax,[ebp-0x38]
00031ED8  89442404          mov [esp+0x4],eax
00031EDC  891424            mov [esp],edx
00031EDF  E83E040000        call dword 0x32322
00031EE4  8B45F0            mov eax,[ebp-0x10]
00031EE7  0145EC            add [ebp-0x14],eax
00031EEA  8D9574FFFFFF      lea edx,[ebp-0x8c]
00031EF0  8B45E8            mov eax,[ebp-0x18]
00031EF3  01D0              add eax,edx
00031EF5  0FB600            movzx eax,byte [eax]
00031EF8  3C04              cmp al,0x4
00031EFA  0F8573010000      jnz dword 0x32073
00031F00  8B4514            mov eax,[ebp+0x14]
00031F03  8D5008            lea edx,[eax+0x8]
00031F06  895514            mov [ebp+0x14],edx
00031F09  8B5004            mov edx,[eax+0x4]
00031F0C  8B00              mov eax,[eax]
00031F0E  8945C0            mov [ebp-0x40],eax
00031F11  8955C4            mov [ebp-0x3c],edx
00031F14  8B45C0            mov eax,[ebp-0x40]
00031F17  8945BC            mov [ebp-0x44],eax
00031F1A  8B45C0            mov eax,[ebp-0x40]
00031F1D  8B55C4            mov edx,[ebp-0x3c]
00031F20  89D0              mov eax,edx
00031F22  31D2              xor edx,edx
00031F24  8945B8            mov [ebp-0x48],eax
00031F27  C744240C64000000  mov dword [esp+0xc],0x64
00031F2F  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031F35  89442408          mov [esp+0x8],eax
00031F39  C744240410000000  mov dword [esp+0x4],0x10
00031F41  8B45B8            mov eax,[ebp-0x48]
00031F44  890424            mov [esp],eax
00031F47  E817080000        call dword 0x32763
00031F4C  C744240864000000  mov dword [esp+0x8],0x64
00031F54  C744240408000000  mov dword [esp+0x4],0x8
00031F5C  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031F62  890424            mov [esp],eax
00031F65  E8CB040000        call dword 0x32435
00031F6A  C745F008000000    mov dword [ebp-0x10],0x8
00031F71  8B45F0            mov eax,[ebp-0x10]
00031F74  8B55EC            mov edx,[ebp-0x14]
00031F77  01D0              add eax,edx
00031F79  3B450C            cmp eax,[ebp+0xc]
00031F7C  7E19              jng 0x31f97
00031F7E  8B45F0            mov eax,[ebp-0x10]
00031F81  8B55EC            mov edx,[ebp-0x14]
00031F84  01C2              add edx,eax
00031F86  8B450C            mov eax,[ebp+0xc]
00031F89  89C1              mov ecx,eax
00031F8B  29D1              sub ecx,edx
00031F8D  89CA              mov edx,ecx
00031F8F  8B450C            mov eax,[ebp+0xc]
00031F92  01D0              add eax,edx
00031F94  8945F0            mov [ebp-0x10],eax
00031F97  8B55EC            mov edx,[ebp-0x14]
00031F9A  8B4508            mov eax,[ebp+0x8]
00031F9D  01C2              add edx,eax
00031F9F  8B45F0            mov eax,[ebp-0x10]
00031FA2  89442408          mov [esp+0x8],eax
00031FA6  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031FAC  89442404          mov [esp+0x4],eax
00031FB0  891424            mov [esp],edx
00031FB3  E86A030000        call dword 0x32322
00031FB8  8B45F0            mov eax,[ebp-0x10]
00031FBB  0145EC            add [ebp-0x14],eax
00031FBE  8B45EC            mov eax,[ebp-0x14]
00031FC1  3B450C            cmp eax,[ebp+0xc]
00031FC4  7C16              jl 0x31fdc
00031FC6  8B450C            mov eax,[ebp+0xc]
00031FC9  8D50FF            lea edx,[eax-0x1]
00031FCC  8B4508            mov eax,[ebp+0x8]
00031FCF  01D0              add eax,edx
00031FD1  C60000            mov byte [eax],0x0
00031FD4  8B450C            mov eax,[ebp+0xc]
00031FD7  E93D010000        jmp dword 0x32119
00031FDC  C744240C64000000  mov dword [esp+0xc],0x64
00031FE4  8D8510FFFFFF      lea eax,[ebp-0xf0]
00031FEA  89442408          mov [esp+0x8],eax
00031FEE  C744240410000000  mov dword [esp+0x4],0x10
00031FF6  8B45BC            mov eax,[ebp-0x44]
00031FF9  890424            mov [esp],eax
00031FFC  E862070000        call dword 0x32763
00032001  C744240864000000  mov dword [esp+0x8],0x64
00032009  C744240408000000  mov dword [esp+0x4],0x8
00032011  8D8510FFFFFF      lea eax,[ebp-0xf0]
00032017  890424            mov [esp],eax
0003201A  E816040000        call dword 0x32435
0003201F  C745F008000000    mov dword [ebp-0x10],0x8
00032026  8B45F0            mov eax,[ebp-0x10]
00032029  8B55EC            mov edx,[ebp-0x14]
0003202C  01D0              add eax,edx
0003202E  3B450C            cmp eax,[ebp+0xc]
00032031  7E19              jng 0x3204c
00032033  8B45F0            mov eax,[ebp-0x10]
00032036  8B55EC            mov edx,[ebp-0x14]
00032039  01C2              add edx,eax
0003203B  8B450C            mov eax,[ebp+0xc]
0003203E  89C1              mov ecx,eax
00032040  29D1              sub ecx,edx
00032042  89CA              mov edx,ecx
00032044  8B450C            mov eax,[ebp+0xc]
00032047  01D0              add eax,edx
00032049  8945F0            mov [ebp-0x10],eax
0003204C  8B55EC            mov edx,[ebp-0x14]
0003204F  8B4508            mov eax,[ebp+0x8]
00032052  01C2              add edx,eax
00032054  8B45F0            mov eax,[ebp-0x10]
00032057  89442408          mov [esp+0x8],eax
0003205B  8D8510FFFFFF      lea eax,[ebp-0xf0]
00032061  89442404          mov [esp+0x4],eax
00032065  891424            mov [esp],edx
00032068  E8B5020000        call dword 0x32322
0003206D  8B45F0            mov eax,[ebp-0x10]
00032070  0145EC            add [ebp-0x14],eax
00032073  8D9574FFFFFF      lea edx,[ebp-0x8c]
00032079  8B45E8            mov eax,[ebp-0x18]
0003207C  01D0              add eax,edx
0003207E  0FB600            movzx eax,byte [eax]
00032081  3C05              cmp al,0x5
00032083  751F              jnz 0x320a4
00032085  8B4514            mov eax,[ebp+0x14]
00032088  8D5004            lea edx,[eax+0x4]
0003208B  895514            mov [ebp+0x14],edx
0003208E  8B00              mov eax,[eax]
00032090  8945B4            mov [ebp-0x4c],eax
00032093  8B55EC            mov edx,[ebp-0x14]
00032096  8B4508            mov eax,[ebp+0x8]
00032099  01C2              add edx,eax
0003209B  8B45B4            mov eax,[ebp-0x4c]
0003209E  8802              mov [edx],al
000320A0  8345EC01          add dword [ebp-0x14],byte +0x1
000320A4  8345F402          add dword [ebp-0xc],byte +0x2
000320A8  8345E801          add dword [ebp-0x18],byte +0x1
000320AC  EB38              jmp short 0x320e6
000320AE  8B55EC            mov edx,[ebp-0x14]
000320B1  8B4508            mov eax,[ebp+0x8]
000320B4  01C2              add edx,eax
000320B6  8B4DE0            mov ecx,[ebp-0x20]
000320B9  8B45F4            mov eax,[ebp-0xc]
000320BC  01C8              add eax,ecx
000320BE  0FB600            movzx eax,byte [eax]
000320C1  8802              mov [edx],al
000320C3  8345EC01          add dword [ebp-0x14],byte +0x1
000320C7  8B45EC            mov eax,[ebp-0x14]
000320CA  3B450C            cmp eax,[ebp+0xc]
000320CD  7C13              jl 0x320e2
000320CF  8B450C            mov eax,[ebp+0xc]
000320D2  8D50FF            lea edx,[eax-0x1]
000320D5  8B4508            mov eax,[ebp+0x8]
000320D8  01D0              add eax,edx
000320DA  C60000            mov byte [eax],0x0
000320DD  8B450C            mov eax,[ebp+0xc]
000320E0  EB37              jmp short 0x32119
000320E2  8345F401          add dword [ebp-0xc],byte +0x1
000320E6  8B45F4            mov eax,[ebp-0xc]
000320E9  3B45DC            cmp eax,[ebp-0x24]
000320EC  7D0C              jnl 0x320fa
000320EE  8B45EC            mov eax,[ebp-0x14]
000320F1  3B450C            cmp eax,[ebp+0xc]
000320F4  0F8CF9F9FFFF      jl dword 0x31af3
000320FA  8B45EC            mov eax,[ebp-0x14]
000320FD  3B450C            cmp eax,[ebp+0xc]
00032100  7C09              jl 0x3210b
00032102  8B450C            mov eax,[ebp+0xc]
00032105  83E801            sub eax,byte +0x1
00032108  8945EC            mov [ebp-0x14],eax
0003210B  8B55EC            mov edx,[ebp-0x14]
0003210E  8B4508            mov eax,[ebp+0x8]
00032111  01D0              add eax,edx
00032113  C60000            mov byte [eax],0x0
00032116  8B45EC            mov eax,[ebp-0x14]
00032119  89DC              mov esp,ebx
0003211B  8D65F8            lea esp,[ebp-0x8]
0003211E  5B                pop ebx
0003211F  5E                pop esi
00032120  5D                pop ebp
00032121  C3                ret
00032122  55                push ebp
00032123  89E5              mov ebp,esp
00032125  83EC28            sub esp,byte +0x28
00032128  C70424903C0300    mov dword [esp],0x33c90
0003212F  E817010000        call dword 0x3224b
00032134  C745F400000000    mov dword [ebp-0xc],0x0
0003213B  EB1A              jmp short 0x32157
0003213D  8B55F4            mov edx,[ebp-0xc]
00032140  8B4508            mov eax,[ebp+0x8]
00032143  01D0              add eax,edx
00032145  0FB600            movzx eax,byte [eax]
00032148  0FBEC0            movsx eax,al
0003214B  890424            mov [esp],eax
0003214E  E85A070000        call dword 0x328ad
00032153  8345F401          add dword [ebp-0xc],byte +0x1
00032157  8B55F4            mov edx,[ebp-0xc]
0003215A  8B4508            mov eax,[ebp+0x8]
0003215D  01D0              add eax,edx
0003215F  0FB600            movzx eax,byte [eax]
00032162  84C0              test al,al
00032164  75D7              jnz 0x3213d
00032166  C70424903C0300    mov dword [esp],0x33c90
0003216D  E82F010000        call dword 0x322a1
00032172  C9                leave
00032173  C3                ret
00032174  55                push ebp
00032175  89E5              mov ebp,esp
00032177  81ECF8000000      sub esp,0xf8
0003217D  8D450C            lea eax,[ebp+0xc]
00032180  8945EC            mov [ebp-0x14],eax
00032183  8B45EC            mov eax,[ebp-0x14]
00032186  8944240C          mov [esp+0xc],eax
0003218A  8B4508            mov eax,[ebp+0x8]
0003218D  89442408          mov [esp+0x8],eax
00032191  C7442404C8000000  mov dword [esp+0x4],0xc8
00032199  8D8524FFFFFF      lea eax,[ebp-0xdc]
0003219F  890424            mov [esp],eax
000321A2  E8F5F6FFFF        call dword 0x3189c
000321A7  8945F0            mov [ebp-0x10],eax
000321AA  C704249C3C0300    mov dword [esp],0x33c9c
000321B1  E895000000        call dword 0x3224b
000321B6  C70424903C0300    mov dword [esp],0x33c90
000321BD  E889000000        call dword 0x3224b
000321C2  837DF000          cmp dword [ebp-0x10],byte +0x0
000321C6  7E2E              jng 0x321f6
000321C8  C745F400000000    mov dword [ebp-0xc],0x0
000321CF  EB1D              jmp short 0x321ee
000321D1  8D9524FFFFFF      lea edx,[ebp-0xdc]
000321D7  8B45F4            mov eax,[ebp-0xc]
000321DA  01D0              add eax,edx
000321DC  0FB600            movzx eax,byte [eax]
000321DF  0FBEC0            movsx eax,al
000321E2  890424            mov [esp],eax
000321E5  E8C3060000        call dword 0x328ad
000321EA  8345F401          add dword [ebp-0xc],byte +0x1
000321EE  8B45F4            mov eax,[ebp-0xc]
000321F1  3B45F0            cmp eax,[ebp-0x10]
000321F4  7CDB              jl 0x321d1
000321F6  C70424903C0300    mov dword [esp],0x33c90
000321FD  E89F000000        call dword 0x322a1
00032202  C704249C3C0300    mov dword [esp],0x33c9c
00032209  E893000000        call dword 0x322a1
0003220E  C9                leave
0003220F  C3                ret
00032210  55                push ebp
00032211  89E5              mov ebp,esp
00032213  83EC28            sub esp,byte +0x28
00032216  C745F401000000    mov dword [ebp-0xc],0x1
0003221D  8D45E8            lea eax,[ebp-0x18]
00032220  8944240C          mov [esp+0xc],eax
00032224  8D45EC            lea eax,[ebp-0x14]
00032227  89442408          mov [esp+0x8],eax
0003222B  8D45F0            lea eax,[ebp-0x10]
0003222E  89442404          mov [esp+0x4],eax
00032232  8D45F4            lea eax,[ebp-0xc]
00032235  890424            mov [esp],eax
00032238  E8040D0000        call dword 0x32f41
0003223D  83EC10            sub esp,byte +0x10
00032240  8B45F0            mov eax,[ebp-0x10]
00032243  C1E818            shr eax,0x18
00032246  83C001            add eax,byte +0x1
00032249  C9                leave
0003224A  C3                ret
0003224B  55                push ebp
0003224C  89E5              mov ebp,esp
0003224E  83EC28            sub esp,byte +0x28
00032251  E8BAFFFFFF        call dword 0x32210
00032256  83C001            add eax,byte +0x1
00032259  8945F4            mov [ebp-0xc],eax
0003225C  8B4508            mov eax,[ebp+0x8]
0003225F  8B00              mov eax,[eax]
00032261  85C0              test eax,eax
00032263  741C              jz 0x32281
00032265  8B4508            mov eax,[ebp+0x8]
00032268  8B4004            mov eax,[eax+0x4]
0003226B  3B45F4            cmp eax,[ebp-0xc]
0003226E  7511              jnz 0x32281
00032270  8B4508            mov eax,[ebp+0x8]
00032273  8B4008            mov eax,[eax+0x8]
00032276  8D5001            lea edx,[eax+0x1]
00032279  8B4508            mov eax,[ebp+0x8]
0003227C  895008            mov [eax+0x8],edx
0003227F  EB1E              jmp short 0x3229f
00032281  8B4508            mov eax,[ebp+0x8]
00032284  890424            mov [esp],eax
00032287  E8F40C0000        call dword 0x32f80
0003228C  8B4508            mov eax,[ebp+0x8]
0003228F  C7400801000000    mov dword [eax+0x8],0x1
00032296  8B4508            mov eax,[ebp+0x8]
00032299  8B55F4            mov edx,[ebp-0xc]
0003229C  895004            mov [eax+0x4],edx
0003229F  C9                leave
000322A0  C3                ret
000322A1  55                push ebp
000322A2  89E5              mov ebp,esp
000322A4  83EC18            sub esp,byte +0x18
000322A7  E864FFFFFF        call dword 0x32210
000322AC  83C001            add eax,byte +0x1
000322AF  8945F4            mov [ebp-0xc],eax
000322B2  8B4508            mov eax,[ebp+0x8]
000322B5  8B00              mov eax,[eax]
000322B7  85C0              test eax,eax
000322B9  7437              jz 0x322f2
000322BB  8B4508            mov eax,[ebp+0x8]
000322BE  8B4004            mov eax,[eax+0x4]
000322C1  3B45F4            cmp eax,[ebp-0xc]
000322C4  752C              jnz 0x322f2
000322C6  8B4508            mov eax,[ebp+0x8]
000322C9  8B4008            mov eax,[eax+0x8]
000322CC  8D50FF            lea edx,[eax-0x1]
000322CF  8B4508            mov eax,[ebp+0x8]
000322D2  895008            mov [eax+0x8],edx
000322D5  8B4508            mov eax,[ebp+0x8]
000322D8  8B4008            mov eax,[eax+0x8]
000322DB  85C0              test eax,eax
000322DD  7513              jnz 0x322f2
000322DF  8B4508            mov eax,[ebp+0x8]
000322E2  C74004FFFFFFFF    mov dword [eax+0x4],0xffffffff
000322E9  8B4508            mov eax,[ebp+0x8]
000322EC  C70000000000      mov dword [eax],0x0
000322F2  C9                leave
000322F3  C3                ret
000322F4  55                push ebp
000322F5  89E5              mov ebp,esp
000322F7  83EC10            sub esp,byte +0x10
000322FA  8B4508            mov eax,[ebp+0x8]
000322FD  8945F8            mov [ebp-0x8],eax
00032300  C745FC00000000    mov dword [ebp-0x4],0x0
00032307  EB0F              jmp short 0x32318
00032309  8B45FC            mov eax,[ebp-0x4]
0003230C  8B55F8            mov edx,[ebp-0x8]
0003230F  01D0              add eax,edx
00032311  C60000            mov byte [eax],0x0
00032314  8345FC01          add dword [ebp-0x4],byte +0x1
00032318  8B45FC            mov eax,[ebp-0x4]
0003231B  3B450C            cmp eax,[ebp+0xc]
0003231E  72E9              jc 0x32309
00032320  C9                leave
00032321  C3                ret
00032322  55                push ebp
00032323  89E5              mov ebp,esp
00032325  83EC10            sub esp,byte +0x10
00032328  8B4508            mov eax,[ebp+0x8]
0003232B  8945F8            mov [ebp-0x8],eax
0003232E  8B450C            mov eax,[ebp+0xc]
00032331  8945F4            mov [ebp-0xc],eax
00032334  C745FC00000000    mov dword [ebp-0x4],0x0
0003233B  EB19              jmp short 0x32356
0003233D  8B55FC            mov edx,[ebp-0x4]
00032340  8B45F8            mov eax,[ebp-0x8]
00032343  01C2              add edx,eax
00032345  8B4DFC            mov ecx,[ebp-0x4]
00032348  8B45F4            mov eax,[ebp-0xc]
0003234B  01C8              add eax,ecx
0003234D  0FB600            movzx eax,byte [eax]
00032350  8802              mov [edx],al
00032352  8345FC01          add dword [ebp-0x4],byte +0x1
00032356  8B45FC            mov eax,[ebp-0x4]
00032359  3B4510            cmp eax,[ebp+0x10]
0003235C  7CDF              jl 0x3233d
0003235E  C9                leave
0003235F  C3                ret
00032360  55                push ebp
00032361  89E5              mov ebp,esp
00032363  83EC10            sub esp,byte +0x10
00032366  C745FC00000000    mov dword [ebp-0x4],0x0
0003236D  C745FC00000000    mov dword [ebp-0x4],0x0
00032374  EB04              jmp short 0x3237a
00032376  8345FC01          add dword [ebp-0x4],byte +0x1
0003237A  8B55FC            mov edx,[ebp-0x4]
0003237D  8B4508            mov eax,[ebp+0x8]
00032380  01D0              add eax,edx
00032382  0FB600            movzx eax,byte [eax]
00032385  84C0              test al,al
00032387  75ED              jnz 0x32376
00032389  8B45FC            mov eax,[ebp-0x4]
0003238C  C9                leave
0003238D  C3                ret
0003238E  55                push ebp
0003238F  89E5              mov ebp,esp
00032391  83EC14            sub esp,byte +0x14
00032394  8B4508            mov eax,[ebp+0x8]
00032397  890424            mov [esp],eax
0003239A  E8C1FFFFFF        call dword 0x32360
0003239F  8945F8            mov [ebp-0x8],eax
000323A2  C745FC00000000    mov dword [ebp-0x4],0x0
000323A9  EB1D              jmp short 0x323c8
000323AB  8B55F8            mov edx,[ebp-0x8]
000323AE  8B4508            mov eax,[ebp+0x8]
000323B1  01C2              add edx,eax
000323B3  8B4DFC            mov ecx,[ebp-0x4]
000323B6  8B450C            mov eax,[ebp+0xc]
000323B9  01C8              add eax,ecx
000323BB  0FB600            movzx eax,byte [eax]
000323BE  8802              mov [edx],al
000323C0  8345FC01          add dword [ebp-0x4],byte +0x1
000323C4  8345F801          add dword [ebp-0x8],byte +0x1
000323C8  8B55FC            mov edx,[ebp-0x4]
000323CB  8B450C            mov eax,[ebp+0xc]
000323CE  01D0              add eax,edx
000323D0  0FB600            movzx eax,byte [eax]
000323D3  84C0              test al,al
000323D5  75D4              jnz 0x323ab
000323D7  8B55F8            mov edx,[ebp-0x8]
000323DA  8B4508            mov eax,[ebp+0x8]
000323DD  01D0              add eax,edx
000323DF  C60000            mov byte [eax],0x0
000323E2  8B4508            mov eax,[ebp+0x8]
000323E5  C9                leave
000323E6  C3                ret
000323E7  55                push ebp
000323E8  89E5              mov ebp,esp
000323EA  83EC14            sub esp,byte +0x14
000323ED  8B450C            mov eax,[ebp+0xc]
000323F0  890424            mov [esp],eax
000323F3  E868FFFFFF        call dword 0x32360
000323F8  8945F8            mov [ebp-0x8],eax
000323FB  C745FC00000000    mov dword [ebp-0x4],0x0
00032402  EB19              jmp short 0x3241d
00032404  8B55FC            mov edx,[ebp-0x4]
00032407  8B4508            mov eax,[ebp+0x8]
0003240A  01C2              add edx,eax
0003240C  8B4DFC            mov ecx,[ebp-0x4]
0003240F  8B450C            mov eax,[ebp+0xc]
00032412  01C8              add eax,ecx
00032414  0FB600            movzx eax,byte [eax]
00032417  8802              mov [edx],al
00032419  8345FC01          add dword [ebp-0x4],byte +0x1
0003241D  8B45FC            mov eax,[ebp-0x4]
00032420  3B45F8            cmp eax,[ebp-0x8]
00032423  7CDF              jl 0x32404
00032425  8B55F8            mov edx,[ebp-0x8]
00032428  8B4508            mov eax,[ebp+0x8]
0003242B  01D0              add eax,edx
0003242D  C60000            mov byte [eax],0x0
00032430  8B4508            mov eax,[ebp+0x8]
00032433  C9                leave
00032434  C3                ret
00032435  55                push ebp
00032436  89E5              mov ebp,esp
00032438  56                push esi
00032439  53                push ebx
0003243A  83EC30            sub esp,byte +0x30
0003243D  89E0              mov eax,esp
0003243F  89C3              mov ebx,eax
00032441  C745F400000000    mov dword [ebp-0xc],0x0
00032448  8B750C            mov esi,[ebp+0xc]
0003244B  8B4508            mov eax,[ebp+0x8]
0003244E  890424            mov [esp],eax
00032451  E80AFFFFFF        call dword 0x32360
00032456  89F2              mov edx,esi
00032458  29C2              sub edx,eax
0003245A  89D0              mov eax,edx
0003245C  8945F0            mov [ebp-0x10],eax
0003245F  8B450C            mov eax,[ebp+0xc]
00032462  83C001            add eax,byte +0x1
00032465  8D50FF            lea edx,[eax-0x1]
00032468  8955EC            mov [ebp-0x14],edx
0003246B  BA10000000        mov edx,0x10
00032470  83EA01            sub edx,byte +0x1
00032473  01D0              add eax,edx
00032475  C745E410000000    mov dword [ebp-0x1c],0x10
0003247C  BA00000000        mov edx,0x0
00032481  F775E4            div dword [ebp-0x1c]
00032484  6BC010            imul eax,eax,byte +0x10
00032487  29C4              sub esp,eax
00032489  8D442408          lea eax,[esp+0x8]
0003248D  83C000            add eax,byte +0x0
00032490  8945E8            mov [ebp-0x18],eax
00032493  8B4508            mov eax,[ebp+0x8]
00032496  890424            mov [esp],eax
00032499  E8C2FEFFFF        call dword 0x32360
0003249E  8B55F0            mov edx,[ebp-0x10]
000324A1  01C2              add edx,eax
000324A3  8B4510            mov eax,[ebp+0x10]
000324A6  39C2              cmp edx,eax
000324A8  7368              jnc 0x32512
000324AA  C745F400000000    mov dword [ebp-0xc],0x0
000324B1  EB0F              jmp short 0x324c2
000324B3  8B55E8            mov edx,[ebp-0x18]
000324B6  8B45F4            mov eax,[ebp-0xc]
000324B9  01D0              add eax,edx
000324BB  C60030            mov byte [eax],0x30
000324BE  8345F401          add dword [ebp-0xc],byte +0x1
000324C2  8B45F4            mov eax,[ebp-0xc]
000324C5  3B45F0            cmp eax,[ebp-0x10]
000324C8  7CE9              jl 0x324b3
000324CA  8B55E8            mov edx,[ebp-0x18]
000324CD  8B45F0            mov eax,[ebp-0x10]
000324D0  01D0              add eax,edx
000324D2  C60000            mov byte [eax],0x0
000324D5  8B55E8            mov edx,[ebp-0x18]
000324D8  8B450C            mov eax,[ebp+0xc]
000324DB  01D0              add eax,edx
000324DD  C60000            mov byte [eax],0x0
000324E0  8B45E8            mov eax,[ebp-0x18]
000324E3  8B5508            mov edx,[ebp+0x8]
000324E6  89542404          mov [esp+0x4],edx
000324EA  890424            mov [esp],eax
000324ED  E89CFEFFFF        call dword 0x3238e
000324F2  8B45E8            mov eax,[ebp-0x18]
000324F5  89442404          mov [esp+0x4],eax
000324F9  8B4508            mov eax,[ebp+0x8]
000324FC  890424            mov [esp],eax
000324FF  E8E3FEFFFF        call dword 0x323e7
00032504  8B4510            mov eax,[ebp+0x10]
00032507  8D50FF            lea edx,[eax-0x1]
0003250A  8B4508            mov eax,[ebp+0x8]
0003250D  01D0              add eax,edx
0003250F  C60000            mov byte [eax],0x0
00032512  89DC              mov esp,ebx
00032514  8D65F8            lea esp,[ebp-0x8]
00032517  5B                pop ebx
00032518  5E                pop esi
00032519  5D                pop ebp
0003251A  C3                ret
0003251B  55                push ebp
0003251C  89E5              mov ebp,esp
0003251E  83EC10            sub esp,byte +0x10
00032521  C745F801000000    mov dword [ebp-0x8],0x1
00032528  837D0C00          cmp dword [ebp+0xc],byte +0x0
0003252C  7507              jnz 0x32535
0003252E  C745F801000000    mov dword [ebp-0x8],0x1
00032535  C745FC00000000    mov dword [ebp-0x4],0x0
0003253C  EB0E              jmp short 0x3254c
0003253E  8B45F8            mov eax,[ebp-0x8]
00032541  0FAF4508          imul eax,[ebp+0x8]
00032545  8945F8            mov [ebp-0x8],eax
00032548  8345FC01          add dword [ebp-0x4],byte +0x1
0003254C  8B45FC            mov eax,[ebp-0x4]
0003254F  3B450C            cmp eax,[ebp+0xc]
00032552  72EA              jc 0x3253e
00032554  8B45F8            mov eax,[ebp-0x8]
00032557  C9                leave
00032558  C3                ret
00032559  55                push ebp
0003255A  89E5              mov ebp,esp
0003255C  53                push ebx
0003255D  83EC28            sub esp,byte +0x28
00032560  C745F400000000    mov dword [ebp-0xc],0x0
00032567  C745F000000000    mov dword [ebp-0x10],0x0
0003256E  C745E800000000    mov dword [ebp-0x18],0x0
00032575  C745E400000000    mov dword [ebp-0x1c],0x0
0003257C  837D0C01          cmp dword [ebp+0xc],byte +0x1
00032580  7F19              jg 0x3259b
00032582  837D1000          cmp dword [ebp+0x10],byte +0x0
00032586  7409              jz 0x32591
00032588  8B4510            mov eax,[ebp+0x10]
0003258B  C700FFFFFFFF      mov dword [eax],0xffffffff
00032591  B800000000        mov eax,0x0
00032596  E9C2010000        jmp dword 0x3275d
0003259B  837D0C24          cmp dword [ebp+0xc],byte +0x24
0003259F  0F8E85000000      jng dword 0x3262a
000325A5  837D1000          cmp dword [ebp+0x10],byte +0x0
000325A9  7409              jz 0x325b4
000325AB  8B4510            mov eax,[ebp+0x10]
000325AE  C700FFFFFFFF      mov dword [eax],0xffffffff
000325B4  B800000000        mov eax,0x0
000325B9  E99F010000        jmp dword 0x3275d
000325BE  8B55F0            mov edx,[ebp-0x10]
000325C1  8B4508            mov eax,[ebp+0x8]
000325C4  01D0              add eax,edx
000325C6  0FB600            movzx eax,byte [eax]
000325C9  3C2D              cmp al,0x2d
000325CB  7513              jnz 0x325e0
000325CD  837DE400          cmp dword [ebp-0x1c],byte +0x0
000325D1  0F94C0            setz al
000325D4  0FB6C0            movzx eax,al
000325D7  8945E4            mov [ebp-0x1c],eax
000325DA  8345F001          add dword [ebp-0x10],byte +0x1
000325DE  EB4B              jmp short 0x3262b
000325E0  8B55F0            mov edx,[ebp-0x10]
000325E3  8B4508            mov eax,[ebp+0x8]
000325E6  01D0              add eax,edx
000325E8  0FB600            movzx eax,byte [eax]
000325EB  3C24              cmp al,0x24
000325ED  750D              jnz 0x325fc
000325EF  C7450C10000000    mov dword [ebp+0xc],0x10
000325F6  8345F001          add dword [ebp-0x10],byte +0x1
000325FA  EB2F              jmp short 0x3262b
000325FC  8B55F0            mov edx,[ebp-0x10]
000325FF  8B4508            mov eax,[ebp+0x8]
00032602  01D0              add eax,edx
00032604  0FB600            movzx eax,byte [eax]
00032607  3C30              cmp al,0x30
00032609  752F              jnz 0x3263a
0003260B  8B45F0            mov eax,[ebp-0x10]
0003260E  8D5001            lea edx,[eax+0x1]
00032611  8B4508            mov eax,[ebp+0x8]
00032614  01D0              add eax,edx
00032616  0FB600            movzx eax,byte [eax]
00032619  3C78              cmp al,0x78
0003261B  751D              jnz 0x3263a
0003261D  C7450C10000000    mov dword [ebp+0xc],0x10
00032624  8345F002          add dword [ebp-0x10],byte +0x2
00032628  EB01              jmp short 0x3262b
0003262A  90                nop
0003262B  8B55F0            mov edx,[ebp-0x10]
0003262E  8B4508            mov eax,[ebp+0x8]
00032631  01D0              add eax,edx
00032633  0FB600            movzx eax,byte [eax]
00032636  84C0              test al,al
00032638  7584              jnz 0x325be
0003263A  8B4508            mov eax,[ebp+0x8]
0003263D  890424            mov [esp],eax
00032640  E81BFDFFFF        call dword 0x32360
00032645  83E801            sub eax,byte +0x1
00032648  8945F8            mov [ebp-0x8],eax
0003264B  E9EF000000        jmp dword 0x3273f
00032650  8B55F8            mov edx,[ebp-0x8]
00032653  8B4508            mov eax,[ebp+0x8]
00032656  01D0              add eax,edx
00032658  0FB600            movzx eax,byte [eax]
0003265B  3C2F              cmp al,0x2f
0003265D  7E22              jng 0x32681
0003265F  8B55F8            mov edx,[ebp-0x8]
00032662  8B4508            mov eax,[ebp+0x8]
00032665  01D0              add eax,edx
00032667  0FB600            movzx eax,byte [eax]
0003266A  3C39              cmp al,0x39
0003266C  7F13              jg 0x32681
0003266E  8B55F8            mov edx,[ebp-0x8]
00032671  8B4508            mov eax,[ebp+0x8]
00032674  01D0              add eax,edx
00032676  0FB600            movzx eax,byte [eax]
00032679  83E830            sub eax,byte +0x30
0003267C  8845EF            mov [ebp-0x11],al
0003267F  EB75              jmp short 0x326f6
00032681  8B55F8            mov edx,[ebp-0x8]
00032684  8B4508            mov eax,[ebp+0x8]
00032687  01D0              add eax,edx
00032689  0FB600            movzx eax,byte [eax]
0003268C  3C60              cmp al,0x60
0003268E  7E22              jng 0x326b2
00032690  8B55F8            mov edx,[ebp-0x8]
00032693  8B4508            mov eax,[ebp+0x8]
00032696  01D0              add eax,edx
00032698  0FB600            movzx eax,byte [eax]
0003269B  3C7A              cmp al,0x7a
0003269D  7F13              jg 0x326b2
0003269F  8B55F8            mov edx,[ebp-0x8]
000326A2  8B4508            mov eax,[ebp+0x8]
000326A5  01D0              add eax,edx
000326A7  0FB600            movzx eax,byte [eax]
000326AA  83E857            sub eax,byte +0x57
000326AD  8845EF            mov [ebp-0x11],al
000326B0  EB44              jmp short 0x326f6
000326B2  8B55F8            mov edx,[ebp-0x8]
000326B5  8B4508            mov eax,[ebp+0x8]
000326B8  01D0              add eax,edx
000326BA  0FB600            movzx eax,byte [eax]
000326BD  3C40              cmp al,0x40
000326BF  7E22              jng 0x326e3
000326C1  8B55F8            mov edx,[ebp-0x8]
000326C4  8B4508            mov eax,[ebp+0x8]
000326C7  01D0              add eax,edx
000326C9  0FB600            movzx eax,byte [eax]
000326CC  3C5A              cmp al,0x5a
000326CE  7F13              jg 0x326e3
000326D0  8B55F8            mov edx,[ebp-0x8]
000326D3  8B4508            mov eax,[ebp+0x8]
000326D6  01D0              add eax,edx
000326D8  0FB600            movzx eax,byte [eax]
000326DB  83E837            sub eax,byte +0x37
000326DE  8845EF            mov [ebp-0x11],al
000326E1  EB13              jmp short 0x326f6
000326E3  837D1000          cmp dword [ebp+0x10],byte +0x0
000326E7  7408              jz 0x326f1
000326E9  8B4510            mov eax,[ebp+0x10]
000326EC  8B55F8            mov edx,[ebp-0x8]
000326EF  8910              mov [eax],edx
000326F1  8B45E8            mov eax,[ebp-0x18]
000326F4  EB67              jmp short 0x3275d
000326F6  0FB645EF          movzx eax,byte [ebp-0x11]
000326FA  3B450C            cmp eax,[ebp+0xc]
000326FD  7C13              jl 0x32712
000326FF  837D1000          cmp dword [ebp+0x10],byte +0x0
00032703  7408              jz 0x3270d
00032705  8B4510            mov eax,[ebp+0x10]
00032708  8B55F8            mov edx,[ebp-0x8]
0003270B  8910              mov [eax],edx
0003270D  8B45E8            mov eax,[ebp-0x18]
00032710  EB4B              jmp short 0x3275d
00032712  0FB65DEF          movzx ebx,byte [ebp-0x11]
00032716  8B55F4            mov edx,[ebp-0xc]
00032719  8B450C            mov eax,[ebp+0xc]
0003271C  89542404          mov [esp+0x4],edx
00032720  890424            mov [esp],eax
00032723  E8F3FDFFFF        call dword 0x3251b
00032728  0FAFC3            imul eax,ebx
0003272B  0145E8            add [ebp-0x18],eax
0003272E  837DE400          cmp dword [ebp-0x1c],byte +0x0
00032732  7403              jz 0x32737
00032734  F75DE8            neg dword [ebp-0x18]
00032737  836DF801          sub dword [ebp-0x8],byte +0x1
0003273B  8345F401          add dword [ebp-0xc],byte +0x1
0003273F  8B45F8            mov eax,[ebp-0x8]
00032742  3B45F0            cmp eax,[ebp-0x10]
00032745  0F8D05FFFFFF      jnl dword 0x32650
0003274B  837D1000          cmp dword [ebp+0x10],byte +0x0
0003274F  7409              jz 0x3275a
00032751  8B4510            mov eax,[ebp+0x10]
00032754  C70000000000      mov dword [eax],0x0
0003275A  8B45E8            mov eax,[ebp-0x18]
0003275D  83C428            add esp,byte +0x28
00032760  5B                pop ebx
00032761  5D                pop ebp
00032762  C3                ret
00032763  55                push ebp
00032764  89E5              mov ebp,esp
00032766  53                push ebx
00032767  83EC34            sub esp,byte +0x34
0003276A  89E0              mov eax,esp
0003276C  89C1              mov ecx,eax
0003276E  8B4514            mov eax,[ebp+0x14]
00032771  8D50FF            lea edx,[eax-0x1]
00032774  8955E8            mov [ebp-0x18],edx
00032777  BA10000000        mov edx,0x10
0003277C  83EA01            sub edx,byte +0x1
0003277F  01D0              add eax,edx
00032781  C745D410000000    mov dword [ebp-0x2c],0x10
00032788  BA00000000        mov edx,0x0
0003278D  F775D4            div dword [ebp-0x2c]
00032790  6BC010            imul eax,eax,byte +0x10
00032793  29C4              sub esp,eax
00032795  89E0              mov eax,esp
00032797  83C000            add eax,byte +0x0
0003279A  8945E4            mov [ebp-0x1c],eax
0003279D  837D0C01          cmp dword [ebp+0xc],byte +0x1
000327A1  7F0A              jg 0x327ad
000327A3  B8FFFFFFFF        mov eax,0xffffffff
000327A8  E9F9000000        jmp dword 0x328a6
000327AD  837D0C24          cmp dword [ebp+0xc],byte +0x24
000327B1  7E0A              jng 0x327bd
000327B3  B8FFFFFFFF        mov eax,0xffffffff
000327B8  E9E9000000        jmp dword 0x328a6
000327BD  837D0800          cmp dword [ebp+0x8],byte +0x0
000327C1  751F              jnz 0x327e2
000327C3  837D1401          cmp dword [ebp+0x14],byte +0x1
000327C7  7E19              jng 0x327e2
000327C9  8B4510            mov eax,[ebp+0x10]
000327CC  C60030            mov byte [eax],0x30
000327CF  8B4510            mov eax,[ebp+0x10]
000327D2  83C001            add eax,byte +0x1
000327D5  C60000            mov byte [eax],0x0
000327D8  B802000000        mov eax,0x2
000327DD  E9C4000000        jmp dword 0x328a6
000327E2  C745F400000000    mov dword [ebp-0xc],0x0
000327E9  EB52              jmp short 0x3283d
000327EB  8B5D0C            mov ebx,[ebp+0xc]
000327EE  8B4508            mov eax,[ebp+0x8]
000327F1  BA00000000        mov edx,0x0
000327F6  F7F3              div ebx
000327F8  89D0              mov eax,edx
000327FA  8945EC            mov [ebp-0x14],eax
000327FD  837DEC09          cmp dword [ebp-0x14],byte +0x9
00032801  7F12              jg 0x32815
00032803  8B45EC            mov eax,[ebp-0x14]
00032806  83C030            add eax,byte +0x30
00032809  8B5DE4            mov ebx,[ebp-0x1c]
0003280C  8B55F4            mov edx,[ebp-0xc]
0003280F  01DA              add edx,ebx
00032811  8802              mov [edx],al
00032813  EB10              jmp short 0x32825
00032815  8B45EC            mov eax,[ebp-0x14]
00032818  83C057            add eax,byte +0x57
0003281B  8B5DE4            mov ebx,[ebp-0x1c]
0003281E  8B55F4            mov edx,[ebp-0xc]
00032821  01DA              add edx,ebx
00032823  8802              mov [edx],al
00032825  8B550C            mov edx,[ebp+0xc]
00032828  8955D4            mov [ebp-0x2c],edx
0003282B  8B4508            mov eax,[ebp+0x8]
0003282E  BA00000000        mov edx,0x0
00032833  F775D4            div dword [ebp-0x2c]
00032836  894508            mov [ebp+0x8],eax
00032839  8345F401          add dword [ebp-0xc],byte +0x1
0003283D  837D0800          cmp dword [ebp+0x8],byte +0x0
00032841  7408              jz 0x3284b
00032843  8B45F4            mov eax,[ebp-0xc]
00032846  3B4514            cmp eax,[ebp+0x14]
00032849  7CA0              jl 0x327eb
0003284B  8B45F4            mov eax,[ebp-0xc]
0003284E  83E801            sub eax,byte +0x1
00032851  8945EC            mov [ebp-0x14],eax
00032854  C745F000000000    mov dword [ebp-0x10],0x0
0003285B  EB1D              jmp short 0x3287a
0003285D  8B55F0            mov edx,[ebp-0x10]
00032860  8B4510            mov eax,[ebp+0x10]
00032863  01C2              add edx,eax
00032865  8B5DE4            mov ebx,[ebp-0x1c]
00032868  8B45EC            mov eax,[ebp-0x14]
0003286B  01D8              add eax,ebx
0003286D  0FB600            movzx eax,byte [eax]
00032870  8802              mov [edx],al
00032872  836DEC01          sub dword [ebp-0x14],byte +0x1
00032876  8345F001          add dword [ebp-0x10],byte +0x1
0003287A  837DEC00          cmp dword [ebp-0x14],byte +0x0
0003287E  79DD              jns 0x3285d
00032880  8B45F4            mov eax,[ebp-0xc]
00032883  3B4514            cmp eax,[ebp+0x14]
00032886  7D0D              jnl 0x32895
00032888  8B55F4            mov edx,[ebp-0xc]
0003288B  8B4510            mov eax,[ebp+0x10]
0003288E  01D0              add eax,edx
00032890  C60000            mov byte [eax],0x0
00032893  EB0E              jmp short 0x328a3
00032895  8B4514            mov eax,[ebp+0x14]
00032898  8D50FF            lea edx,[eax-0x1]
0003289B  8B4510            mov eax,[ebp+0x10]
0003289E  01D0              add eax,edx
000328A0  C60000            mov byte [eax],0x0
000328A3  8B45F4            mov eax,[ebp-0xc]
000328A6  89CC              mov esp,ecx
000328A8  8B5DFC            mov ebx,[ebp-0x4]
000328AB  C9                leave
000328AC  C3                ret
000328AD  55                push ebp
000328AE  89E5              mov ebp,esp
000328B0  83EC04            sub esp,byte +0x4
000328B3  8B4508            mov eax,[ebp+0x8]
000328B6  8845FC            mov [ebp-0x4],al
000328B9  90                nop
000328BA  C9                leave
000328BB  C3                ret
000328BC  55                push ebp
000328BD  89E5              mov ebp,esp
000328BF  83EC28            sub esp,byte +0x28
000328C2  C7042460000000    mov dword [esp],0x60
000328C9  E868060000        call dword 0x32f36
000328CE  83EC04            sub esp,byte +0x4
000328D1  8845F7            mov [ebp-0x9],al
000328D4  90                nop
000328D5  C7042460000000    mov dword [esp],0x60
000328DC  E855060000        call dword 0x32f36
000328E1  83EC04            sub esp,byte +0x4
000328E4  3A45F7            cmp al,[ebp-0x9]
000328E7  74EC              jz 0x328d5
000328E9  C9                leave
000328EA  C3                ret
000328EB  55                push ebp
000328EC  89E5              mov ebp,esp
000328EE  B800000000        mov eax,0x0
000328F3  5D                pop ebp
000328F4  C3                ret
000328F5  55                push ebp
000328F6  89E5              mov ebp,esp
000328F8  83EC10            sub esp,byte +0x10
000328FB  C645FF00          mov byte [ebp-0x1],0x0
000328FF  EB08              jmp short 0x32909
00032901  E8E5FFFFFF        call dword 0x328eb
00032906  8845FF            mov [ebp-0x1],al
00032909  807DFF00          cmp byte [ebp-0x1],0x0
0003290D  74F2              jz 0x32901
0003290F  0FB645FF          movzx eax,byte [ebp-0x1]
00032913  C9                leave
00032914  C3                ret
00032915  55                push ebp
00032916  89E5              mov ebp,esp
00032918  53                push ebx
00032919  83EC14            sub esp,byte +0x14
0003291C  C745F800000000    mov dword [ebp-0x8],0x0
00032923  EB6A              jmp short 0x3298f
00032925  8B55F8            mov edx,[ebp-0x8]
00032928  8B4508            mov eax,[ebp+0x8]
0003292B  8D1C02            lea ebx,[edx+eax]
0003292E  E8C2FFFFFF        call dword 0x328f5
00032933  8803              mov [ebx],al
00032935  8B55F8            mov edx,[ebp-0x8]
00032938  8B4508            mov eax,[ebp+0x8]
0003293B  01D0              add eax,edx
0003293D  0FB600            movzx eax,byte [eax]
00032940  3C0D              cmp al,0xd
00032942  740F              jz 0x32953
00032944  8B55F8            mov edx,[ebp-0x8]
00032947  8B4508            mov eax,[ebp+0x8]
0003294A  01D0              add eax,edx
0003294C  0FB600            movzx eax,byte [eax]
0003294F  3C0A              cmp al,0xa
00032951  7513              jnz 0x32966
00032953  8B55F8            mov edx,[ebp-0x8]
00032956  8B4508            mov eax,[ebp+0x8]
00032959  01D0              add eax,edx
0003295B  C60000            mov byte [eax],0x0
0003295E  8B45F8            mov eax,[ebp-0x8]
00032961  E991000000        jmp dword 0x329f7
00032966  8B55F8            mov edx,[ebp-0x8]
00032969  8B4508            mov eax,[ebp+0x8]
0003296C  01D0              add eax,edx
0003296E  0FB600            movzx eax,byte [eax]
00032971  0FBEC0            movsx eax,al
00032974  890424            mov [esp],eax
00032977  E831FFFFFF        call dword 0x328ad
0003297C  8B55F8            mov edx,[ebp-0x8]
0003297F  8B4508            mov eax,[ebp+0x8]
00032982  01D0              add eax,edx
00032984  0FB600            movzx eax,byte [eax]
00032987  84C0              test al,al
00032989  7404              jz 0x3298f
0003298B  8345F801          add dword [ebp-0x8],byte +0x1
0003298F  8B45F8            mov eax,[ebp-0x8]
00032992  3B450C            cmp eax,[ebp+0xc]
00032995  7C8E              jl 0x32925
00032997  8B55F8            mov edx,[ebp-0x8]
0003299A  8B4508            mov eax,[ebp+0x8]
0003299D  01D0              add eax,edx
0003299F  C60000            mov byte [eax],0x0
000329A2  EB39              jmp short 0x329dd
000329A4  8B55F8            mov edx,[ebp-0x8]
000329A7  8B4508            mov eax,[ebp+0x8]
000329AA  8D1C02            lea ebx,[edx+eax]
000329AD  E839FFFFFF        call dword 0x328eb
000329B2  8803              mov [ebx],al
000329B4  8345F801          add dword [ebp-0x8],byte +0x1
000329B8  8B55F8            mov edx,[ebp-0x8]
000329BB  8B4508            mov eax,[ebp+0x8]
000329BE  01D0              add eax,edx
000329C0  0FB600            movzx eax,byte [eax]
000329C3  84C0              test al,al
000329C5  7416              jz 0x329dd
000329C7  8B55F8            mov edx,[ebp-0x8]
000329CA  8B4508            mov eax,[ebp+0x8]
000329CD  01D0              add eax,edx
000329CF  0FB600            movzx eax,byte [eax]
000329D2  0FBEC0            movsx eax,al
000329D5  890424            mov [esp],eax
000329D8  E8D0FEFFFF        call dword 0x328ad
000329DD  8B45F8            mov eax,[ebp-0x8]
000329E0  3B4510            cmp eax,[ebp+0x10]
000329E3  7D0F              jnl 0x329f4
000329E5  8B55F8            mov edx,[ebp-0x8]
000329E8  8B4508            mov eax,[ebp+0x8]
000329EB  01D0              add eax,edx
000329ED  0FB600            movzx eax,byte [eax]
000329F0  84C0              test al,al
000329F2  75B0              jnz 0x329a4
000329F4  8B45F8            mov eax,[ebp-0x8]
000329F7  83C414            add esp,byte +0x14
000329FA  5B                pop ebx
000329FB  5D                pop ebp
000329FC  C3                ret
000329FD  55                push ebp
000329FE  89E5              mov ebp,esp
00032A00  83EC1C            sub esp,byte +0x1c
00032A03  8B4D08            mov ecx,[ebp+0x8]
00032A06  8B5514            mov edx,[ebp+0x14]
00032A09  8B4518            mov eax,[ebp+0x18]
00032A0C  884DEC            mov [ebp-0x14],cl
00032A0F  8855E8            mov [ebp-0x18],dl
00032A12  8845E4            mov [ebp-0x1c],al
00032A15  C745FC00800B00    mov dword [ebp-0x4],0xb8000
00032A1C  8B5510            mov edx,[ebp+0x10]
00032A1F  89D0              mov eax,edx
00032A21  C1E002            shl eax,0x2
00032A24  01D0              add eax,edx
00032A26  C1E004            shl eax,0x4
00032A29  89C2              mov edx,eax
00032A2B  8B450C            mov eax,[ebp+0xc]
00032A2E  01D0              add eax,edx
00032A30  8D1400            lea edx,[eax+eax]
00032A33  8B45FC            mov eax,[ebp-0x4]
00032A36  01C2              add edx,eax
00032A38  0FB645EC          movzx eax,byte [ebp-0x14]
00032A3C  8802              mov [edx],al
00032A3E  8B5510            mov edx,[ebp+0x10]
00032A41  89D0              mov eax,edx
00032A43  C1E002            shl eax,0x2
00032A46  01D0              add eax,edx
00032A48  C1E004            shl eax,0x4
00032A4B  89C2              mov edx,eax
00032A4D  8B450C            mov eax,[ebp+0xc]
00032A50  01D0              add eax,edx
00032A52  8D1400            lea edx,[eax+eax]
00032A55  8B45FC            mov eax,[ebp-0x4]
00032A58  01C2              add edx,eax
00032A5A  0FB645E8          movzx eax,byte [ebp-0x18]
00032A5E  C1E004            shl eax,0x4
00032A61  C0F804            sar al,0x4
00032A64  89C1              mov ecx,eax
00032A66  83E10F            and ecx,byte +0xf
00032A69  0FB64201          movzx eax,byte [edx+0x1]
00032A6D  83E0F0            and eax,byte -0x10
00032A70  09C8              or eax,ecx
00032A72  884201            mov [edx+0x1],al
00032A75  8B5510            mov edx,[ebp+0x10]
00032A78  89D0              mov eax,edx
00032A7A  C1E002            shl eax,0x2
00032A7D  01D0              add eax,edx
00032A7F  C1E004            shl eax,0x4
00032A82  89C2              mov edx,eax
00032A84  8B450C            mov eax,[ebp+0xc]
00032A87  01D0              add eax,edx
00032A89  8D1400            lea edx,[eax+eax]
00032A8C  8B45FC            mov eax,[ebp-0x4]
00032A8F  01C2              add edx,eax
00032A91  0FB645E4          movzx eax,byte [ebp-0x1c]
00032A95  C1E004            shl eax,0x4
00032A98  C0F804            sar al,0x4
00032A9B  89C1              mov ecx,eax
00032A9D  C1E104            shl ecx,0x4
00032AA0  0FB64201          movzx eax,byte [edx+0x1]
00032AA4  83E00F            and eax,byte +0xf
00032AA7  09C8              or eax,ecx
00032AA9  884201            mov [edx+0x1],al
00032AAC  C9                leave
00032AAD  C3                ret
00032AAE  55                push ebp
00032AAF  89E5              mov ebp,esp
00032AB1  83EC10            sub esp,byte +0x10
00032AB4  C745FC00800B00    mov dword [ebp-0x4],0xb8000
00032ABB  8B550C            mov edx,[ebp+0xc]
00032ABE  89D0              mov eax,edx
00032AC0  C1E002            shl eax,0x2
00032AC3  01D0              add eax,edx
00032AC5  C1E004            shl eax,0x4
00032AC8  89C2              mov edx,eax
00032ACA  8B4508            mov eax,[ebp+0x8]
00032ACD  01D0              add eax,edx
00032ACF  8D1400            lea edx,[eax+eax]
00032AD2  8B45FC            mov eax,[ebp-0x4]
00032AD5  01C2              add edx,eax
00032AD7  8B4510            mov eax,[ebp+0x10]
00032ADA  0FB712            movzx edx,word [edx]
00032ADD  668910            mov [eax],dx
00032AE0  C9                leave
00032AE1  C3                ret
00032AE2  55                push ebp
00032AE3  89E5              mov ebp,esp
00032AE5  57                push edi
00032AE6  56                push esi
00032AE7  53                push ebx
00032AE8  83EC2C            sub esp,byte +0x2c
00032AEB  8B5514            mov edx,[ebp+0x14]
00032AEE  8B4518            mov eax,[ebp+0x18]
00032AF1  8855E0            mov [ebp-0x20],dl
00032AF4  8845DC            mov [ebp-0x24],al
00032AF7  C745F000000000    mov dword [ebp-0x10],0x0
00032AFE  EB76              jmp short 0x32b76
00032B00  0FBE75DC          movsx esi,byte [ebp-0x24]
00032B04  0FBE5DE0          movsx ebx,byte [ebp-0x20]
00032B08  8B4D0C            mov ecx,[ebp+0xc]
00032B0B  BA67666666        mov edx,0x66666667
00032B10  89C8              mov eax,ecx
00032B12  F7EA              imul edx
00032B14  C1FA05            sar edx,0x5
00032B17  89C8              mov eax,ecx
00032B19  C1F81F            sar eax,0x1f
00032B1C  29C2              sub edx,eax
00032B1E  8B4510            mov eax,[ebp+0x10]
00032B21  8D3C02            lea edi,[edx+eax]
00032B24  8B4D0C            mov ecx,[ebp+0xc]
00032B27  BA67666666        mov edx,0x66666667
00032B2C  89C8              mov eax,ecx
00032B2E  F7EA              imul edx
00032B30  C1FA05            sar edx,0x5
00032B33  89C8              mov eax,ecx
00032B35  C1F81F            sar eax,0x1f
00032B38  29C2              sub edx,eax
00032B3A  89D0              mov eax,edx
00032B3C  C1E002            shl eax,0x2
00032B3F  01D0              add eax,edx
00032B41  C1E004            shl eax,0x4
00032B44  89CA              mov edx,ecx
00032B46  29C2              sub edx,eax
00032B48  8B4DF0            mov ecx,[ebp-0x10]
00032B4B  8B4508            mov eax,[ebp+0x8]
00032B4E  01C8              add eax,ecx
00032B50  0FB600            movzx eax,byte [eax]
00032B53  0FBEC0            movsx eax,al
00032B56  89742410          mov [esp+0x10],esi
00032B5A  895C240C          mov [esp+0xc],ebx
00032B5E  897C2408          mov [esp+0x8],edi
00032B62  89542404          mov [esp+0x4],edx
00032B66  890424            mov [esp],eax
00032B69  E88FFEFFFF        call dword 0x329fd
00032B6E  8345F001          add dword [ebp-0x10],byte +0x1
00032B72  83450C01          add dword [ebp+0xc],byte +0x1
00032B76  8B55F0            mov edx,[ebp-0x10]
00032B79  8B4508            mov eax,[ebp+0x8]
00032B7C  01D0              add eax,edx
00032B7E  0FB600            movzx eax,byte [eax]
00032B81  84C0              test al,al
00032B83  0F8577FFFFFF      jnz dword 0x32b00
00032B89  83C42C            add esp,byte +0x2c
00032B8C  5B                pop ebx
00032B8D  5E                pop esi
00032B8E  5F                pop edi
00032B8F  5D                pop ebp
00032B90  C3                ret
00032B91  55                push ebp
00032B92  89E5              mov ebp,esp
00032B94  83EC1C            sub esp,byte +0x1c
00032B97  C745F400800B00    mov dword [ebp-0xc],0xb8000
00032B9E  C745F800000000    mov dword [ebp-0x8],0x0
00032BA5  EB56              jmp short 0x32bfd
00032BA7  C745FC00000000    mov dword [ebp-0x4],0x0
00032BAE  EB43              jmp short 0x32bf3
00032BB0  8D45F2            lea eax,[ebp-0xe]
00032BB3  89442408          mov [esp+0x8],eax
00032BB7  8B45F8            mov eax,[ebp-0x8]
00032BBA  89442404          mov [esp+0x4],eax
00032BBE  8B45FC            mov eax,[ebp-0x4]
00032BC1  890424            mov [esp],eax
00032BC4  E8E5FEFFFF        call dword 0x32aae
00032BC9  8B45F8            mov eax,[ebp-0x8]
00032BCC  8D50FF            lea edx,[eax-0x1]
00032BCF  89D0              mov eax,edx
00032BD1  C1E002            shl eax,0x2
00032BD4  01D0              add eax,edx
00032BD6  C1E004            shl eax,0x4
00032BD9  89C2              mov edx,eax
00032BDB  8B45FC            mov eax,[ebp-0x4]
00032BDE  01D0              add eax,edx
00032BE0  8D1400            lea edx,[eax+eax]
00032BE3  8B45F4            mov eax,[ebp-0xc]
00032BE6  01C2              add edx,eax
00032BE8  0FB745F2          movzx eax,word [ebp-0xe]
00032BEC  668902            mov [edx],ax
00032BEF  8345FC01          add dword [ebp-0x4],byte +0x1
00032BF3  837DFC4F          cmp dword [ebp-0x4],byte +0x4f
00032BF7  7EB7              jng 0x32bb0
00032BF9  8345F801          add dword [ebp-0x8],byte +0x1
00032BFD  837DF818          cmp dword [ebp-0x8],byte +0x18
00032C01  7EA4              jng 0x32ba7
00032C03  C745F818000000    mov dword [ebp-0x8],0x18
00032C0A  C645F220          mov byte [ebp-0xe],0x20
00032C0E  0FB645F3          movzx eax,byte [ebp-0xd]
00032C12  83E00F            and eax,byte +0xf
00032C15  8845F3            mov [ebp-0xd],al
00032C18  0FB645F3          movzx eax,byte [ebp-0xd]
00032C1C  83C80F            or eax,byte +0xf
00032C1F  8845F3            mov [ebp-0xd],al
00032C22  C745FC00000000    mov dword [ebp-0x4],0x0
00032C29  EB27              jmp short 0x32c52
00032C2B  8B55F8            mov edx,[ebp-0x8]
00032C2E  89D0              mov eax,edx
00032C30  C1E002            shl eax,0x2
00032C33  01D0              add eax,edx
00032C35  C1E004            shl eax,0x4
00032C38  89C2              mov edx,eax
00032C3A  8B45FC            mov eax,[ebp-0x4]
00032C3D  01D0              add eax,edx
00032C3F  8D1400            lea edx,[eax+eax]
00032C42  8B45F4            mov eax,[ebp-0xc]
00032C45  01C2              add edx,eax
00032C47  0FB745F2          movzx eax,word [ebp-0xe]
00032C4B  668902            mov [edx],ax
00032C4E  8345FC01          add dword [ebp-0x4],byte +0x1
00032C52  837DFC4F          cmp dword [ebp-0x4],byte +0x4f
00032C56  7ED3              jng 0x32c2b
00032C58  C9                leave
00032C59  C3                ret
00032C5A  55                push ebp
00032C5B  89E5              mov ebp,esp
00032C5D  C705843C03000000  mov dword [dword 0x33c84],0x0
         -0000
00032C67  A18C3C0300        mov eax,[0x33c8c]
00032C6C  83F817            cmp eax,byte +0x17
00032C6F  7E11              jng 0x32c82
00032C71  E81BFFFFFF        call dword 0x32b91
00032C76  C7058C3C03001800  mov dword [dword 0x33c8c],0x18
         -0000
00032C80  EB0D              jmp short 0x32c8f
00032C82  A18C3C0300        mov eax,[0x33c8c]
00032C87  83C001            add eax,byte +0x1
00032C8A  A38C3C0300        mov [0x33c8c],eax
00032C8F  5D                pop ebp
00032C90  C3                ret
00032C91  55                push ebp
00032C92  89E5              mov ebp,esp
00032C94  83EC28            sub esp,byte +0x28
00032C97  8B158C3C0300      mov edx,[dword 0x33c8c]
00032C9D  89D0              mov eax,edx
00032C9F  C1E002            shl eax,0x2
00032CA2  01D0              add eax,edx
00032CA4  C1E004            shl eax,0x4
00032CA7  89C2              mov edx,eax
00032CA9  A1843C0300        mov eax,[0x33c84]
00032CAE  01D0              add eax,edx
00032CB0  8945F4            mov [ebp-0xc],eax
00032CB3  C74424040E000000  mov dword [esp+0x4],0xe
00032CBB  C70424D4030000    mov dword [esp],0x3d4
00032CC2  E8E0020000        call dword 0x32fa7
00032CC7  83EC08            sub esp,byte +0x8
00032CCA  8B45F4            mov eax,[ebp-0xc]
00032CCD  C1F808            sar eax,0x8
00032CD0  0FBEC0            movsx eax,al
00032CD3  89442404          mov [esp+0x4],eax
00032CD7  C70424D5030000    mov dword [esp],0x3d5
00032CDE  E8C4020000        call dword 0x32fa7
00032CE3  83EC08            sub esp,byte +0x8
00032CE6  C74424040F000000  mov dword [esp+0x4],0xf
00032CEE  C70424D4030000    mov dword [esp],0x3d4
00032CF5  E8AD020000        call dword 0x32fa7
00032CFA  83EC08            sub esp,byte +0x8
00032CFD  8B45F4            mov eax,[ebp-0xc]
00032D00  0FBEC0            movsx eax,al
00032D03  89442404          mov [esp+0x4],eax
00032D07  C70424D5030000    mov dword [esp],0x3d5
00032D0E  E894020000        call dword 0x32fa7
00032D13  83EC08            sub esp,byte +0x8
00032D16  C9                leave
00032D17  C3                ret
00032D18  55                push ebp
00032D19  89E5              mov ebp,esp
00032D1B  53                push ebx
00032D1C  81EC04010000      sub esp,0x104
00032D22  8D450C            lea eax,[ebp+0xc]
00032D25  8945EC            mov [ebp-0x14],eax
00032D28  8B45EC            mov eax,[ebp-0x14]
00032D2B  8944240C          mov [esp+0xc],eax
00032D2F  8B4508            mov eax,[ebp+0x8]
00032D32  89442408          mov [esp+0x8],eax
00032D36  C7442404C8000000  mov dword [esp+0x4],0xc8
00032D3E  8D8524FFFFFF      lea eax,[ebp-0xdc]
00032D44  890424            mov [esp],eax
00032D47  E850EBFFFF        call dword 0x3189c
00032D4C  8945F0            mov [ebp-0x10],eax
00032D4F  837DF000          cmp dword [ebp-0x10],byte +0x0
00032D53  0F8EFC000000      jng dword 0x32e55
00032D59  C704249C3C0300    mov dword [esp],0x33c9c
00032D60  E8E6F4FFFF        call dword 0x3224b
00032D65  C70424903C0300    mov dword [esp],0x33c90
00032D6C  E8DAF4FFFF        call dword 0x3224b
00032D71  C745F400000000    mov dword [ebp-0xc],0x0
00032D78  E9AF000000        jmp dword 0x32e2c
00032D7D  8D9524FFFFFF      lea edx,[ebp-0xdc]
00032D83  8B45F4            mov eax,[ebp-0xc]
00032D86  01D0              add eax,edx
00032D88  0FB600            movzx eax,byte [eax]
00032D8B  3C0A              cmp al,0xa
00032D8D  750A              jnz 0x32d99
00032D8F  E8C6FEFFFF        call dword 0x32c5a
00032D94  E98F000000        jmp dword 0x32e28
00032D99  8D9524FFFFFF      lea edx,[ebp-0xdc]
00032D9F  8B45F4            mov eax,[ebp-0xc]
00032DA2  01D0              add eax,edx
00032DA4  0FB600            movzx eax,byte [eax]
00032DA7  3C0D              cmp al,0xd
00032DA9  747C              jz 0x32e27
00032DAB  8B0D8C3C0300      mov ecx,[dword 0x33c8c]
00032DB1  8B15843C0300      mov edx,[dword 0x33c84]
00032DB7  8D9D24FFFFFF      lea ebx,[ebp-0xdc]
00032DBD  8B45F4            mov eax,[ebp-0xc]
00032DC0  01D8              add eax,ebx
00032DC2  0FB600            movzx eax,byte [eax]
00032DC5  0FBEC0            movsx eax,al
00032DC8  C744241000000000  mov dword [esp+0x10],0x0
00032DD0  C744240C0F000000  mov dword [esp+0xc],0xf
00032DD8  894C2408          mov [esp+0x8],ecx
00032DDC  89542404          mov [esp+0x4],edx
00032DE0  890424            mov [esp],eax
00032DE3  E815FCFFFF        call dword 0x329fd
00032DE8  A1843C0300        mov eax,[0x33c84]
00032DED  83C001            add eax,byte +0x1
00032DF0  A3843C0300        mov [0x33c84],eax
00032DF5  8B0D843C0300      mov ecx,[dword 0x33c84]
00032DFB  BA67666666        mov edx,0x66666667
00032E00  89C8              mov eax,ecx
00032E02  F7EA              imul edx
00032E04  C1FA05            sar edx,0x5
00032E07  89C8              mov eax,ecx
00032E09  C1F81F            sar eax,0x1f
00032E0C  29C2              sub edx,eax
00032E0E  89D0              mov eax,edx
00032E10  C1E002            shl eax,0x2
00032E13  01D0              add eax,edx
00032E15  C1E004            shl eax,0x4
00032E18  89CA              mov edx,ecx
00032E1A  29C2              sub edx,eax
00032E1C  85D2              test edx,edx
00032E1E  7508              jnz 0x32e28
00032E20  E835FEFFFF        call dword 0x32c5a
00032E25  EB01              jmp short 0x32e28
00032E27  90                nop
00032E28  8345F401          add dword [ebp-0xc],byte +0x1
00032E2C  8B45F4            mov eax,[ebp-0xc]
00032E2F  3B45F0            cmp eax,[ebp-0x10]
00032E32  0F8C45FFFFFF      jl dword 0x32d7d
00032E38  E854FEFFFF        call dword 0x32c91
00032E3D  C70424903C0300    mov dword [esp],0x33c90
00032E44  E858F4FFFF        call dword 0x322a1
00032E49  C704249C3C0300    mov dword [esp],0x33c9c
00032E50  E84CF4FFFF        call dword 0x322a1
00032E55  81C404010000      add esp,0x104
00032E5B  5B                pop ebx
00032E5C  5D                pop ebp
00032E5D  C3                ret
00032E5E  55                push ebp
00032E5F  89E5              mov ebp,esp
00032E61  83EC10            sub esp,byte +0x10
00032E64  C745FC00000000    mov dword [ebp-0x4],0x0
00032E6B  C745F800000000    mov dword [ebp-0x8],0x0
00032E72  EB1F              jmp short 0x32e93
00032E74  8B55FC            mov edx,[ebp-0x4]
00032E77  8B4508            mov eax,[ebp+0x8]
00032E7A  01D0              add eax,edx
00032E7C  0FB600            movzx eax,byte [eax]
00032E7F  0FB6D0            movzx edx,al
00032E82  8B45F8            mov eax,[ebp-0x8]
00032E85  01D0              add eax,edx
00032E87  25FFFF0000        and eax,0xffff
00032E8C  8945F8            mov [ebp-0x8],eax
00032E8F  8345FC01          add dword [ebp-0x4],byte +0x1
00032E93  8B45FC            mov eax,[ebp-0x4]
00032E96  3B450C            cmp eax,[ebp+0xc]
00032E99  7CD9              jl 0x32e74
00032E9B  8B45F8            mov eax,[ebp-0x8]
00032E9E  C9                leave
00032E9F  C3                ret
00032EA0  55                push ebp
00032EA1  89E5              mov ebp,esp
00032EA3  53                push ebx
00032EA4  83EC24            sub esp,byte +0x24
00032EA7  8B4508            mov eax,[ebp+0x8]
00032EAA  83C004            add eax,byte +0x4
00032EAD  8B10              mov edx,[eax]
00032EAF  8B4508            mov eax,[ebp+0x8]
00032EB2  8B00              mov eax,[eax]
00032EB4  89542408          mov [esp+0x8],edx
00032EB8  89442404          mov [esp+0x4],eax
00032EBC  C70424B6360300    mov dword [esp],0x336b6
00032EC3  E8ACF2FFFF        call dword 0x32174
00032EC8  8B4508            mov eax,[ebp+0x8]
00032ECB  83C014            add eax,byte +0x14
00032ECE  8B18              mov ebx,[eax]
00032ED0  8B4508            mov eax,[ebp+0x8]
00032ED3  83C010            add eax,byte +0x10
00032ED6  8B08              mov ecx,[eax]
00032ED8  8B4508            mov eax,[ebp+0x8]
00032EDB  83C00C            add eax,byte +0xc
00032EDE  8B10              mov edx,[eax]
00032EE0  8B4508            mov eax,[ebp+0x8]
00032EE3  83C008            add eax,byte +0x8
00032EE6  8B00              mov eax,[eax]
00032EE8  895C2410          mov [esp+0x10],ebx
00032EEC  894C240C          mov [esp+0xc],ecx
00032EF0  89542408          mov [esp+0x8],edx
00032EF4  89442404          mov [esp+0x4],eax
00032EF8  C70424CE360300    mov dword [esp],0x336ce
00032EFF  E870F2FFFF        call dword 0x32174
00032F04  8B4508            mov eax,[ebp+0x8]
00032F07  83C018            add eax,byte +0x18
00032F0A  8B00              mov eax,[eax]
00032F0C  89442404          mov [esp+0x4],eax
00032F10  C70424EC360300    mov dword [esp],0x336ec
00032F17  E858F2FFFF        call dword 0x32174
00032F1C  83C424            add esp,byte +0x24
00032F1F  5B                pop ebx
00032F20  5D                pop ebp
00032F21  C3                ret
00032F22  6690              xchg ax,ax
00032F24  6690              xchg ax,ax
00032F26  6690              xchg ax,ax
00032F28  6690              xchg ax,ax
00032F2A  6690              xchg ax,ax
00032F2C  6690              xchg ax,ax
00032F2E  6690              xchg ax,ax
00032F30  89E0              mov eax,esp
00032F32  83C004            add eax,byte +0x4
00032F35  C3                ret
00032F36  55                push ebp
00032F37  89E5              mov ebp,esp
00032F39  8B5508            mov edx,[ebp+0x8]
00032F3C  EC                in al,dx
00032F3D  5D                pop ebp
00032F3E  C20400            ret 0x4
00032F41  55                push ebp
00032F42  89E5              mov ebp,esp
00032F44  50                push eax
00032F45  53                push ebx
00032F46  51                push ecx
00032F47  52                push edx
00032F48  8B4508            mov eax,[ebp+0x8]
00032F4B  8B00              mov eax,[eax]
00032F4D  8B5D0C            mov ebx,[ebp+0xc]
00032F50  8B1B              mov ebx,[ebx]
00032F52  8B4D10            mov ecx,[ebp+0x10]
00032F55  8B09              mov ecx,[ecx]
00032F57  8B5514            mov edx,[ebp+0x14]
00032F5A  8B12              mov edx,[edx]
00032F5C  0FA2              cpuid
00032F5E  53                push ebx
00032F5F  89C3              mov ebx,eax
00032F61  8B4508            mov eax,[ebp+0x8]
00032F64  8918              mov [eax],ebx
00032F66  5B                pop ebx
00032F67  8B450C            mov eax,[ebp+0xc]
00032F6A  8918              mov [eax],ebx
00032F6C  8B4510            mov eax,[ebp+0x10]
00032F6F  8908              mov [eax],ecx
00032F71  8B4514            mov eax,[ebp+0x14]
00032F74  8910              mov [eax],edx
00032F76  5A                pop edx
00032F77  59                pop ecx
00032F78  5B                pop ebx
00032F79  58                pop eax
00032F7A  89EC              mov esp,ebp
00032F7C  5D                pop ebp
00032F7D  C21000            ret 0x10
00032F80  55                push ebp
00032F81  89E5              mov ebp,esp
00032F83  53                push ebx
00032F84  8B5D08            mov ebx,[ebp+0x8]
00032F87  60                pushad
00032F88  31C0              xor eax,eax
00032F8A  0FA2              cpuid
00032F8C  61                popad
00032F8D  833B00            cmp dword [ebx],byte +0x0
00032F90  7404              jz 0x32f96
00032F92  F390              pause
00032F94  EBF1              jmp short 0x32f87
00032F96  B801000000        mov eax,0x1
00032F9B  8703              xchg eax,[ebx]
00032F9D  83F800            cmp eax,byte +0x0
00032FA0  75E5              jnz 0x32f87
00032FA2  5B                pop ebx
00032FA3  5D                pop ebp
00032FA4  C20400            ret 0x4
00032FA7  55                push ebp
00032FA8  89E5              mov ebp,esp
00032FAA  8B5508            mov edx,[ebp+0x8]
00032FAD  8B450C            mov eax,[ebp+0xc]
00032FB0  EE                out dx,al
00032FB1  5D                pop ebp
00032FB2  C20800            ret 0x8
00032FB5  60                pushad
00032FB6  9C                pushfd
00032FB7  0F20C0            mov eax,cr0
00032FBA  50                push eax
00032FBB  B800000000        mov eax,0x0
00032FC0  50                push eax
00032FC1  0F20D0            mov eax,cr2
00032FC4  50                push eax
00032FC5  0F20D8            mov eax,cr3
00032FC8  50                push eax
00032FC9  83EC08            sub esp,byte +0x8
00032FCC  0F010424          sgdt [esp]
00032FD0  8B442402          mov eax,[esp+0x2]
00032FD4  83C408            add esp,byte +0x8
00032FD7  50                push eax
00032FD8  83EC08            sub esp,byte +0x8
00032FDB  0F010C24          sidt [esp]
00032FDF  8B442402          mov eax,[esp+0x2]
00032FE3  83C408            add esp,byte +0x8
00032FE6  50                push eax
00032FE7  54                push esp
00032FE8  E8B3FEFFFF        call dword 0x32ea0
00032FED  83C440            add esp,byte +0x40
00032FF0  C3                ret
00032FF1  0000              add [eax],al
00032FF3  000A              add [edx],cl
00032FF5  0A2D2D2D2D2D      or ch,[dword 0x2d2d2d2d]
00032FFB  2D2D2D2D2D        sub eax,0x2d2d2d2d
00033000  2D2D2D2D2D        sub eax,0x2d2d2d2d
00033005  2D2D2D2D2D        sub eax,0x2d2d2d2d
0003300A  2D2D2D2D2D        sub eax,0x2d2d2d2d
0003300F  2D2D2D2D2D        sub eax,0x2d2d2d2d
00033014  2D2D0A0D00        sub eax,0xd0a2d
00033019  0000              add [eax],al
0003301B  005765            add [edi+0x65],dl
0003301E  6C                insb
0003301F  636F6D            arpl [edi+0x6d],bp
00033022  6520746F20        and [gs:edi+ebp*2+0x20],dh
00033027  44                inc esp
00033028  61                popad
00033029  726B              jc 0x33096
0003302B  204279            and [edx+0x79],al
0003302E  7465              jz 0x33095
00033030  27                daa
00033031  7320              jnc 0x33053
00033033  766D              jna 0x330a2
00033035  6C                insb
00033036  6F                outsd
00033037  61                popad
00033038  6465720A          gs jc 0x33046
0003303C  0D0000002D        or eax,0x2d000000
00033041  2D2D2D2D2D        sub eax,0x2d2d2d2d
00033046  2D2D2D2D2D        sub eax,0x2d2d2d2d
0003304B  2D2D2D2D2D        sub eax,0x2d2d2d2d
00033050  2D2D2D2D2D        sub eax,0x2d2d2d2d
00033055  2D2D2D2D2D        sub eax,0x2d2d2d2d
0003305A  2D2D2D2D2D        sub eax,0x2d2d2d2d
0003305F  2D0A0D0061        sub eax,0x61000d0a
00033064  3D25380A0D        cmp eax,0xd0a3825
00033069  00623D            add [edx+0x3d],ah
0003306C  25380A0D00        and eax,0xd0a38
00033071  0000              add [eax],al
00033073  005F76            add [edi+0x76],bl
00033076  6D                insd
00033077  6C                insb
00033078  6F                outsd
00033079  61                popad
0003307A  6465725F          gs jc 0x330dd
0003307E  6D                insd
0003307F  61                popad
00033080  696E20676F7420    imul ebp,[esi+0x20],dword 0x20746f67
00033087  6C                insb
00033088  6F                outsd
00033089  61                popad
0003308A  646564206174      and [fs:ecx+0x74],ah
00033090  206164            and [ecx+0x64],ah
00033093  647265            fs jc 0x330fb
00033096  7373              jnc 0x3310b
00033098  2025380A0D00      and [dword 0xd0a38],ah
0003309E  0000              add [eax],al
000330A0  7265              jc 0x33107
000330A2  7365              jnc 0x33109
000330A4  7276              jc 0x3311c
000330A6  65646D            fs insd
000330A9  656D              gs insd
000330AB  5F                pop edi
000330AC  6C                insb
000330AD  697374636F756E    imul esi,[ebx+0x74],dword 0x6e756f63
000330B4  743D              jz 0x330f3
000330B6  2564202861        and eax,0x61282064
000330BB  64647265          fs jc 0x33124
000330BF  7373              jnc 0x33134
000330C1  206F66            and [edi+0x66],ch
000330C4  206974            and [ecx+0x74],ch
000330C7  203D20253820      and [dword 0x20382520],bh
000330CD  2920              sub [eax],esp
000330CF  0A0D00000047      or cl,[dword 0x47000000]
000330D5  6F                outsd
000330D6  696E6720746F20    imul ebp,[esi+0x67],dword 0x206f7420
000330DD  7265              jc 0x33144
000330DF  61                popad
000330E0  6420746865        and [fs:eax+ebp*2+0x65],dh
000330E5  20564D            and [esi+0x4d],dl
000330E8  4D                dec ebp
000330E9  20696E            and [ecx+0x6e],ch
000330EC  746F              jz 0x3315d
000330EE  206D65            and [ebp+0x65],ch
000330F1  6D                insd
000330F2  6F                outsd
000330F3  7279              jc 0x3316e
000330F5  2E2E2E0A0D006973  or cl,[dword cs:0x41736900]
         -41
000330FE  50                push eax
000330FF  207661            and [esi+0x61],dh
00033102  6C                insb
00033103  7565              jnz 0x3316a
00033105  3D25322020        cmp eax,0x20203225
0003310A  286164            sub [ecx+0x64],ah
0003310D  647265            fs jc 0x33175
00033110  7373              jnc 0x33185
00033112  3D2538290A        cmp eax,0xa293825
00033117  0D00000000        or eax,0x0
0003311C  C9                leave
0003311D  CDCD              int 0xcd
0003311F  CDCD              int 0xcd
00033121  CDCD              int 0xcd
00033123  CDCD              int 0xcd
00033125  CDCD              int 0xcd
00033127  CDCD              int 0xcd
00033129  CDCD              int 0xcd
0003312B  CDCD              int 0xcd
0003312D  CDCD              int 0xcd
0003312F  CDCD              int 0xcd
00033131  CDCD              int 0xcd
00033133  CDCD              int 0xcd
00033135  CDCD              int 0xcd
00033137  CDCD              int 0xcd
00033139  CDCD              int 0xcd
0003313B  CDCD              int 0xcd
0003313D  CDCD              int 0xcd
0003313F  CDCD              int 0xcd
00033141  CDCD              int 0xcd
00033143  CDCD              int 0xcd
00033145  CDCD              int 0xcd
00033147  CDCD              int 0xcd
00033149  CDCD              int 0xcd
0003314B  CDCD              int 0xcd
0003314D  CDCD              int 0xcd
0003314F  CDCD              int 0xcd
00033151  CDCD              int 0xcd
00033153  CDCD              int 0xcd
00033155  CDCD              int 0xcd
00033157  CDCD              int 0xcd
00033159  CDCD              int 0xcd
0003315B  CDCD              int 0xcd
0003315D  CDCD              int 0xcd
0003315F  CDCD              int 0xcd
00033161  CDCD              int 0xcd
00033163  CDCD              int 0xcd
00033165  CDCD              int 0xcd
00033167  CDCD              int 0xcd
00033169  CDBB              int 0xbb
0003316B  00BA20202020      add [edx+0x20202020],bh
00033171  2020              and [eax],ah
00033173  2020              and [eax],ah
00033175  2020              and [eax],ah
00033177  2020              and [eax],ah
00033179  2020              and [eax],ah
0003317B  2020              and [eax],ah
0003317D  2020              and [eax],ah
0003317F  2020              and [eax],ah
00033181  2020              and [eax],ah
00033183  2020              and [eax],ah
00033185  2020              and [eax],ah
00033187  2020              and [eax],ah
00033189  2020              and [eax],ah
0003318B  2020              and [eax],ah
0003318D  2020              and [eax],ah
0003318F  2020              and [eax],ah
00033191  2020              and [eax],ah
00033193  2020              and [eax],ah
00033195  2020              and [eax],ah
00033197  2020              and [eax],ah
00033199  2020              and [eax],ah
0003319B  2020              and [eax],ah
0003319D  2020              and [eax],ah
0003319F  2020              and [eax],ah
000331A1  2020              and [eax],ah
000331A3  2020              and [eax],ah
000331A5  2020              and [eax],ah
000331A7  2020              and [eax],ah
000331A9  2020              and [eax],ah
000331AB  2020              and [eax],ah
000331AD  2020              and [eax],ah
000331AF  2020              and [eax],ah
000331B1  2020              and [eax],ah
000331B3  2020              and [eax],ah
000331B5  2020              and [eax],ah
000331B7  2020              and [eax],ah
000331B9  20BA00C8CDCD      and [edx-0x32323800],bh
000331BF  CDCD              int 0xcd
000331C1  CDCD              int 0xcd
000331C3  CDCD              int 0xcd
000331C5  CDCD              int 0xcd
000331C7  CDCD              int 0xcd
000331C9  CDCD              int 0xcd
000331CB  CDCD              int 0xcd
000331CD  CDCD              int 0xcd
000331CF  CDCD              int 0xcd
000331D1  CDCD              int 0xcd
000331D3  CDCD              int 0xcd
000331D5  CDCD              int 0xcd
000331D7  CDCD              int 0xcd
000331D9  CDCD              int 0xcd
000331DB  CDCD              int 0xcd
000331DD  CDCD              int 0xcd
000331DF  CDCD              int 0xcd
000331E1  CDCD              int 0xcd
000331E3  CDCD              int 0xcd
000331E5  CDCD              int 0xcd
000331E7  CDCD              int 0xcd
000331E9  CDCD              int 0xcd
000331EB  CDCD              int 0xcd
000331ED  CDCD              int 0xcd
000331EF  CDCD              int 0xcd
000331F1  CDCD              int 0xcd
000331F3  CDCD              int 0xcd
000331F5  CDCD              int 0xcd
000331F7  CDCD              int 0xcd
000331F9  CDCD              int 0xcd
000331FB  CDCD              int 0xcd
000331FD  CDCD              int 0xcd
000331FF  CDCD              int 0xcd
00033201  CDCD              int 0xcd
00033203  CDCD              int 0xcd
00033205  CDCD              int 0xcd
00033207  CDCD              int 0xcd
00033209  CDBC              int 0xbc
0003320B  00546869          add [eax+ebp*2+0x69],dl
0003320F  7320              jnc 0x33231
00033211  746F              jz 0x33282
00033213  6F                outsd
00033214  6C                insb
00033215  206F72            and [edi+0x72],ch
00033218  6967696E617465    imul esp,[edi+0x69],dword 0x6574616e
0003321F  64206174          and [fs:ecx+0x74],ah
00033223  007777            add [edi+0x77],dh
00033226  772E              ja 0x33256
00033228  636865            arpl [eax+0x65],bp
0003322B  61                popad
0003322C  7465              jz 0x33293
0003322E  6E                outsb
0003322F  67696E652E6F7267  imul ebp,[bp+0x65],dword 0x67726f2e
00033237  006132            add [ecx+0x32],ah
0003323A  307374            xor [ebx+0x74],dh
0003323D  61                popad
0003323E  7465              jz 0x332a5
00033240  3D25642028        cmp eax,0x28206425
00033245  7368              jnc 0x332af
00033247  6F                outsd
00033248  756C              jnz 0x332b6
0003324A  64206265          and [fs:edx+0x65],ah
0003324E  2031              and [ecx],dh
00033250  290A              sub [edx],ecx
00033252  00626F            add [edx+0x6f],ah
00033255  6F                outsd
00033256  7464              jz 0x332bc
00033258  7269              jc 0x332c3
0003325A  7665              jna 0x332c1
0003325C  3D25322028        cmp eax,0x28203225
00033261  7374              jnc 0x332d7
00033263  6F                outsd
00033264  7265              jc 0x332cb
00033266  64206174          and [fs:ecx+0x74],ah
0003326A  202570290A00      and [dword 0xa2970],ah
00033270  53                push ebx
00033271  6563746F72        arpl [gs:edi+ebp*2+0x72],si
00033276  7350              jnc 0x332c8
00033278  657254            gs jc 0x332cf
0003327B  7261              jc 0x332de
0003327D  636B3D            arpl [ebx+0x3d],bp
00033280  25640A004E        and eax,0x4e000a64
00033285  756D              jnz 0x332f4
00033287  626572            bound esp,[ebp+0x72]
0003328A  4F                dec edi
0003328B  6648              dec ax
0003328D  6561              gs popad
0003328F  64733D            fs jnc 0x332cf
00033292  25640A004E        and eax,0x4e000a64
00033297  756D              jnz 0x33306
00033299  626572            bound esp,[ebp+0x72]
0003329C  4F                dec edi
0003329D  6643              inc bx
0003329F  796C              jns 0x3330d
000332A1  696E646572733D    imul ebp,[esi+0x64],dword 0x3d737265
000332A8  25640A0026        and eax,0x26000a64
000332AD  626F6F            bound ebp,[edi+0x6f]
000332B0  7473              jz 0x33325
000332B2  6563746F72        arpl [gs:edi+ebp*2+0x72],si
000332B7  3D25700A00        cmp eax,0xa7025
000332BC  54                push esp
000332BD  657374            gs jnc 0x33334
000332C0  696E6720646973    imul ebp,[esi+0x67],dword 0x73696420
000332C7  6B726561          imul esi,[edx+0x65],byte +0x61
000332CB  64206162          and [fs:ecx+0x62],ah
000332CF  696C6974792E2E2E  imul ebp,[ecx+ebp*2+0x74],dword 0x2e2e2e79
000332D7  2E0000            add [cs:eax],al
000332DA  0000              add [eax],al
000332DC  45                inc ebp
000332DD  7272              jc 0x33351
000332DF  6F                outsd
000332E0  7220              jc 0x33302
000332E2  6C                insb
000332E3  6F                outsd
000332E4  61                popad
000332E5  64696E6720746865  imul ebp,[fs:esi+0x67],dword 0x65687420
000332ED  20626F            and [edx+0x6f],ah
000332F0  6F                outsd
000332F1  7473              jz 0x33366
000332F3  6563746F72        arpl [gs:edi+ebp*2+0x72],si
000332F8  2E0A00            or al,[cs:eax]
000332FB  005375            add [ebx+0x75],dl
000332FE  636365            arpl [ebx+0x65],sp
00033301  7373              jnc 0x33376
00033303  66756C            o16 jnz 0x33372
00033306  6C                insb
00033307  7920              jns 0x33329
00033309  6C                insb
0003330A  6F                outsd
0003330B  61                popad
0003330C  64656420746865    and [fs:eax+ebp*2+0x65],dh
00033313  20626F            and [edx+0x6f],ah
00033316  6F                outsd
00033317  7473              jz 0x3338c
00033319  6563746F72        arpl [gs:edi+ebp*2+0x72],si
0003331E  0A00              or al,[eax]
00033320  56                push esi
00033321  4D                dec ebp
00033322  4D                dec ebp
00033323  207374            and [ebx+0x74],dh
00033326  61                popad
00033327  7274              jc 0x3339d
00033329  7320              jnc 0x3334b
0003332B  61                popad
0003332C  7420              jz 0x3334e
0003332E  7365              jnc 0x33395
00033330  63746F72          arpl [edi+ebp*2+0x72],si
00033334  2025640A0D00      and [dword 0xd0a64],ah
0003333A  4C                dec esp
0003333B  4F                dec edi
0003333C  47                inc edi
0003333D  4F                dec edi
0003333E  207374            and [ebx+0x74],dh
00033341  61                popad
00033342  7274              jc 0x333b8
00033344  7320              jnc 0x33366
00033346  61                popad
00033347  7420              jz 0x33369
00033349  7365              jnc 0x333b0
0003334B  63746F72          arpl [edi+ebp*2+0x72],si
0003334F  2025640A0D00      and [dword 0xd0a64],ah
00033355  56                push esi
00033356  4D                dec ebp
00033357  4D                dec ebp
00033358  207374            and [ebx+0x74],dh
0003335B  61                popad
0003335C  7274              jc 0x333d2
0003335E  7320              jnc 0x33380
00033360  61                popad
00033361  7420              jz 0x33383
00033363  7365              jnc 0x333ca
00033365  63746F72          arpl [edi+ebp*2+0x72],si
00033369  2025640A0000      and [dword 0xa64],ah
0003336F  00693D            add [ecx+0x3d],ch
00033372  2564203A20        and eax,0x203a2064
00033377  42                inc edx
00033378  61                popad
00033379  7365              jnc 0x333e0
0003337B  41                inc ecx
0003337C  64647265          fs jc 0x333e5
00033380  7373              jnc 0x333f5
00033382  3D25362C20        cmp eax,0x202c3625
00033387  4C                dec esp
00033388  656E              gs outsb
0003338A  677468            jz 0x333f5
0003338D  3D25362C20        cmp eax,0x202c3625
00033392  54                push esp
00033393  7970              jns 0x33405
00033395  653D2564200A      gs cmp eax,0xa206425
0003339B  0D00000000        or eax,0x0
000333A0  46                inc esi
000333A1  61                popad
000333A2  696C75726520696E  imul ebp,[ebp+esi*2+0x72],dword 0x6e692065
000333AA  207069            and [eax+0x69],dh
000333AD  636B69            arpl [ebx+0x69],bp
000333B0  6E                outsb
000333B1  67206120          and [bx+di+0x20],ah
000333B5  66697474696E67    imul si,[esp+esi*2+0x69],word 0x676e
000333BC  207265            and [edx+0x65],dh
000333BF  67696F6E0A000000  imul ebp,[bx+0x6e],dword 0xa
000333C7  004E6F            add [esi+0x6f],cl
000333CA  7420              jz 0x333ec
000333CC  656E              gs outsb
000333CE  6F                outsd
000333CF  7567              jnz 0x33438
000333D1  6820757361        push dword 0x61737520
000333D6  626C6520          bound ebp,[ebp+0x20]
000333DA  6D                insd
000333DB  656D              gs insd
000333DD  6F                outsd
000333DE  7279              jc 0x33459
000333E0  2028              and [eax],ch
000333E2  61                popad
000333E3  7420              jz 0x33405
000333E5  656E              gs outsb
000333E7  64290A            sub [fs:edx],ecx
000333EA  0D004D6178        or eax,0x78614d00
000333EF  206164            and [ecx+0x64],ah
000333F2  647265            fs jc 0x3345a
000333F5  7373              jnc 0x3346a
000333F7  3D25780A0D        cmp eax,0xd0a7825
000333FC  0031              add [ecx],dh
000333FE  3A7374            cmp dh,[ebx+0x74]
00033401  61                popad
00033402  7274              jc 0x33478
00033404  3D25380A0D        cmp eax,0xd0a3825
00033409  0000              add [eax],al
0003340B  0031              add [ecx],dh
0003340D  2E353A737461      cs xor eax,0x6174733a
00033413  7274              jc 0x33489
00033415  206D6F            and [ebp+0x6f],ch
00033418  642030            and [fs:eax],dh
0003341B  7830              js 0x3344d
0003341D  303430            xor [eax+esi],dh
00033420  3030              xor [eax],dh
00033422  3030              xor [eax],dh
00033424  3D2538200A        cmp eax,0xa203825
00033429  0D00323A73        or eax,0x733a3200
0003342E  7461              jz 0x33491
00033430  7274              jc 0x334a6
00033432  3D25380A0D        cmp eax,0xd0a3825
00033437  006368            add [ebx+0x68],ah
0003343A  6F                outsd
0003343B  7365              jnc 0x334a2
0003343D  6E                outsb
0003343E  7265              jc 0x334a5
00033440  67696F6E3D25640A  imul ebp,[bx+0x6e],dword 0xa64253d
00033448  0D00000041        or eax,0x41000000
0003344D  646A75            fs push byte +0x75
00033450  7374              jnc 0x334c6
00033452  696E67206D656D    imul ebp,[esi+0x67],dword 0x6d656d20
00033459  6F                outsd
0003345A  7279              jc 0x334d5
0003345C  206D61            and [ebp+0x61],ch
0003345F  7020              jo 0x33481
00033461  28646F6E          sub [edi+ebp*2+0x6e],ah
00033465  6520666F          and [gs:esi+0x6f],ah
00033469  7220              jc 0x3348b
0003346B  7068              jo 0x334d5
0003346D  7973              jns 0x334e2
0003346F  6963616C206D65    imul esp,[ebx+0x61],dword 0x656d206c
00033476  6D                insd
00033477  6F                outsd
00033478  7279              jc 0x334f3
0003347A  206163            and [ecx+0x63],ah
0003347D  636573            arpl [ebp+0x73],sp
00033480  7320              jnc 0x334a2
00033482  64657669          gs jna 0x334ef
00033486  636573            arpl [ebp+0x73],sp
00033489  290A              sub [edx],ecx
0003348B  0D00000000        or eax,0x0
00033490  41                inc ecx
00033491  646A75            fs push byte +0x75
00033494  7374              jnc 0x3350a
00033496  6564207265        and [fs:edx+0x65],dh
0003349B  7365              jnc 0x33502
0003349D  7276              jc 0x33515
0003349F  6564206D65        and [fs:ebp+0x65],ch
000334A4  6D                insd
000334A5  6F                outsd
000334A6  7279              jc 0x33521
000334A8  207369            and [ebx+0x69],dh
000334AB  7A65              jpe 0x33512
000334AD  206173            and [ecx+0x73],ah
000334B0  207765            and [edi+0x65],dh
000334B3  6C                insb
000334B4  6C                insb
000334B5  0A0D00696E73      or cl,[dword 0x736e6900]
000334BB  657274            gs jc 0x33532
000334BE  6564206E65        and [fs:esi+0x65],ch
000334C3  7720              ja 0x334e5
000334C5  7265              jc 0x3352c
000334C7  67696F6E0A0D006E  imul ebp,[bx+0x6e],dword 0x6e000d0a
000334CF  65776D            gs ja 0x3353f
000334D2  61                popad
000334D3  703D              jo 0x33512
000334D5  0A0D00726573      or cl,[dword 0x73657200]
000334DB  657276            gs jc 0x33554
000334DE  65646D            fs insd
000334E1  656D              gs insd
000334E3  5F                pop edi
000334E4  6C                insb
000334E5  697374636F756E    imul esi,[ebx+0x74],dword 0x6e756f63
000334EC  743D              jz 0x3352b
000334EE  25640A0000        and eax,0xa64
000334F3  00476F            add [edi+0x6f],al
000334F6  696E6720746F20    imul ebp,[esi+0x67],dword 0x206f7420
000334FD  7A65              jpe 0x33564
000334FF  726F              jc 0x33570
00033501  2030              and [eax],dh
00033503  7830              js 0x33535
00033505  303430            xor [eax+esi],dh
00033508  3030              xor [eax],dh
0003350A  3030              xor [eax],dh
0003350C  206279            and [edx+0x79],ah
0003350F  7465              jz 0x33576
00033511  7320              jnc 0x33533
00033513  61                popad
00033514  7420              jz 0x33536
00033516  25700A007A        and eax,0x7a000a70
0003351B  65726F            gs jc 0x3358d
0003351E  27                daa
0003351F  6420746865        and [fs:eax+ebp*2+0x65],dh
00033524  206279            and [edx+0x79],ah
00033527  7465              jz 0x3358e
00033529  730A              jnc 0x33535
0003352B  005265            add [edx+0x65],dl
0003352E  61                popad
0003352F  64696E6720256420  imul ebp,[fs:esi+0x67],dword 0x20642520
00033537  627974            bound edi,[ecx+0x74]
0003353A  657320            gs jnc 0x3355d
0003353D  66726F            o16 jc 0x335af
00033540  6D                insd
00033541  20646973          and [ecx+ebp*2+0x73],ah
00033545  6B2069            imul esp,[eax],byte +0x69
00033548  6E                outsb
00033549  746F              jz 0x335ba
0003354B  202538202E2E      and [dword 0x2e2e2038],ah
00033551  2E00564D          add [cs:esi+0x4d],dl
00033555  4D                dec ebp
00033556  6C                insb
00033557  6F                outsd
00033558  636174            arpl [ecx+0x74],sp
0003355B  696F6E3D25380A    imul ebp,[edi+0x6e],dword 0xa38253d
00033562  007374            add [ebx+0x74],dh
00033565  61                popad
00033566  7274              jc 0x335dc
00033568  3D25380A00        cmp eax,0xa3825
0003356D  56                push esi
0003356E  4D                dec ebp
0003356F  4D                dec ebp
00033570  53                push ebx
00033571  49                dec ecx
00033572  5A                pop edx
00033573  45                inc ebp
00033574  3D25380A00        cmp eax,0xa3825
00033579  2B00              sub eax,[eax]
0003357B  52                push edx
0003357C  6561              gs popad
0003357E  64206572          and [fs:ebp+0x72],ah
00033582  726F              jc 0x335f3
00033584  7220              jc 0x335a6
00033586  61                popad
00033587  667465            o16 jz 0x335ef
0003358A  7220              jc 0x335ac
0003358C  2564206279        and eax,0x79622064
00033591  7465              jz 0x335f8
00033593  730A              jnc 0x3359f
00033595  004552            add [ebp+0x52],al
00033598  52                push edx
00033599  4F                dec edi
0003359A  52                push edx
0003359B  210A              and [edx],ecx
0003359D  000A              add [edx],cl
0003359F  005265            add [edx+0x65],dl
000335A2  61                popad
000335A3  64207375          and [fs:ebx+0x75],dh
000335A7  636365            arpl [ebx+0x65],sp
000335AA  7373              jnc 0x3361f
000335AC  66756C            o16 jnz 0x3361b
000335AF  0A00              or al,[eax]
000335B1  67647462          fs jz 0x33617
000335B5  61                popad
000335B6  7365              jnc 0x3361d
000335B8  3D25380A0D        cmp eax,0xd0a3825
000335BD  007061            add [eax+0x61],dh
000335C0  6765646972707472  imul esi,[fs:bp+si+0x70],dword 0x61627274
         -6261
000335CA  7365              jnc 0x33631
000335CC  3D25380A0D        cmp eax,0xd0a3825
000335D1  005061            add [eax+0x61],dl
000335D4  676544            gs inc esp
000335D7  69725074725B30    imul esi,[edx+0x50],dword 0x305b7274
000335DE  5D                pop ebp
000335DF  2E50              cs push eax
000335E1  46                inc esi
000335E2  4E                dec esi
000335E3  3D25380A0D        cmp eax,0xd0a3825
000335E8  000A              add [edx],cl
000335EA  0D0A0D2538        or eax,0x38250d0a
000335EF  3A0A              cmp cl,[edx]
000335F1  0D00253820        or eax,0x20382500
000335F6  3A20              cmp ah,[eax]
000335F8  00253220000A      add [dword 0xa002032],ah
000335FE  0D00766D6C        or eax,0x6c6d7600
00033603  6F                outsd
00033604  61                popad
00033605  64657220          gs jc 0x33629
00033609  66696E697368      imul bp,[esi+0x69],word 0x6873
0003360F  65642E205377      and [cs:ebx+0x77],dl
00033615  69746368696E6720  imul esi,[ebx+0x68],dword 0x20676e69
0003361D  66726F            o16 jc 0x3368f
00033620  6D                insd
00033621  2033              and [ebx],dh
00033623  322D62697420      xor ch,[dword 0x20746962]
00033629  746F              jz 0x3369a
0003362B  2036              and [esi],dh
0003362D  342D              xor al,0x2d
0003362F  626974            bound ebp,[ecx+0x74]
00033632  20616E            and [ecx+0x6e],ah
00033635  6420656E          and [fs:ebp+0x6e],ah
00033639  7465              jz 0x336a0
0003363B  7269              jc 0x336a6
0003363D  6E                outsb
0003363E  6720564D          and [bp+0x4d],dl
00033642  4D                dec ebp
00033643  0A00              or al,[eax]
00033645  0000              add [eax],al
00033647  00546869          add [eax+ebp*2+0x69],dl
0003364B  7320              jnc 0x3366d
0003364D  69732061204170    imul esi,[ebx+0x20],dword 0x70412061
00033654  706C              jo 0x336c2
00033656  69636174696F6E    imul esp,[ebx+0x61],dword 0x6e6f6974
0003365D  205072            and [eax+0x72],dl
00033660  6F                outsd
00033661  636573            arpl [ebp+0x73],sp
00033664  736F              jnc 0x336d5
00033666  720A              jc 0x33672
00033668  0D00000049        or eax,0x49000000
0003366D  206D61            and [ebp+0x61],ch
00033670  6E                outsb
00033671  61                popad
00033672  67656420746F      and [fs:si+0x6f],dh
00033678  206765            and [edi+0x65],ah
0003367B  7420              jz 0x3369d
0003367D  746F              jz 0x336ee
0003367F  20746865          and [eax+ebp*2+0x65],dh
00033683  20656E            and [ebp+0x6e],ah
00033686  64206F66          and [fs:edi+0x66],ch
0003368A  20746865          and [eax+ebp*2+0x65],dh
0003368E  206675            and [esi+0x75],ah
00033691  6E                outsb
00033692  6374696F          arpl [ecx+ebp*2+0x6f],si
00033696  6E                outsb
00033697  2E205945          and [cs:ecx+0x45],bl
0003369B  45                inc ebp
0003369C  45                inc ebp
0003369D  48                dec eax
0003369E  2121              and [ecx],esp
000336A0  210A              and [edx],ecx
000336A2  0D00554E44        or eax,0x444e5500
000336A7  45                inc ebp
000336A8  46                inc esi
000336A9  49                dec ecx
000336AA  4E                dec esi
000336AB  45                inc ebp
000336AC  44                inc esp
000336AD  205641            and [esi+0x41],dl
000336B0  52                push edx
000336B1  4C                dec esp
000336B2  49                dec ecx
000336B3  53                push ebx
000336B4  54                push esp
000336B5  006964            add [ecx+0x64],ch
000336B8  7462              jz 0x3371c
000336BA  61                popad
000336BB  7365              jnc 0x33722
000336BD  3D25382067        cmp eax,0x67203825
000336C2  647462            fs jz 0x33727
000336C5  61                popad
000336C6  7365              jnc 0x3372d
000336C8  3D25380A0D        cmp eax,0xd0a3825
000336CD  006372            add [ebx+0x72],ah
000336D0  333D25382063      xor edi,[dword 0x63203825]
000336D6  7232              jc 0x3370a
000336D8  3D25382063        cmp eax,0x63203825
000336DD  7231              jc 0x33710
000336DF  3D25382063        cmp eax,0x63203825
000336E4  7230              jc 0x33716
000336E6  3D25380A0D        cmp eax,0xd0a3825
000336EB  006566            add [ebp+0x66],ah
000336EE  6C                insb
000336EF  61                popad
000336F0  67733D            jnc 0x33730
000336F3  25380A0D00        and eax,0xd0a38
000336F8  1400              adc al,0x0
000336FA  0000              add [eax],al
000336FC  0000              add [eax],al
000336FE  0000              add [eax],al
00033700  017A52            add [edx+0x52],edi
00033703  0001              add [ecx],al
00033705  7C08              jl 0x3370f
00033707  011B              add [ebx],ebx
00033709  0C04              or al,0x4
0003370B  0488              add al,0x88
0003370D  0100              add [eax],eax
0003370F  001C00            add [eax+eax],bl
00033712  0000              add [eax],al
00033714  1C00              sbb al,0x0
00033716  0000              add [eax],al
00033718  7CCF              jl 0x336e9
0003371A  FF                db 0xff
0003371B  FFE6              jmp esi
0003371D  0000              add [eax],al
0003371F  0000              add [eax],al
00033721  41                inc ecx
00033722  0E                push cs
00033723  088502420D05      or [ebp+0x50d4202],al
00033729  02E2              add ah,dl
0003372B  C50C04            lds ecx,[esp+eax]
0003372E  0400              add al,0x0
00033730  2400              and al,0x0
00033732  0000              add [eax],al
00033734  3C00              cmp al,0x0
00033736  0000              add [eax],al
00033738  42                inc edx
00033739  D0FF              sar bh,1
0003373B  FF21              jmp dword [ecx]
0003373D  1100              adc [eax],eax
0003373F  0000              add [eax],al
00033741  41                inc ecx
00033742  0E                push cs
00033743  088502420D05      or [ebp+0x50d4202],al
00033749  47                inc edi
0003374A  830303            add dword [ebx],byte +0x3
0003374D  1511C341C5        adc eax,0xc541c311
00033752  0C04              or al,0x4
00033754  0400              add al,0x0
00033756  0000              add [eax],al
00033758  1400              adc al,0x0
0003375A  0000              add [eax],al
0003375C  0000              add [eax],al
0003375E  0000              add [eax],al
00033760  017A52            add [edx+0x52],edi
00033763  0001              add [ecx],al
00033765  7C08              jl 0x3376f
00033767  011B              add [ebx],ebx
00033769  0C04              or al,0x4
0003376B  0488              add al,0x88
0003376D  0100              add [eax],eax
0003376F  0028              add [eax],ch
00033771  0000              add [eax],al
00033773  001C00            add [eax+eax],bl
00033776  0000              add [eax],al
00033778  24E1              and al,0xe1
0003377A  FF                db 0xff
0003377B  FF8608000000      inc dword [esi+0x8]
00033781  41                inc ecx
00033782  0E                push cs
00033783  088502420D05      or [ebp+0x50d4202],al
00033789  48                dec eax
0003378A  8603              xchg al,[ebx]
0003378C  83040378          add dword [ebx+eax],byte +0x78
00033790  08C3              or bl,al
00033792  41                inc ecx
00033793  C641C50C          mov byte [ecx-0x3b],0xc
00033797  0404              add al,0x4
00033799  0000              add [eax],al
0003379B  001C00            add [eax+eax],bl
0003379E  0000              add [eax],al
000337A0  48                dec eax
000337A1  0000              add [eax],al
000337A3  007EE9            add [esi-0x17],bh
000337A6  FF                db 0xff
000337A7  FF5200            call dword [edx+0x0]
000337AA  0000              add [eax],al
000337AC  00410E            add [ecx+0xe],al
000337AF  088502420D05      or [ebp+0x50d4202],al
000337B5  024EC5            add cl,[esi-0x3b]
000337B8  0C04              or al,0x4
000337BA  0400              add al,0x0
000337BC  1C00              sbb al,0x0
000337BE  0000              add [eax],al
000337C0  68000000B0        push dword 0xb0000000
000337C5  E9FFFF9C00        jmp dword 0xa037c9
000337CA  0000              add [eax],al
000337CC  00410E            add [ecx+0xe],al
000337CF  088502420D05      or [ebp+0x50d4202],al
000337D5  0298C50C0404      add bl,[eax+0x4040cc5]
000337DB  001C00            add [eax+eax],bl
000337DE  0000              add [eax],al
000337E0  8800              mov [eax],al
000337E2  0000              add [eax],al
000337E4  2CEA              sub al,0xea
000337E6  FF                db 0xff
000337E7  FF                db 0xff
000337E8  3B00              cmp eax,[eax]
000337EA  0000              add [eax],al
000337EC  00410E            add [ecx+0xe],al
000337EF  088502420D05      or [ebp+0x50d4202],al
000337F5  77C5              ja 0x337bc
000337F7  0C04              or al,0x4
000337F9  0400              add al,0x0
000337FB  001C00            add [eax+eax],bl
000337FE  0000              add [eax],al
00033800  A800              test al,0x0
00033802  0000              add [eax],al
00033804  47                inc edi
00033805  EAFFFF56000000    jmp dword 0x0:0x56ffff
0003380C  00410E            add [ecx+0xe],al
0003380F  088502420D05      or [ebp+0x50d4202],al
00033815  0252C5            add dl,[edx-0x3b]
00033818  0C04              or al,0x4
0003381A  0400              add al,0x0
0003381C  1C00              sbb al,0x0
0003381E  0000              add [eax],al
00033820  C8000000          enter 0x0,0x0
00033824  7DEA              jnl 0x33810
00033826  FF                db 0xff
00033827  FF5300            call dword [ebx+0x0]
0003382A  0000              add [eax],al
0003382C  00410E            add [ecx+0xe],al
0003382F  088502420D05      or [ebp+0x50d4202],al
00033835  024FC5            add cl,[edi-0x3b]
00033838  0C04              or al,0x4
0003383A  0400              add al,0x0
0003383C  1C00              sbb al,0x0
0003383E  0000              add [eax],al
00033840  E8000000B0        call dword 0xb0033845
00033845  EAFFFF2E000000    jmp dword 0x0:0x2effff
0003384C  00410E            add [ecx+0xe],al
0003384F  088502420D05      or [ebp+0x50d4202],al
00033855  6AC5              push byte -0x3b
00033857  0C04              or al,0x4
00033859  0400              add al,0x0
0003385B  001C00            add [eax+eax],bl
0003385E  0000              add [eax],al
00033860  0801              or [ecx],al
00033862  0000              add [eax],al
00033864  BEEAFFFF3E        mov esi,0x3effffea
00033869  0000              add [eax],al
0003386B  0000              add [eax],al
0003386D  41                inc ecx
0003386E  0E                push cs
0003386F  088502420D05      or [ebp+0x50d4202],al
00033875  7AC5              jpe 0x3383c
00033877  0C04              or al,0x4
00033879  0400              add al,0x0
0003387B  001C00            add [eax+eax],bl
0003387E  0000              add [eax],al
00033880  2801              sub [ecx],al
00033882  0000              add [eax],al
00033884  DCEA              fsub to st2
00033886  FF                db 0xff
00033887  FF2E              jmp dword far [esi]
00033889  0000              add [eax],al
0003388B  0000              add [eax],al
0003388D  41                inc ecx
0003388E  0E                push cs
0003388F  088502420D05      or [ebp+0x50d4202],al
00033895  6AC5              push byte -0x3b
00033897  0C04              or al,0x4
00033899  0400              add al,0x0
0003389B  001C00            add [eax+eax],bl
0003389E  0000              add [eax],al
000338A0  48                dec eax
000338A1  0100              add [eax],eax
000338A3  00EA              add dl,ch
000338A5  EAFFFF59000000    jmp dword 0x0:0x59ffff
000338AC  00410E            add [ecx+0xe],al
000338AF  088502420D05      or [ebp+0x50d4202],al
000338B5  0255C5            add dl,[ebp-0x3b]
000338B8  0C04              or al,0x4
000338BA  0400              add al,0x0
000338BC  1C00              sbb al,0x0
000338BE  0000              add [eax],al
000338C0  6801000023        push dword 0x23000001
000338C5  EBFF              jmp short 0x338c6
000338C7  FF4E00            dec dword [esi+0x0]
000338CA  0000              add [eax],al
000338CC  00410E            add [ecx+0xe],al
000338CF  088502420D05      or [ebp+0x50d4202],al
000338D5  024AC5            add cl,[edx-0x3b]
000338D8  0C04              or al,0x4
000338DA  0400              add al,0x0
000338DC  2400              and al,0x0
000338DE  0000              add [eax],al
000338E0  8801              mov [ecx],al
000338E2  0000              add [eax],al
000338E4  51                push ecx
000338E5  EBFF              jmp short 0x338e6
000338E7  FFE6              jmp esi
000338E9  0000              add [eax],al
000338EB  0000              add [eax],al
000338ED  41                inc ecx
000338EE  0E                push cs
000338EF  088502420D05      or [ebp+0x50d4202],al
000338F5  45                inc ebp
000338F6  8603              xchg al,[ebx]
000338F8  830402DB          add dword [edx+eax],byte -0x25
000338FC  C3                ret
000338FD  41                inc ecx
000338FE  C641C50C          mov byte [ecx-0x3b],0xc
00033902  0404              add al,0x4
00033904  1C00              sbb al,0x0
00033906  0000              add [eax],al
00033908  B001              mov al,0x1
0003390A  0000              add [eax],al
0003390C  0FECFF            paddsb mm7,mm7
0003390F  FF                db 0xff
00033910  3E0000            add [ds:eax],al
00033913  0000              add [eax],al
00033915  41                inc ecx
00033916  0E                push cs
00033917  088502420D05      or [ebp+0x50d4202],al
0003391D  7AC5              jpe 0x338e4
0003391F  0C04              or al,0x4
00033921  0400              add al,0x0
00033923  002400            add [eax+eax],ah
00033926  0000              add [eax],al
00033928  D001              rol byte [ecx],1
0003392A  0000              add [eax],al
0003392C  2DECFFFF0A        sub eax,0xaffffec
00033931  0200              add al,[eax]
00033933  0000              add [eax],al
00033935  41                inc ecx
00033936  0E                push cs
00033937  088502420D05      or [ebp+0x50d4202],al
0003393D  44                inc esp
0003393E  830303            add dword [ebx],byte +0x3
00033941  0102              add [edx],eax
00033943  C3                ret
00033944  41                inc ecx
00033945  C50C04            lds ecx,[esp+eax]
00033948  0400              add al,0x0
0003394A  0000              add [eax],al
0003394C  2000              and [eax],al
0003394E  0000              add [eax],al
00033950  F8                clc
00033951  0100              add [eax],eax
00033953  000F              add [edi],cl
00033955  EE                out dx,al
00033956  FF                db 0xff
00033957  FF4A01            dec dword [edx+0x1]
0003395A  0000              add [eax],al
0003395C  00410E            add [ecx+0xe],al
0003395F  088502420D05      or [ebp+0x50d4202],al
00033965  44                inc esp
00033966  830303            add dword [ebx],byte +0x3
00033969  42                inc edx
0003396A  01C5              add ebp,eax
0003396C  C3                ret
0003396D  0C04              or al,0x4
0003396F  041C              add al,0x1c
00033971  0000              add [eax],al
00033973  001C02            add [edx+eax],bl
00033976  0000              add [eax],al
00033978  35EFFFFF0F        xor eax,0xfffffef
0003397D  0000              add [eax],al
0003397F  0000              add [eax],al
00033981  41                inc ecx
00033982  0E                push cs
00033983  088502420D05      or [ebp+0x50d4202],al
00033989  4B                dec ebx
0003398A  C50C04            lds ecx,[esp+eax]
0003398D  0400              add al,0x0
0003398F  001C00            add [eax+eax],bl
00033992  0000              add [eax],al
00033994  3C02              cmp al,0x2
00033996  0000              add [eax],al
00033998  24EF              and al,0xef
0003399A  FF                db 0xff
0003399B  FF2F              jmp dword far [edi]
0003399D  0000              add [eax],al
0003399F  0000              add [eax],al
000339A1  41                inc ecx
000339A2  0E                push cs
000339A3  088502420D05      or [ebp+0x50d4202],al
000339A9  6BC50C            imul eax,ebp,byte +0xc
000339AC  0404              add al,0x4
000339AE  0000              add [eax],al
000339B0  1C00              sbb al,0x0
000339B2  0000              add [eax],al
000339B4  5C                pop esp
000339B5  0200              add al,[eax]
000339B7  0033              add [ebx],dh
000339B9  EF                out dx,eax
000339BA  FF                db 0xff
000339BB  FF0A              dec dword [edx]
000339BD  0000              add [eax],al
000339BF  0000              add [eax],al
000339C1  41                inc ecx
000339C2  0E                push cs
000339C3  088502420D05      or [ebp+0x50d4202],al
000339C9  46                inc esi
000339CA  C50C04            lds ecx,[esp+eax]
000339CD  0400              add al,0x0
000339CF  001C00            add [eax+eax],bl
000339D2  0000              add [eax],al
000339D4  7C02              jl 0x339d8
000339D6  0000              add [eax],al
000339D8  1DEFFFFF20        sbb eax,0x20ffffef
000339DD  0000              add [eax],al
000339DF  0000              add [eax],al
000339E1  41                inc ecx
000339E2  0E                push cs
000339E3  088502420D05      or [ebp+0x50d4202],al
000339E9  5C                pop esp
000339EA  C50C04            lds ecx,[esp+eax]
000339ED  0400              add al,0x0
000339EF  0020              add [eax],ah
000339F1  0000              add [eax],al
000339F3  009C0200001DEF    add [edx+eax-0x10e30000],bl
000339FA  FF                db 0xff
000339FB  FF                db 0xff
000339FC  E800000000        call dword 0x33a01
00033A01  41                inc ecx
00033A02  0E                push cs
00033A03  088502420D05      or [ebp+0x50d4202],al
00033A09  44                inc esp
00033A0A  830302            add dword [ebx],byte +0x2
00033A0D  DFC3              ffreep st3
00033A0F  41                inc ecx
00033A10  C50C04            lds ecx,[esp+eax]
00033A13  041C              add al,0x1c
00033A15  0000              add [eax],al
00033A17  00C0              add al,al
00033A19  0200              add al,[eax]
00033A1B  00E1              add cl,ah
00033A1D  EF                out dx,eax
00033A1E  FF                db 0xff
00033A1F  FFB100000000      push dword [ecx+0x0]
00033A25  41                inc ecx
00033A26  0E                push cs
00033A27  088502420D05      or [ebp+0x50d4202],al
00033A2D  02ADC50C0404      add ch,[ebp+0x4040cc5]
00033A33  001C00            add [eax+eax],bl
00033A36  0000              add [eax],al
00033A38  E002              loopne 0x33a3c
00033A3A  0000              add [eax],al
00033A3C  72F0              jc 0x33a2e
00033A3E  FF                db 0xff
00033A3F  FF3400            push dword [eax+eax]
00033A42  0000              add [eax],al
00033A44  00410E            add [ecx+0xe],al
00033A47  088502420D05      or [ebp+0x50d4202],al
00033A4D  70C5              jo 0x33a14
00033A4F  0C04              or al,0x4
00033A51  0400              add al,0x0
00033A53  0028              add [eax],ch
00033A55  0000              add [eax],al
00033A57  0000              add [eax],al
00033A59  0300              add eax,[eax]
00033A5B  0086F0FFFFAF      add [esi-0x50000010],al
00033A61  0000              add [eax],al
00033A63  0000              add [eax],al
00033A65  41                inc ecx
00033A66  0E                push cs
00033A67  088502420D05      or [ebp+0x50d4202],al
00033A6D  46                inc esi
00033A6E  8703              xchg eax,[ebx]
00033A70  860483            xchg al,[ebx+eax*4]
00033A73  0502A2C341        add eax,0x41c3a202
00033A78  C641C741          mov byte [ecx-0x39],0x41
00033A7C  C50C04            lds ecx,[esp+eax]
00033A7F  041C              add al,0x1c
00033A81  0000              add [eax],al
00033A83  002C03            add [ebx+eax],ch
00033A86  0000              add [eax],al
00033A88  09F1              or ecx,esi
00033A8A  FF                db 0xff
00033A8B  FFC9              dec ecx
00033A8D  0000              add [eax],al
00033A8F  0000              add [eax],al
00033A91  41                inc ecx
00033A92  0E                push cs
00033A93  088502420D05      or [ebp+0x50d4202],al
00033A99  02C5              add al,ch
00033A9B  C50C04            lds ecx,[esp+eax]
00033A9E  0400              add al,0x0
00033AA0  1C00              sbb al,0x0
00033AA2  0000              add [eax],al
00033AA4  4C                dec esp
00033AA5  0300              add eax,[eax]
00033AA7  00B2F1FFFF37      add [edx+0x37fffff1],dh
00033AAD  0000              add [eax],al
00033AAF  0000              add [eax],al
00033AB1  41                inc ecx
00033AB2  0E                push cs
00033AB3  088502420D05      or [ebp+0x50d4202],al
00033AB9  73C5              jnc 0x33a80
00033ABB  0C04              or al,0x4
00033ABD  0400              add al,0x0
00033ABF  001C00            add [eax+eax],bl
00033AC2  0000              add [eax],al
00033AC4  6C                insb
00033AC5  0300              add eax,[eax]
00033AC7  00C9              add cl,cl
00033AC9  F1                int1
00033ACA  FF                db 0xff
00033ACB  FF8700000000      inc dword [edi+0x0]
00033AD1  41                inc ecx
00033AD2  0E                push cs
00033AD3  088502420D05      or [ebp+0x50d4202],al
00033AD9  0283C50C0404      add al,[ebx+0x4040cc5]
00033ADF  002400            add [eax+eax],ah
00033AE2  0000              add [eax],al
00033AE4  8C03              mov [ebx],es
00033AE6  0000              add [eax],al
00033AE8  30F2              xor dl,dh
00033AEA  FF                db 0xff
00033AEB  FF4601            inc dword [esi+0x1]
00033AEE  0000              add [eax],al
00033AF0  00410E            add [ecx+0xe],al
00033AF3  088502420D05      or [ebp+0x50d4202],al
00033AF9  47                inc edi
00033AFA  830303            add dword [ebx],byte +0x3
00033AFD  3A01              cmp al,[ecx]
00033AFF  C3                ret
00033B00  41                inc ecx
00033B01  C50C04            lds ecx,[esp+eax]
00033B04  0400              add al,0x0
00033B06  0000              add [eax],al
00033B08  1C00              sbb al,0x0
00033B0A  0000              add [eax],al
00033B0C  B403              mov ah,0x3
00033B0E  0000              add [eax],al
00033B10  4E                dec esi
00033B11  F3                rep
00033B12  FF                db 0xff
00033B13  FF4200            inc dword [edx+0x0]
00033B16  0000              add [eax],al
00033B18  00410E            add [ecx+0xe],al
00033B1B  088502420D05      or [ebp+0x50d4202],al
00033B21  7EC5              jng 0x33ae8
00033B23  0C04              or al,0x4
00033B25  0400              add al,0x0
00033B27  0020              add [eax],ah
00033B29  0000              add [eax],al
00033B2B  00D4              add ah,dl
00033B2D  0300              add eax,[eax]
00033B2F  0070F3            add [eax-0xd],dh
00033B32  FF                db 0xff
00033B33  FF8200000000      inc dword [edx+0x0]
00033B39  41                inc ecx
00033B3A  0E                push cs
00033B3B  088502420D05      or [ebp+0x50d4202],al
00033B41  44                inc esp
00033B42  830302            add dword [ebx],byte +0x2
00033B45  79C3              jns 0x33b0a
00033B47  41                inc ecx
00033B48  C50C04            lds ecx,[esp+eax]
00033B4B  0400              add al,0x0
00033B4D  0000              add [eax],al
00033B4F  0000              add [eax],al
00033B51  0000              add [eax],al
00033B53  0000              add [eax],al
00033B55  0000              add [eax],al
00033B57  0000              add [eax],al
00033B59  0000              add [eax],al
00033B5B  0000              add [eax],al
00033B5D  0000              add [eax],al
00033B5F  0000              add [eax],al
00033B61  0000              add [eax],al
00033B63  0000              add [eax],al
00033B65  0000              add [eax],al
00033B67  0000              add [eax],al
00033B69  0000              add [eax],al
00033B6B  0000              add [eax],al
00033B6D  0000              add [eax],al
00033B6F  0000              add [eax],al
00033B71  0000              add [eax],al
00033B73  0000              add [eax],al
00033B75  0000              add [eax],al
00033B77  0000              add [eax],al
00033B79  0000              add [eax],al
00033B7B  0000              add [eax],al
00033B7D  0000              add [eax],al
00033B7F  0000              add [eax],al
00033B81  0000              add [eax],al
00033B83  0000              add [eax],al
00033B85  0000              add [eax],al
00033B87  0000              add [eax],al
00033B89  0000              add [eax],al
00033B8B  0000              add [eax],al
00033B8D  0000              add [eax],al
00033B8F  0000              add [eax],al
00033B91  0000              add [eax],al
00033B93  0000              add [eax],al
00033B95  0000              add [eax],al
00033B97  0000              add [eax],al
00033B99  0000              add [eax],al
00033B9B  0000              add [eax],al
00033B9D  0000              add [eax],al
00033B9F  0000              add [eax],al
00033BA1  0000              add [eax],al
00033BA3  0000              add [eax],al
00033BA5  0000              add [eax],al
00033BA7  0000              add [eax],al
00033BA9  0000              add [eax],al
00033BAB  0000              add [eax],al
00033BAD  0000              add [eax],al
00033BAF  0000              add [eax],al
00033BB1  0000              add [eax],al
00033BB3  0000              add [eax],al
00033BB5  0000              add [eax],al
00033BB7  0000              add [eax],al
00033BB9  0000              add [eax],al
00033BBB  0000              add [eax],al
00033BBD  0000              add [eax],al
00033BBF  0000              add [eax],al
00033BC1  0000              add [eax],al
00033BC3  0000              add [eax],al
00033BC5  0000              add [eax],al
00033BC7  0000              add [eax],al
00033BC9  0000              add [eax],al
00033BCB  0000              add [eax],al
00033BCD  0000              add [eax],al
00033BCF  0000              add [eax],al
00033BD1  0000              add [eax],al
00033BD3  0000              add [eax],al
00033BD5  0000              add [eax],al
00033BD7  0000              add [eax],al
00033BD9  0000              add [eax],al
00033BDB  0000              add [eax],al
00033BDD  0000              add [eax],al
00033BDF  0000              add [eax],al
00033BE1  0000              add [eax],al
00033BE3  0000              add [eax],al
00033BE5  0000              add [eax],al
00033BE7  0000              add [eax],al
00033BE9  0000              add [eax],al
00033BEB  0000              add [eax],al
00033BED  0000              add [eax],al
00033BEF  0000              add [eax],al
00033BF1  0000              add [eax],al
00033BF3  0000              add [eax],al
00033BF5  0000              add [eax],al
00033BF7  0000              add [eax],al
00033BF9  0000              add [eax],al
00033BFB  0000              add [eax],al
00033BFD  0000              add [eax],al
00033BFF  0000              add [eax],al
00033C01  0000              add [eax],al
00033C03  0000              add [eax],al
00033C05  0000              add [eax],al
00033C07  0000              add [eax],al
00033C09  0000              add [eax],al
00033C0B  0000              add [eax],al
00033C0D  0000              add [eax],al
00033C0F  0000              add [eax],al
00033C11  0000              add [eax],al
00033C13  0000              add [eax],al
00033C15  0000              add [eax],al
00033C17  0000              add [eax],al
00033C19  0000              add [eax],al
00033C1B  0000              add [eax],al
00033C1D  0000              add [eax],al
00033C1F  0000              add [eax],al
00033C21  0000              add [eax],al
00033C23  0000              add [eax],al
00033C25  0000              add [eax],al
00033C27  0000              add [eax],al
00033C29  0000              add [eax],al
00033C2B  0000              add [eax],al
00033C2D  0000              add [eax],al
00033C2F  0000              add [eax],al
00033C31  0000              add [eax],al
00033C33  0000              add [eax],al
00033C35  0000              add [eax],al
00033C37  0000              add [eax],al
00033C39  0000              add [eax],al
00033C3B  0000              add [eax],al
00033C3D  0000              add [eax],al
00033C3F  0000              add [eax],al
00033C41  0000              add [eax],al
00033C43  0000              add [eax],al
00033C45  0000              add [eax],al
00033C47  0000              add [eax],al
00033C49  0000              add [eax],al
00033C4B  0000              add [eax],al
00033C4D  0000              add [eax],al
00033C4F  0000              add [eax],al
00033C51  0000              add [eax],al
00033C53  0000              add [eax],al
00033C55  0000              add [eax],al
00033C57  0000              add [eax],al
00033C59  0000              add [eax],al
00033C5B  0000              add [eax],al
00033C5D  0000              add [eax],al
00033C5F  0000              add [eax],al
00033C61  0000              add [eax],al
00033C63  0000              add [eax],al
00033C65  0000              add [eax],al
00033C67  0000              add [eax],al
00033C69  0000              add [eax],al
00033C6B  0000              add [eax],al
00033C6D  0000              add [eax],al
00033C6F  0000              add [eax],al
00033C71  0000              add [eax],al
00033C73  0000              add [eax],al
00033C75  0000              add [eax],al
00033C77  0000              add [eax],al
00033C79  0000              add [eax],al
00033C7B  0000              add [eax],al
00033C7D  0000              add [eax],al
00033C7F  0000              add [eax],al
00033C81  0000              add [eax],al
00033C83  0000              add [eax],al
00033C85  0000              add [eax],al
00033C87  0000              add [eax],al
00033C89  0000              add [eax],al
00033C8B  0000              add [eax],al
00033C8D  0000              add [eax],al
00033C8F  0000              add [eax],al
00033C91  0000              add [eax],al
00033C93  0000              add [eax],al
00033C95  0000              add [eax],al
00033C97  0000              add [eax],al
00033C99  0000              add [eax],al
00033C9B  0000              add [eax],al
00033C9D  0000              add [eax],al
00033C9F  0000              add [eax],al
00033CA1  0000              add [eax],al
00033CA3  0000              add [eax],al
00033CA5  0000              add [eax],al
00033CA7  00                db 0x00
