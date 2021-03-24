/*
 * symbols.c
 *
 *  Created on: Aug 7, 2013
 *      Author: eric
 */

#include <stdio.h>
#include <elf.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stddef.h>
#include <unistd.h>
#include <string.h>
#include <zlib.h>

#pragma pack(1)
typedef struct
{
  uint64_t address;
  int size;
  int type;
  unsigned char namelength;
  char name[0];
} symbolinfo, *psymbolinfo;
#pragma pack()

#define TEMPBUFSIZE 64*1024


void loadStringTable64(int f, Elf64_Shdr *sectionHeaders, unsigned char **stringTable, int index)
{
  if ((stringTable[index]==NULL) && (sectionHeaders[index].sh_type==SHT_STRTAB))
  {
    stringTable[index]=malloc(sectionHeaders[index].sh_size);
    if (pread(f, stringTable[index], sectionHeaders[index].sh_size, sectionHeaders[index].sh_offset)==-1)
    {
      debug_log("Failure loading the stringtable\n");
      free(stringTable[index]);
      stringTable[index]=NULL;
    }

  }
  else
    debug_log("Not a string table\n");
}

void loadStringTable32(int f, Elf32_Shdr *sectionHeaders, unsigned char **stringTable, int index)
{
  if ((stringTable[index]==NULL) && (sectionHeaders[index].sh_type==SHT_STRTAB))
  {
    stringTable[index]=malloc(sectionHeaders[index].sh_size);
    if (pread(f, stringTable[index], sectionHeaders[index].sh_size, sectionHeaders[index].sh_offset)==-1)
    {
      debug_log("Failure loading the stringtable\n");
      free(stringTable[index]);
      stringTable[index]=NULL;
    }

  }
  else
    debug_log("Not a string table\n");
}

int ELF32(int f, Elf32_Ehdr *b, unsigned char **output)
/*
Caller must free output manually
*/
{
  int i,j;

  unsigned char *tempbuffer=NULL;
  int tempbufferpos=0;
  int maxoutputsize=TEMPBUFSIZE;
  tempbuffer=malloc(TEMPBUFSIZE);

  //setup zlib
  z_stream strm;
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  deflateInit(&strm, 9);

  *output=malloc(maxoutputsize); //allocate 256KB. This "should" be enough, but reallocate if more is needed

  strm.avail_out=maxoutputsize-3*sizeof(uint32_t); //first if it's an exe, followed by the compressed size, followed by the decompressed size
  strm.next_out=(unsigned char *)&(*output)[sizeof(uint32_t)*3];

  *(uint32_t *)(&(*output)[0])=(b->e_type==ET_EXEC);

/*

  debug_log("e_shoff=%x\n", b->e_shoff);
  debug_log("e_shentsize=%d\n", b->e_shentsize);
  debug_log("e_shnum=%d\n", b->e_shnum);
  debug_log("e_shstrndx=%d\n", b->e_shstrndx);*/

  Elf32_Shdr *sectionHeaders=malloc(b->e_shentsize*b->e_shnum);

  if (pread(f, sectionHeaders, b->e_shentsize*b->e_shnum, b->e_shoff)==-1)
  {
    //printf("Failure to read sectionHeaders\n");
    deflateEnd(&strm);
    if (sectionHeaders)
      free(sectionHeaders);

    if (*output)
    {
      free(*output);
      *output=NULL;
    }

    if (tempbuffer)
      free(tempbuffer);

    return -1;
  }

  unsigned char **stringTable=calloc(b->e_shnum, sizeof(unsigned char*) );

  loadStringTable32(f, sectionHeaders, stringTable, b->e_shstrndx);


  for (i=0; i<b->e_shnum; i++)
  {
    //printf("Section %d (%x): name=%s\n", i, sectionHeaders[i].sh_addr, &stringTable[b->e_shstrndx][sectionHeaders[i].sh_name]);

    if ((sectionHeaders[i].sh_type==SHT_SYMTAB) || (sectionHeaders[i].sh_type==SHT_DYNSYM))
    {
     // debug_log("Symbol data:\n", i);

     // debug_log("sh_addr=%x\n", sectionHeaders[i].sh_addr);
      //printf("sh_offset=%x\n", sectionHeaders[i].sh_offset);
      //printf("sh_size=%x\n", sectionHeaders[i].sh_size);
      //printf("sh_link=%d (string table)\n", sectionHeaders[i].sh_link);
      //printf("sh_info=%d\n", sectionHeaders[i].sh_info);

      Elf32_Sym *symbolTable=malloc(sectionHeaders[i].sh_size);
      if (pread(f, symbolTable, sectionHeaders[i].sh_size, sectionHeaders[i].sh_offset)==-1)
      {
       // debug_log("Failure reading symbol table\n");
        return -1;
      }
      int maxindex=sectionHeaders[i].sh_size / sizeof(Elf32_Sym);

      loadStringTable32(f, sectionHeaders, stringTable, sectionHeaders[i].sh_link);

      //printf("maxindex=%d\n", maxindex);
      for (j=0; j<maxindex; j++)
      {
        //printf("symbolTable[%d]:\n", i);
        //printf("st_name=%s\n", &stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name] );
       // debug_log("st_value=%x\n", symbolTable[j].st_value);
        //printf("st_size=%d\n", symbolTable[j].st_size);
       // debug_log("st_info=%d\n", symbolTable[j].st_info);
        //printf("  Bind=%d\n", ELF32_ST_BIND(symbolTable[j].st_info));
        //printf("  Type=%d\n", ELF32_ST_TYPE(symbolTable[j].st_info));
       // debug_log("st_other=%d\n", symbolTable[j].st_other);
       // debug_log("st_shndx=%d\n", symbolTable[j].st_shndx);

        if (symbolTable[j].st_value)
        {
          //add it to the tempbuffer
          char *symbolname=(char *)&stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name];
          size_t namelength=strlen(symbolname);
          int entrysize=sizeof(symbolinfo)+namelength;
          if (tempbufferpos+entrysize>=TEMPBUFSIZE)
          {
             //compress the current temp buffer
             //printf("compressing\n");
             strm.avail_in=tempbufferpos;
             strm.next_in=tempbuffer;

             while (strm.avail_in)
             {
               if (deflate(&strm, Z_NO_FLUSH)!=Z_OK)
               {
                 //printf("FAILURE TO COMPRESS!\n");
                 return -1;
               }
               //printf("strm.avail_out=%d\n", strm.avail_out);

               if (strm.avail_out==0)
               {


                 //printf("Out buffer full. Reallocating\n");
                 *output=realloc(*output, maxoutputsize*2);

                 strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
                 strm.avail_out=maxoutputsize;
                 maxoutputsize=maxoutputsize*2;


               }

             }
             tempbufferpos=0;
          }



          psymbolinfo si=(psymbolinfo)&tempbuffer[tempbufferpos];
          si->address=symbolTable[j].st_value;
          si->size=symbolTable[j].st_size;
          si->type=symbolTable[j].st_info;
          si->namelength=namelength;
          memcpy(&si->name, symbolname, namelength);


          tempbufferpos+=entrysize;
        }
      }

      free(symbolTable);

    }

  }

  for (i=0; i<b->e_shnum; i++)
  {
    if (stringTable[i])
      free(stringTable[i]);
  }
  free(stringTable);

  free(sectionHeaders);


  debug_log("end:\n");
  strm.avail_in=tempbufferpos;
  strm.next_in=tempbuffer;

  while (1)
  {

    i=deflate(&strm, Z_FINISH);
    debug_log("i=%d\n", i);
    if (i==Z_STREAM_END) //done
      break;

    if (i!=Z_OK)
    {
      debug_log("Failure to compress: %i\n", i);
      return -1;
    }

    if (strm.avail_out==0)
    {
      debug_log("Out buffer full. Reallocating :%d\n", maxoutputsize*2);
      *output=realloc(*output, maxoutputsize*2);

      strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
      strm.avail_out=maxoutputsize;
      maxoutputsize=maxoutputsize*2;

    }
    else
      break;

  };

  /*printf("strm.avail_out=%d\n", strm.avail_out);

  debug_log("total_in = %lu\n", strm.total_in);
  debug_log("total_out = %lu\n", strm.total_out);*/

  deflateEnd(&strm);


  //update the size
  *(uint32_t *)(&(*output)[4])=strm.total_out+3*sizeof(uint32_t);
  *(uint32_t *)(&(*output)[8])=strm.total_in;

  free(tempbuffer);

  return 0;
}

int ELF64(int f, Elf64_Ehdr *b, unsigned char **output)
/*
Caller must free output manually
*/
{
  int i,j;


  unsigned char *tempbuffer=NULL;
  int tempbufferpos=0;
  int maxoutputsize=TEMPBUFSIZE;
  tempbuffer=malloc(TEMPBUFSIZE);

  //setup zlib
  z_stream strm;
  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;
  deflateInit(&strm, 9);

  *output=malloc(maxoutputsize); //allocate 256KB. This "should" be enough, but reallocate if more is needed

  strm.avail_out=maxoutputsize-3*sizeof(uint32_t); //the first 8 bytes will contain the compressed and uncompressed size
  strm.next_out=(unsigned char *)&(*output)[sizeof(uint32_t)*3];

  *(uint32_t *)(&(*output)[0])=(b->e_type==ET_EXEC);
/*

  debug_log("e_shoff=%lx\n", b->e_shoff);
  debug_log("e_shentsize=%d\n", b->e_shentsize);
  debug_log("e_shnum=%d\n", b->e_shnum);
  debug_log("e_shstrndx=%d\n", b->e_shstrndx);*/

  Elf64_Shdr *sectionHeaders=malloc(b->e_shentsize*b->e_shnum);

  if (pread(f, sectionHeaders, b->e_shentsize*b->e_shnum, b->e_shoff)==-1)
  {
    //printf("Failure to read sectionHeaders\n");
    deflateEnd(&strm);
    if (sectionHeaders)
      free(sectionHeaders);

    if (*output)
    {
      free(*output);
      *output=NULL;
    }

    if (tempbuffer)
      free(tempbuffer);

    return -1;
  }

  unsigned char **stringTable=calloc(b->e_shnum, sizeof(unsigned char*) );

  loadStringTable64(f, sectionHeaders, stringTable, b->e_shstrndx);


  for (i=0; i<b->e_shnum; i++)
  {
   // debug_log("Section %d (%lx): name=%s\n", i, sectionHeaders[i].sh_addr, &stringTable[b->e_shstrndx][sectionHeaders[i].sh_name]);

    if ((sectionHeaders[i].sh_type==SHT_SYMTAB) || (sectionHeaders[i].sh_type==SHT_DYNSYM))
    {/*
      debug_log("Symbol data:\n", i);

      debug_log("sh_addr=%lx\n", sectionHeaders[i].sh_addr);
      debug_log("sh_offset=%lx\n", sectionHeaders[i].sh_offset);
      debug_log("sh_size=%ld\n", sectionHeaders[i].sh_size);
      debug_log("sh_link=%d (string table)\n", sectionHeaders[i].sh_link);
      debug_log("sh_info=%d\n", sectionHeaders[i].sh_info);*/

      Elf64_Sym *symbolTable=malloc(sectionHeaders[i].sh_size);
      if (pread(f, symbolTable, sectionHeaders[i].sh_size, sectionHeaders[i].sh_offset)==-1)
      {
        //printf("Failure reading symbol table\n");
        return -1;
      }
      int maxindex=sectionHeaders[i].sh_size / sizeof(Elf64_Sym);

      loadStringTable64(f, sectionHeaders, stringTable, sectionHeaders[i].sh_link);

      //printf("maxindex=%d\n", maxindex);
      for (j=0; j<maxindex; j++)
      {
        /*
        debug_log("symbolTable[%d]:\n", i);
        debug_log("st_name=%s\n", &stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name] );
        debug_log("st_value=%lx\n", symbolTable[j].st_value);
        debug_log("st_size=%ld\n", symbolTable[j].st_size);
        debug_log("st_info=%d\n", symbolTable[j].st_info);
        debug_log("  Bind=%d\n", ELF64_ST_BIND(symbolTable[j].st_info));
        debug_log("  Type=%d\n", ELF64_ST_TYPE(symbolTable[j].st_info));
        debug_log("st_other=%d\n", symbolTable[j].st_other);
        debug_log("st_shndx=%d\n", symbolTable[j].st_shndx);*/

        if (symbolTable[j].st_value)
        {
          //add it to the tempbuffer
          char *symbolname=(char *)&stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name];
          size_t namelength=strlen(symbolname);
          int entrysize=sizeof(symbolinfo)+namelength;
          if (tempbufferpos+entrysize>=TEMPBUFSIZE)
          {
             //compress the current temp buffer
             //printf("compressing\n");
             strm.avail_in=tempbufferpos;
             strm.next_in=tempbuffer;

             while (strm.avail_in)
             {
               if (deflate(&strm, Z_NO_FLUSH)!=Z_OK)
               {
                 debug_log("FAILURE TO COMPRESS!\n");
                 return -1;
               }
               //printf("strm.avail_out=%d\n", strm.avail_out);

               if (strm.avail_out==0)
               {

                  // debug_log("Out buffer full. Reallocating\n");
                 *output=realloc(*output, maxoutputsize*2);

                 strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
                 strm.avail_out=maxoutputsize;
                 maxoutputsize=maxoutputsize*2;
               }

             }
             tempbufferpos=0;
          }



          psymbolinfo si=(psymbolinfo)&tempbuffer[tempbufferpos];
          si->address=symbolTable[j].st_value;
          si->size=symbolTable[j].st_size;
          si->type=symbolTable[j].st_info;
          si->namelength=namelength;
          memcpy(&si->name, symbolname, namelength);


          tempbufferpos+=entrysize;
        }
      }

      free(symbolTable);

    }

  }

  for (i=0; i<b->e_shnum; i++)
  {
    if (stringTable[i])
      free(stringTable[i]);
  }
  free(stringTable);

  free(sectionHeaders);


  debug_log("end:\n");
  strm.avail_in=tempbufferpos;
  strm.next_in=tempbuffer;

  while (1)
  {

    i=deflate(&strm, Z_FINISH);
    debug_log("i=%d\n", i);
    if (i==Z_STREAM_END) //done
      break;

    if (i!=Z_OK)
    {
      debug_log("Failure to compress: %i\n", i);
      return -1;
    }

    if (strm.avail_out==0)
    {
      debug_log("Out buffer full. Reallocating :%d\n", maxoutputsize*2);
      *output=realloc(*output, maxoutputsize*2);

      strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
      strm.avail_out=maxoutputsize;
      maxoutputsize=maxoutputsize*2;
    }
    else
      break;

  };

  debug_log("strm.avail_out=%d\n", strm.avail_out);

  debug_log("total_in = %lu\n", strm.total_in);
  debug_log("total_out = %lu\n", strm.total_out);

  deflateEnd(&strm);


  *(uint32_t *)(&(*output)[4])=strm.total_out+3*sizeof(uint32_t);
  *(uint32_t *)(&(*output)[8])=strm.total_in;


  free(tempbuffer);

  return 0; //still alive

}

int GetSymbolListFromFile(char *filename, unsigned char **output, int *outputsize)
/*
 * Returns a pointer to a compressed stream. The caller needs to free it
 */
{
  int i, f;
  unsigned char *b=NULL;

  debug_log("GetSymbolListFromFile(%s)\n", filename);

  *output=NULL;
  f=open(filename, O_RDONLY);
  if (f==-1)
    return -1;

  b=malloc(sizeof(Elf64_Ehdr));
  if (b)
  {
    i=pread(f, b, sizeof(Elf64_Ehdr), 0);

    if (*(uint32_t *)b!=0x464c457f)
      return -1; //not an ELF file

    if (b[EI_CLASS]==ELFCLASS32)
      i=ELF32(f, (Elf32_Ehdr *)b, output);
    else
      i=ELF64(f, (Elf64_Ehdr *)b, output);

    free(b);
  }

  close(f);

  return i;
}


int GetModuleSize32(int f, Elf32_Ehdr *b)
{
 /* debug_log("32 bit\n");
  debug_log("b->e_ehsize=%d  (%d)\n", (int)b->e_ehsize, (int)sizeof(Elf32_Ehdr));*/

  //Elf32_Shdr *sectionHeaders=malloc(b->e_shentsize*b->e_shnum);
  Elf32_Phdr *programHeaders=malloc(b->e_phentsize*b->e_phnum);
/*  debug_log("e_shoff=%x\n", b->e_shoff);
  debug_log("e_shentsize=%d\n", b->e_shentsize);
  debug_log("e_shnum=%d\n", b->e_shnum);
  debug_log("e_shstrndx=%d\n", b->e_shstrndx);

  debug_log("e_phoff=%x\n", b->e_phoff);
  debug_log("e_phentsize=%d\n", b->e_phentsize);
  debug_log("e_phnum=%d\n", b->e_phnum); */

  if (pread(f, programHeaders, b->e_phentsize*b->e_phnum, b->e_phoff)==-1)
  {
    if (programHeaders)
      free(programHeaders);

    return 0;
  }


  int i;
  unsigned long long lowest=0;//programHeaders[0].p_vaddr;
  unsigned long long highest=0;//programHeaders[0].p_vaddr+programHeaders[0].p_memsz;

  for (i=0; i<b->e_phnum; i++)
  {
     if (programHeaders[i].p_memsz>0)
     {
       if ((i==0) || (programHeaders[i].p_vaddr<lowest))
          lowest=programHeaders[i].p_vaddr;

       if ((i==0) || (programHeaders[i].p_vaddr+programHeaders[i].p_memsz>highest))
          highest=programHeaders[i].p_vaddr+programHeaders[i].p_memsz;


/*
       debug_log("%d: %x\n", i, programHeaders[i].p_type);
       debug_log("Virtual Address: %llx-%llx:\n", (long long unsigned int)programHeaders[i].p_vaddr,(long long unsigned int)programHeaders[i].p_vaddr+(long long unsigned int)programHeaders[i].p_memsz);
       debug_log("Size: %d (%x)\n", (int)programHeaders[i].p_memsz, (int)programHeaders[i].p_memsz);
       */
     }
  }

    if (programHeaders)
      free(programHeaders);

 // debug_log("lowest=%llx highest=%llx\n", lowest, highest);
 // debug_log("size=%llx\n", highest-lowest);
   return highest-lowest;
}

int GetModuleSize64(int f, Elf64_Ehdr *b)
{
  /*printf("64 bit\n");
  debug_log("b->e_ehsize=%d  (%d)\n", (int)b->e_ehsize, (int)sizeof(Elf32_Ehdr));*/

  Elf64_Phdr *programHeaders=malloc(b->e_phentsize*b->e_phnum);
/*  debug_log("e_shoff=%x\n", (int)b->e_shoff);
  debug_log("e_shentsize=%d\n", b->e_shentsize);
  debug_log("e_shnum=%d\n", b->e_shnum);
  debug_log("e_shstrndx=%d\n", b->e_shstrndx);

  debug_log("e_phoff=%x\n", (int)b->e_phoff);
  debug_log("e_phentsize=%d\n", b->e_phentsize);
  debug_log("e_phnum=%d\n", b->e_phnum);
  */

  if (pread(f, programHeaders, b->e_phentsize*b->e_phnum, b->e_phoff)==-1)
  {
    if (programHeaders)
      free(programHeaders);

    return 0;
  }


  int i;
  unsigned long long lowest=0;//programHeaders[0].p_vaddr;
  unsigned long long highest=0;//programHeaders[0].p_vaddr+programHeaders[0].p_memsz;

  for (i=0; i<b->e_phnum; i++)
  {
     if (programHeaders[i].p_memsz>0)
     {
       if ((i==0) || (programHeaders[i].p_vaddr<lowest))
          lowest=programHeaders[i].p_vaddr;

       if ((i==0) || (programHeaders[i].p_vaddr+programHeaders[i].p_memsz>highest))
          highest=programHeaders[i].p_vaddr+programHeaders[i].p_memsz;


/*
       debug_log("%d: %x\n", i, programHeaders[i].p_type);
       debug_log("Virtual Address: %llx-%llx:\n", (long long unsigned int)programHeaders[i].p_vaddr,(long long unsigned int)programHeaders[i].p_vaddr+(long long unsigned int)programHeaders[i].p_memsz);
       debug_log("Size: %d (%x)\n", (int)programHeaders[i].p_memsz, (int)programHeaders[i].p_memsz);
       */
     }
  }

    if (programHeaders)
      free(programHeaders);

 // debug_log("lowest=%llx highest=%llx\n", lowest, highest);
 // debug_log("size=%llx\n", highest-lowest);
   return highest-lowest;
}


unsigned long long GetModuleSize(char *filename, unsigned long long defaultsize)
/*
 * Returns size the module will take in memory
 */
{
  int i,f;
  unsigned char *b=NULL;
  int result=defaultsize;

//  debug_log("GetModuleSize(\"%s\")=",filename);

  f=open(filename, O_RDONLY);
  if (f==-1)
  {
    debug_log("Failed to open %s\n", filename);
    return defaultsize;
  }
  else
  {
    b=malloc(sizeof(Elf64_Ehdr));
    if (b)
    {
      i=pread(f, b, sizeof(Elf64_Ehdr), 0);

      if (*(uint32_t *)b!=0x464c457f)
      {
        debug_log("%s is not an elf\n", filename);
        free(b);
        close(f);
        return defaultsize; //not an ELF file
      }

      if (b[EI_CLASS]==ELFCLASS32)
        i=GetModuleSize32(f, (Elf32_Ehdr *)b);
      else
        i=GetModuleSize64(f, (Elf64_Ehdr *)b);

      free(b);
      close(f);

      //printf("%x\n",i);
      return i;
    }
    else
    {
      close(f);
      return defaultsize;
    }



  }


}
