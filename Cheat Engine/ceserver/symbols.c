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
      printf("Failure loading the stringtable\n");
      free(stringTable[index]);
      stringTable[index]=NULL;
    }

  }
  else
    printf("Not a string table\n");
}

void loadStringTable32(int f, Elf32_Shdr *sectionHeaders, unsigned char **stringTable, int index)
{
  if ((stringTable[index]==NULL) && (sectionHeaders[index].sh_type==SHT_STRTAB))
  {
    stringTable[index]=malloc(sectionHeaders[index].sh_size);
    if (pread(f, stringTable[index], sectionHeaders[index].sh_size, sectionHeaders[index].sh_offset)==-1)
    {
      printf("Failure loading the stringtable\n");
      free(stringTable[index]);
      stringTable[index]=NULL;
    }

  }
  else
    printf("Not a string table\n");
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

  strm.avail_out=maxoutputsize-2*sizeof(uint32_t); //the first 8 bytes will contain the compressed and uncompressed size
  strm.next_out=(unsigned char *)&(*output)[sizeof(uint32_t)*2];

/*

  printf("e_shoff=%x\n", b->e_shoff);
  printf("e_shentsize=%d\n", b->e_shentsize);
  printf("e_shnum=%d\n", b->e_shnum);
  printf("e_shstrndx=%d\n", b->e_shstrndx);*/

  Elf32_Shdr *sectionHeaders=malloc(b->e_shentsize*b->e_shnum);

  if (pread(f, sectionHeaders, b->e_shentsize*b->e_shnum, b->e_shoff)==-1)
  {
    //printf("Failure to read sectionHeaders\n");
    deflateEnd(&strm);
    free(sectionHeaders);
    free(output);
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
     // printf("Symbol data:\n", i);

     // printf("sh_addr=%x\n", sectionHeaders[i].sh_addr);
      //printf("sh_offset=%x\n", sectionHeaders[i].sh_offset);
      //printf("sh_size=%x\n", sectionHeaders[i].sh_size);
      //printf("sh_link=%d (string table)\n", sectionHeaders[i].sh_link);
      //printf("sh_info=%d\n", sectionHeaders[i].sh_info);

      Elf32_Sym *symbolTable=malloc(sectionHeaders[i].sh_size);
      if (pread(f, symbolTable, sectionHeaders[i].sh_size, sectionHeaders[i].sh_offset)==-1)
      {
       // printf("Failure reading symbol table\n");
        return -1;
      }
      int maxindex=sectionHeaders[i].sh_size / sizeof(Elf32_Sym);

      loadStringTable32(f, sectionHeaders, stringTable, sectionHeaders[i].sh_link);

      //printf("maxindex=%d\n", maxindex);
      for (j=0; j<maxindex; j++)
      {
        //printf("symbolTable[%d]:\n", i);
        //printf("st_name=%s\n", &stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name] );
       // printf("st_value=%x\n", symbolTable[j].st_value);
        //printf("st_size=%d\n", symbolTable[j].st_size);
       // printf("st_info=%d\n", symbolTable[j].st_info);
        //printf("  Bind=%d\n", ELF32_ST_BIND(symbolTable[j].st_info));
        //printf("  Type=%d\n", ELF32_ST_TYPE(symbolTable[j].st_info));
       // printf("st_other=%d\n", symbolTable[j].st_other);
       // printf("st_shndx=%d\n", symbolTable[j].st_shndx);

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


  printf("end:\n");
  strm.avail_in=tempbufferpos;
  strm.next_in=tempbuffer;

  while (1)
  {

    i=deflate(&strm, Z_FINISH);
    printf("i=%d\n", i);
    if (i==Z_STREAM_END) //done
      break;

    if (i!=Z_OK)
    {
      printf("Failure to compress: %i\n", i);
      return -1;
    }

    if (strm.avail_out==0)
    {
      printf("Out buffer full. Reallocating :%d\n", maxoutputsize*2);
      *output=realloc(*output, maxoutputsize*2);

      strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
      maxoutputsize=maxoutputsize*2;
      strm.avail_out=maxoutputsize/2;
    }
    else
      break;

  };

  /*printf("strm.avail_out=%d\n", strm.avail_out);

  printf("total_in = %lu\n", strm.total_in);
  printf("total_out = %lu\n", strm.total_out);*/

  deflateEnd(&strm);


  //update the size
  *(uint32_t *)(&(*output)[0])=strm.total_out+2*sizeof(uint32_t);
  *(uint32_t *)(&(*output)[4])=strm.total_in;

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

  strm.avail_out=maxoutputsize-2*sizeof(uint32_t); //the first 8 bytes will contain the compressed and uncompressed size
  strm.next_out=(unsigned char *)&(*output)[sizeof(uint32_t)*2];

/*

  printf("e_shoff=%lx\n", b->e_shoff);
  printf("e_shentsize=%d\n", b->e_shentsize);
  printf("e_shnum=%d\n", b->e_shnum);
  printf("e_shstrndx=%d\n", b->e_shstrndx);*/

  Elf64_Shdr *sectionHeaders=malloc(b->e_shentsize*b->e_shnum);

  if (pread(f, sectionHeaders, b->e_shentsize*b->e_shnum, b->e_shoff)==-1)
  {
    //printf("Failure to read sectionHeaders\n");
    deflateEnd(&strm);
    free(sectionHeaders);
    free(output);
    free(tempbuffer);

    return -1;
  }

  unsigned char **stringTable=calloc(b->e_shnum, sizeof(unsigned char*) );

  loadStringTable64(f, sectionHeaders, stringTable, b->e_shstrndx);


  for (i=0; i<b->e_shnum; i++)
  {
   // printf("Section %d (%lx): name=%s\n", i, sectionHeaders[i].sh_addr, &stringTable[b->e_shstrndx][sectionHeaders[i].sh_name]);

    if ((sectionHeaders[i].sh_type==SHT_SYMTAB) || (sectionHeaders[i].sh_type==SHT_DYNSYM))
    {/*
      printf("Symbol data:\n", i);

      printf("sh_addr=%lx\n", sectionHeaders[i].sh_addr);
      printf("sh_offset=%lx\n", sectionHeaders[i].sh_offset);
      printf("sh_size=%ld\n", sectionHeaders[i].sh_size);
      printf("sh_link=%d (string table)\n", sectionHeaders[i].sh_link);
      printf("sh_info=%d\n", sectionHeaders[i].sh_info);*/

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
        printf("symbolTable[%d]:\n", i);
        printf("st_name=%s\n", &stringTable[sectionHeaders[i].sh_link][symbolTable[j].st_name] );
        printf("st_value=%lx\n", symbolTable[j].st_value);
        printf("st_size=%ld\n", symbolTable[j].st_size);
        printf("st_info=%d\n", symbolTable[j].st_info);
        printf("  Bind=%d\n", ELF64_ST_BIND(symbolTable[j].st_info));
        printf("  Type=%d\n", ELF64_ST_TYPE(symbolTable[j].st_info));
        printf("st_other=%d\n", symbolTable[j].st_other);
        printf("st_shndx=%d\n", symbolTable[j].st_shndx);*/

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
                // printf("FAILURE TO COMPRESS!\n");
                 return -1;
               }
               //printf("strm.avail_out=%d\n", strm.avail_out);

               if (strm.avail_out==0)
               {


                // printf("Out buffer full. Reallocating\n");
                 *output=realloc(*output, maxoutputsize*2);

                 strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
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


  printf("end:\n");
  strm.avail_in=tempbufferpos;
  strm.next_in=tempbuffer;

  while (1)
  {

    i=deflate(&strm, Z_FINISH);
    printf("i=%d\n", i);
    if (i==Z_STREAM_END) //done
      break;

    if (i!=Z_OK)
    {
      printf("Failure to compress: %i\n", i);
      exit(1);
    }

    if (strm.avail_out==0)
    {
      printf("Out buffer full. Reallocating :%d\n", maxoutputsize*2);
      *output=realloc(*output, maxoutputsize*2);

      strm.next_out=(unsigned char *)&(*output)[maxoutputsize];
      maxoutputsize=maxoutputsize*2;
      strm.avail_out=maxoutputsize/2;
    }
    else
      break;

  };

  printf("strm.avail_out=%d\n", strm.avail_out);

  printf("total_in = %lu\n", strm.total_in);
  printf("total_out = %lu\n", strm.total_out);

  deflateEnd(&strm);


  *(uint32_t *)(&(*output)[0])=strm.total_out+2*sizeof(uint32_t);
  *(uint32_t *)(&(*output)[4])=strm.total_in;


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

  *output=NULL;
  f=open(filename, O_RDONLY);
  if (f==-1)
    return -1;

  b=malloc(sizeof(Elf64_Ehdr));
  i=pread(f, b, sizeof(Elf64_Ehdr), 0);

  if (*(uint32_t *)b!=0x464c457f)
    return -1; //not an ELF file

  if (b[EI_CLASS]==ELFCLASS32)
    i=ELF32(f, (Elf32_Ehdr *)b, output);
  else
    i=ELF64(f, (Elf64_Ehdr *)b, output);

  free(b);

  return i;
}
