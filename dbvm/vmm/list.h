/*
 * list.h
 *
 *  Created on: Jun 25, 2019
 *      Author: eric
 */

#ifndef VMM_LIST_H_
#define VMM_LIST_H_

#include "common.h"
#include "mm.h"

typedef struct
{
  QWORD address;
  void *data;
} AddressListEntry,*PAddressListEntry;

typedef struct
{
  PAddressListEntry list;
  int size;
  int capacity;
} AddressList, *PAddressList;


void addresslist_add(PAddressList l, QWORD address, void *data);
void addresslist_remove(PAddressList l, QWORD address);
void *addresslist_find(PAddressList l, QWORD address);

void addresslist_destroy(PAddressList l);
PAddressList addresslist_create();


#endif /* VMM_LIST_H_ */
