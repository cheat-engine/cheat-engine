/*
 * pci.h
 *
 *  Created on: Jun 8, 2009
 *      Author: erich
 */

#ifndef PCI_H_
#define PCI_H_

DWORD pciConfigReadDWord (unsigned short bus, unsigned short slot, unsigned short func, unsigned short offset);
WORD pciConfigReadWord (unsigned short bus, unsigned short slot, unsigned short func, unsigned short offset);

void pciConfigEnumPci(void);

#endif /* PCI_H_ */
