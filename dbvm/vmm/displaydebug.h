/*
 * displaydebug.h
 *
 *  Created on: Aug 18, 2019
 *      Author: eric
 */

#ifndef VMM_DISPLAYDEBUG_H_
#define VMM_DISPLAYDEBUG_H_

extern unsigned char        *DDFrameBuffer;
extern unsigned long long    DDFrameBufferBase;
extern unsigned long long    DDFrameBufferSize;
extern unsigned long long    DDHorizontalResolution;
extern unsigned long long    DDVerticalResolution;
extern unsigned long long    DDPixelsPerScanLine;

void ddDrawRectangle(int x, int y, int width, int height, DWORD color);

#endif /* VMM_DISPLAYDEBUG_H_ */
