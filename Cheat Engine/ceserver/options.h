/*
 * options.h
 *
 *  Created on: Oct 31, 2022
 *      Author: eric
 */

#ifndef OPTIONS_H_
#define OPTIONS_H_

typedef struct {
  char *optname;
  char *parent;
  char *description;
  char *acceptablevalues; //contains a string of acceptable values, and their descriptions (value1=description;value2=description;value3=description;...) , if empty, the user can type in anything
  int type; //way to parse the value. 0=parent (no values), 1=boolean, 2=int, 3=float, 4=double, 5=text

  void *data;
  //setter/getter customization?
} CEServerOption, *PCEServerOption;

void handleGetOptions(int currentsocket);
void handleGetOption(int currentsocket);
void handleSetOption(int currentsocket);

char *getOptionValue(PCEServerOption o);

#endif /* OPTIONS_H_ */
