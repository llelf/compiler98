#ifndef _NODECACHE_H
#define _NODECACHE_H

#include "art.h"

int		ncFind	(FileOffset np);
FileOffset	ncRef	(int ref);
int		ncInsert(FileOffset np, int hist);
void		ncHist	(int ref);
int		ncInit	(void);
void		ncDump	(void);

#endif
