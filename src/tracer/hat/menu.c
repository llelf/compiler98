/**************************************************************************/
/* menu.c                                                                 */
/* does nice IO operations, menus etc                                     */
/*                                                                        */
/* Thorsten Brehm, 5/2001                                                 */
/**************************************************************************/

#include <stdio.h>
#include <curses.h>
#include "Expressions.h"
#include "hatgeneral.h"
#include "menu.h"

#define STATUSLINES 2

void showMenu(char* title,int highlighted,long offs,long max,char* lineProc(int i)) {
  int maxx,maxy,i;
  char* line;
  char s[20];
  erase();
  maxy = getmaxy(stdscr);
  maxx = getmaxx(stdscr);
  i=(maxx-strlen(title))/2;
  if (i<0) i=0;
  mvaddstr(maxy-1,i,title);
  for (i=0;i<maxx;i++) {
    mvaddch(maxy-2,i,'-');
  }
  sprintf(s,"%i/%i",highlighted+1,max);
  mvaddstr(maxy-1,0,s);
  i=0;
  while ((i<maxy-STATUSLINES)&&(i+offs<max)) {
    line = lineProc(i+offs);
    if (strlen(line)>maxx) {
      line[maxx]=0;
    }
    if (i+offs==highlighted) attron(A_REVERSE);
    mvaddstr(i,0,line);
    if (i+offs==highlighted) attroff(A_REVERSE);
    freeStr(line);
    i++;
  }
}

long menu(char* title,long max,char* lineProc(int i)) {
  int ch,maxx,maxy;
  long selection=0,offset=0;
  char repaint=1;

  initscr();
  cbreak();
  noecho();
  nonl();
  intrflush(stdscr, FALSE);
  keypad(stdscr, TRUE);

  do {
    if (repaint) {
      showMenu(title,selection,offset,max,lineProc);
      repaint=0;
    }
    maxy = getmaxy(stdscr);
    maxx = getmaxx(stdscr);
    mvaddstr(maxy-1,maxx-1,"");
    ch = getch();
    maxy = getmaxy(stdscr)-STATUSLINES;
    maxx = getmaxx(stdscr);
    switch (ch) {
    case KEY_UP:
      if (selection>0) {
	selection--;
	if (selection<offset) offset--;
	repaint=1;
      }
      break;
    case KEY_DOWN:
      if (selection+1<max) {
	selection++;
	if (selection>=offset+maxy) offset++;
	repaint=1;
      }
      break;
    case KEY_PPAGE:
      if (selection-maxy>=0) {
	selection=selection-maxy;
	if (offset-maxy>0)
	  offset=offset-maxy;
	else
	  offset=0;
      } else {
	offset=0;
	selection=0;
      }
      repaint=1;
      break;
    case KEY_FIND:
    case KEY_HOME:
      offset=0;
      selection=0;
      repaint=1;
      break;
    case KEY_SELECT:
    case KEY_END:
      selection=max-1;
      if (maxy<max) {
	offset=max-maxy;
      } else {
	offset=0;
      }
      repaint=1;
      break;
    case ' ':
    case KEY_NPAGE:
      if (selection+maxy<max) {
	selection=selection+maxy;
	offset=offset+maxy;
      } else {
	selection=max-1;
	if (maxy>selection) offset=0;
	else offset=selection-maxy+1;
      }
      if ((offset+maxy>max)&&(maxy<max)) {
	offset=max-maxy;
      }
      repaint=1;
      break;
    case KEY_RESIZE:
      maxy = getmaxy(stdscr);
      maxx = getmaxx(stdscr);
      if (selection>=offset+maxy) {
	offset = selection-maxy+1;
      }
      repaint=1;
      break;
    }
  } while ((ch!=13)&&(ch!='q')&&(ch!='Q')&&(ch!=27));
  endwin();
  if (ch==13)
    return selection;
  return -1;
}





