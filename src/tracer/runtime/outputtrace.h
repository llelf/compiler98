/* Interface to the structure that keeps references to traces from output */

typedef struct {
	int ch;
	NodePtr trace;
} otElement;

int		otSize();
otElement*	otRef(int ref);
void		otInsert(int ch, NodePtr trace);
int		otInit();
void		otMark();
void		otFlip();
void    	otMap(void (*f)(NodePtr*));
