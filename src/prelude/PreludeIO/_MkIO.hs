module Prelude
  ( _mkIOok0,  _mkIOok1,  _mkIOok2,  _mkIOok3, _mkIOok4,  _mkIOok5
  , _mkIOok6,  _mkIOok7,  _mkIOok8,  _mkIOok9, _mkIOok10, _mkIOok11, _mkIOok12
#if 0
  , _mkIOok13, _mkIOok14, _mkIOok15
#endif
  ) where

import DIO

-- mkIOok functions lift any non-failing pure function to become a
--        monadic IO action, eliminating sharing of results etc.
-- These are intended to be applied by machine-generated code only.
-- Arities: 0-15


_mkIOok0 :: (()->a) -> IO a	-- Note: no CAFs allowed!
_mkIOok1 :: (b->a) -> (b->IO a)
_mkIOok2 :: (c->b->a) -> (c->b->IO a)
_mkIOok3 :: (d->c->b->a) -> (d->c->b->IO a)
_mkIOok4 :: (e->d->c->b->a) -> (e->d->c->b->IO a)
_mkIOok5 :: (f->e->d->c->b->a) -> (f->e->d->c->b->IO a)
_mkIOok6 :: (g->f->e->d->c->b->a) -> (g->f->e->d->c->b->IO a)
_mkIOok7 :: (h->g->f->e->d->c->b->a) -> (h->g->f->e->d->c->b->IO a)
_mkIOok8 :: (i->h->g->f->e->d->c->b->a) -> (i->h->g->f->e->d->c->b->IO a)
_mkIOok9 :: (j->i->h->g->f->e->d->c->b->a) -> (j->i->h->g->f->e->d->c->b->IO a)

_mkIOok10 :: (k->j->i->h->g->f->e->d->c->b->a)
          -> (k->j->i->h->g->f->e->d->c->b->IO a)
_mkIOok11 :: (l->k->j->i->h->g->f->e->d->c->b->a)
          -> (l->k->j->i->h->g->f->e->d->c->b->IO a)
_mkIOok12 :: (m->l->k->j->i->h->g->f->e->d->c->b->a)
          -> (m->l->k->j->i->h->g->f->e->d->c->b->IO a)
#if 0
_mkIOok13 :: (n->m->l->k->j->i->h->g->f->e->d->c->b->a)
          -> (n->m->l->k->j->i->h->g->f->e->d->c->b->IO a)
_mkIOok14 :: (o->n->m->l->k->j->i->h->g->f->e->d->c->b->a)
          -> (o->n->m->l->k->j->i->h->g->f->e->d->c->b->IO a)
_mkIOok15 :: (p->o->n->m->l->k->j->i->h->g->f->e->d->c->b->a)
          -> (p->o->n->m->l->k->j->i->h->g->f->e->d->c->b->IO a)
#endif

_mkIOok0  fn = IO (\_->Right $! fn ())
_mkIOok1  fn = \a-> IO (\_->Right $! fn a)
_mkIOok2  fn = \a b-> IO (\_->Right $! fn a b)
_mkIOok3  fn = \a b c-> IO (\_->Right $! fn a b c)
_mkIOok4  fn = \a b c d-> IO (\_->Right $! fn a b c d)
_mkIOok5  fn = \a b c d e-> IO (\_->Right $! fn a b c d e)
_mkIOok6  fn = \a b c d e f-> IO (\_->Right $! fn a b c d e f)
_mkIOok7  fn = \a b c d e f g-> IO (\_->Right $! fn a b c d e f g)
_mkIOok8  fn = \a b c d e f g h-> IO (\_->Right $! fn a b c d e f g h)
_mkIOok9  fn = \a b c d e f g h i-> IO (\_->Right $! fn a b c d e f g h i)
_mkIOok10 fn = \a b c d e f g h i j->
               IO (\_->Right $! fn a b c d e f g h i j)
_mkIOok11 fn = \a b c d e f g h i j k->
               IO (\_->Right $! fn a b c d e f g h i j k)
_mkIOok12 fn = \a b c d e f g h i j k l->
               IO (\_->Right $! fn a b c d e f g h i j k l)
#if 0
_mkIOok13 fn = \a b c d e f g h i j k l m->
               IO (\_->Right $! fn a b c d e f g h i j k l m)
_mkIOok14 fn = \a b c d e f g h i j k l m n->
               IO (\_->Right $! fn a b c d e f g h i j k l m n)
_mkIOok15 fn = \a b c d e f g h i j k l m n o->
               IO (\_->Right $! fn a b c d e f g h i j k l m n o)
#endif

