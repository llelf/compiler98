module Prelude
  ( _mkIOok0,  _mkIOok1,  _mkIOok2,  _mkIOok3, _mkIOok4,  _mkIOok5
  , _mkIOok6,  _mkIOok7,  _mkIOok8,  _mkIOok9, _mkIOok10, _mkIOok11, _mkIOok12
#if 0
  , _mkIOok13, _mkIOok14, _mkIOok15
#endif
  , _mkIOwf0,  _mkIOwf1,  _mkIOwf2,  _mkIOwf3, _mkIOwf4,  _mkIOwf5
  , _mkIOwf6,  _mkIOwf7,  _mkIOwf8,  _mkIOwf9, _mkIOwf10, _mkIOwf11, _mkIOwf12
  ) where

import DIO

-- mkIOok functions lift any non-failing pure function to become a
--        monadic IO action, eliminating sharing of results etc.
-- These are intended to be applied by machine-generated code only.
-- Arities: 0-15
--
--
-- mkIOwf functions lift any potentially-failing pure function
--        (expressed by the Either type) into the IO monad.
--        Required: an injection function for the error value.
-- These are intended to be applied by hand.
-- Arities: 0-12


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


_mkIOwf0  :: (z->IOError) -> (()->Either z a) -> IO a	-- Note: not a CAF
_mkIOwf1  :: (z->IOError) -> (b->Either z a) -> (b->IO a)
_mkIOwf2  :: (z->IOError) -> (c->b->Either z a) -> (c->b->IO a)
_mkIOwf3  :: (z->IOError) -> (d->c->b->Either z a) -> (d->c->b->IO a)
_mkIOwf4  :: (z->IOError) -> (e->d->c->b->Either z a) -> (e->d->c->b->IO a)
_mkIOwf5  :: (z->IOError) -> (f->e->d->c->b->Either z a)
                          -> (f->e->d->c->b->IO a)
_mkIOwf6  :: (z->IOError) -> (g->f->e->d->c->b->Either z a)
                          -> (g->f->e->d->c->b->IO a)
_mkIOwf7  :: (z->IOError) -> (h->g->f->e->d->c->b->Either z a)
                          -> (h->g->f->e->d->c->b->IO a)
_mkIOwf8  :: (z->IOError) -> (i->h->g->f->e->d->c->b->Either z a)
                          -> (i->h->g->f->e->d->c->b->IO a)
_mkIOwf9  :: (z->IOError) -> (j->i->h->g->f->e->d->c->b->Either z a)
                          -> (j->i->h->g->f->e->d->c->b->IO a)
_mkIOwf10 :: (z->IOError) -> (k->j->i->h->g->f->e->d->c->b->Either z a)
                          -> (k->j->i->h->g->f->e->d->c->b->IO a)
_mkIOwf11 :: (z->IOError) -> (l->k->j->i->h->g->f->e->d->c->b->Either z a)
                          -> (l->k->j->i->h->g->f->e->d->c->b->IO a)
_mkIOwf12 :: (z->IOError) -> (m->l->k->j->i->h->g->f->e->d->c->b->Either z a)
                          -> (m->l->k->j->i->h->g->f->e->d->c->b->IO a)

_mkIOwf0  z fn = IO (\_->mapLeft z $! fn ())
_mkIOwf1  z fn = \a-> IO (\_->mapLeft z $! fn a)
_mkIOwf2  z fn = \a b-> IO (\_->mapLeft z $! fn a b)
_mkIOwf3  z fn = \a b c-> IO (\_->mapLeft z $! fn a b c)
_mkIOwf4  z fn = \a b c d-> IO (\_->mapLeft z $! fn a b c d)
_mkIOwf5  z fn = \a b c d e-> IO (\_->mapLeft z $! fn a b c d e)
_mkIOwf6  z fn = \a b c d e f-> IO (\_->mapLeft z $! fn a b c d e f)
_mkIOwf7  z fn = \a b c d e f g-> IO (\_->mapLeft z $! fn a b c d e f g)
_mkIOwf8  z fn = \a b c d e f g h-> IO (\_->mapLeft z $! fn a b c d e f g h)
_mkIOwf9  z fn = \a b c d e f g h i->
                 IO (\_->mapLeft z $! fn a b c d e f g h i)
_mkIOwf10 z fn = \a b c d e f g h i j->
                 IO (\_->mapLeft z $! fn a b c d e f g h i j)
_mkIOwf11 z fn = \a b c d e f g h i j k->
                 IO (\_->mapLeft z $! fn a b c d e f g h i j k)
_mkIOwf12 z fn = \a b c d e f g h i j k l->
                 IO (\_->mapLeft z $! fn a b c d e f g h i j k l)

mapLeft f (Left a)  = Left (f a)
mapLeft f (Right b) = Right b	-- can't just reuse val - change of type
