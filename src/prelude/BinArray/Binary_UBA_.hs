module UnboxedArray where

import GreenCard
import Binary    ({-class-}Binary(..))
import BinHandle ({-type-} BinHandle(..))
import BinPtr    ({-type-} BinPtr(..))
import Bin       ({-type-} Bin(..))
import DUnboxedArray

instance Binary UnboxedArray where
  put = putUBA
  get = getUBA


gr_putUBA primitive 2 :: ForeignObj -> ForeignObj -> Int

putUBA :: BinHandle -> UnboxedArray -> IO (Bin UnboxedArray)
putUBA (BH bh) (UBA uba) =
  IO (\_ ->
	let gr_result = gr_putUBA bh uba
	    p = gr_result
	in seq gr_result (Right (BP p)))



gr_getUBA primitive 1 :: ForeignObj -> ForeignObj

getUBA :: BinHandle -> IO UnboxedArray
getUBA (BH bh) =
  IO (\_ ->
	let gr_result = gr_getUBA bh
	    uba = gr_result
	in seq gr_result (Right (UBA uba)))





-- %fun putUBA :: BinHandle -> UnboxedArray -> IO (Bin UnboxedArray)
-- %call (binHandle bh) (unboxedArray uba)
-- %code  /* BH is always a file - never memory */
-- %  unsigned temp; int i;
-- %  p = vtell(bh);
-- %  closecache(bh);
-- %  temp = htonl(uba->free);
-- %  write(bh->loc.fd,&temp,sizeof(unsigned));
-- %  temp = htonl(uba->write);
-- %  write(bh->loc.fd,&temp,sizeof(unsigned));
-- %  temp = htonl(uba->end);
-- %  write(bh->loc.fd,&temp,sizeof(unsigned));
-- %  for (i=0; i<uba->write; i++) {
-- %    temp = htonl(uba->block[i]);
-- %    write(bh->loc.fd,&temp,sizeof(unsigned));
-- %  }
-- %  opencache(bh);
-- %result (binPtr p)
--
-- %fun getUBA :: BinHandle -> IO UnboxedArray
-- %call (binHandle bh)
-- %code  /* BH is always a file - never memory */
-- %  unsigned temp; int i;
-- %    fprintf(stderr,"getUBA: starting\n");
-- %  closecache(bh);
-- %    fprintf(stderr,"getUBA: cache closed\n");
-- %  uba = (UBA)malloc(sizeof(struct UnboxedArray));
-- %    fprintf(stderr,"getUBA: uba  = 0x%x\n",uba);
-- %  i = read(bh->loc.fd,&temp,sizeof(unsigned));
-- %    fprintf(stderr,"getUBA: read %d bytes\n",i);
-- %  uba->free = ntohl(temp);
-- %    fprintf(stderr,"getUBA: uba->free  = %d\n",uba->free);
-- %  i = read(bh->loc.fd,&temp,sizeof(unsigned));
-- %    fprintf(stderr,"getUBA: read %d bytes\n",i);
-- %  uba->write = ntohl(temp);
-- %    fprintf(stderr,"getUBA: uba->write = %d\n",uba->write);
-- %  i = read(bh->loc.fd,&temp,sizeof(unsigned));
-- %    fprintf(stderr,"getUBA: read %d bytes\n",i);
-- %  uba->end = ntohl(temp);
-- %    fprintf(stderr,"getUBA: uba->end   = %d\n",uba->end);
-- %  uba->block = (unsigned*)malloc((uba->write+uba->free)*sizeof(unsigned));
-- %  for (i=0; i<uba->write; i++) {
-- %    read(bh->loc.fd,&temp,sizeof(unsigned));
-- %    uba->block[i] = ntohl(temp);
-- %      fprintf(stderr,"getUBA: uba->block[%d] = %d\n",i,uba->block[i]);
-- %  }
-- %  opencache(bh);
-- %result (unboxedArray uba)
