module DErrNo
  ( ErrNo(..)
  , eqErrNo
  ) where

{-
Note: We really ought to generate this somehow from the standard include
files for any particular machine.  I got this list of errors from
  /usr/include/asm/errno.h
on my Slackware Linux box.
-}

data ErrNo =
    Enoerror	-- dummy value because enumeration starts at 0.
  | EPERM
  | ENOENT
  | ESRCH
  | EINTR
  | EIO
  | ENXIO
  | E2BIG
  | ENOEXEC
  | EBADF
  | ECHILD
  | EAGAIN
  | ENOMEM
  | EACCES
  | EFAULT
  | ENOTBLK
  | EBUSY
  | EEXIST
  | EXDEV
  | ENODEV
  | ENOTDIR
  | EISDIR
  | EINVAL
  | ENFILE
  | EMFILE
  | ENOTTY
  | ETXTBSY
  | EFBIG
  | ENOSPC
  | ESPIPE
  | EROFS
  | EMLINK
  | EPIPE
  | EDOM
  | ERANGE
  | EDEADLK
  | ENAMETOOLONG
  | ENOLCK
  | ENOSYS
  | ENOTEMPTY
  | ELOOP
  | EWOULDBLOCK
  | ENOMSG
  | EIDRM
  | ECHRNG
  | EL2NSYNC
  | EL3HLT
  | EL3RST
  | ELNRNG
  | EUNATCH
  | ENOCSI
  | EL2HLT
  | EBADE
  | EBADR
  | EXFULL
  | ENOANO
  | EBADRQC
  | EBADSLT

  | EDEADLOCK

  | EBFONT
  | ENOSTR
  | ENODATA
  | ETIME
  | ENOSR
  | ENONET
  | ENOPKG
  | EREMOTE
  | ENOLINK
  | EADV
  | ESRMNT
  | ECOMM
  | EPROTO
  | EMULTIHOP
  | EDOTDOT
  | EBADMSG
  | EOVERFLOW
  | ENOTUNIQ
  | EBADFD
  | EREMCHG
  | ELIBACC
  | ELIBBAD
  | ELIBSCN
  | ELIBMAX
  | ELIBEXEC
  | EILSEQ
  | ERESTART
  | ESTRPIPE
  | EUSERS
  | ENOTSOCK
  | EDESTADDRREQ
  | EMSGSIZE
  | EPROTOTYPE
  | ENOPROTOOPT
  | EPROTONOSUPPORT
  | ESOCKTNOSUPPORT
  | EOPNOTSUPP
  | EPFNOSUPPORT
  | EAFNOSUPPORT
  | EADDRINUSE
  | EADDRNOTAVAIL
  | ENETDOWN
  | ENETUNREACH
  | ENETRESET
  | ECONNABORTED
  | ECONNRESET
  | ENOBUFS
  | EISCONN
  | ENOTCONN
  | ESHUTDOWN
  | ETOOMANYREFS
  | ETIMEDOUT
  | ECONNREFUSED
  | EHOSTDOWN
  | EHOSTUNREACH
  | EALREADY
  | EINPROGRESS
  | ESTALE
  | EUCLEAN
  | ENOTNAM
  | ENAVAIL
  | EISNAM
  | EREMOTEIO
  | EDQUOT
  deriving (Eq,Ord,Enum)

-- Apparently some errors should be treated identically.
eqErrNo :: ErrNo -> ErrNo -> Bool
eqErrNo EWOULDBLOCK EAGAIN = True
eqErrNo EAGAIN EWOULDBLOCK = True
eqErrNo EDEADLOCK EDEADLK  = True
eqErrNo EDEADLK EDEADLOCK  = True
eqErrNo e1      e2         = e1==e2

instance Show ErrNo where
  show = strErrNo

strErrNo :: ErrNo -> String
strErrNo	EPERM		 =	"Operation not permitted"
strErrNo	ENOENT		 =	"No such file or directory"
strErrNo	ESRCH		 =	"No such process"
strErrNo	EINTR		 =	"Interrupted system call"
strErrNo	EIO		 =	"I/O error"
strErrNo	ENXIO		 =	"No such device or address"
strErrNo	E2BIG		 =	"Arg list too long"
strErrNo	ENOEXEC		 =	"Exec format error"
strErrNo	EBADF		 =	"Bad file number"
strErrNo	ECHILD		 =	"No child processes"
strErrNo	EAGAIN		 =	"Try again"
strErrNo	ENOMEM		 =	"Out of memory"
strErrNo	EACCES		 =	"Permission denied"
strErrNo	EFAULT		 =	"Bad address"
strErrNo	ENOTBLK		 =	"Block device required"
strErrNo	EBUSY		 =	"Device or resource busy"
strErrNo	EEXIST		 =	"File exists"
strErrNo	EXDEV		 =	"Cross-device link"
strErrNo	ENODEV		 =	"No such device"
strErrNo	ENOTDIR		 =	"Not a directory"
strErrNo	EISDIR		 =	"Is a directory"
strErrNo	EINVAL		 =	"Invalid argument"
strErrNo	ENFILE		 =	"File table overflow"
strErrNo	EMFILE		 =	"Too many open files"
strErrNo	ENOTTY		 =	"Not a typewriter"
strErrNo	ETXTBSY		 =	"Text file busy"
strErrNo	EFBIG		 =	"File too large"
strErrNo	ENOSPC		 =	"No space left on device"
strErrNo	ESPIPE		 =	"Illegal seek"
strErrNo	EROFS		 =	"Read-only file system"
strErrNo	EMLINK		 =	"Too many links"
strErrNo	EPIPE		 =	"Broken pipe"
strErrNo	EDOM		 =	"Math argument out of domain of func"
strErrNo	ERANGE		 =	"Math result not representable"
strErrNo	EDEADLK		 =	"Resource deadlock would occur"
strErrNo	ENAMETOOLONG	 =	"File name too long"
strErrNo	ENOLCK		 =	"No record locks available"
strErrNo	ENOSYS		 =	"Function not implemented"
strErrNo	ENOTEMPTY	 =	"Directory not empty"
strErrNo	ELOOP		 =	"Too many symbolic links encountered"
strErrNo	EWOULDBLOCK	 =	"Operation would block"
strErrNo	ENOMSG		 =	"No message of desired type"
strErrNo	EIDRM		 =	"Identifier removed"
strErrNo	ECHRNG		 =	"Channel number out of range"
strErrNo	EL2NSYNC	 =	"Level 2 not synchronized"
strErrNo	EL3HLT		 =	"Level 3 halted"
strErrNo	EL3RST		 =	"Level 3 reset"
strErrNo	ELNRNG		 =	"Link number out of range"
strErrNo	EUNATCH		 =	"Protocol driver not attached"
strErrNo	ENOCSI		 =	"No CSI structure available"
strErrNo	EL2HLT		 =	"Level 2 halted"
strErrNo	EBADE		 =	"Invalid exchange"
strErrNo	EBADR		 =	"Invalid request descriptor"
strErrNo	EXFULL		 =	"Exchange full"
strErrNo	ENOANO		 =	"No anode"
strErrNo	EBADRQC		 =	"Invalid request code"
strErrNo	EBADSLT		 =	"Invalid slot"
 
strErrNo	EDEADLOCK	 = strErrNo EDEADLK

strErrNo	EBFONT		 =	"Bad font file format"
strErrNo	ENOSTR		 =	"Device not a stream"
strErrNo	ENODATA		 =	"No data available"
strErrNo	ETIME		 =	"Timer expired"
strErrNo	ENOSR		 =	"Out of streams resources"
strErrNo	ENONET		 =	"Machine is not on the network"
strErrNo	ENOPKG		 =	"Package not installed"
strErrNo	EREMOTE		 =	"Object is remote"
strErrNo	ENOLINK		 =	"Link has been severed"
strErrNo	EADV		 =	"Advertise error"
strErrNo	ESRMNT		 =	"Srmount error"
strErrNo	ECOMM		 =	"Communication error on send"
strErrNo	EPROTO		 =	"Protocol error"
strErrNo	EMULTIHOP	 =	"Multihop attempted"
strErrNo	EDOTDOT		 =	"RFS specific error"
strErrNo	EBADMSG		 =	"Not a data message"
strErrNo	EOVERFLOW	 =	"Value too large for defined data type"
strErrNo	ENOTUNIQ	 =	"Name not unique on network"
strErrNo	EBADFD		 =	"File descriptor in bad state"
strErrNo	EREMCHG		 =	"Remote address changed"
strErrNo	ELIBACC		 =	"Can not access a needed shared library"
strErrNo	ELIBBAD		 =	"Accessing a corrupted shared library"
strErrNo	ELIBSCN		 =	".lib section in a.out corrupted"
strErrNo	ELIBMAX		 =	"Attempting to link in too many shared libraries"
strErrNo	ELIBEXEC	 =	"Cannot exec a shared library directly"
strErrNo	EILSEQ		 =	"Illegal byte sequence"
strErrNo	ERESTART	 =	"Interrupted system call should be restarted"
strErrNo	ESTRPIPE	 =	"Streams pipe error"
strErrNo	EUSERS		 =	"Too many users"
strErrNo	ENOTSOCK	 =	"Socket operation on non-socket"
strErrNo	EDESTADDRREQ	 =	"Destination address required"
strErrNo	EMSGSIZE	 =	"Message too long"
strErrNo	EPROTOTYPE	 =	"Protocol wrong type for socket"
strErrNo	ENOPROTOOPT	 =	"Protocol not available"
strErrNo	EPROTONOSUPPORT	 =	"Protocol not supported"
strErrNo	ESOCKTNOSUPPORT	 =	"Socket type not supported"
strErrNo	EOPNOTSUPP	 =	"Operation not supported on transport endpoint"
strErrNo	EPFNOSUPPORT	 =	"Protocol family not supported"
strErrNo	EAFNOSUPPORT	 =	"Address family not supported by protocol"
strErrNo	EADDRINUSE	 =	"Address already in use"
strErrNo	EADDRNOTAVAIL	 =	"Cannot assign requested address"
strErrNo	ENETDOWN	 =	"Network is down"
strErrNo	ENETUNREACH	 =	"Network is unreachable"
strErrNo	ENETRESET	 =	"Network dropped connection because of reset"
strErrNo	ECONNABORTED	 =	"Software caused connection abort"
strErrNo	ECONNRESET	 =	"Connection reset by peer"
strErrNo	ENOBUFS		 =	"No buffer space available"
strErrNo	EISCONN		 =	"Transport endpoint is already connected"
strErrNo	ENOTCONN	 =	"Transport endpoint is not connected"
strErrNo	ESHUTDOWN	 =	"Cannot send after transport endpoint shutdown"
strErrNo	ETOOMANYREFS	 =	"Too many references: cannot splice"
strErrNo	ETIMEDOUT	 =	"Connection timed out"
strErrNo	ECONNREFUSED	 =	"Connection refused"
strErrNo	EHOSTDOWN	 =	"Host is down"
strErrNo	EHOSTUNREACH	 =	"No route to host"
strErrNo	EALREADY	 =	"Operation already in progress"
strErrNo	EINPROGRESS	 =	"Operation now in progress"
strErrNo	ESTALE		 =	"Stale NFS file handle"
strErrNo	EUCLEAN		 =	"Structure needs cleaning"
strErrNo	ENOTNAM		 =	"Not a XENIX named type file"
strErrNo	ENAVAIL		 =	"No XENIX semaphores available"
strErrNo	EISNAM		 =	"Is a named type file"
strErrNo	EREMOTEIO	 =	"Remote I/O error"
strErrNo	EDQUOT		 =	"Quota exceeded"

