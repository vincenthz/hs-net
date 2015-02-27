-- |
-- Module      : Net.Socket.System.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Net.Socket.System.Internal
    ( SocketMsgFlags(..)
    , CSockLen(..)
    , MsgHdr(..)
    , IOVec(..)
    , SocketFamily(..)
    , packFamily
    , unpackFamily
    , isSupportedFamily
    , SocketType(..)
    , packSocketType
    , unpackSocketType
    ) where

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

import Control.Applicative
import Data.Bits
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr

-- | Socket Types.
--
-- The existence of a constructor does not necessarily imply that that
-- socket type is supported on your system: see 'isSupportedSocketType'.
data SocketType
        = NoSocketType -- ^ 0, used in getAddrInfo hints, for example
        | Stream -- ^ SOCK_STREAM
        | Datagram -- ^ SOCK_DGRAM
        | Raw -- ^ SOCK_RAW
        | RDM -- ^ SOCK_RDM
        | SeqPacket -- ^ SOCK_SEQPACKET
        deriving (Eq, Ord, Read, Show)

packSocketType' :: SocketType -> Maybe CInt
packSocketType' stype = case Just stype of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just NoSocketType -> Just 0
#ifdef SOCK_STREAM
    Just Stream -> Just #const SOCK_STREAM
#endif
#ifdef SOCK_DGRAM
    Just Datagram -> Just #const SOCK_DGRAM
#endif
#ifdef SOCK_RAW
    Just Raw -> Just #const SOCK_RAW
#endif
#ifdef SOCK_RDM
    Just RDM -> Just #const SOCK_RDM
#endif
#ifdef SOCK_SEQPACKET
    Just SeqPacket -> Just #const SOCK_SEQPACKET
#endif
    _ -> Nothing

packSocketType :: SocketType -> CInt
packSocketType stype = case packSocketType' stype of
    Nothing -> error errMsg
    Just v  -> v
  where
    errMsg = concat ["Network.Socket.packSocketType: ",
                     "socket type ", show stype, " unsupported on this system"]

unpackSocketType':: CInt -> Maybe SocketType
unpackSocketType' t = case t of
        0 -> Just NoSocketType
#ifdef SOCK_STREAM
        (#const SOCK_STREAM) -> Just Stream
#endif
#ifdef SOCK_DGRAM
        (#const SOCK_DGRAM) -> Just Datagram
#endif
#ifdef SOCK_RAW
        (#const SOCK_RAW) -> Just Raw
#endif
#ifdef SOCK_RDM
        (#const SOCK_RDM) -> Just RDM
#endif
#ifdef SOCK_SEQPACKET
        (#const SOCK_SEQPACKET) -> Just SeqPacket
#endif
        _ -> Nothing

-- | Try unpackSocketType on the CInt, if it fails throw an error with
-- message starting "Network.Socket." ++ the String parameter
unpackSocketType :: CInt -> SocketType
unpackSocketType ty = case unpackSocketType' ty of
    Nothing -> error errMsg
    Just v  -> v
  where
    errMsg = "Net.Socket: socket type " ++ show ty ++ " unsupported on this system"


data SocketFamily
    = AF_UNSPEC           -- unspecified
    | AF_UNIX             -- local to host (pipes, portals)
    | AF_INET             -- internetwork: UDP, TCP, etc
    | AF_INET6            -- Internet Protocol version 6
    | AF_IMPLINK          -- arpanet imp addresses
    | AF_PUP              -- pup protocols: e.g. BSP
    | AF_CHAOS            -- mit CHAOS protocols
    | AF_NS               -- XEROX NS protocols
    | AF_NBS              -- nbs protocols
    | AF_ECMA             -- european computer manufacturers
    | AF_DATAKIT          -- datakit protocols
    | AF_CCITT            -- CCITT protocols, X.25 etc
    | AF_SNA              -- IBM SNA
    | AF_DECnet           -- DECnet
    | AF_DLI              -- Direct data link interface
    | AF_LAT              -- LAT
    | AF_HYLINK           -- NSC Hyperchannel
    | AF_APPLETALK        -- Apple Talk
    | AF_ROUTE            -- Internal Routing Protocol
    | AF_NETBIOS          -- NetBios-style addresses
    | AF_NIT              -- Network Interface Tap
    | AF_802              -- IEEE 802.2, also ISO 8802
    | AF_ISO              -- ISO protocols
    | AF_OSI              -- umbrella of all families used by OSI
    | AF_NETMAN           -- DNA Network Management
    | AF_X25              -- CCITT X.25
    | AF_AX25
    | AF_OSINET           -- AFI
    | AF_GOSSIP           -- US Government OSI
    | AF_IPX              -- Novell Internet Protocol
    | Pseudo_AF_XTP       -- eXpress Transfer Protocol (no AF)
    | AF_CTF              -- Common Trace Facility
    | AF_WAN              -- Wide Area Network protocols
    | AF_SDL              -- SGI Data Link for DLPI
    | AF_NETWARE
    | AF_NDD
    | AF_INTF             -- Debugging use only
    | AF_COIP             -- connection-oriented IP, aka ST II
    | AF_CNT              -- Computer Network Technology
    | Pseudo_AF_RTIP      -- Help Identify RTIP packets
    | Pseudo_AF_PIP       -- Help Identify PIP packets
    | AF_SIP              -- Simple Internet Protocol
    | AF_ISDN             -- Integrated Services Digital Network
    | Pseudo_AF_KEY       -- Internal key-management function
    | AF_NATM             -- native ATM access
    | AF_ARP              -- (rev.) addr. res. prot. (RFC 826)
    | Pseudo_AF_HDRCMPLT  -- Used by BPF to not rewrite hdrs in iface output
    | AF_ENCAP
    | AF_LINK             -- Link layer interface
    | AF_RAW              -- Link layer interface
    | AF_RIF              -- raw interface
    | AF_NETROM           -- Amateur radio NetROM
    | AF_BRIDGE           -- multiprotocol bridge
    | AF_ATMPVC           -- ATM PVCs
    | AF_ROSE             -- Amateur Radio X.25 PLP
    | AF_NETBEUI          -- 802.2LLC
    | AF_SECURITY         -- Security callback pseudo AF
    | AF_PACKET           -- Packet family
    | AF_ASH              -- Ash
    | AF_ECONET           -- Acorn Econet
    | AF_ATMSVC           -- ATM SVCs
    | AF_IRDA             -- IRDA sockets
    | AF_PPPOX            -- PPPoX sockets
    | AF_WANPIPE          -- Wanpipe API sockets
    | AF_BLUETOOTH        -- bluetooth sockets
    | AF_CAN              -- Controller Area Network
      deriving (Eq, Ord, Read, Show)

packFamily :: SocketFamily -> CInt
packFamily f = case packFamily' f of
    Just fam -> fam
    Nothing -> error $
               "Network.Socket.packFamily: unsupported address family: " ++
               show f

-- | Does the AF_ constant corresponding to the given family exist on this
-- system?
isSupportedFamily :: SocketFamily -> Bool
isSupportedFamily f =
    case packFamily' f of
        Nothing -> False
        Just _  -> True

packFamily' :: SocketFamily -> Maybe CInt
packFamily' f = case Just f of
    -- the Just above is to disable GHC's overlapping pattern
    -- detection: see comments for packSocketOption
    Just AF_UNSPEC -> Just #const AF_UNSPEC
#ifdef AF_UNIX
    Just AF_UNIX -> Just #const AF_UNIX
#endif
#ifdef AF_INET
    Just AF_INET -> Just #const AF_INET
#endif
#ifdef AF_INET6
    Just AF_INET6 -> Just #const AF_INET6
#endif
#ifdef AF_IMPLINK
    Just AF_IMPLINK -> Just #const AF_IMPLINK
#endif
#ifdef AF_PUP
    Just AF_PUP -> Just #const AF_PUP
#endif
#ifdef AF_CHAOS
    Just AF_CHAOS -> Just #const AF_CHAOS
#endif
#ifdef AF_NS
    Just AF_NS -> Just #const AF_NS
#endif
#ifdef AF_NBS
    Just AF_NBS -> Just #const AF_NBS
#endif
#ifdef AF_ECMA
    Just AF_ECMA -> Just #const AF_ECMA
#endif
#ifdef AF_DATAKIT
    Just AF_DATAKIT -> Just #const AF_DATAKIT
#endif
#ifdef AF_CCITT
    Just AF_CCITT -> Just #const AF_CCITT
#endif
#ifdef AF_SNA
    Just AF_SNA -> Just #const AF_SNA
#endif
#ifdef AF_DECnet
    Just AF_DECnet -> Just #const AF_DECnet
#endif
#ifdef AF_DLI
    Just AF_DLI -> Just #const AF_DLI
#endif
#ifdef AF_LAT
    Just AF_LAT -> Just #const AF_LAT
#endif
#ifdef AF_HYLINK
    Just AF_HYLINK -> Just #const AF_HYLINK
#endif
#ifdef AF_APPLETALK
    Just AF_APPLETALK -> Just #const AF_APPLETALK
#endif
#ifdef AF_ROUTE
    Just AF_ROUTE -> Just #const AF_ROUTE
#endif
#ifdef AF_NETBIOS
    Just AF_NETBIOS -> Just #const AF_NETBIOS
#endif
#ifdef AF_NIT
    Just AF_NIT -> Just #const AF_NIT
#endif
#ifdef AF_802
    Just AF_802 -> Just #const AF_802
#endif
#ifdef AF_ISO
    Just AF_ISO -> Just #const AF_ISO
#endif
#ifdef AF_OSI
    Just AF_OSI -> Just #const AF_OSI
#endif
#ifdef AF_NETMAN
    Just AF_NETMAN -> Just #const AF_NETMAN
#endif
#ifdef AF_X25
    Just AF_X25 -> Just #const AF_X25
#endif
#ifdef AF_AX25
    Just AF_AX25 -> Just #const AF_AX25
#endif
#ifdef AF_OSINET
    Just AF_OSINET -> Just #const AF_OSINET
#endif
#ifdef AF_GOSSIP
    Just AF_GOSSIP -> Just #const AF_GOSSIP
#endif
#ifdef AF_IPX
    Just AF_IPX -> Just #const AF_IPX
#endif
#ifdef Pseudo_AF_XTP
    Just Pseudo_AF_XTP -> Just #const Pseudo_AF_XTP
#endif
#ifdef AF_CTF
    Just AF_CTF -> Just #const AF_CTF
#endif
#ifdef AF_WAN
    Just AF_WAN -> Just #const AF_WAN
#endif
#ifdef AF_SDL
    Just AF_SDL -> Just #const AF_SDL
#endif
#ifdef AF_NETWARE
    Just AF_NETWARE -> Just #const AF_NETWARE
#endif
#ifdef AF_NDD
    Just AF_NDD -> Just #const AF_NDD
#endif
#ifdef AF_INTF
    Just AF_INTF -> Just #const AF_INTF
#endif
#ifdef AF_COIP
    Just AF_COIP -> Just #const AF_COIP
#endif
#ifdef AF_CNT
    Just AF_CNT -> Just #const AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
    Just Pseudo_AF_RTIP -> Just #const Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
    Just Pseudo_AF_PIP -> Just #const Pseudo_AF_PIP
#endif
#ifdef AF_SIP
    Just AF_SIP -> Just #const AF_SIP
#endif
#ifdef AF_ISDN
    Just AF_ISDN -> Just #const AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
    Just Pseudo_AF_KEY -> Just #const Pseudo_AF_KEY
#endif
#ifdef AF_NATM
    Just AF_NATM -> Just #const AF_NATM
#endif
#ifdef AF_ARP
    Just AF_ARP -> Just #const AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
    Just Pseudo_AF_HDRCMPLT -> Just #const Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
    Just AF_ENCAP -> Just #const AF_ENCAP
#endif
#ifdef AF_LINK
    Just AF_LINK -> Just #const AF_LINK
#endif
#ifdef AF_RAW
    Just AF_RAW -> Just #const AF_RAW
#endif
#ifdef AF_RIF
    Just AF_RIF -> Just #const AF_RIF
#endif
#ifdef AF_NETROM
    Just AF_NETROM -> Just #const AF_NETROM
#endif
#ifdef AF_BRIDGE
    Just AF_BRIDGE -> Just #const AF_BRIDGE
#endif
#ifdef AF_ATMPVC
    Just AF_ATMPVC -> Just #const AF_ATMPVC
#endif
#ifdef AF_ROSE
    Just AF_ROSE -> Just #const AF_ROSE
#endif
#ifdef AF_NETBEUI
    Just AF_NETBEUI -> Just #const AF_NETBEUI
#endif
#ifdef AF_SECURITY
    Just AF_SECURITY -> Just #const AF_SECURITY
#endif
#ifdef AF_PACKET
    Just AF_PACKET -> Just #const AF_PACKET
#endif
#ifdef AF_ASH
    Just AF_ASH -> Just #const AF_ASH
#endif
#ifdef AF_ECONET
    Just AF_ECONET -> Just #const AF_ECONET
#endif
#ifdef AF_ATMSVC
    Just AF_ATMSVC -> Just #const AF_ATMSVC
#endif
#ifdef AF_IRDA
    Just AF_IRDA -> Just #const AF_IRDA
#endif
#ifdef AF_PPPOX
    Just AF_PPPOX -> Just #const AF_PPPOX
#endif
#ifdef AF_WANPIPE
    Just AF_WANPIPE -> Just #const AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
    Just AF_BLUETOOTH -> Just #const AF_BLUETOOTH
#endif
#ifdef AF_CAN
    Just AF_CAN -> Just #const AF_CAN
#endif
    _ -> Nothing

--------- ----------

unpackFamily :: CInt -> SocketFamily
unpackFamily f = case f of
        (#const AF_UNSPEC) -> AF_UNSPEC
#ifdef AF_UNIX
        (#const AF_UNIX) -> AF_UNIX
#endif
#ifdef AF_INET
        (#const AF_INET) -> AF_INET
#endif
#ifdef AF_INET6
        (#const AF_INET6) -> AF_INET6
#endif
#ifdef AF_IMPLINK
        (#const AF_IMPLINK) -> AF_IMPLINK
#endif
#ifdef AF_PUP
        (#const AF_PUP) -> AF_PUP
#endif
#ifdef AF_CHAOS
        (#const AF_CHAOS) -> AF_CHAOS
#endif
#ifdef AF_NS
        (#const AF_NS) -> AF_NS
#endif
#ifdef AF_NBS
        (#const AF_NBS) -> AF_NBS
#endif
#ifdef AF_ECMA
        (#const AF_ECMA) -> AF_ECMA
#endif
#ifdef AF_DATAKIT
        (#const AF_DATAKIT) -> AF_DATAKIT
#endif
#ifdef AF_CCITT
        (#const AF_CCITT) -> AF_CCITT
#endif
#ifdef AF_SNA
        (#const AF_SNA) -> AF_SNA
#endif
#ifdef AF_DECnet
        (#const AF_DECnet) -> AF_DECnet
#endif
#ifdef AF_DLI
        (#const AF_DLI) -> AF_DLI
#endif
#ifdef AF_LAT
        (#const AF_LAT) -> AF_LAT
#endif
#ifdef AF_HYLINK
        (#const AF_HYLINK) -> AF_HYLINK
#endif
#ifdef AF_APPLETALK
        (#const AF_APPLETALK) -> AF_APPLETALK
#endif
#ifdef AF_ROUTE
        (#const AF_ROUTE) -> AF_ROUTE
#endif
#ifdef AF_NETBIOS
        (#const AF_NETBIOS) -> AF_NETBIOS
#endif
#ifdef AF_NIT
        (#const AF_NIT) -> AF_NIT
#endif
#ifdef AF_802
        (#const AF_802) -> AF_802
#endif
#ifdef AF_ISO
        (#const AF_ISO) -> AF_ISO
#endif
#ifdef AF_OSI
# if (!defined(AF_ISO)) || (defined(AF_ISO) && (AF_ISO != AF_OSI))
        (#const AF_OSI) -> AF_OSI
# endif
#endif
#ifdef AF_NETMAN
        (#const AF_NETMAN) -> AF_NETMAN
#endif
#ifdef AF_X25
        (#const AF_X25) -> AF_X25
#endif
#ifdef AF_AX25
        (#const AF_AX25) -> AF_AX25
#endif
#ifdef AF_OSINET
        (#const AF_OSINET) -> AF_OSINET
#endif
#ifdef AF_GOSSIP
        (#const AF_GOSSIP) -> AF_GOSSIP
#endif
#if defined(AF_IPX) && (!defined(AF_NS) || AF_NS != AF_IPX)
        (#const AF_IPX) -> AF_IPX
#endif
#ifdef Pseudo_AF_XTP
        (#const Pseudo_AF_XTP) -> Pseudo_AF_XTP
#endif
#ifdef AF_CTF
        (#const AF_CTF) -> AF_CTF
#endif
#ifdef AF_WAN
        (#const AF_WAN) -> AF_WAN
#endif
#ifdef AF_SDL
        (#const AF_SDL) -> AF_SDL
#endif
#ifdef AF_NETWARE
        (#const AF_NETWARE) -> AF_NETWARE
#endif
#ifdef AF_NDD
        (#const AF_NDD) -> AF_NDD
#endif
#ifdef AF_INTF
        (#const AF_INTF) -> AF_INTF
#endif
#ifdef AF_COIP
        (#const AF_COIP) -> AF_COIP
#endif
#ifdef AF_CNT
        (#const AF_CNT) -> AF_CNT
#endif
#ifdef Pseudo_AF_RTIP
        (#const Pseudo_AF_RTIP) -> Pseudo_AF_RTIP
#endif
#ifdef Pseudo_AF_PIP
        (#const Pseudo_AF_PIP) -> Pseudo_AF_PIP
#endif
#ifdef AF_SIP
        (#const AF_SIP) -> AF_SIP
#endif
#ifdef AF_ISDN
        (#const AF_ISDN) -> AF_ISDN
#endif
#ifdef Pseudo_AF_KEY
        (#const Pseudo_AF_KEY) -> Pseudo_AF_KEY
#endif
#ifdef AF_NATM
        (#const AF_NATM) -> AF_NATM
#endif
#ifdef AF_ARP
        (#const AF_ARP) -> AF_ARP
#endif
#ifdef Pseudo_AF_HDRCMPLT
        (#const Pseudo_AF_HDRCMPLT) -> Pseudo_AF_HDRCMPLT
#endif
#ifdef AF_ENCAP
        (#const AF_ENCAP) -> AF_ENCAP
#endif
#ifdef AF_LINK
        (#const AF_LINK) -> AF_LINK
#endif
#ifdef AF_RAW
        (#const AF_RAW) -> AF_RAW
#endif
#ifdef AF_RIF
        (#const AF_RIF) -> AF_RIF
#endif
#ifdef AF_NETROM
        (#const AF_NETROM) -> AF_NETROM
#endif
#ifdef AF_BRIDGE
        (#const AF_BRIDGE) -> AF_BRIDGE
#endif
#ifdef AF_ATMPVC
        (#const AF_ATMPVC) -> AF_ATMPVC
#endif
#ifdef AF_ROSE
        (#const AF_ROSE) -> AF_ROSE
#endif
#ifdef AF_NETBEUI
        (#const AF_NETBEUI) -> AF_NETBEUI
#endif
#ifdef AF_SECURITY
        (#const AF_SECURITY) -> AF_SECURITY
#endif
#ifdef AF_PACKET
        (#const AF_PACKET) -> AF_PACKET
#endif
#ifdef AF_ASH
        (#const AF_ASH) -> AF_ASH
#endif
#ifdef AF_ECONET
        (#const AF_ECONET) -> AF_ECONET
#endif
#ifdef AF_ATMSVC
        (#const AF_ATMSVC) -> AF_ATMSVC
#endif
#ifdef AF_IRDA
        (#const AF_IRDA) -> AF_IRDA
#endif
#ifdef AF_PPPOX
        (#const AF_PPPOX) -> AF_PPPOX
#endif
#ifdef AF_WANPIPE
        (#const AF_WANPIPE) -> AF_WANPIPE
#endif
#ifdef AF_BLUETOOTH
        (#const AF_BLUETOOTH) -> AF_BLUETOOTH
#endif
#ifdef AF_CAN
        (#const AF_CAN) -> AF_CAN
#endif
        unknown -> error ("Network.Socket.unpackFamily: unknown address " ++
                          "family " ++ show unknown)

newtype SocketMsgFlags = SocketMsgFlags CInt
    deriving (Storable)

instance Monoid SocketMsgFlags where
    mempty = SocketMsgFlags 0
    mappend (SocketMsgFlags f1) (SocketMsgFlags f2) =
        SocketMsgFlags (f1 .|. f2)

newtype CSockLen = CSockLen CInt
    deriving (Storable)

data MsgHdr = MsgHdr
    { msgHdrName     :: !(Ptr Word8)
    , msgHdrNameLen  :: !CInt
    , msgHdrIovecs   :: !IOVecs
    , msgHdrIovecLen :: !CInt
    , msgHdrFlags    :: !SocketMsgFlags
    }

type IOVecs = Ptr IOVec

data IOVec = IOVec
    { iovecData :: !(Ptr Word8)
    , iovecSize :: !CSize
    }

instance Storable MsgHdr where
    sizeOf _        = (#const sizeof(struct msghdr))
    alignment _     = 8
    peek ptr        =
        MsgHdr <$> (#peek struct msghdr, msg_name) ptr
               <*> (#peek struct msghdr, msg_namelen) ptr
               <*> (#peek struct msghdr, msg_iov) ptr
               <*> (#peek struct msghdr, msg_iovlen) ptr
               <*> (#peek struct msghdr, msg_flags) ptr
    poke ptr msghdr = do
        (#poke struct msghdr, msg_name)    ptr (msgHdrName msghdr)
        (#poke struct msghdr, msg_namelen) ptr (msgHdrNameLen msghdr)
        (#poke struct msghdr, msg_iov)     ptr (msgHdrIovecs msghdr)
        (#poke struct msghdr, msg_iovlen)  ptr (msgHdrIovecLen msghdr)
        (#poke struct msghdr, msg_flags)   ptr (msgHdrFlags msghdr)

instance Storable IOVec where
    sizeOf _        = (#const sizeof(struct iovec))
    alignment _     = 8
    peek ptr        =
        IOVec <$> (#peek struct iovec, iov_base) ptr
              <*> (#peek struct iovec, iov_len) ptr
    poke ptr iovec  = do
        (#poke struct iovec, iov_base) ptr (iovecData iovec)
        (#poke struct iovec, iov_len) ptr (iovecSize iovec)
