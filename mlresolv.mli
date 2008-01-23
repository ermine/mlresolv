(*
 * (c) 2006-2008 Anastasia Gornostaeva, <ermine@ermine.pp.ru>
 *
 * RFC 1035 - Domain names - implementation and specification
 * RFC 2782 - A DNS RR for specifying the location of services (DNS SRV)
 * RFC 1183 - New DNS RR Definitions
 * RFC 1348 - DNS NSAP RRs. B. Manning.
 * RFC 2163
 * RFC 1712 --DNS Encoding of Geographical Location 
 * RFC 1876 - Location Information in the DNS
 *)

type rr_class = [ `ANY | `CHAOS | `HS | `IN | `NONE ]

type rr_type =
    [ `A
    | `AAAA
    | `AFSDB
    | `ANY
    | `ATMA
    | `AXFR
    | `CNAME
    | `EID
    | `GPOS
    | `HINFO
    | `ISDN
    | `IXFR
    | `KEY
    | `LOC
    | `MAILA
    | `MAILB
    | `MB
    | `MD
    | `MF
    | `MG
    | `MINFO
    | `MR
    | `MX
    | `NAPTR
    | `NIMLOC
    | `NS
    | `NSAP
    | `NSAP_PTR
    | `NULL
    | `NXT
    | `OPT
    | `PTR
    | `PX
    | `RP
    | `RT
    | `SIG
    | `SOA
    | `SRV
    | `TXT
    | `WKS
    | `X25 ]

type rdata =
    [ `A of string
    | `AAAA
    | `AFSDB of int * string
    | `ATMA
    | `CNAME of string
    | `EID
    | `GPOS of float * float * float
    | `HINFO of string * string
    | `ISDN of string * string
    | `KEY
    | `LOC
    | `MB of string
    | `MD of string
    | `MF of string
    | `MG of string
    | `MINFO of string * string
    | `MR of string
    | `MX of int * string
    | `NAPTR
    | `NIMLOC
    | `NS of string
    | `NSAP of int * string
    | `NSAP_PTR of string
    | `NULL of string
    | `NXT
    | `OPT
    | `PTR of string
    | `PX of int * string * string
    | `RP of string * string
    | `RT of int * string
    | `SIG
    | `SOA of string * string * int * int * int * int * int
    | `SRV of int * int * int * string
    | `TXT of string list
    | `WKS of string * int * string
    | `X25 of string ]

exception Error of int
exception NoRecovery
exception HostNotFound
exception NoData
exception TryAgain

type rr = {
  rr_name : string;
  rr_type : rr_type;
  rr_class : rr_class;
  rr_ttl : int32;
  rr_rdata : rdata;
}

external query : string -> rr_class -> rr_type -> rr list = "mlresolv_query"
