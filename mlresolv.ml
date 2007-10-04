(*
 * (c) 2006-2007 Anastasia Gornostaeva, <ermine@ermine.pp.ru>
 *
 * RFC 1035 - Domain names - implementation and specification
 * RFC 2782 - A DNS RR for specifying the location of services (DNS SRV)
 * RFC 1183 - New DNS RR Definitions
 * RFC 1348 - DNS NSAP RRs. B. Manning.
 * RFC 2163
 * RFC 1712 --DNS Encoding of Geographical Location 
 * RFC 1876 - Location Information in the DNS
 *)

type rr_class = [
| `IN
| `CHAOS
| `HS
| `NONE
| `ANY
]

type rr_type = [
| `A             (* Host address. *)
| `NS            (* Authoritative server. *)
| `MD            (* Mail destination. *)
| `MF            (* Mail forwarder. *)
| `CNAME         (* Canonical name. *)
| `SOA           (* Start of authority zone. *)
| `MB            (* Mailbox domain name. *)
| `MG            (* Mail group member. *)
| `MR            (* Mail rename name. *)
| `NULL         (* Null resource record. *)
| `WKS          (* Well known service. *)
| `PTR          (* Domain name pointer. *)
| `HINFO        (* Host information. *)
| `MINFO        (* Mailbox information. *)
| `MX           (* Mail routing information. *)
| `TXT          (* Text strings. *)
| `RP           (* Responsible person. *)
| `AFSDB        (* AFS cell database. *)
| `X25          (* X_25 calling address. *)
| `ISDN         (* ISDN calling address. *)
| `RT           (* Router. *)
| `NSAP         (* NSAP address. *)
| `NSAP_PTR     (* Reverse NSAP lookup (deprecated). *)
| `SIG          (* Security signature. *)
| `KEY          (* Security key. *)
| `PX           (* X.400 mail mapping. *)
| `GPOS         (* Geographical position (withdrawn). *)
| `AAAA         (* Ip6 Address. *)
| `LOC          (* Location Information. *)
| `NXT          (* Next domain (security). *)
| `EID          (* Endpoint identifier. *)
| `NIMLOC       (* Nimrod Locator. *)
| `SRV          (* Server Selection. *)
| `ATMA         (* ATM Address *)
| `NAPTR        (* Naming Authority PoinTeR *)
| `OPT          (* OPT pseudo-RR, RFC2761 *)
	(* Query type values which do not appear in resource records. *)
| `IXFR        (* Incremental zone transfer. *)
| `AXFR        (* Transfer zone of authority. *)
| `MAILB       (* Transfer mailbox records. *)
| `MAILA       (* Transfer mail agent records. *)
| `ANY         (* Wildcard match. *)
]

type rdata = [
| `A of string                    (* Host address. *)
| `NS of string                   (* Authoritative server. *)
| `MD of string                   (* Mail destination. *)
| `MF of string                   (* Mail forwarder. *)
| `CNAME of string                (* Canonical name. *)
| `SOA of string * string * int * int * int * int * int
                                       (* Start of authority zone. *)
| `MB of string                   (* Mailbox domain name. *)
| `MG of string                   (* Mail group member. *)
| `MR of string                   (* Mail rename name. *)
| `NULL of string                 (* Null resource record. *)
| `WKS of string * int * string   (* Well known service. *)
| `PTR of string                  (* Domain name pointer. *)
| `HINFO of string * string       (* Host information. (cpu, os) *)
| `MINFO of string * string       (* Mailbox information. *)
| `MX of int * string             (* Mail routing information. *)
| `TXT of string list             (* Text strings. *)
| `RP of string * string          (* Responsible person. *)
| `AFSDB of int * string          (* AFS cell database. *)
| `X25 of string                  (* X_25 calling address. *)
| `ISDN of string * string        (* ISDN calling address. *)
| `RT of int * string             (* Router. *)
| `NSAP of int * string           (* NSAP address. *)
| `NSAP_PTR of string             (* Reverse NSAP lookup (deprecated). *)
| `SIG          (* Security signature. *)
| `KEY          (* Security key. *)
| `PX of int * string * string    (* X.400 mail mapping. *)
| `GPOS of float * float * float (* Geographical position (withdrawn). *)
| `AAAA         (* Ip6 Address. *)
| `LOC          (* Location Information. *)
| `NXT          (* Next domain (security). *)
| `EID          (* Endpoint identifier. *)
| `NIMLOC       (* Nimrod Locator. *)
| `SRV of int * int * int * string (* Server Selection. *)
| `ATMA         (* ATM Address *)
| `NAPTR        (* Naming Authority PoinTeR *)
| `OPT          (* OPT pseudo-RR, RFC2761 *)
]
	
type rr = {
   rr_name: string;
   rr_type: rr_type;
   rr_class: rr_class;
   rr_ttl: int32;
   rr_rdata: rdata
}

external query: string -> rr_class -> rr_type -> rr list
   = "camlresolv_query"
