#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>
#include <netdb.h>
#include <errno.h>
#include <string.h>


#ifdef DEBUG
#include <stdarg.h>
#endif

void LOG(char* fmt, ...) {
#ifdef DEBUG
  va_list ap;
  va_start(ap, fmt);
  vprintf(fmt, ap);
  va_end(ap);
#endif
}

typedef struct {
  u_short val;
  char *name;
} variant;

static variant rr_class[] = {
  { 5, ""},
  {ns_c_in, "IN"},
  {ns_c_chaos, "CHAOS"},
  {ns_c_hs, "HS"},
  {ns_c_none, "NONE"},
  {ns_c_any, "ANY"}
};

static variant rr_type[] = {
  {41, ""},
  {ns_t_a,        "A"},        /* Host address. */
  {ns_t_ns,       "NS"},       /* Authoritative server. */
  {ns_t_md,       "MD"},       /* Mail destination. */
  {ns_t_mf,       "MF"},       /* Mail forwarder. */
  {ns_t_cname,    "CNAME"},    /* Canonical name. */
  {ns_t_soa,      "SOA"},      /* Start of authority zone. */
  {ns_t_mb,       "MB"},       /* Mailbox domain name. */
  {ns_t_mg,       "MG"},       /* Mail group member. */
  {ns_t_mr,       "MR"},       /* Mail rename name. */
  {ns_t_null,     "NULL"},     /* Null resource record. */
  {ns_t_wks,      "WKS"},      /* Well known service. */
  {ns_t_ptr,      "PTR"},      /* Domain name pointer. */
  {ns_t_hinfo,    "HINFO"},    /* Host information. */
  {ns_t_minfo,    "MINFO"},    /* Mailbox information. */
  {ns_t_mx,       "MX"},       /* Mail routing information. */
  {ns_t_txt,      "TXT"},      /* Text strings. */
  {ns_t_rp,       "RP"},       /* Responsible person. */
  {ns_t_afsdb,    "AFSDB"},    /* AFS cell database. */
  {ns_t_x25,      "X25"},      /* X_25 calling address. */
  {ns_t_isdn,     "ISDN"},     /* ISDN calling address. */
  {ns_t_rt,       "RT"},       /* Router. */
  {ns_t_nsap,     "NSAP"},     /* NSAP address. */
  {ns_t_nsap_ptr, "NSAP_PTR"}, /* Reverse NSAP lookup (deprecated). */
  {ns_t_sig,      "SIG"},      /* Security signature. */
  {ns_t_key,      "KEY"},      /* Security key. */
  {ns_t_px,       "PX"},       /* X.400 mail mapping. */
  {ns_t_gpos,     "GPOS"},     /* Geographical position (withdrawn). */
  {ns_t_aaaa,     "AAAA"},     /* Ip6 Address. */
  {ns_t_loc,      "LOC"},      /* Location Information. */
  {ns_t_nxt,      "NXT"},      /* Next domain (security). */
  {ns_t_eid,      "EID"},      /* Endpoint identifier. */
  {ns_t_nimloc,   "NIMLOC"},   /* Nimrod Locator. */
  {ns_t_srv,      "SRV"},      /* Server Selection. */
  {ns_t_atma,     "ATMA"},     /* ATM Address */
  {ns_t_naptr,    "NAPTR"},    /* Naming Authority PoinTeR */
  {ns_t_opt,      "OPT"},      /* OPT pseudo-RR, RFC2761 */
  /* Query type values which do not appear in resource records. */
  {ns_t_ixfr,     "IXFR"},     /* Incremental zone transfer. */
  {ns_t_axfr,     "AXFR"},     /* Transfer zone of authority. */
  {ns_t_mailb,    "MAILB"},    /* Transfer mailbox records. */
  {ns_t_maila,    "MAILA"},    /* Transfer mail agent records. */
  {ns_t_any,      "ANY"}       /* Wildcard match. */
};

static int mlvariant_to_c(variant *vtable, value variant)
{
  int size = vtable[0].val;
  int i;

  for(i=1; i <= size; i++)
    if(variant == hash_variant(vtable[i].name)) {
      LOG("DEBUG: Found variant %s\n", vtable[i].name);
      return vtable[i].val;
    }
  failwith("This variant cannot be converted from ML");
}

static value c_to_mlvariant(variant *vtable, int val)
{
  int size = vtable[0].val;
  int i;

  for(i=1; i <= size; i++)
    if(val == vtable[i].val) {
      LOG("DEBUG: hash_variant(%s)\n", vtable[i].name);
      return caml_hash_variant(vtable[i].name);
    }
  LOG("no equals %d\n", val);
  return caml_hash_variant(vtable[1].name);
}

static value *mlresolv_error_exn = NULL;
static value *mlresolv_host_not_found_exn = NULL;
static value *mlresolv_try_again_exn = NULL;
static value *mlresolv_no_recovery_exn = NULL;
static value *mlresolv_no_data_exn = NULL;

CAMLprim value mlresolv_init(value unit) {
  mlresolv_error_exn = caml_named_value("Mlresolv_error");
  if (mlresolv_error_exn == NULL)
    invalid_argument("Exception Mlresolv_Error not initialized");

  mlresolv_no_recovery_exn = caml_named_value("Mlresolv_no_recovery");
  if (mlresolv_no_recovery_exn == NULL)
    invalid_argument("Exception Mlresolv_NoRecovery not initialized");

  mlresolv_host_not_found_exn = caml_named_value("Mlresolv_host_not_found");
  if (mlresolv_host_not_found_exn == NULL)
    invalid_argument("Exception Mlresolv_HostNotFound not initialized");

  mlresolv_try_again_exn = caml_named_value("Mlresolv_try_again");
  if (mlresolv_try_again_exn == NULL)
    invalid_argument("Exception Mlresolv_TryAgain not initialized");

  mlresolv_no_data_exn = caml_named_value("Mlresolv_no_data");
  if (mlresolv_no_data_exn == NULL)
    invalid_argument("Exception Mlresolv_NoData not initialized");

  return Val_unit;
}

static void mlresolv_error(int errcode) {
  value res;
  value err;

  err = alloc_small(1, 0);
  Field(err, 0) = Val_int(errcode);

  Begin_roots1(err);
    res = alloc_small(2, 0);
    Field(res, 0) = *mlresolv_error_exn;
    Field(res, 1) = err;
  End_roots();
  mlraise(res);
}

CAMLprim value mlresolv_query(value vdname, value vclass, value vtype)
{
  union {
    HEADER hdr;              /* defined in resolv.h */
    u_char buf[PACKETSZ];    /* defined in arpa/nameser.h */
  } response;
  int rc;

  u_char *cp, *tcp;
  u_char *eom;

  char r_name[MAXDNAME+1];
  u_short r_class;
  u_short r_type;
  u_int32_t r_ttl;
  u_short r_len;

  int ancount, qdcount;

  value vres = Val_emptylist;

  if(vtype == caml_hash_variant("PTR")) {
    int a, b, c, d;
    a = b = c = d = 0;
    sscanf(String_val(vdname), "%u.%u.%u.%u", &a, &b, &c, &d);
    sprintf(r_name, "%u.%u.%u.%u.in-addr.arpa", d, c, b, a);
    rc = res_query(r_name,
		   mlvariant_to_c(rr_class, vclass),
		   mlvariant_to_c(rr_type, vtype),
		   (u_char*)&response, sizeof(response));
  } else
    rc = res_query(String_val(vdname),
		   mlvariant_to_c(rr_class, vclass),
		   mlvariant_to_c(rr_type, vtype),
		   (u_char*)&response, sizeof(response));

  if (rc < 0) {
    switch (h_errno) {
    case NETDB_INTERNAL:  
      mlresolv_error(errno);
    case HOST_NOT_FOUND:  /* Authoritative Answer Host not found */
      raise_constant(*mlresolv_host_not_found_exn);
    case TRY_AGAIN:       /* Non-Authoritative Host not found, or SERVERFAIL */
      raise_constant(*mlresolv_try_again_exn);
    case NO_RECOVERY:
      raise_constant(*mlresolv_no_recovery_exn);
    case NO_DATA:         /* Valid name, no data record of requested type */
      raise_constant(*mlresolv_no_data_exn);
    case NETDB_SUCCESS:   /* no problem */
    defaykt:
      failwith("res_query: unknown error");
    }
  }

  cp = (u_char *)&response.buf + sizeof(HEADER);
  eom = (u_char *)&response.buf + rc;

  ancount = ntohs(response.hdr.ancount) + ntohs(response.hdr.nscount);
  qdcount = ntohs(response.hdr.qdcount);
  for (; (qdcount > 0) && (cp < eom); qdcount--) {
    rc = dn_skipname(cp, eom) + QFIXEDSZ;
    if(rc < 0)
      failwith("dn_skipname failed");
    cp += rc;
  }

  for (; (ancount > 0) && (cp < eom); ancount--) {
    value vrdata, vfields = Val_unit;

    rc = dn_expand(response.buf, eom, cp, (void*)r_name, MAXDNAME);
    if(rc < 0)
      failwith("dn_expand1 failed");

    cp += rc;

    NS_GET16(r_type, cp);
    NS_GET16(r_class, cp);
    NS_GET32(r_ttl, cp);
    NS_GET16(r_len, cp);

    if(cp + r_len > eom) /* is this check necessary? */
      r_len = eom - cp;

    tcp = cp;

    switch(r_type) {

    case ns_t_a:
      /* if(r_class == ns_c_in || r_class == ns_c_hs) { */

      if(INADDRSZ > r_len)
	vfields = copy_string("");
      else {
	struct in_addr inaddr;
	char *address;

	bcopy(tcp, (char *)&inaddr, INADDRSZ);
	address = (char *)inet_ntoa(inaddr);
	vfields = copy_string(address);
      }
      break;

    case ns_t_cname:
    case ns_t_ns:
    case ns_t_mb:
    case ns_t_md:
    case ns_t_mf:
    case ns_t_mg:
    case ns_t_mr:
    case ns_t_ptr: 
    case ns_t_nsap_ptr:
      {
	char r_name[MAXDNAME+1];
	rc = dn_expand(response.buf, eom, cp, (void *) r_name, MAXDNAME);
	if(rc < 0)
	  vfields = copy_string("");
	else
	  vfields = copy_string(r_name);
	break;
    }

    case ns_t_null:  /* max up to 65535 */
      vfields = caml_alloc_string(r_len);
      memmove(String_val(vfields), cp, r_len);
      break;

    case ns_t_txt: {
      int txtlen, rdata_len = r_len;
      value newcons, txt;
      vfields = Val_emptylist;

      while(tcp < eom && *tcp <= rdata_len) {
	txtlen = *tcp++;
	txt = caml_alloc_string(txtlen);
	memmove(String_val(txt), tcp, txtlen);
	tcp += txtlen;
	rdata_len -= txtlen+1;

	newcons = alloc_small(2, 0);
	Field(newcons, 0) = txt;
	Field(newcons, 1) = vfields;
	vfields = newcons;
      }
      break;
    }

    case ns_t_srv:
      if(INT16SZ * 3 <= r_len) {
	char r_name[MAXDNAME+1];
	int prio, weight, port;
      
	NS_GET16(prio, tcp);
	NS_GET16(weight, tcp);
	NS_GET16(port, tcp);

	rc = dn_expand(response.buf, eom, tcp, (void *) r_name, MAXDNAME);

	vfields = alloc_small(4, 0);
	Field(vfields, 0) = Val_int(prio);
	Field(vfields, 1) = Val_int(weight);
	Field(vfields, 2) = Val_int(port);
      
	if(rc < 0)
	  Field(vfields, 3) = copy_string("");
	else
	  Field(vfields, 3) = copy_string(r_name);
      }
      break;

    case ns_t_mx:
    case ns_t_rt:
    case ns_t_afsdb:
      if(INT16SZ <= r_len) {
	char r_name[MAXDNAME+1];
	int prio;

	NS_GET16(prio, tcp);

	rc = dn_expand(response.buf, eom, tcp, (void *) r_name, MAXDNAME);

	vfields = alloc_small(2, 0);
	Field(vfields, 0) = Val_int(prio);

	if(rc < 0)
	  Field(vfields, 1) = copy_string("");
	else
	  Field(vfields, 1) = copy_string(r_name);
      }
      break;

    case ns_t_soa: 
      {
	char mname[MAXDNAME+1];
	char rname[MAXDNAME+1];
	u_int serial, minimum;
	int refresh, retry, expire;
	
	if((rc = dn_expand(response.buf, eom, tcp, (void *)mname, MAXDNAME)) < 0)
	  break;
	tcp += rc;
	
	if((rc = dn_expand(response.buf, eom, tcp, (void *)rname, MAXDNAME)) < 0)
	  break;
	tcp += rc;
	
	if (tcp - cp + INT32SZ * 5 > r_len)
	  break;
      
	NS_GET32(serial, tcp);
	NS_GET32(refresh, tcp);
	NS_GET32(retry, tcp);
	NS_GET32(expire, tcp);
	NS_GET32(minimum, tcp);
      
	vfields = alloc_small(7, 0);
	Field(vfields, 0) = copy_string(mname);
	Field(vfields, 1) = copy_string(rname);
	Field(vfields, 2) = Val_int(serial);
	Field(vfields, 3) = Val_int(refresh);
	Field(vfields, 4) = Val_int(retry);
	Field(vfields, 5) = Val_int(expire);
	Field(vfields, 6) = Val_int(minimum);
      }
      break;

    case ns_t_minfo: 
      {
	char rmailbx[MAXDNAME+1];
	char emailbx[MAXDNAME+1];

	if((rc = dn_expand(response.buf, eom, tcp, rmailbx, MAXDNAME)) < 0)
	  break;
	tcp += rc;
	if((rc = dn_expand(response.buf, eom, tcp, emailbx, MAXDNAME)) < 0)
	  break;

	vfields = alloc_small(2, 0);
	Field(vfields, 0) = copy_string(rmailbx);
	Field(vfields, 1) = copy_string(emailbx);
      }
      break;

      /* two strings */
    case ns_t_hinfo:
    case ns_t_isdn: /* <ISDN-address> <sa> */
    case ns_t_nsap:
      if(r_len > 0 && *tcp < r_len) {
	value str1;
	value str2;

	rc = *tcp++;
	if(r_type == ns_t_nsap) {
	  int result = 0;
	  for(; rc; rc--, tcp++)
	    result += result * 10 + (*tcp - 0x38);
	  str1 = Val_int(result);
	}
	else {
	  str1 = caml_alloc_string(rc);
	  memmove(String_val(str1), tcp, rc);
	  tcp += rc;
	}      
	if(rc + 1 > r_len && *tcp + rc + 2 >= r_len) {
	  rc = *tcp++;
	  str2 = caml_alloc_string(rc);
	  memmove(String_val(str2), tcp, rc);
	}
	else
	  str2 = copy_string("");

	vfields = caml_alloc_small(2, 0);
	Field(vfields, 0) = str1;
	Field(vfields, 1) = str2;
      }
      break;

    case ns_t_wks:
      
      if(INADDRSZ + 1 <= r_len) {
	struct in_addr inaddr;
	char* address;
	u_short protocol;
	value bitmap;

	bcopy(tcp, (char *) &inaddr, INADDRSZ);
	address = (char*) inet_ntoa(inaddr);
	tcp += INADDRSZ;
      
	protocol = *tcp++;  /* getprotobynumber(*cp) */
      
	/*
	  n = 0;
	  while (cp < eom) {
	  c = *cp++;
	  do {
	  if (c & 0200) {
	  int port;
	  
	  port = htons((u_short)n);
	  if (protocol != NULL)
	  service = getservbyport(port, protocol->p_name);
	  else
	  service = NULL;
	  
	  if (service != NULL)
	  doprintf((" %s", service->s_name));
	  else
	  doprintf((" %s", dtoa(n)));
	}
	c <<= 1;
	} while (++n & 07);
	}
	doprintf((" )"));
	*/
      
	bitmap = caml_alloc_string(r_len - INADDRSZ - 1);
	memmove(String_val(bitmap), tcp, eom - tcp);
      
	vfields = alloc_small(4, 0);
	Field(vfields, 0) = copy_string(address);
	Field(vfields, 1) = Val_int(protocol);
	Field(vfields, 2) = bitmap;
      }
      break;

    case ns_t_rp:  /* <mbox-dname> <txt-dname> */
      {
	char rname1[MAXDNAME+1];
	char rname2[MAXDNAME+1];

	rc = dn_expand(response.buf, eom, tcp, rname1, MAXDNAME);
	if(rc < 0)
	  break;
	tcp += rc;
	
	rc = dn_expand(response.buf, eom, tcp, rname2, MAXDNAME);
	if(rc < 0)
	  break;

	vfields = alloc_small(2, 0);
	Field(vfields, 0) = copy_string(rname1);
	Field(vfields, 1) = copy_string(rname2);
      }
      break;

    case ns_t_x25: /* <PSDN-address> */
      if(r_len > 0 && *tcp >= r_len) {
	rc = *tcp++;
	vfields = caml_alloc_string(rc);
	memmove(String_val(vfields), tcp, rc);
      }
      else
	vfields = copy_string("");
      break;
      

    case ns_t_px:
      if(r_len > INT16SZ) {
	int pref;
	char rname1[MAXDNAME];
	char rname2[MAXDNAME];

	NS_GET16(pref, tcp);
	rc = dn_expand(response.buf, eom, tcp, rname1, MAXDNAME);
	if(rc < 0)
	  break;
	tcp += rc;
	rc = dn_expand(response.buf, eom, tcp, rname2, MAXDNAME);
	if(rc < 0)
	  break;
	tcp += rc;

	vfields = alloc_small(2, 0);
	Field(vfields, 0) = copy_string(rname1);
	Field(vfields, 1) = copy_string(rname2);
      }
      break;

    case ns_t_gpos:
      if(r_len > 0 && *tcp <= r_len) {
	float f1, f2, f3;
	char *tmp;
	rc = *tcp++;

	tmp = (char *) malloc(rc + 1);
	bcopy(tcp, tmp, rc);
	tmp[rc] = '\0';
	f1 = atof(tmp);
	tcp += rc;
	
	if(tcp < eom && tcp + *tcp <= eom) {
	  if(*tcp > rc)
	    tmp = realloc(tmp, *tcp);
	  rc = *tcp++;
	  bcopy(tcp, tmp, rc);
	  tmp[rc] = '\0';
	  f2 = atof(tmp);
	  tcp += rc;
	}
	else
	  f2 = 0.0;

	if(tcp < eom && tcp + *tcp <= eom) {
	  if(*tcp > rc)
	    tmp = realloc(tmp, *tcp);
	  rc = *tcp++;
	  bcopy(tcp, tmp, rc);
	  tmp[rc] = '\0';
	  f3 = atof(tmp);
	  tcp += rc;
	}
	else
	  f3 = 0.0;

	free(tmp);

	vfields = alloc_small(3, 0);
	Field(vfields, 0) = copy_double((double)f1);
	Field(vfields, 1) = copy_double((double)f2);
	Field(vfields, 2) = copy_double((double)f3);
      }	
      break;

    case ns_t_loc:
      failwith("LOC not implemented");

      /*
      if(r_len > 0 && *tcp != 0)
	failwith("Invalid version in LOC RDATA");

      if(r_len > 0) {
	rc = INT
      n = INT32SZ + 3*INT32SZ;
      if (check_size(rname, type, cp, msg, eor, n) < 0)
	break;
      c = _getlong(cp);
      cp += INT32SZ;
      
      n = _getlong(cp);
      doprintf(("\t%s ", pr_spherical(n, "N", "S")));
      cp += INT32SZ;
      
      n = _getlong(cp);
      doprintf((" %s ", pr_spherical(n, "E", "W")));
      cp += INT32SZ;
      
      n = _getlong(cp);
      doprintf((" %sm ", pr_vertical(n, "", "-")));
      cp += INT32SZ;
      
      doprintf((" %sm", pr_precision((c >> 16) & 0xff)));
      doprintf((" %sm", pr_precision((c >>  8) & 0xff)));
      doprintf((" %sm", pr_precision((c >>  0) & 0xff)));
      break;
      */

      /*
    case T_UID:
    case T_GID:
      if(INT32SZ <= r_len)
	NS_GET32(rc, cp);
      
      if (dlen == INT32SZ) {
        n = _getlong(cp);
	doprintf(("\t%s", dtoa(n)));
	cp += INT32SZ;
      }
      break;
      
    case T_UINFO:
      doprintf(("\t\"%s\"", stoa(cp, dlen, TRUE)));
      cp += dlen;
      break;

    case T_UNSPEC:
      cp += dlen;
      break;
      
    case T_AAAA:
      if (dlen == IPNGSIZE) {
	doprintf(("\t%s", ipng_ntoa(cp)));
	cp += IPNGSIZE;
      }
      break;
      
    case T_SIG:
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      if (n >= T_FIRST && n <= T_LAST)
	doprintf(("\t%s", pr_type(n)));
      else
	doprintf(("\t%s", dtoa(n)));
      cp += INT16SZ;
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
      doprintf((" %s", dtoa(n)));
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
      doprintf((" %s", dtoa(n)));
      
      n = 3*INT32SZ + INT16SZ;
      if (check_size(rname, type, cp, msg, eor, n) < 0)
	break;
      doprintf((" ("));
      
      n = _getlong(cp);
      doprintf(("\n\t\t\t%s", dtoa(n)));
      doprintf(("\t\t;original ttl"));
      cp += INT32SZ;
      
      n = _getlong(cp);
      doprintf(("\n\t\t\t%s", pr_date(n)));
      doprintf(("\t;signature expiration"));
      cp += INT32SZ;
      
      n = _getlong(cp);
      doprintf(("\n\t\t\t%s", pr_date(n)));
      doprintf(("\t;signature inception"));
      cp += INT32SZ;
      
      n = _getshort(cp);
      doprintf(("\n\t\t\t%s", dtoa(n)));
      doprintf(("\t\t;key tag"));
      cp += INT16SZ;
      
      n = expand_name(rname, type, cp, msg, eom, dname);
      if (n < 0)
	break;
      doprintf(("\n\t\t\t%s", pr_name(dname)));
      cp += n;
      
      if (cp < eor) {
	register char *buf;
	register int size;
	  
	n = eor - cp;
	buf = base_ntoa(cp, n);
	size = strlength(buf);
	cp += n;
	  
	while ((n = (size > 64) ? 64 : size) > 0) {
	  doprintf(("\n\t%s", stoa((u_char *)buf, n, FALSE)));
	  buf += n; size -= n;
	}
      }
      doprintf(("\n\t\t\t)"));
      break;
      
    case T_KEY:
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      doprintf(("\t0x%s", xtoa(n)));
      cp += INT16SZ;
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
        doprintf((" %s", dtoa(n)));
	
        if (check_size(rname, type, cp, msg, eor, 1) < 0)
	  break;
        n = *cp++;
        doprintf((" %s", dtoa(n)));
	
        if (cp < eor) {
	  register char *buf;
	  register int size;
	    
	  n = eor - cp;
	  buf = base_ntoa(cp, n);
	  size = strlength(buf);
	  cp += n;
	    
	  doprintf((" ("));
	  while ((n = (size > 64) ? 64 : size) > 0) {
	    doprintf(("\n\t%s", stoa((u_char *)buf, n, FALSE)));
	    buf += n; size -= n;
	  }
	  doprintf(("\n\t\t\t)"));
	}
        break;
	
    case T_NXT:
      n = expand_name(rname, type, cp, msg, eom, dname);
      if (n < 0)
	break;
      doprintf(("\t%s", pr_name(dname)));
      cp += n;
      
      n = 0;
      while (cp < eor) {
	c = *cp++;
	do {
	  if (c & 0200) {
	    if (n >= T_FIRST && n <= T_LAST)
	      doprintf((" %s", pr_type(n)));
	    else
	      doprintf((" %s", dtoa(n)));
                }
	  c <<= 1;
	} while (++n & 07);
      }
      break;
      
    case T_NAPTR:
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      doprintf(("\t%s", dtoa(n)));
      cp += INT16SZ;
      
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      doprintf((" %s", dtoa(n)));
      cp += INT16SZ;
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
      doprintf((" \"%s\"", stoa(cp, n, TRUE)));
      cp += n;
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
      doprintf((" \"%s\"", stoa(cp, n, TRUE)));
      cp += n;
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
      doprintf((" \"%s\"", stoa(cp, n, TRUE)));
      cp += n;
      
      n = expand_name(rname, type, cp, msg, eom, dname);
      if (n < 0)
	break;
      doprintf((" %s", pr_name(dname)));
      cp += n;
      break;
      
    case T_KX:
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      doprintf(("\t%s", dtoa(n)));
      cp += INT16SZ;
      
      n = expand_name(rname, type, cp, msg, eom, dname);
      if (n < 0)
	break;
      doprintf((" %s", pr_name(dname)));
      cp += n;
      break;
      
    case T_CERT:
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      doprintf(("\t%s", dtoa(n)));
      cp += INT16SZ;
      
      if (check_size(rname, type, cp, msg, eor, INT16SZ) < 0)
	break;
      n = _getshort(cp);
      doprintf((" %s", dtoa(n)));
      cp += INT16SZ;
      
      if (check_size(rname, type, cp, msg, eor, 1) < 0)
	break;
      n = *cp++;
      doprintf((" %s", dtoa(n)));
      
      if (cp < eor) {
	register char *buf;
	register int size;
	
	n = eor - cp;
	buf = base_ntoa(cp, n);
	size = strlength(buf);
	cp += n;
	
	doprintf((" ("));
	while ((n = (size > 64) ? 64 : size) > 0) {
	  doprintf(("\n\t%s", stoa((u_char *)buf, n, FALSE)));
	  buf += n; size -= n;
	}
	doprintf(("\n\t\t\t)"));
      }
      break;

    case T_EID:
      failwith("EID not implemented");
      break;

    case T_NIMLOC:
      failwith("NIMLOC not implemented");
      break;

    case T_ATMA:
      failwith("ATMA not implemented");

      */

    default:
      failwith("unknown RDATA type");
    }

    if(vfields != Val_unit) {
      value vrecord, vrdata, newcons;

      Begin_root(vres);

      vrecord = alloc_small(5, 0);
      Field(vrecord, 0) = copy_string(r_name);
      Field(vrecord, 1) = c_to_mlvariant(rr_type, r_type);
      Field(vrecord, 2) = c_to_mlvariant(rr_class, r_class);
      Field(vrecord, 3) = Val_int(r_ttl);
      vrdata = alloc_small(2, 0);
      Field(vrdata, 0) = c_to_mlvariant(rr_type, r_type);
      Field(vrdata, 1) = vfields;
      Field(vrecord, 4) = vrdata;

      newcons = alloc_small(2, 0);
      Field(newcons, 0) = vrecord;
      Field(newcons, 1) = vres;
      vres = newcons;
      End_roots();
      vrdata = Val_unit;
    }
    cp += r_len;
  }
  return vres;
}
