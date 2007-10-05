(*
  sayshell.umd.edu. A     128.8.1.14
                     MX    10 sayshell.umd.edu.
                     HINFO NeXT UNIX
                     WKS   128.8.1.14 tcp ftp telnet smtp
                     RP    louie.trantor.umd.edu.  LAM1.people.umd.edu.
                                                                                
   LAM1.people.umd.edu. TXT (
         "Louis A. Mamakos, (301) 454-2946, don't call me at home!" )
*)

open Mlresolv

let _ =
   let print_result rr =
      List.iter (fun rr ->
		    match rr.rr_rdata with
		       | `NS domain ->
			    print_endline ("NS: " ^ domain)
		       | `CNAME domain ->
			    print_endline ("CNAME: " ^ domain)
		       | `A domain ->
			    print_endline ("A: " ^ domain)
		       | `PTR domain ->
			    print_endline ("PTR: " ^ domain)
		       | `TXT txts ->
			    print_endline ("TXT: " ^ 
					      (String.concat "\n\t" txts))
			       
		       | `SRV (pref, weight, port, domain) ->
			    Printf.printf "SRV: %d %d %d %s\n" 
			       pref weight port domain
		       | `MX (pref, domain) ->
			    Printf.printf "MX: %d %s\n" pref domain
		       | `SOA (mname, rname, 
				    serial, refresh, retry, expire, minimum) ->
			    Printf.printf "SOA: %s %s %u %d %d %d %u\n"
			       mname rname serial refresh retry expire minimum
		       | _ ->
			    print_endline "other"
		) rr
   in
   let req = ["A",     `IN, `A, "ya.ru";
	      "NS",    `IN, `NS, "ya.ru";
	      "CNAME", `IN, `CNAME, "rss.jabber.ru";
	      "PTR",   `IN, `PTR, "194.87.0.50";
	      "MX",    `IN, `MX, "ya.ru";
	      "TXT",   `IN, `TXT, "_xmppconnect.jabber.ru";
	      "SRV",   `IN, `SRV, "_xmpp-server._tcp.ya.ru";
	      "SRV",   `IN, `SRV, "_xmpp-server._tcp.ya.ru";
	      "SOA",   `IN, `SOA, "ya.ru";
	     ]
   in
      List.iter(fun (text, r_class, r_type, dname) ->
		   print_endline ("Testing: " ^ text);
		   let rr = query dname r_class r_type in
		      print_result rr;
		      print_newline ();
		      flush stdout;
	       ) req
