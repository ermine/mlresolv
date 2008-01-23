open Mlresolv

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

let _ =
   let t = Sys.argv.(1) in
   let host = Sys.argv.(2) in

   let r_class = `IN in
   let r_type =
      match t with
	 | "srv" -> `SRV
	 | "a" -> `A
	 | "ns" -> `NS
	 | "cname" -> `CNAME
	 | "ptr" -> `PTR
	 | "mx" -> `MX
	 | "txt" -> `TXT
	 | "soa" -> `SOA
	 | _ -> `A
   in
   let rr = query host r_class r_type in
      print_result rr

