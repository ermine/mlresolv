open Ocamlbuild_plugin
open Myocamlbuild_config

let _ =  dispatch begin function
  | After_rules ->
      make_binding ~lib:"-lc" "mlresolv";
      
      install_lib "mlresolv" ["libmlresolv.a"; "dllmlresolv.so"]
        
  | _ ->
      ()
end
