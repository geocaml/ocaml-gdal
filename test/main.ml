let () =
  print_int @@ Gdal.Datatype.size `Int_32;
  print_endline "";
  (print_int @@ Gdal.Driver.(get_count ()));
  let _mem = Gdal.Driver.get_by_name "MEM" in
  ()
