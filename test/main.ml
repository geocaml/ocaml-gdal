open Gdal

let () =
  Printf.printf "Registered Drivers: %i\n" (Driver.get_count ());
  match Driver.get_by_name "MEM" with
  | Ok driver ->
      Printf.printf "Driver info: %s\n" (Driver.description driver);
      let dataset = Dataset.open' "./test/data/tinymarble.png" RO in
      Printf.printf "Dataset info: %s\n" (Dataset.description dataset)
  | Error (`Msg m) -> failwith m
