module T = Gdal_c.C.Type
module F = Gdal_c.C.Functions

module Error = struct
  type t = [ `Debug | `Warning | `Failure ]
end

let register = Lazy.from_fun F.all_register

module Datatype = struct
  type t = T.Datatype.t
  (** @include *)

  let size v = F.Datatype.get_size v
  let signed v = F.Datatype.is_signed v
end

module Driver = struct
  type t = unit Ctypes_static.ptr

  let get_by_name s =
    Lazy.force register;
    let driver = F.Driver.get_by_name s in
    if Ctypes.is_null driver then Error (`Msg "Could not find driver")
    else Ok driver

  let get_count =
    Lazy.force register;
    F.Driver.get_count

  let description = F.description
end

module RasterBand = struct
  type t = unit Ctypes_static.ptr

  let x_size = F.RasterBand.x_size
  let y_size = F.RasterBand.y_size

  let block_size v =
    let open Ctypes in
    let x = allocate int 0 in
    let y = allocate int 0 in
    F.RasterBand.block_size v x y;
    (!@x, !@y)
end

module Dataset = struct
  type t = unit Ctypes_static.ptr
  type access = T.Access.t

  let open' file access : t = F.Dataset.open' file access
  let close = F.Dataset.close
  let description v = F.description v

  let geo_transform v =
    let open Ctypes in
    let arr =
      bigarray_of_array array1 Bigarray.float64 (CArray.make double 6)
    in
    let addr = bigarray_start array1 in
    match F.Dataset.geo_transform v (addr arr) with
    | `None -> Ok arr
    | (`Debug | `Warning | `Failure) as e -> Error e

  let translate ~dst ~options dataset =
    let open Ctypes in
    let arr = CArray.of_list string options in
    let options = F.Dataset.translate_options_new (CArray.start arr) null in
    let x = allocate int 0 in
    F.Dataset.translate dst dataset options x

  let raster_band = F.Dataset.raster_band
  let raster_count = F.Dataset.raster_count
  let raster_x_size = F.Dataset.raster_x_size
  let raster_y_size = F.Dataset.raster_y_size
end
