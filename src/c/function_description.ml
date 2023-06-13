open Ctypes
module T = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let all_register = foreign "GDALAllRegister" (void @-> returning void)
  let description = foreign "GDALGetDescription" (ptr void @-> returning string)

  module Datatype = struct
    let get_size =
      foreign "GDALGetDataTypeSizeBytes" (T.Datatype.t @-> returning int)

    let is_signed =
      foreign "GDALDataTypeIsSigned" (T.Datatype.t @-> returning bool)
  end

  module Driver = struct
    let get_by_name =
      foreign "GDALGetDriverByName" (string @-> returning T.Driver.t)

    let get_count = foreign "GDALGetDriverCount" (void @-> returning int)

    let create = foreign "GDALCreate" (T.Driver.t @-> string @-> int @-> int @-> int @-> T.Datatype.t @-> ptr string @-> returning T.Dataset.t)
  end

  module RasterBand = struct
    (* TODO: Not the right write I think... *)
    let write = foreign "GDALWriteBlock" (T.RasterBand.t @-> int @-> int @-> ptr float @-> returning T.Error.t)

    let x_size =
      foreign "GDALGetRasterBandXSize" (T.RasterBand.t @-> returning int)

    let y_size =
      foreign "GDALGetRasterBandYSize" (T.RasterBand.t @-> returning int)

    let block_size =
      foreign "GDALGetBlockSize"
        (T.RasterBand.t @-> ptr int @-> ptr int @-> returning void)
  end

  module Dataset = struct
    let open' =
      foreign "GDALOpen" (string @-> T.Access.t @-> returning T.Dataset.t)

    let close = foreign "GDALClose" (T.Dataset.t @-> returning void)

    let geo_transform =
      foreign "GDALGetGeoTransform"
        (T.Dataset.t @-> ptr double @-> returning T.Error.t)

    let translate =
      foreign "GDALTranslate"
        (string @-> T.Dataset.t
        @-> ptr T.Dataset.translate_options
        @-> ptr int @-> returning T.Dataset.t)

    let translate_options_new =
      foreign "GDALTranslateOptionsNew"
        (ptr string @-> ptr void @-> returning (ptr T.Dataset.translate_options))

    let raster_count =
      foreign "GDALGetRasterCount" (T.Dataset.t @-> returning int)

    let raster_band =
      foreign "GDALGetRasterBand"
        (T.Dataset.t @-> int @-> returning (ptr_opt void))

    let raster_x_size =
      foreign "GDALGetRasterXSize" (T.Dataset.t @-> returning int)

    let raster_y_size =
      foreign "GDALGetRasterYSize" (T.Dataset.t @-> returning int)

    let set_geo_transform =
      foreign "GDALSetGeoTransform" (T.Dataset.t @-> ptr float @-> returning T.Error.t)

    let set_projection =
      foreign "GDALSetProjection" (T.Dataset.t @-> string @-> returning T.Error.t)
  end
end
