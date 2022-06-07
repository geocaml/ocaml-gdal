open Ctypes
module T = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

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
  end

  module RasterBand = struct
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

    let raster_count =
      foreign "GDALGetRasterCount" (T.Dataset.t @-> returning int)

    let raster_band =
      foreign "GDALGetRasterBand"
        (T.Dataset.t @-> int @-> returning (ptr_opt void))

    let raster_x_size =
      foreign "GDALGetRasterXSize" (T.Dataset.t @-> returning int)

    let raster_y_size =
      foreign "GDALGetRasterYSize" (T.Dataset.t @-> returning int)
  end
end
