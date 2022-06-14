open Ctypes

module Types (F : TYPE) = struct
  open F

  module Error = struct
    let none = constant "CE_None" int64_t
    let debug = constant "CE_Debug" int64_t
    let warning = constant "CE_Warning" int64_t
    let failure = constant "CE_Failure" int64_t

    type t = [ `None | `Debug | `Warning | `Failure ]

    let t : t F.typ =
      enum ~typedef:true "CPLErr"
        [
          (`None, none);
          (`Debug, debug);
          (`Warning, warning);
          (`Failure, failure);
        ]
        ~unexpected:(fun _ -> `Failure)
  end

  module Datatype = struct
    let unknown = constant "GDT_Unknown" int64_t
    let byte = constant "GDT_Byte" int64_t
    let uint_16 = constant "GDT_UInt16" int64_t
    let int_16 = constant "GDT_Int16" int64_t
    let uint_32 = constant "GDT_UInt32" int64_t
    let int_32 = constant "GDT_Int32" int64_t
    let uint_64 = constant "GDT_UInt64" int64_t
    let int_64 = constant "GDT_Int64" int64_t
    let float_32 = constant "GDT_Float32" int64_t
    let float_64 = constant "GDT_Float64" int64_t
    let cint_16 = constant "GDT_CInt16" int64_t
    let cint_32 = constant "GDT_CInt32" int64_t
    let cfloat_32 = constant "GDT_CFloat32" int64_t
    let cfloat_64 = constant "GDT_CFloat64" int64_t
    let type_count = constant "GDT_TypeCount" int64_t

    type t =
      [ `Unknown
      | `Byte
      | `Uint_16
      | `Int_16
      | `Uint_32
      | `Int_32
      | `Uint_64
      | `Int_64
      | `Float_32
      | `Float_64
      | `Cint_16
      | `Cint_32
      | `Cfloat_32
      | `Cfloat_64
      | `TypeCount ]

    let t : t F.typ =
      enum ~typedef:true "GDALDataType"
        [
          (`Unknown, unknown);
          (`Byte, byte);
          (`Uint_16, uint_16);
          (`Int_16, int_16);
          (`Uint_32, uint_32);
          (`Int_32, int_32);
          (`Uint_64, uint_64);
          (`Int_64, int_64);
          (`Float_32, float_32);
          (`Float_64, float_64);
          (`Cint_16, cint_16);
          (`Cint_32, cint_32);
          (`Cfloat_32, cfloat_32);
          (`Cfloat_64, cfloat_64);
          (`TypeCount, type_count);
        ]
        ~unexpected:(fun _ -> `Unknown)
  end

  module Access = struct
    type t = RO | RW

    let ro = constant "GA_ReadOnly" int64_t
    let rw = constant "GA_Update" int64_t

    let t : t F.typ =
      enum ~typedef:true "GDALAccess"
        [ (RO, ro); (RW, rw) ]
        ~unexpected:(fun _ -> RO)
  end

  module RasterBand = struct
    let t = typedef (ptr void) "GDALRasterBandH"
  end

  module Dataset = struct
    let t = typedef (ptr void) "GDALDatasetH"

    type translate_options

    let translate_options : translate_options Ctypes_static.structure F.typ =
      let s = structure "GDALTranslateOptions" in
      typedef s "GDALTranslateOptions"
  end

  module Driver = struct
    let t = typedef (ptr void) "GDALDriverH"
  end
end
