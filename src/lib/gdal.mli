module T = Gdal_c.C.Type
module F = Gdal_c.C.Functions

module Error : sig
  type t = [ `Debug | `Warning | `Failure ]

  val to_msg : t -> [ `Msg of string ]
end

module Datatype : sig
  type t = T.Datatype.t
  (** The different pixel data types *)

  val size : t -> int
  (** [size b] returns the size of the type in bytes. *)

  val signed : t -> bool
  (** [signed t] returns whether or not the data type is signed. *)
end

module RasterBand : sig
  type t

  val write : x:int -> y:int -> t -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t -> (unit, T.Error.t) result

  val x_size : t -> int
  (** [x_size t] returns the XSize of the raster band [t]. *)

  val y_size : t -> int
  (** [y_size t] returns the YSize of the raster band [t]. *)

  val block_size : t -> int * int
  (** [block_size t] returns the natural block size of the band [t]. *)
end

module Dataset : sig
  type t
  (** A set of associated raster bands, usually from one file. *)

  type access = T.Access.t
  (** How to access a dataset when opening it. *)

  val open' : string -> access -> t
  (** [open' file access] opens the dataset in [file] with the corresponding level of [access]. *)

  val close : t -> unit
  (** [close d] closes the dataset [d]. *)

  val description : t -> string
  (** The objects description *)

  val geo_transform :
    t ->
    ( (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t,
      Error.t )
    result
  (** Fetch the affine transformation coefficients. *)

  val translate : dst:string -> options:string list -> t -> t
  (* Converts raster data between different formats. *)

  (** {3 Raster Bands}*)

  val raster_count : t -> int
  (** [raster_count d] gets the number of raster bands for the dataset [d]. *)

  val raster_band : t -> int -> RasterBand.t option
  (** [raster_band d i] gets the band object for a dataset. *)

  val raster_x_size : t -> int
  (** The raster width in pixels. *)

  val raster_y_size : t -> int
  (** The raster height in pixels. *)

  val set_geo_transform : t -> float list -> (unit, Error.t) result
  (** Set the affine transformation coefficients *)

  val set_projection : t -> string -> (unit, Error.t) result
  (** Set the projection reference string *)
end

module Driver : sig
  type t
  (** Drivers roughly correspond to file formats. *)

  val get_by_name : string -> (t, [> `Msg of string ]) result
  (** [get_by_name name] tries to find the driver associated with [name] (e.g. ["MEM"]). *)

  val get_count : unit -> int
  (** [get_count ()] gets the number of registered drivers. *)

  val description : t -> string
  (** The objects description *)

  val create :
    ?options:string list ->
    sw:Eio.Switch.t ->
    filename:string ->
    xsize:int ->
    ysize:int ->
    bands:int ->
    typ:Datatype.t ->
    t -> Dataset.t
    (** Create a new dataset from a driver, the dataset will be closed when the switch also closes. *)
end

