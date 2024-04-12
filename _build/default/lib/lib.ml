module Sig = Sig
module Query = Query
module Sum = Query.Make (Nodesum)
module MaxOcc = Query.Make (Nodemaxocc)
module MaxSubSeg = Query.Make (Nodemaxsubseg)
